use futures::{
    channel::oneshot::{channel, Receiver, Sender},
    future::{select, BoxFuture, Either},
    FutureExt, SinkExt, StreamExt,
};
use std::{
    any::Any,
    fmt::Debug,
    future::Future,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

use crate::par::{parse::Loc, types::Type};

use super::{compiler::TypedTree, Net, Tree};

#[derive(Default)]
pub struct CoroState {
    pub(crate) net: Arc<Mutex<Net>>,
    new_redex: Mutex<Vec<futures::channel::mpsc::Sender<()>>>,
    new_var: AtomicUsize,
}

impl Debug for CoroState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CoroState")
            .field("net", &self.net)
            .field("new_redex", &self.new_redex)
            .field("new_var", &self.new_var)
            .finish_non_exhaustive()
    }
}

struct WaitingOutsideExt(Tree, async_watch::Receiver<bool>);
struct WaitingInsideExt(Tree, async_watch::Sender<bool>);
struct ReadbackExt(Sender<PortContents>);

#[derive(Default, Clone, Debug)]
pub struct SharedState {
    pub(crate) shared: Arc<CoroState>,
}

#[derive(Debug)]
pub enum PortContents {
    Aux(Sender<PortContents>),
    AuxLazy(Sender<PortContents>),
    AuxLazyPicked(usize),

    // Waiting :)
    Waiting(Tree, async_watch::Receiver<bool>),
    // A tree, which should be readback further, if necessary
    Tree(Tree),
}

fn do_copy(
    net: &mut Net,
    root: Tree,
    a: Tree,
    b: Tree,
    cons: fn(Tree, Tree) -> Tree,
    build: impl Fn(Tree) -> Tree,
) {
    let (v0, v1) = net.create_wire();
    let (w0, w1) = net.create_wire();
    net.link(root, cons(v0, w0));
    net.link(a, build(v1));
    net.link(b, build(w1));
}

impl SharedState {
    pub fn with_net(net: Arc<Mutex<Net>>) -> Self {
        Self {
            shared: Arc::new(CoroState {
                net,
                new_redex: Default::default(),
                new_var: Default::default(),
            }),
        }
    }
    pub fn make_oneshot_ext_from_tx(tx: Sender<PortContents>) -> Tree {
        Tree::ext(
            move |net, tree, ext| {
                let ext: Box<ReadbackExt> = ext.downcast().unwrap();
                let tx = ext.0;
                match tree {
                    Err(ext) => {
                        if ext.is::<WaitingOutsideExt>() {
                            let ext: Box<WaitingOutsideExt> = ext.downcast().unwrap();
                            tx.send(PortContents::Waiting(ext.0, ext.1)).unwrap();
                        } else if ext.is::<PortContents>() {
                            tx.send(*ext.downcast().unwrap()).unwrap();
                        } else {
                            unreachable!()
                        }
                    }
                    Ok(Tree::Ext(f, ext)) => {
                        if ext.is::<WaitingOutsideExt>() {
                            let ext: Box<WaitingOutsideExt> = ext.downcast().unwrap();
                            tx.send(PortContents::Waiting(ext.0, ext.1)).unwrap();
                        } else if ext.is::<WaitingInsideExt>() {
                            f(net, Ok(Self::make_oneshot_ext_from_tx(tx)), ext);
                        } else if ext.is::<ReadbackExt>() {
                            f(net, Err(Box::new(PortContents::Aux(tx))), ext);
                        } else {
                            unreachable!()
                        }
                    }
                    Ok(tree) => {
                        tx.send(PortContents::Tree(tree)).unwrap();
                    }
                }
            },
            ReadbackExt(tx),
        )
    }
    pub fn make_oneshot_ext(&self) -> (Tree, Receiver<PortContents>) {
        let (tx, rx) = channel();
        let tree = Self::make_oneshot_ext_from_tx(tx);
        (tree, rx)
    }
    pub async fn expand_once(&self, tree: Tree) -> Tree {
        struct ExpandOnceExt(Tree);

        fn expand_once_f(
            net: &mut Net,
            tree: Result<Tree, Box<dyn Any + Send + Sync>>,
            ext: Box<dyn Any + Send + Sync>,
        ) {
            let ext = ext.downcast::<ExpandOnceExt>().unwrap();
            match tree {
                Ok(Tree::Ext(f, data)) => f(net, Err(ext), data),
                Ok(Tree::Era) => net.link(ext.0, Tree::e()),
                Ok(Tree::Con(a, b)) => {
                    do_copy(net, ext.0, *a, *b, Tree::c, |w| {
                        Tree::ext(expand_once_f, ExpandOnceExt(w))
                    });
                }
                Ok(Tree::Dup(a, b)) => {
                    do_copy(net, ext.0, *a, *b, Tree::d, |w| {
                        Tree::ext(expand_once_f, ExpandOnceExt(w))
                    });
                }
                Ok(Tree::Package(id)) => {
                    let package = net.dereference_package(id);
                    net.link(package, ext.0);
                }
                Ok(tree) => {
                    net.link(tree, ext.0);
                }
                Err(_) => unreachable!(),
            }
        }
        let (v0, v1) = self.shared.net.lock().unwrap().create_wire();
        self.add_redex(tree, Tree::ext(expand_once_f, ExpandOnceExt(v1)))
            .await;
        v0
    }
    // this is async because it will only work when ran under an async context.
    pub async fn add_redex(&self, a: Tree, b: Tree) {
        self.shared.net.lock().unwrap().link(a, b);
        let notify_tgts = self.shared.new_redex.lock().unwrap().clone();
        for mut i in notify_tgts {
            let _ = i.send(()).await;
        }
    }
    pub async fn read_port(&self, tree: Tree) -> PortContents {
        let (ext, rx) = self.make_oneshot_ext();
        self.add_redex(ext, tree).await;
        rx.await.unwrap()
    }
    pub async fn read_port_as_tree(&self, tree: Tree) -> Tree {
        let (ext, rx) = self.make_oneshot_ext();
        self.add_redex(ext, tree).await;
        match rx.await.unwrap() {
            PortContents::Aux(sender) | PortContents::AuxLazy(sender) => {
                Self::make_oneshot_ext_from_tx(sender)
            }
            PortContents::Tree(tree) => tree,
            _ => unreachable!(),
        }
    }
    pub async fn as_con(&self, tree: Tree) -> (Tree, Tree) {
        match tree {
            Tree::Con(a, b) => (*a, *b),
            other => {
                // eta expand
                let ((v0, v1), (w0, w1)) = {
                    let mut net = self.shared.net.lock().unwrap();
                    (net.create_wire(), net.create_wire())
                };
                self.add_redex(other, Tree::c(v0, w0)).await;
                (v1, w1)
            }
        }
    }
    pub async fn as_era(&self, tree: Tree) -> () {
        match tree {
            Tree::Era => (),
            _ => todo!("TODO: Eta-reduce trees that are eta-equivalent to erasers"),
        }
    }
    /// Eagerly read back a CON node.
    /// If we don't get a CON node, eta-expand and return the readback of that.
    pub async fn read_con(&self, tree: Tree) -> (Tree, Tree) {
        let tree = self.read_port_as_tree(tree).await;
        self.as_con(tree).await
    }
    pub async fn read_era(&self, tree: Tree) -> () {
        let tree = self.read_port_as_tree(tree).await;
        self.as_era(tree).await
    }
    pub async fn as_par(&self, tree: Tree) -> (Tree, Tree) {
        self.as_con(tree).await
    }
    pub async fn as_times(&self, tree: Tree) -> (Tree, Tree) {
        self.as_con(tree).await
    }
    pub fn flatten_multiplexed(&self, tree: Tree, len: usize) -> BoxFuture<Vec<Tree>> {
        use futures::future::FutureExt;
        async move {
            if len == 0 {
                self.read_era(tree).await;
                vec![]
            } else if len == 1 {
                vec![tree]
            } else {
                let fst_len = len / 2;
                let snd_len = len - fst_len;
                let (fst, snd) = self.read_con(tree).await;
                [
                    self.flatten_multiplexed(fst, fst_len).await,
                    self.flatten_multiplexed(snd, snd_len).await,
                ]
                .concat()
            }
        }
        .boxed()
    }
    pub async fn as_either<Name: Debug + Clone>(
        &self,
        tree: Tree,
        variants: Vec<Name>,
    ) -> (Name, Tree) {
        // TODO: we might have to eta-reduce `context` here
        // because it's possible that it's link to the context variable in the variant is eta-expnded
        // A less brittle solution is to make this interact with a `choice`
        let tree_s = self.shared.net.lock().unwrap().show_tree(&tree);
        let variants_s = variants.clone();
        let (_context, possibilities) = self.as_con(tree).await;
        let possibilities = self
            .flatten_multiplexed(possibilities, variants.len())
            .await;
        let mut name_payload = None;
        for (possible, name) in possibilities.into_iter().zip(variants) {
            if let Tree::Con(_ctx, b) = self.read_port_as_tree(possible).await {
                name_payload = Some((name, *b));
            }
        }
        name_payload
            .unwrap_or_else(|| panic!("Couldn't readback *either* {}; {:?}", tree_s, variants_s))
    }
    pub async fn as_choice<Name>(
        &self,
        tree: Tree,
        variants: Vec<Name>,
    ) -> (Tree, Vec<(Name, Tree, Tree)>) {
        let (context, cases) = self.as_con(tree).await;
        let cases = self.flatten_multiplexed(cases, variants.len()).await;
        let mut res = vec![];
        for (case, name) in cases.into_iter().zip(variants) {
            let (ctx, payload) = self.read_con(case).await;
            res.push((name, ctx, payload));
        }
        (context, res)
    }
    pub async fn read_with_type<Name: Clone + Ord + std::hash::Hash + 'static + Debug + Send>(
        &self,
        tree: TypedTree<Name>,
    ) -> ReadbackResult<Name> {
        let ty = tree.ty;
        let port = self.read_port(tree.tree).await;
        self.as_with_type(port, ty).await
    }
    pub fn as_with_type<Name: Clone + Ord + std::hash::Hash + 'static + Debug + Send>(
        &self,
        port: PortContents,
        ty: Type<Loc, Name>,
    ) -> BoxFuture<ReadbackResult<Name>> {
        async move {
            let tree = match port {
                PortContents::Aux(tx) => {
                    let (tx_, rx) = channel();
                    tx.send(PortContents::AuxLazy(tx_)).unwrap();
                    return self.as_with_type(rx.await.unwrap(), ty).await;
                }
                PortContents::AuxLazy(tx) => {
                    let var_id = self.shared.new_var.fetch_add(1, Ordering::AcqRel);
                    tx.send(PortContents::AuxLazyPicked(var_id)).unwrap();
                    return ReadbackResult::Variable(var_id);
                }
                PortContents::AuxLazyPicked(n) => {
                    return ReadbackResult::Variable(n);
                }
                PortContents::Tree(Tree::Package(id)) => {
                    return ReadbackResult::Expand(Tree::Package(id).with_type(ty));
                }
                PortContents::Tree(tree) => tree,
                PortContents::Waiting(tree, rx) => {
                    return ReadbackResult::Waiting(tree.with_type(ty), rx)
                }
            };
            match ty {
                crate::par::types::Type::Send(_, from, to) => {
                    let (a, b) = self.as_par(tree).await;
                    ReadbackResult::Send(
                        b.with_type(*from).into_readback_result_boxed(),
                        a.with_type(*to).into_readback_result_boxed(),
                    )
                }
                crate::par::types::Type::Receive(_, from, to) => {
                    let (a, b) = self.as_par(tree).await;
                    ReadbackResult::Receive(
                        b.with_type((*from).dual()).into_readback_result_boxed(),
                        a.with_type(*to).into_readback_result_boxed(),
                    )
                }
                crate::par::types::Type::Either(_, mut variants) => {
                    variants.sort_keys();
                    let (name, payload) = self
                        .as_either(tree, variants.keys().cloned().collect())
                        .await;
                    ReadbackResult::Either(
                        name.clone(),
                        payload
                            .with_type(variants.get(&name).unwrap().clone())
                            .into_readback_result_boxed(),
                    )
                }
                crate::par::types::Type::Choice(_, mut variants) => {
                    variants.sort_keys();
                    let (ctx, cases) = self
                        .as_choice(tree, variants.keys().cloned().collect())
                        .await;
                    ReadbackResult::Choice(
                        ctx,
                        cases
                            .into_iter()
                            .map(|(name, b, c)| {
                                (
                                    name.clone(),
                                    b,
                                    c.with_type(variants.get(&name).unwrap().clone()),
                                )
                            })
                            .collect(),
                    )
                }

                crate::par::types::Type::Break(_) => {
                    self.read_era(tree).await;
                    ReadbackResult::Break
                }
                crate::par::types::Type::Continue(_) => {
                    self.read_era(tree).await;
                    ReadbackResult::Continue
                }
                ty => ReadbackResult::Unsupported(tree.with_type(ty)),
            }
        }
        .boxed()
    }
    // returns outside and inside
    pub fn create_waiting_ext(&self) -> (Tree, Tree) {
        use crate::icombs::net::Net;
        let (v0, v1) = self.shared.net.lock().as_mut().unwrap().create_wire();
        let (tx, rx) = async_watch::channel(false);
        fn inside_f(
            net: &mut Net,
            tree: Result<Tree, Box<dyn Any + Send + Sync>>,
            ext: Box<dyn Any + Send + Sync>,
        ) {
            let ext = ext.downcast::<WaitingInsideExt>().unwrap();
            match tree {
                Ok(tree) => {
                    let _ = ext.1.send(true);
                    net.link(tree, ext.0);
                }
                Err(_) => unreachable!(),
            }
        }
        fn outside_f(
            net: &mut Net,
            tree: Result<Tree, Box<dyn Any + Send + Sync>>,
            ext: Box<dyn Any + Send + Sync>,
        ) {
            let ext = ext.downcast::<WaitingOutsideExt>().unwrap();
            match tree {
                Ok(Tree::Ext(f, data)) => f(net, Err(ext), data),
                Ok(Tree::Era) => net.link(ext.0, Tree::e()),
                Ok(Tree::Con(a, b)) => {
                    do_copy(net, ext.0, *a, *b, Tree::c, |w| {
                        Tree::ext(outside_f, WaitingOutsideExt(w, ext.1.clone()))
                    });
                }
                Ok(Tree::Dup(a, b)) => {
                    do_copy(net, ext.0, *a, *b, Tree::d, |w| {
                        Tree::ext(outside_f, WaitingOutsideExt(w, ext.1.clone()))
                    });
                }
                Ok(tree) => {
                    net.link(tree, ext.0);
                }
                Err(ext2) => {
                    if ext2.is::<WaitingOutsideExt>() {
                    } else {
                        unreachable!()
                    }
                }
            }
        }
        (
            Tree::ext(outside_f, WaitingOutsideExt(v0, rx)),
            Tree::ext(inside_f, WaitingInsideExt(v1, tx)),
        )
    }
    pub fn create_net_reducer(
        &self,
        mut drop_channel: Receiver<()>,
    ) -> impl Future<Output = ()> + Send + Sync {
        let (tx, mut rx) = futures::channel::mpsc::channel(16);
        self.shared.new_redex.lock().unwrap().push(tx);
        let shared = self.shared.clone();
        async move {
            loop {
                {
                    let mut net = shared.net.lock().unwrap();
                    while net.reduce_one() {}
                }
                match select(drop_channel, rx.next()).await {
                    Either::Left(_) => {
                        return;
                    }
                    Either::Right((a, b)) => {
                        a.unwrap();
                        drop_channel = b;
                    }
                }
            }
        }
    }
}

#[derive(Debug, Default)]
pub enum ReadbackResult<Name: Clone> {
    #[default]
    Break,
    Continue,
    Send(Box<ReadbackResult<Name>>, Box<ReadbackResult<Name>>),
    Receive(Box<ReadbackResult<Name>>, Box<ReadbackResult<Name>>),
    Either(Name, Box<ReadbackResult<Name>>),
    Choice(Tree, Vec<(Name, Tree, TypedTree<Name>)>),
    Expand(TypedTree<Name>),
    Halted(TypedTree<Name>),
    Unsupported(TypedTree<Name>),
    Waiting(TypedTree<Name>, async_watch::Receiver<bool>),
    Variable(usize),
}

impl<Name: Clone> TypedTree<Name> {
    fn into_readback_result(self) -> ReadbackResult<Name> {
        ReadbackResult::Halted(self)
    }
    fn into_readback_result_boxed(self) -> Box<ReadbackResult<Name>> {
        Box::new(self.into_readback_result())
    }
}
