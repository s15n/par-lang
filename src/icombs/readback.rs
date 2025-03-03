use futures::{
    channel::{
        mpsc::{channel as mpsc_channel, Receiver as MpscReceiver, Sender as MpscSender},
        oneshot::{channel, Receiver, Sender},
    },
    executor::LocalPool,
    future::{BoxFuture, LocalBoxFuture},
};
use std::{
    collections::VecDeque,
    sync::atomic::Ordering,
    sync::{atomic::AtomicUsize, Arc, Mutex},
};

use crate::par::parse::Name;

use super::Tree;

pub struct CoroState {
    executor: Mutex<futures::executor::LocalPool>,
    net: Mutex<Option<super::net::Net>>,
}

impl Default for CoroState {
    fn default() -> Self {
        Self {
            executor: Mutex::new(LocalPool::new()),
            net: Default::default(),
        }
    }
}

pub struct SharedState {
    shared: Arc<CoroState>,
}

#[derive(Debug)]
pub enum PortContents {
    Aux(Sender<PortContents>),
    // A tree, which should be readback further, if necessary
    Tree(Tree),
}

impl SharedState {
    pub fn make_oneshot_ext_from_tx(&self, tx: Sender<PortContents>) -> Tree {
        let shared = self.shared.clone();
        Tree::ext(
            move |net, tree, tx| {
                let tx: Box<Sender<PortContents>> = tx.downcast().unwrap();
                let res = match tree {
                    Err(other_tx) => {
                        let other_tx: Box<Sender<PortContents>> = other_tx.downcast().unwrap();
                        PortContents::Aux(*other_tx)
                    }
                    Ok(tree) => PortContents::Tree(tree),
                };
                *shared.net.lock().unwrap() = Some(core::mem::take(net));
                shared
                    .executor
                    .lock()
                    .unwrap()
                    .run_until(async { tx.send(res).unwrap() });
                *net = shared.net.lock().unwrap().take().unwrap();
            },
            tx,
        )
    }
    pub fn make_oneshot_ext(&self) -> (Tree, Receiver<PortContents>) {
        let (tx, rx) = channel();
        let tree = self.make_oneshot_ext_from_tx(tx);
        (tree, rx)
    }
    // this is async because it will only work when ran under an async context.
    pub async fn add_redex(&self, a: Tree, b: Tree) {
        self.shared
            .net
            .lock()
            .unwrap()
            .as_mut()
            .unwrap()
            .redexes
            .push_back((a, b))
    }
    pub async fn read_port(&self, tree: Tree) -> PortContents {
        let (ext, rx) = self.make_oneshot_ext();
        self.add_redex(ext, tree).await;
        rx.await.unwrap()
    }
    pub async fn read_port_as_tree(&self, tree: Tree) -> Tree {
        let (ext, rx) = self.make_oneshot_ext();
        self.add_redex(ext, tree);
        match rx.await.unwrap() {
            PortContents::Aux(sender) => self.make_oneshot_ext_from_tx(sender),
            PortContents::Tree(tree) => tree,
        }
    }
    /// Eagerly read back a CON node.
    /// If we don't get a CON node, eta-expand and return the readback of that.
    pub async fn read_con(&self, tree: Tree) -> Result<(Tree, Tree), Tree> {
        let tree = self.read_port_as_tree(tree).await;
        match tree {
            Tree::Con(a, b) => Ok((*a, *b)),
            other => {
                // eta expand
                let mut binding = self.shared.net.lock().unwrap();
                let net = binding.as_mut().unwrap();
                let (v0, v1) = net.create_wire();
                let (w0, w1) = net.create_wire();
                net.link(other, Tree::c(v0, w0));
                Ok((v1, w1))
            }
        }
    }
    pub async fn read_par(&self, tree: Tree) -> Result<(Tree, Tree), Tree> {
        self.read_con(tree).await
    }
    pub async fn read_times(&self, tree: Tree) -> Result<(Tree, Tree), Tree> {
        self.read_con(tree).await
    }
    pub async fn read_era(&self, tree: Tree) -> Result<(), Tree> {
        let tree = self.read_port_as_tree(tree).await;
        match tree {
            Tree::Era => Ok(()),
            other => Err(other),
        }
    }
    pub fn flatten_multiplexed(&self, tree: Tree, len: usize) -> LocalBoxFuture<Vec<Tree>> {
        use futures::future::FutureExt;
        async move {
            if len == 0 {
                self.read_era(tree).await.unwrap();
                vec![]
            } else if len == 1 {
                vec![tree]
            } else {
                let fst_len = len / 2;
                let snd_len = len - fst_len;
                let (fst, snd) = self.read_con(tree).await.unwrap();
                [
                    self.flatten_multiplexed(fst, fst_len).await,
                    self.flatten_multiplexed(snd, snd_len).await,
                ]
                .concat()
            }
        }
        .boxed_local()
    }
    pub async fn read_either(&self, tree: Tree, variants: Vec<Name>) -> (Name, Tree) {
        // TODO: we might have to eta-reduce `context` here
        // because it's possible that it's link to the context variable in the variant is eta-expnded
        // A less brittle solution is to make this interact with a `choice`
        let (context, possibilities) = self.read_con(tree).await.unwrap();
        let possibilities = self
            .flatten_multiplexed(possibilities, variants.len())
            .await;
        let mut name_payload = None;
        for (possible, name) in possibilities.into_iter().zip(variants) {
            if let Tree::Con(a, b) = self.read_port_as_tree(possible).await {
                name_payload = Some((name, *b));
            }
        }
        name_payload.expect("couldn't readback either")
    }
    pub async fn read_choice(
        &self,
        tree: Tree,
        variants: Vec<Name>,
    ) -> (Tree, Vec<(Name, Tree, Tree)>) {
        let (context, cases) = self.read_con(tree).await.unwrap();
        let cases = self.flatten_multiplexed(cases, variants.len()).await;
        let mut res = vec![];
        for (case, name) in cases.into_iter().zip(variants) {
            let (ctx, payload) = self.read_con(case).await.unwrap();
            res.push((name, ctx, payload));
        }
        (context, res)
    }
}
