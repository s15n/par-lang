use eframe::egui::{self, Color32, RichText};
use futures::{
    channel::oneshot::{channel, Sender},
    future::{join, BoxFuture},
    task::{Spawn, SpawnExt},
    FutureExt,
};

use crate::icombs::{net::number_to_string, Net};

pub trait NameRequiredTraits:
    Clone + Hash + Eq + core::fmt::Debug + Ord + Display + Send + Sync + 'static
{
}

impl<T: Clone + Hash + Eq + core::fmt::Debug + Ord + Display + Send + Sync + 'static>
    NameRequiredTraits for T
{
}

use crate::{
    icombs::{
        compiler::TypedTree,
        readback::{ReadbackResult, SharedState},
        Tree,
    },
    par::{
        parse::{Loc, Program},
        process::Expression,
        types::Type,
    },
};

use core::fmt::{Debug, Display};
use std::{
    hash::Hash,
    sync::{Arc, Mutex},
};

type Prog<Name> = Arc<Program<Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>>;
#[derive(Clone)]
pub struct ReadbackStateInner {
    pub shared: SharedState,
    pub spawner: Arc<dyn Spawn + Send + Sync>,
}

pub struct Handle<Name: NameRequiredTraits> {
    refresh: Arc<dyn Fn() + Send + Sync>,
    history: Vec<Event<Name>>,
    end: Option<Request<Name>>,
    net: Arc<Mutex<Net>>,
}

impl<Name: NameRequiredTraits> Handle<Name> {
    fn child(&self) -> Self {
        Self {
            refresh: self.refresh.clone(),
            history: vec![],
            end: None,
            net: self.net.clone(),
        }
    }
    fn add_event(&mut self, ev: Event<Name>) {
        self.history.push(ev);
        (self.refresh)()
    }
    fn set_end(&mut self, end: Option<Request<Name>>) {
        self.end = end;
        (self.refresh)()
    }
}

#[derive(Debug)]
enum Request<Name: NameRequiredTraits> {
    Waiting,
    Port(TypedTree<Name>),
    Expand(TypedTree<Name>),
    Variable(usize),
    Choose(Tree, Vec<(Name, Tree, TypedTree<Name>)>),
}

#[derive(Debug)]
pub enum Event<Name: NameRequiredTraits> {
    Send(Arc<Mutex<Handle<Name>>>),
    Receive(Arc<Mutex<Handle<Name>>>),
    Break,
    Continue,
    Either(Name),
    Choose(Name),
}

impl<Name: NameRequiredTraits> Debug for Handle<Name> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Handle").finish_non_exhaustive()
    }
}

pub struct ReadbackState<Name: NameRequiredTraits> {
    pub root: Arc<Mutex<Handle<Name>>>,
    pub inner: ReadbackStateInner,
    // it's OK that this is unused, because we only use it to drop it when we're dropped.
    #[allow(unused)]
    pub net_tx: Sender<()>,
}

impl<Name: NameRequiredTraits> ReadbackState<Name> {
    pub fn show_readback(&mut self, ui: &mut egui::Ui, prog: Prog<Name>) {
        self.inner.show_handle(ui, self.root.clone(), prog);
    }
    pub fn initialize(
        ui: &mut egui::Ui,
        net: Net,
        tree: TypedTree<Name>,
        spawner: Arc<dyn Spawn + Send + Sync>,
    ) -> Self {
        let ctx = ui.ctx().clone();
        let handle = Handle {
            refresh: Arc::new(move || {
                ctx.request_repaint();
            }),
            history: vec![],
            end: Some(Request::Port(tree)),
            net: Arc::new(Mutex::new(net)),
        };
        let (net_tx, net_rx) = channel();
        let shared = SharedState::with_net(handle.net.clone());
        // the net_tx channel gets dropped when the readback state is dropped.
        spawner.spawn(shared.create_net_reducer(net_rx)).unwrap();
        Self {
            root: Arc::new(Mutex::new(handle)),
            net_tx: net_tx,
            inner: ReadbackStateInner {
                shared,
                spawner: spawner,
            },
        }
    }
}
// first, we render as deep as we can. Button clicks replace nodes by a Halt node
// then, we read back, replacing Halt nodes with other nodes. This is the async part
impl ReadbackStateInner {
    pub fn show_handle<Name: NameRequiredTraits>(
        &mut self,
        ui: &mut egui::Ui,
        handle: Arc<Mutex<Handle<Name>>>,
        prog: Prog<Name>,
    ) {
        egui::Frame::default()
            .stroke(egui::Stroke::new(1.0, egui::Color32::GRAY))
            .inner_margin(egui::Margin::same(4))
            .outer_margin(egui::Margin::same(2))
            .show(ui, |ui| {
                ui.vertical(|ui| {
                    for event in handle.lock().unwrap().history.iter_mut() {
                        let prog = prog.clone();
                        match event {
                            Event::Send(handle) => {
                                ui.horizontal(|ui| {
                                    ui.label("+");
                                    self.show_handle(ui, handle.clone(), prog);
                                });
                            }
                            Event::Receive(handle) => {
                                ui.horizontal(|ui| {
                                    ui.label("-");
                                    self.show_handle(ui, handle.clone(), prog);
                                });
                            }
                            Event::Either(name) => {
                                ui.horizontal(|ui| {
                                    ui.label("+");
                                    ui.label(
                                        RichText::from(name.to_string())
                                            .color(Color32::WHITE)
                                            .strong(),
                                    );
                                });
                            }
                            Event::Choose(name) => {
                                ui.horizontal(|ui| {
                                    ui.label("-");
                                    ui.label(
                                        RichText::from(name.to_string()).color(Color32::WHITE),
                                    );
                                });
                            }
                            Event::Break => {
                                ui.horizontal(|ui| {
                                    ui.label("+");
                                    ui.label(RichText::from("!").color(Color32::WHITE).strong());
                                });
                            }
                            Event::Continue => {
                                ui.horizontal(|ui| {
                                    ui.label("-");
                                    ui.label(RichText::from("!").color(Color32::WHITE));
                                });
                            }
                        }
                    }
                    let mut lock = handle.lock().unwrap();
                    if let Some(end) = lock.end.as_mut() {
                        match end {
                            port @ Request::Port(..) => {
                                let Request::Port(port) =
                                    core::mem::replace(port, Request::Waiting)
                                else {
                                    unreachable!()
                                };

                                let shared = self.shared.clone();
                                {
                                    let handle = handle.clone();
                                    let mut this = self.clone();
                                    self.spawner
                                        .spawn(async move {
                                            let port = shared.read_with_type(port).await;
                                            this.readback_result(handle, port, prog).await;
                                        })
                                        .unwrap();
                                }
                                (lock.refresh)()
                            }
                            Request::Waiting => {
                                ui.label(RichText::from("waiting").italics());
                            }
                            Request::Choose(ctx, options) => {
                                let mut chosen = None;
                                ui.vertical(|ui| {
                                    for (idx, (name, _, _)) in options.iter().enumerate() {
                                        if ui
                                            .button(
                                                RichText::new(name.to_string())
                                                    .color(Color32::WHITE),
                                            )
                                            .clicked()
                                        {
                                            chosen = Some(idx);
                                        }
                                    }
                                });
                                if let Some(chosen) = chosen {
                                    let Some(Request::Choose(ctx, options)) =
                                        core::mem::replace(&mut lock.end, Some(Request::Waiting))
                                    else {
                                        unreachable!()
                                    };
                                    let mut ctx = Some(ctx);
                                    for (idx, (name, ctx_here, payload)) in
                                        options.into_iter().enumerate()
                                    {
                                        let this = self.clone();
                                        let handle = handle.clone();
                                        let prog = prog.clone();
                                        if idx == chosen {
                                            lock.add_event(Event::Choose(name));
                                            let ctx = ctx.take().unwrap();
                                            self.spawner
                                                .spawn(async move {
                                                    let mut this2 = this.clone();
                                                    join(
                                                        this.shared.add_redex(ctx_here, ctx),
                                                        this2.readback_result(
                                                            handle,
                                                            ReadbackResult::Halted(payload),
                                                            prog,
                                                        ),
                                                    )
                                                    .await;
                                                })
                                                .unwrap();
                                        } else {
                                            self.spawner
                                                .spawn(async move {
                                                    join(
                                                        this.shared.add_redex(ctx_here, Tree::e()),
                                                        this.shared
                                                            .add_redex(payload.tree, Tree::e()),
                                                    )
                                                    .await;
                                                })
                                                .unwrap();
                                        }
                                    }
                                }
                            }
                            Request::Expand(package) => {
                                if ui
                                    .button(
                                        RichText::from("expand").italics().color(Color32::WHITE),
                                    )
                                    .clicked()
                                {
                                    let Some(Request::Expand(package)) =
                                        core::mem::replace(&mut lock.end, Some(Request::Waiting))
                                    else {
                                        unreachable!()
                                    };
                                    let handle = handle.clone();
                                    let this = self.clone();
                                    self.spawner
                                        .spawn(async move {
                                            let tree = this.shared.expand_once(package.tree).await;
                                            let mut lock = handle.lock().unwrap();
                                            lock.end =
                                                Some(Request::Port(tree.with_type(package.ty)));
                                        })
                                        .unwrap();
                                }
                            }
                            Request::Variable(id) => {
                                ui.horizontal(|ui| {
                                    ui.label("Variable: ");
                                    ui.label(
                                        RichText::new(number_to_string(*id))
                                            .color(Color32::WHITE)
                                            .code(),
                                    );
                                });
                            }
                        }
                    }
                });
            });
    }
    pub fn readback_result<Name: NameRequiredTraits>(
        &mut self,
        handle: Arc<Mutex<Handle<Name>>>,
        port: ReadbackResult<Name>,
        prog: Prog<Name>,
    ) -> BoxFuture<()> {
        async move {
            let handle_2 = handle.clone();
            match port {
                ReadbackResult::Break => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.add_event(Event::Break);
                    lock.end = None;
                }
                ReadbackResult::Continue => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.add_event(Event::Continue);
                    lock.end = None;
                }
                ReadbackResult::Send(lft, rgt) => {
                    let child = {
                        let mut lock = handle_2.lock().unwrap();
                        let child = Arc::new(Mutex::new(lock.child()));
                        lock.add_event(Event::Send(child.clone()));
                        child
                    };
                    let mut this = self.clone();
                    join(
                        self.readback_result(child, *lft, prog.clone()),
                        this.readback_result(handle, *rgt, prog),
                    )
                    .await;
                }
                ReadbackResult::Receive(lft, rgt) => {
                    let child = {
                        let mut lock = handle_2.lock().unwrap();
                        let child = Arc::new(Mutex::new(lock.child()));
                        lock.add_event(Event::Receive(child.clone()));
                        child
                    };
                    let mut this = self.clone();
                    join(
                        self.readback_result(child, *lft, prog.clone()),
                        this.readback_result(handle, *rgt, prog),
                    )
                    .await;
                }
                ReadbackResult::Either(name, payload) => {
                    {
                        let mut lock = handle_2.lock().unwrap();
                        lock.add_event(Event::Either(name));
                    }
                    self.readback_result(handle, *payload, prog).await;
                }
                ReadbackResult::Choice(ctx_in, options) => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.set_end(Some(Request::Choose(ctx_in, options)));
                }
                ReadbackResult::Expand(package) => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.set_end(Some(Request::Expand(package)));
                }
                ReadbackResult::Halted(mut tree) => {
                    let mut this = self.clone();
                    self.spawner
                        .spawn(async move {
                            tree.ty = prepare_type_for_readback(&prog, tree.ty);
                            let res = this.shared.read_with_type(tree).await;
                            this.readback_result(handle, res, prog).await;
                        })
                        .unwrap();
                }
                ReadbackResult::Variable(id) => {
                    let mut lock = handle_2.lock().unwrap();
                    lock.set_end(Some(Request::Variable(id)));
                }
                e => {
                    eprintln!("Don't know how to read back: {e:?}");
                }
            }
        }
        .boxed()
    }
}
pub fn prepare_type_for_readback<Name: NameRequiredTraits>(
    program: &Program<Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>,
    mut ty: Type<Loc, Name>,
) -> Type<Loc, Name> {
    loop {
        ty = match ty {
            Type::Name(_, name, args) => program.dereference_type_def(&name, &args),
            Type::DualName(_, name, args) => program.dereference_type_def(&name, &args).dual(),
            Type::Recursive(_, label, body) => Type::expand_recursive(&label, &*body),
            Type::Iterative(_, label, body) => Type::expand_iterative(&label, &*body),
            ty => break ty,
        };
    }
}
