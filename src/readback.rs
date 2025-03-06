use eframe::egui::{self, Color32, RichText, Stroke};
use futures::{
    channel::oneshot,
    future::{join_all, LocalBoxFuture},
    stream::FuturesUnordered,
};

use crate::{
    icombs::{
        compiler::TypedTree,
        net::number_to_string,
        readback::{ReadbackResult, SharedState},
        Tree,
    },
    par::{
        parse::{Loc, Name, Program},
        process::Expression,
        types::Type,
    },
};

use core::fmt::{Debug, Display};
use std::{
    hash::Hash,
    sync::{atomic::AtomicBool, Arc},
};

#[derive(Debug, Clone)]
pub enum PathElement {
    ParLeft,
    ParRight,
    TimesLeft,
    TimesRight,
}

type Prog<Name> = Program<Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>;
#[derive(Debug)]
pub struct ReadbackStateInner {
    pub net: Option<crate::icombs::net::Net>,
    pub shared: SharedState,
    pub needs_further_readback: Arc<AtomicBool>,
    pub path: Vec<PathElement>,
}

impl Clone for ReadbackStateInner {
    fn clone(&self) -> Self {
        Self {
            net: None,
            shared: self.shared.clone(),
            needs_further_readback: self.needs_further_readback.clone(),
            path: self.path.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ReadbackState<Name: Clone> {
    pub result: ReadbackResult<Name>,
    pub inner: ReadbackStateInner,
}

impl<Name: Clone + Hash + Eq + core::fmt::Debug + Ord + Display + 'static> ReadbackState<Name> {
    pub fn show_readback(&mut self, ui: &mut egui::Ui, prog: &Prog<Name>) {
        use std::sync::atomic::Ordering;
        let shared = self.inner.shared.clone();

        self.inner.set_needs_further_readback();
        while self.inner.needs_further_readback.load(Ordering::Relaxed) {
            self.inner
                .needs_further_readback
                .store(false, std::sync::atomic::Ordering::Release);
            let mut net = core::mem::take(&mut self.inner.net).unwrap();
            let fut = async {
                self.inner.carry_out_readback(&mut self.result, prog).await;
            };
            shared.execute(&mut net, fut);
            self.inner.net = Some(net);
        }

        self.inner.show_readback(ui, &mut self.result, prog);
    }
}
// first, we render as deep as we can. Button clicks replace nodes by a Halt node
// then, we read back, replacing Halt nodes with other nodes. This is the async part
impl ReadbackStateInner {
    pub fn set_needs_further_readback(&self) {
        self.needs_further_readback
            .store(true, std::sync::atomic::Ordering::Release);
    }
    pub fn show_readback<Name: Clone + Hash + Eq + core::fmt::Debug + Ord + Display + 'static>(
        &mut self,
        ui: &mut egui::Ui,
        res: &mut ReadbackResult<Name>,
        prog: &Prog<Name>,
    ) {
        use ReadbackResult::*;
        egui::Frame::default()
            .stroke(egui::Stroke::new(1.0, egui::Color32::GRAY))
            .inner_margin(egui::Margin::same(4))
            .outer_margin(egui::Margin::same(2))
            .show(ui, |ui| {
                let mut replace_by = None;
                match res {
                    Break => {
                        ui.code("!");
                    }
                    Continue => {
                        ui.code("?");
                    }
                    Send(arg, bod) => {
                        ui.vertical(|ui| {
                            self.path.push(PathElement::TimesLeft);
                            self.show_readback(ui, arg, prog);
                            self.path.pop();
                            self.path.push(PathElement::TimesRight);
                            self.show_readback(ui, bod, prog);
                            self.path.pop();
                        });
                    }
                    Receive(arg, bod) => {
                        ui.horizontal(|ui| {
                            self.path.push(PathElement::ParLeft);
                            self.show_readback(ui, arg, prog);
                            self.path.pop();
                            self.path.push(PathElement::ParRight);
                            self.show_readback(ui, bod, prog);
                            self.path.pop();
                        });
                    }
                    Either(name, payload) => {
                        ui.code(format!(".{name}"));
                        self.show_readback(ui, payload, prog);
                    }
                    Choice(ctx, cases) => {
                        ui.vertical(|ui| {
                            for (name, ctx_in, payload) in cases {
                                if ui.button(format!("{name}")).clicked() {
                                    use crate::icombs::net::Tree;
                                    self.net.as_mut().unwrap().link(
                                        core::mem::replace(ctx, Tree::e()),
                                        core::mem::replace(ctx_in, Tree::e()),
                                    );
                                    let payload = core::mem::replace(
                                        payload,
                                        Box::new(ReadbackResult::Break),
                                    );
                                    replace_by = Some(*payload);
                                    self.set_needs_further_readback();
                                    break;
                                }
                            }
                        });
                    }
                    Variable(id) => {
                        ui.label(RichText::from(number_to_string(*id)).italics());
                    }
                    Halted(tree) => {
                        ui.label(RichText::from("working").italics());
                        self.set_needs_further_readback();
                    }
                    Waiting(tree, rx) => {
                        ui.label(RichText::from("waiting").italics());
                    }
                    ref tree => {
                        ui.code(format!("{tree:?}"));
                    }
                }
                if let Some(a) = replace_by {
                    *res = a;
                }
            });
    }
    pub fn with_path(mut self, e: PathElement) -> Self {
        self.path.push(e);
        self
    }
    pub fn carry_out_readback<
        'a,
        Name: Clone + Hash + Eq + core::fmt::Debug + Ord + Display + 'static,
    >(
        &'a mut self,
        res: &'a mut ReadbackResult<Name>,
        prog: &'a Prog<Name>,
    ) -> LocalBoxFuture<'a, ()> {
        use futures::FutureExt;
        async move {
            use ReadbackResult::*;
            let mut replace_by = None;
            match res {
                Break => {}
                Continue => {}
                Send(arg, bod) => {
                    futures::future::join(
                        self.clone()
                            .with_path(PathElement::TimesLeft)
                            .carry_out_readback(arg, prog),
                        self.clone()
                            .with_path(PathElement::TimesRight)
                            .carry_out_readback(bod, prog),
                    )
                    .await;
                }
                Receive(arg, bod) => {
                    futures::future::join(
                        self.clone()
                            .with_path(PathElement::ParLeft)
                            .carry_out_readback(arg, prog),
                        self.clone()
                            .with_path(PathElement::ParRight)
                            .carry_out_readback(bod, prog),
                    )
                    .await;
                }
                Either(name, payload) => {
                    self.carry_out_readback(payload, prog).await;
                }
                Choice(ctx, cases) => {
                    let (v0, v1) = self
                        .shared
                        .shared
                        .net
                        .lock()
                        .unwrap()
                        .as_mut()
                        .unwrap()
                        .create_wire();
                    let (ext1, mut ext0) = self.shared.create_waiting_ext();
                    core::mem::swap(&mut ext0, ctx);
                    println!("{:?} choice ", self.path);
                    self.shared.add_redex(ext0, ext1).await;
                }
                Halted(tree) => {
                    let mut tree = core::mem::take(tree);
                    tree.ty = prepare_type_for_readback(prog, tree.ty);
                    self.set_needs_further_readback();
                    println!("Waiting for: {:?}", self.path);
                    let mut res = self.shared.read_with_type(tree).await;
                    self.carry_out_readback(&mut res, prog).await;
                    println!("Finished: {:?}", self.path);
                    replace_by = Some(res);
                }
                Waiting(tree, rx) => {
                    if *rx.borrow() == true {
                        self.set_needs_further_readback();
                        replace_by = Some(ReadbackResult::Halted(core::mem::take(tree)));
                    }
                }
                _ => {}
            }
            if let Some(a) = replace_by {
                *res = a;
            }
        }
        .boxed_local()
    }
}
pub fn prepare_type_for_readback<Name: Clone + Eq + Hash + std::fmt::Debug>(
    program: &Program<Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>,
    ty: Type<Loc, Name>,
) -> Type<Loc, Name> {
    match ty {
        Type::Name(_, name, args) => program.dereference_type_def(&name, &args),
        Type::DualName(_, name, args) => program.dereference_type_def(&name, &args).dual(),
        Type::Recursive(_, label, body) => Type::expand_recursive(&label, &*body),
        Type::Iterative(_, label, body) => Type::expand_iterative(&label, &*body),
        ty => ty,
    }
}
