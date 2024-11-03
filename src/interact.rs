use std::{collections::BTreeMap, sync::Arc};

use crate::base::{Context, Process};

pub struct Environment<I, X> {
    objects: BTreeMap<X, Object<I, X>>,
}

pub struct Object<I, X> {
    history: Vec<Event<I, X>>,
    state: State<I, X>,
}

pub enum Event<I, X> {
    Break,
    Continue,
    Send(X),
    Receive(X),
    Select(I),
    Case(I),
}

pub enum State<I, X> {
    Wait,
    Run(Context<I, X>, I, Arc<Process<I>>),
    Select(Vec<I>, Option<()>),
    Selected(Option<I>),
}
