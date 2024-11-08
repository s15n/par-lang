#![allow(unused)]

use std::{cell::RefCell, sync::Arc};

use crate::base::{Capture, Command, Expression, Process};

pub fn ref_<I>(name: I) -> Expression<I> {
    Expression::Ref(name)
}

pub fn fork_<I>(chan: I, proc: Process<I>) -> Expression<I> {
    Expression::Fork(RefCell::new(Capture::default()), chan, Arc::new(proc))
}

pub fn let_<I>(name: I, def: Expression<I>, then: Process<I>) -> Process<I> {
    Process::Let(name, Arc::new(def), Arc::new(then))
}

pub fn link_<I>(name: I, expr: Expression<I>) -> Process<I> {
    Process::Link(name, Arc::new(expr))
}

pub fn break_<I>(id: I) -> Process<I> {
    Process::Do(id, Command::Break)
}

pub fn continue_<I>(id: I, then: Process<I>) -> Process<I> {
    Process::Do(id, Command::Continue(Arc::new(then)))
}

pub fn send_<I>(id: I, arg: Expression<I>, then: Process<I>) -> Process<I> {
    Process::Do(id, Command::Send(Arc::new(arg), Arc::new(then)))
}

pub fn receive_<I>(id: I, param: I, then: Process<I>) -> Process<I> {
    Process::Do(id, Command::Receive(param, Arc::new(then)))
}

pub fn select_<I>(id: I, branch: I, then: Process<I>) -> Process<I> {
    Process::Do(id, Command::Select(branch, Arc::new(then)))
}

pub fn case_<I>(
    id: I,
    branches: impl IntoIterator<Item = (I, Process<I>)>,
    otherwise: Process<I>,
) -> Process<I> {
    Process::Do(
        id,
        Command::Case(
            branches
                .into_iter()
                .map(|(branch, then)| (branch, Arc::new(then)))
                .collect(),
            Some(Arc::new(otherwise)),
        ),
    )
}

pub fn case_exhaust_<I>(id: I, branches: impl IntoIterator<Item = (I, Process<I>)>) -> Process<I> {
    Process::Do(
        id,
        Command::Case(
            branches
                .into_iter()
                .map(|(branch, then)| (branch, Arc::new(then)))
                .collect(),
            None,
        ),
    )
}
