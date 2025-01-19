use indexmap::IndexMap;
use std::{hash::Hash, sync::Arc};

#[derive(Debug)]
pub enum Process<Loc, Name> {
    Let(Loc, Name, Arc<Expression<Loc, Name>>, Arc<Self>),
    Do(Loc, Name, Command<Loc, Name>),
}

#[derive(Debug)]
pub enum Command<Loc, Name> {
    Link(Arc<Expression<Loc, Name>>),
    Send(Arc<Expression<Loc, Name>>, Arc<Process<Loc, Name>>),
    Receive(Name, Arc<Process<Loc, Name>>),
    Choose(Name, Arc<Process<Loc, Name>>),
    Either(Arc<[Name]>, Box<[Arc<Process<Loc, Name>>]>),
    Break,
    Continue(Arc<Process<Loc, Name>>),
    Iterate(Option<Name>, Arc<Process<Loc, Name>>),
    Loop(Option<Name>),
}

#[derive(Debug)]
pub enum Expression<Loc, Name> {
    Reference(Loc, Name),
    Fork(Loc, Captures<Loc, Name>, Name, Arc<Process<Loc, Name>>),
}

#[derive(Clone, Debug)]
pub struct Captures<Loc, Name> {
    pub names: IndexMap<Name, Loc>,
}

impl<Loc, Name> Default for Captures<Loc, Name> {
    fn default() -> Self {
        Self {
            names: IndexMap::new(),
        }
    }
}

impl<Loc, Name: Hash + Eq> Captures<Loc, Name> {
    pub fn new() -> Self {
        Self {
            names: IndexMap::new(),
        }
    }

    pub fn single(name: Name, loc: Loc) -> Self {
        let mut caps = Self::new();
        caps.add(name, loc);
        caps
    }

    pub fn extend(&mut self, other: Self) {
        for (name, loc) in other.names {
            self.names.insert(name, loc);
        }
    }

    pub fn add(&mut self, name: Name, loc: Loc) {
        self.names.insert(name, loc);
    }

    pub fn remove(&mut self, name: &Name) -> Option<Loc> {
        self.names.shift_remove(name)
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Process<Loc, Name> {
    pub fn fix_captures(
        &self,
        loop_points: &IndexMap<Option<Name>, Captures<Loc, Name>>,
    ) -> (Self, Captures<Loc, Name>) {
        match self {
            Self::Let(loc, name, expression, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(name);
                let (expression, caps1) = expression.fix_captures(loop_points);
                caps.extend(caps1);
                (
                    Self::Let(
                        loc.clone(),
                        name.clone(),
                        Arc::new(expression),
                        Arc::new(process),
                    ),
                    caps,
                )
            }
            Self::Do(loc, name, command) => {
                let (command, mut caps) = command.fix_captures(loop_points);
                caps.add(name.clone(), loc.clone());
                (Self::Do(loc.clone(), name.clone(), command), caps)
            }
        }
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Command<Loc, Name> {
    pub fn fix_captures(
        &self,
        loop_points: &IndexMap<Option<Name>, Captures<Loc, Name>>,
    ) -> (Self, Captures<Loc, Name>) {
        match self {
            Self::Link(expression) => {
                let (expression, caps) = expression.fix_captures(loop_points);
                (Self::Link(Arc::new(expression)), caps)
            }
            Self::Send(argument, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                let (argument, caps1) = argument.fix_captures(loop_points);
                caps.extend(caps1);
                (Self::Send(Arc::new(argument), Arc::new(process)), caps)
            }
            Self::Receive(parameter, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(parameter);
                (Self::Receive(parameter.clone(), Arc::new(process)), caps)
            }
            Self::Choose(chosen, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::Choose(chosen.clone(), Arc::new(process)), caps)
            }
            Self::Either(branches, processes) => {
                let mut fixed_processes = Vec::new();
                let mut caps = Captures::new();
                for process in processes {
                    let (process, caps1) = process.fix_captures(loop_points);
                    fixed_processes.push(Arc::new(process));
                    caps.extend(caps1);
                }
                (
                    Self::Either(branches.clone(), fixed_processes.into_boxed_slice()),
                    caps,
                )
            }
            Self::Break => (Self::Break, Captures::new()),
            Self::Continue(process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::Continue(Arc::new(process)), caps)
            }
            Self::Iterate(label, process) => {
                let (_, caps) = process.fix_captures(loop_points);
                let mut loop_points = loop_points.clone();
                loop_points.insert(label.clone(), caps);
                let (process, caps) = process.fix_captures(&loop_points);
                (Self::Iterate(label.clone(), Arc::new(process)), caps)
            }
            Self::Loop(label) => (
                Self::Loop(label.clone()),
                loop_points.get(label).cloned().unwrap_or_default(),
            ),
        }
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Expression<Loc, Name> {
    pub fn fix_captures(
        &self,
        loop_points: &IndexMap<Option<Name>, Captures<Loc, Name>>,
    ) -> (Self, Captures<Loc, Name>) {
        match self {
            Self::Reference(loc, name) => (
                Self::Reference(loc.clone(), name.clone()),
                Captures::single(name.clone(), loc.clone()),
            ),
            Self::Fork(loc, _, channel, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(channel);
                (
                    Self::Fork(
                        loc.clone(),
                        caps.clone(),
                        channel.clone(),
                        Arc::new(process),
                    ),
                    caps,
                )
            }
        }
    }
}
