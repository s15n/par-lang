use indexmap::IndexMap;
use std::{
    fmt::{self, Display, Write},
    hash::Hash,
    sync::Arc,
};
use crate::par::location::Span;
use super::types::Type;

#[derive(Clone, Debug)]
pub enum Process<Name, Typ> {
    Let(
        Span,
        Name,
        Option<Type<Name>>,
        Typ,
        Arc<Expression<Name, Typ>>,
        Arc<Self>,
    ),
    Do(Span, Name, Typ, Command<Name, Typ>),
    Telltypes(Span, Arc<Self>),
}

#[derive(Clone, Debug)]
pub enum Command<Name, Typ> {
    Link(Arc<Expression<Name, Typ>>),
    Send(
        Arc<Expression<Name, Typ>>,
        Arc<Process<Name, Typ>>,
    ),
    Receive(Name, Option<Type<Name>>, Arc<Process<Name, Typ>>),
    Choose(Name, Arc<Process<Name, Typ>>),
    Match(Arc<[Name]>, Box<[Arc<Process<Name, Typ>>]>),
    Break,
    Continue(Arc<Process<Name, Typ>>),
    Begin(bool, Option<Name>, Arc<Process<Name, Typ>>),
    Loop(Option<Name>),

    SendType(Type<Name>, Arc<Process<Name, Typ>>),
    ReceiveType(Name, Arc<Process<Name, Typ>>),
}

#[derive(Clone, Debug)]
pub enum Expression<Name, Typ> {
    Reference(Span, Name, Typ),
    Fork(
        Span,
        Captures<Name>,
        Name,
        Option<Type<Name>>,
        Typ,
        Arc<Process<Name, Typ>>,
    ),
}

#[derive(Clone, Debug)]
pub struct Captures<Name> {
    pub names: IndexMap<Name, Span>,
}

impl<Name> Default for Captures<Name> {
    fn default() -> Self {
        Self {
            names: IndexMap::new(),
        }
    }
}

impl<Name: Hash + Eq> Captures<Name> {
    pub fn new() -> Self {
        Self {
            names: IndexMap::new(),
        }
    }

    pub fn single(name: Name, loc: Span) -> Self {
        let mut caps = Self::new();
        caps.add(name, loc);
        caps
    }

    pub fn extend(&mut self, other: Self) {
        for (name, loc) in other.names {
            self.names.insert(name, loc);
        }
    }

    pub fn add(&mut self, name: Name, loc: Span) {
        self.names.insert(name, loc);
    }

    pub fn remove(&mut self, name: &Name) -> Option<Span> {
        self.names.shift_remove(name)
    }
}

impl<Span: Clone, Name: Clone + Hash + Eq, Typ: Clone> Process<Name, Typ> {
    pub fn fix_captures(
        &self,
        loop_points: &IndexMap<Option<Name>, Captures<Name>>,
    ) -> (Arc<Self>, Captures<Name>) {
        match self {
            Self::Let(loc, name, annotation, typ, expression, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(name);
                let (expression, caps1) = expression.fix_captures(loop_points);
                caps.extend(caps1);
                (
                    Arc::new(Self::Let(
                        loc.clone(),
                        name.clone(),
                        annotation.clone(),
                        typ.clone(),
                        expression,
                        process,
                    )),
                    caps,
                )
            }
            Self::Do(loc, name, typ, command) => {
                let (command, mut caps) = command.fix_captures(loop_points);
                caps.add(name.clone(), loc.clone());
                (
                    Arc::new(Self::Do(loc.clone(), name.clone(), typ.clone(), command)),
                    caps,
                )
            }
            Self::Telltypes(loc, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Arc::new(Self::Telltypes(loc.clone(), process)), caps)
            }
        }
    }

    pub fn optimize(&self) -> Arc<Self> {
        match self {
            Self::Let(loc, name, annotation, typ, expression, process) => Arc::new(Self::Let(
                loc.clone(),
                name.clone(),
                annotation.clone(),
                typ.clone(),
                expression.optimize(),
                process.optimize(),
            )),
            Self::Do(loc, name, typ, command) => Arc::new(Self::Do(
                loc.clone(),
                name.clone(),
                typ.clone(),
                match command {
                    Command::Link(expression) => {
                        let expression = expression.optimize();
                        match expression.optimize().as_ref() {
                            Expression::Fork(_, _, channel, _, _, process) if name == channel => {
                                return Arc::clone(&process)
                            }
                            _ => Command::Link(expression),
                        }
                    }
                    Command::Send(argument, process) => {
                        Command::Send(argument.optimize(), process.optimize())
                    }
                    Command::Receive(parameter, annotation, process) => {
                        Command::Receive(parameter.clone(), annotation.clone(), process.optimize())
                    }
                    Command::Choose(chosen, process) => {
                        Command::Choose(chosen.clone(), process.optimize())
                    }
                    Command::Match(branches, processes) => {
                        let processes = processes.iter().map(|p| p.optimize()).collect();
                        Command::Match(Arc::clone(branches), processes)
                    }
                    Command::Break => Command::Break,
                    Command::Continue(process) => Command::Continue(process.optimize()),
                    Command::Begin(unfounded, label, process) => {
                        Command::Begin(unfounded.clone(), label.clone(), process.optimize())
                    }
                    Command::Loop(label) => Command::Loop(label.clone()),
                    Command::SendType(argument, process) => {
                        Command::SendType(argument.clone(), process.optimize())
                    }
                    Command::ReceiveType(parameter, process) => {
                        Command::ReceiveType(parameter.clone(), process.optimize())
                    }
                },
            )),
            Self::Telltypes(loc, process) => {
                Arc::new(Self::Telltypes(loc.clone(), process.optimize()))
            }
        }
    }
}

impl<Span: Clone, Name: Clone + Hash + Eq, Typ: Clone> Command<Name, Typ> {
    pub fn fix_captures(
        &self,
        loop_points: &IndexMap<Option<Name>, Captures<Name>>,
    ) -> (Self, Captures<Name>) {
        match self {
            Self::Link(expression) => {
                let (expression, caps) = expression.fix_captures(loop_points);
                (Self::Link(expression), caps)
            }
            Self::Send(argument, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                let (argument, caps1) = argument.fix_captures(loop_points);
                caps.extend(caps1);
                (Self::Send(argument, process), caps)
            }
            Self::Receive(parameter, annotation, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(parameter);
                (
                    Self::Receive(parameter.clone(), annotation.clone(), process),
                    caps,
                )
            }
            Self::Choose(chosen, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::Choose(chosen.clone(), process), caps)
            }
            Self::Match(branches, processes) => {
                let mut fixed_processes = Vec::new();
                let mut caps = Captures::new();
                for process in processes {
                    let (process, caps1) = process.fix_captures(loop_points);
                    fixed_processes.push(process);
                    caps.extend(caps1);
                }
                (
                    Self::Match(branches.clone(), fixed_processes.into_boxed_slice()),
                    caps,
                )
            }
            Self::Break => (Self::Break, Captures::new()),
            Self::Continue(process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::Continue(process), caps)
            }
            Self::Begin(unfounded, label, process) => {
                let (_, caps) = process.fix_captures(loop_points);
                let mut loop_points = loop_points.clone();
                loop_points.insert(label.clone(), caps);
                let (process, caps) = process.fix_captures(&loop_points);
                (Self::Begin(unfounded.clone(), label.clone(), process), caps)
            }
            Self::Loop(label) => (
                Self::Loop(label.clone()),
                loop_points.get(label).cloned().unwrap_or_default(),
            ),
            Self::SendType(argument, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::SendType(argument.clone(), process), caps)
            }
            Self::ReceiveType(parameter, process) => {
                let (process, caps) = process.fix_captures(loop_points);
                (Self::ReceiveType(parameter.clone(), process), caps)
            }
        }
    }
}

impl<Span: Clone, Name: Clone + Hash + Eq, Typ: Clone> Expression<Name, Typ> {
    pub fn fix_captures(
        &self,
        loop_points: &IndexMap<Option<Name>, Captures<Name>>,
    ) -> (Arc<Self>, Captures<Name>) {
        match self {
            Self::Reference(loc, name, typ) => (
                Arc::new(Self::Reference(loc.clone(), name.clone(), typ.clone())),
                Captures::single(name.clone(), loc.clone()),
            ),
            Self::Fork(loc, _, channel, annotation, typ, process) => {
                let (process, mut caps) = process.fix_captures(loop_points);
                caps.remove(channel);
                (
                    Arc::new(Self::Fork(
                        loc.clone(),
                        caps.clone(),
                        channel.clone(),
                        annotation.clone(),
                        typ.clone(),
                        process,
                    )),
                    caps,
                )
            }
        }
    }

    pub fn optimize(&self) -> Arc<Self> {
        match self {
            Self::Reference(loc, name, typ) => {
                Arc::new(Self::Reference(loc.clone(), name.clone(), typ.clone()))
            }
            Self::Fork(loc, captures, channel, annotation, typ, process) => Arc::new(Self::Fork(
                loc.clone(),
                captures.clone(),
                channel.clone(),
                annotation.clone(),
                typ.clone(),
                process.optimize(),
            )),
        }
    }
}

impl<Name: Display, Typ> Process<Name, Typ> {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Let(_, name, _, _, expression, process) => {
                indentation(f, indent)?;
                write!(f, "let {} = ", name)?;
                expression.pretty(f, indent)?;
                process.pretty(f, indent)
            }

            Self::Do(_, subject, _, command) => {
                indentation(f, indent)?;
                write!(f, "{}", subject)?;

                match command {
                    Command::Link(expression) => {
                        write!(f, " <> ")?;
                        expression.pretty(f, indent)
                    }

                    Command::Send(argument, process) => {
                        write!(f, "(")?;
                        argument.pretty(f, indent)?;
                        write!(f, ")")?;
                        process.pretty(f, indent)
                    }

                    Command::Receive(parameter, _, process) => {
                        write!(f, "[{}]", parameter)?;
                        process.pretty(f, indent)
                    }

                    Command::Choose(chosen, process) => {
                        write!(f, ".{}", chosen)?;
                        process.pretty(f, indent)
                    }

                    Command::Match(choices, branches) => {
                        write!(f, " {{")?;
                        for (choice, process) in choices.iter().zip(branches.iter()) {
                            indentation(f, indent + 1)?;
                            write!(f, "{} => {{", choice)?;
                            process.pretty(f, indent + 2)?;
                            indentation(f, indent + 1)?;
                            write!(f, "}}")?;
                        }
                        indentation(f, indent)?;
                        write!(f, "}}")
                    }

                    Command::Break => {
                        write!(f, "!")
                    }

                    Command::Continue(process) => {
                        write!(f, "?")?;
                        process.pretty(f, indent)
                    }

                    Command::Begin(unfounded, label, process) => {
                        if *unfounded {
                            write!(f, " unfounded")?;
                        }
                        write!(f, " begin")?;
                        if let Some(label) = label {
                            write!(f, " {}", label)?;
                        }
                        process.pretty(f, indent)
                    }

                    Command::Loop(label) => {
                        write!(f, " loop")?;
                        if let Some(label) = label {
                            write!(f, " {}", label)?;
                        }
                        Ok(())
                    }

                    Command::SendType(argument, process) => {
                        write!(f, "(type ")?;
                        argument.pretty(f, indent)?;
                        write!(f, ")")?;
                        process.pretty(f, indent)
                    }

                    Command::ReceiveType(parameter, process) => {
                        write!(f, "[type {}]", parameter)?;
                        process.pretty(f, indent)
                    }
                }
            }

            Self::Telltypes(_, process) => {
                indentation(f, indent)?;
                write!(f, "telltypes")?;
                process.pretty(f, indent)
            }
        }
    }
}

impl<Name: Display, Typ> Expression<Name, Typ> {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Reference(_, name, _) => {
                write!(f, "{}", name)
            }

            Self::Fork(_, captures, channel, _, _, process) => {
                write!(f, "chan {} |", channel)?;
                for (i, cap) in captures.names.keys().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", cap)?;
                }
                write!(f, "| {{")?;
                process.pretty(f, indent + 1)?;
                indentation(f, indent)?;
                write!(f, "}}")
            }
        }
    }
}

fn indentation(f: &mut impl Write, indent: usize) -> fmt::Result {
    write!(f, "\n")?;
    for _ in 0..indent {
        write!(f, "  ")?;
    }
    Ok(())
}
