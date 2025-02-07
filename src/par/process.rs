use indexmap::IndexMap;
use std::{
    fmt::{self, Display, Write},
    hash::Hash,
    sync::Arc,
};

#[derive(Clone, Debug)]
pub enum Process<Loc, Name> {
    Let(Loc, Name, Arc<Expression<Loc, Name>>, Arc<Self>),
    Do(Loc, Name, Command<Loc, Name>),
}

#[derive(Clone, Debug)]
pub enum Command<Loc, Name> {
    Link(Arc<Expression<Loc, Name>>),
    Send(Arc<Expression<Loc, Name>>, Arc<Process<Loc, Name>>),
    Receive(Name, Arc<Process<Loc, Name>>),
    Choose(Name, Arc<Process<Loc, Name>>),
    Either(Arc<[Name]>, Box<[Arc<Process<Loc, Name>>]>),
    Break,
    Continue(Arc<Process<Loc, Name>>),
    Begin(Option<Name>, Arc<Process<Loc, Name>>),
    Loop(Option<Name>),
}

#[derive(Clone, Debug)]
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

    pub fn optimize(&self) -> Self {
        match self {
            Self::Let(loc, name, expression, process) => {
                let expression = expression.optimize();
                let process = process.optimize();
                Self::Let(
                    loc.clone(),
                    name.clone(),
                    Arc::new(expression),
                    Arc::new(process),
                )
            }
            Self::Do(loc, name, command) => match command {
                Command::Link(expression) => match expression.optimize() {
                    Expression::Fork(_, _, channel, process) if name == &channel => {
                        process.as_ref().clone()
                    }
                    expression => Self::Do(
                        loc.clone(),
                        name.clone(),
                        Command::Link(Arc::new(expression)),
                    ),
                },
                Command::Send(argument, process) => {
                    let argument = argument.optimize();
                    let process = process.optimize();
                    Self::Do(
                        loc.clone(),
                        name.clone(),
                        Command::Send(Arc::new(argument), Arc::new(process)),
                    )
                }
                Command::Receive(parameter, process) => {
                    let process = process.optimize();
                    Self::Do(
                        loc.clone(),
                        name.clone(),
                        Command::Receive(parameter.clone(), Arc::new(process)),
                    )
                }
                Command::Choose(chosen, process) => {
                    let process = process.optimize();
                    Self::Do(
                        loc.clone(),
                        name.clone(),
                        Command::Choose(chosen.clone(), Arc::new(process)),
                    )
                }
                Command::Either(branches, processes) => {
                    let processes = processes.iter().map(|p| Arc::new(p.optimize())).collect();
                    Self::Do(
                        loc.clone(),
                        name.clone(),
                        Command::Either(Arc::clone(branches), processes),
                    )
                }
                Command::Break => Self::Do(loc.clone(), name.clone(), Command::Break),
                Command::Continue(process) => {
                    let process = process.optimize();
                    Self::Do(
                        loc.clone(),
                        name.clone(),
                        Command::Continue(Arc::new(process)),
                    )
                }
                Command::Begin(label, process) => {
                    let process = process.optimize();
                    Self::Do(
                        loc.clone(),
                        name.clone(),
                        Command::Begin(label.clone(), Arc::new(process)),
                    )
                }
                Command::Loop(label) => {
                    Self::Do(loc.clone(), name.clone(), Command::Loop(label.clone()))
                }
            },
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
            Self::Begin(label, process) => {
                let (_, caps) = process.fix_captures(loop_points);
                let mut loop_points = loop_points.clone();
                loop_points.insert(label.clone(), caps);
                let (process, caps) = process.fix_captures(&loop_points);
                (Self::Begin(label.clone(), Arc::new(process)), caps)
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

    pub fn optimize(&self) -> Self {
        match self {
            Self::Reference(loc, name) => Self::Reference(loc.clone(), name.clone()),
            Self::Fork(loc, captures, channel, process) => {
                let process = process.optimize();
                Self::Fork(
                    loc.clone(),
                    captures.clone(),
                    channel.clone(),
                    Arc::new(process),
                )
            }
        }
    }
}

impl<Loc, Name: Display> Process<Loc, Name> {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Let(_, name, expression, process) => {
                indentation(f, indent)?;
                write!(f, "let {} = ", name)?;
                expression.pretty(f, indent)?;
                process.pretty(f, indent)
            }

            Self::Do(_, subject, command) => {
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

                    Command::Receive(parameter, process) => {
                        write!(f, "[{}]", parameter)?;
                        process.pretty(f, indent)
                    }

                    Command::Choose(chosen, process) => {
                        write!(f, ".{}", chosen)?;
                        process.pretty(f, indent)
                    }

                    Command::Either(choices, branches) => {
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

                    Command::Begin(label, process) => {
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
                }
            }
        }
    }
}

impl<Loc, Name: Display> Expression<Loc, Name> {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Self::Reference(_, name) => {
                write!(f, "{}", name)
            }

            Self::Fork(_, captures, channel, process) => {
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
