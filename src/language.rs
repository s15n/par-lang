use std::{fmt::Display, hash::Hash, sync::Arc};

use indexmap::IndexMap;

use crate::process::{self, Captures};

#[derive(Clone, Debug)]
pub enum Expression<Loc, Name> {
    Fork(Loc, Name, Box<Process<Loc, Name>>),
    Construction(Loc, Construct<Loc, Name>),
    Application(Loc, Name, Apply<Loc, Name>),
}

#[derive(Clone, Debug)]
pub enum Construct<Loc, Name> {
    Then(Loc, Box<Expression<Loc, Name>>),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Receive(Loc, Name, Box<Self>),
    Choose(Loc, Name, Box<Self>),
    Either(Loc, ConstructBranches<Loc, Name>),
    Break(Loc),
}

#[derive(Clone, Debug)]
pub struct ConstructBranches<Loc, Name>(pub IndexMap<Name, ConstructBranch<Loc, Name>>);

#[derive(Clone, Debug)]
pub enum ConstructBranch<Loc, Name> {
    Then(Loc, Expression<Loc, Name>),
    Receive(Loc, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Apply<Loc, Name> {
    Noop(Loc),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Choose(Loc, Name, Box<Self>),
    Either(Loc, ApplyBranches<Loc, Name>),
}

#[derive(Clone, Debug)]
pub struct ApplyBranches<Loc, Name>(pub IndexMap<Name, ApplyBranch<Loc, Name>>);

#[derive(Clone, Debug)]
pub enum ApplyBranch<Loc, Name> {
    Then(Loc, Name, Expression<Loc, Name>),
    Receive(Loc, Name, Box<Self>),
    Continue(Loc, Expression<Loc, Name>),
}

#[derive(Clone, Debug)]
pub enum Process<Loc, Name> {
    Let(Loc, Name, Box<Expression<Loc, Name>>, Box<Self>),
    Command(Name, Command<Loc, Name>),
    Pass(Loc),
}

#[derive(Clone, Debug)]
pub enum Command<Loc, Name> {
    Then(Box<Process<Loc, Name>>),
    Link(Loc, Box<Expression<Loc, Name>>),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Receive(Loc, Name, Box<Self>),
    Choose(Loc, Name, Box<Self>),
    Either(
        Loc,
        CommandBranches<Loc, Name>,
        Option<Box<Process<Loc, Name>>>,
    ),
    Break(Loc),
    Continue(Loc, Box<Process<Loc, Name>>),
    Begin(Loc, Option<Name>, Box<Self>),
    Loop(Loc, Option<Name>),
}

#[derive(Clone, Debug)]
pub struct CommandBranches<Loc, Name>(pub IndexMap<Name, CommandBranch<Loc, Name>>);

#[derive(Clone, Debug)]
pub enum CommandBranch<Loc, Name> {
    Then(Process<Loc, Name>),
    Receive(Loc, Name, Box<Self>),
    Continue(Loc, Process<Loc, Name>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Internal<Name> {
    Original(Name),
    Result,
    Object,
}

impl<Name: Display> Display for Internal<Name> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Original(name) => write!(f, "{}", name),
            Self::Result => write!(f, "#result"),
            Self::Object => write!(f, "#object"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompileError<Loc> {
    PassNotPossible(Loc),
}

type Pass<Loc, Name> = Option<Arc<process::Process<Loc, Internal<Name>>>>;

impl<Loc: Clone, Name: Clone + Hash + Eq> Expression<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Expression<Loc, Internal<Name>>>, CompileError<Loc>> {
        Ok(match self {
            Self::Fork(loc, channel, process) => Arc::new(process::Expression::Fork(
                loc.clone(),
                Captures::new(),
                Internal::Original(channel.clone()),
                process.compile(None)?,
            )),

            Self::Construction(loc, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result,
                    process,
                ))
            }

            Self::Application(loc, name, Apply::Noop(_)) => Arc::new(
                process::Expression::Reference(loc.clone(), Internal::Original(name.clone())),
            ),

            Self::Application(loc, name, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result,
                    Arc::new(process::Process::Let(
                        loc.clone(),
                        Internal::Object,
                        Arc::new(process::Expression::Reference(
                            loc.clone(),
                            Internal::Original(name.clone()),
                        )),
                        process,
                    )),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Construct<Loc, Name> {
    pub fn compile(&self) -> Result<Arc<process::Process<Loc, Internal<Name>>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result,
                    process::Command::Link(expression),
                ))
            }

            Self::Send(loc, argument, construct) => {
                let argument = argument.compile()?;
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result,
                    process::Command::Send(argument, process),
                ))
            }

            Self::Receive(loc, parameter, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result,
                    process::Command::Receive(Internal::Original(parameter.clone()), process),
                ))
            }

            Self::Choose(loc, chosen, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result,
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(loc, ConstructBranches(construct_branches)) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, construct_branch) in construct_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(construct_branch.compile()?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result,
                    process::Command::Either(branches, processes),
                ))
            }

            Self::Break(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Result,
                process::Command::Break,
            )),
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> ConstructBranch<Loc, Name> {
    pub fn compile(&self) -> Result<Arc<process::Process<Loc, Internal<Name>>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result,
                    process::Command::Link(expression),
                ))
            }

            Self::Receive(loc, parameter, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result,
                    process::Command::Receive(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Apply<Loc, Name> {
    pub fn compile(&self) -> Result<Arc<process::Process<Loc, Internal<Name>>>, CompileError<Loc>> {
        Ok(match self {
            Self::Noop(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Result,
                process::Command::Link(Arc::new(process::Expression::Reference(
                    loc.clone(),
                    Internal::Object,
                ))),
            )),

            Self::Send(loc, expression, apply) => {
                let expression = expression.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object,
                    process::Command::Send(expression, process),
                ))
            }

            Self::Choose(loc, chosen, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object,
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(loc, ApplyBranches(expression_branches)) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, expression_branch) in expression_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(expression_branch.compile()?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object,
                    process::Command::Either(branches, processes),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> ApplyBranch<Loc, Name> {
    pub fn compile(&self) -> Result<Arc<process::Process<Loc, Internal<Name>>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, name, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Let(
                    loc.clone(),
                    Internal::Original(name.clone()),
                    Arc::new(process::Expression::Reference(
                        loc.clone(),
                        Internal::Object,
                    )),
                    Arc::new(process::Process::Do(
                        loc.clone(),
                        Internal::Result,
                        process::Command::Link(expression),
                    )),
                ))
            }

            Self::Receive(loc, parameter, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object,
                    process::Command::Receive(Internal::Original(parameter.clone()), process),
                ))
            }

            Self::Continue(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object,
                    process::Command::Continue(Arc::new(process::Process::Do(
                        loc.clone(),
                        Internal::Result,
                        process::Command::Link(expression),
                    ))),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Process<Loc, Name> {
    pub fn compile(
        &self,
        pass: Pass<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>>>, CompileError<Loc>> {
        Ok(match self {
            Self::Let(loc, name, expression, process) => Arc::new(process::Process::Let(
                loc.clone(),
                Internal::Original(name.clone()),
                expression.compile()?,
                process.compile(pass)?,
            )),

            Self::Command(name, command) => command.compile(name, pass)?,

            Self::Pass(loc) => match pass {
                Some(process) => process,
                None => Err(CompileError::PassNotPossible(loc.clone()))?,
            },
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Command<Loc, Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>>>, CompileError<Loc>> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass)?,

            Self::Link(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Link(expression),
                ))
            }

            Self::Send(loc, argument, command) => {
                let argument = argument.compile()?;
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Send(argument, process),
                ))
            }

            Self::Receive(loc, parameter, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Receive(Internal::Original(parameter.clone()), process),
                ))
            }

            Self::Choose(loc, chosen, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(loc, CommandBranches(process_branches), optional_process) => {
                let pass = match optional_process {
                    Some(process) => Some(process.compile(pass)?),
                    None => pass,
                };

                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, process_branch) in process_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(process_branch.compile(object_name, pass.clone())?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Either(branches, processes),
                ))
            }

            Self::Break(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                object_internal,
                process::Command::Break,
            )),

            Self::Continue(loc, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Continue(process),
                ))
            }

            Self::Begin(loc, label, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Begin(label.clone().map(Internal::Original), process),
                ))
            }

            Self::Loop(loc, label) => Arc::new(process::Process::Do(
                loc.clone(),
                object_internal,
                process::Command::Loop(label.clone().map(Internal::Original)),
            )),
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> CommandBranch<Loc, Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>>>, CompileError<Loc>> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass)?,

            Self::Receive(loc, parameter, branch) => {
                let process = branch.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Receive(Internal::Original(parameter.clone()), process),
                ))
            }

            Self::Continue(loc, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    process::Command::Continue(process),
                ))
            }
        })
    }
}
