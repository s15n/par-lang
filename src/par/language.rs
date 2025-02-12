use std::{fmt::Display, hash::Hash, sync::Arc};

use indexmap::IndexMap;

use super::{
    process::{self, Captures},
    types::Type,
};

#[derive(Clone, Debug)]
pub enum Expression<Loc, Name> {
    Let(Loc, Name, Option<Type<Loc, Name>>, Box<Self>, Box<Self>),
    Do(Loc, Box<Process<Loc, Name>>, Box<Self>),
    Fork(Loc, Name, Option<Type<Loc, Name>>, Box<Process<Loc, Name>>),
    Construction(Loc, Construct<Loc, Name>),
    Application(Loc, Name, Apply<Loc, Name>),
}

#[derive(Clone, Debug)]
pub enum Construct<Loc, Name> {
    Then(Loc, Box<Expression<Loc, Name>>),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Receive(Loc, Name, Option<Type<Loc, Name>>, Box<Self>),
    Choose(Loc, Name, Box<Self>),
    Either(Loc, ConstructBranches<Loc, Name>),
    Break(Loc),
    Begin(Loc, Option<Name>, Box<Self>),
    Loop(Loc, Option<Name>),
}

#[derive(Clone, Debug)]
pub struct ConstructBranches<Loc, Name>(pub IndexMap<Name, ConstructBranch<Loc, Name>>);

#[derive(Clone, Debug)]
pub enum ConstructBranch<Loc, Name> {
    Then(Loc, Expression<Loc, Name>),
    Receive(Loc, Name, Option<Type<Loc, Name>>, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Apply<Loc, Name> {
    Noop(Loc),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Choose(Loc, Name, Box<Self>),
    Either(Loc, ApplyBranches<Loc, Name>),
    Begin(Loc, Option<Name>, Box<Self>),
    Loop(Loc, Option<Name>),
}

#[derive(Clone, Debug)]
pub struct ApplyBranches<Loc, Name>(pub IndexMap<Name, ApplyBranch<Loc, Name>>);

#[derive(Clone, Debug)]
pub enum ApplyBranch<Loc, Name> {
    Then(Loc, Name, Expression<Loc, Name>),
    Receive(Loc, Name, Option<Type<Loc, Name>>, Box<Self>),
    Continue(Loc, Expression<Loc, Name>),
}

#[derive(Clone, Debug)]
pub enum Process<Loc, Name> {
    Let(
        Loc,
        Name,
        Option<Type<Loc, Name>>,
        Box<Expression<Loc, Name>>,
        Box<Self>,
    ),
    Command(Name, Command<Loc, Name>),
    Pass(Loc),
    Telltypes(Loc),
    Noop(Loc),
}

#[derive(Clone, Debug)]
pub enum Command<Loc, Name> {
    Then(Box<Process<Loc, Name>>),
    Link(Loc, Box<Expression<Loc, Name>>),
    Send(Loc, Box<Expression<Loc, Name>>, Box<Self>),
    Receive(Loc, Name, Option<Type<Loc, Name>>, Box<Self>),
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
    Receive(Loc, Name, Option<Type<Loc, Name>>, Box<Self>),
    Continue(Loc, Process<Loc, Name>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Internal<Name> {
    Original(Name),
    Result(Option<Name>),
    Object(Option<Name>),
}

impl<Name: Display> Display for Internal<Name> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Original(name) => write!(f, "{}", name),
            Self::Result(name) => {
                if let Some(name) = name {
                    write!(f, "{}", name)?;
                }
                write!(f, "#result")
            }
            Self::Object(name) => {
                if let Some(name) = name {
                    write!(f, "{}", name)?;
                }
                write!(f, "#object")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompileError<Loc> {
    PassNotPossible(Loc),
    MustEndProcess(Loc),
    CannotEndInDoExpression(Loc),
}

type Pass<Loc, Name> = Option<Arc<process::Process<Loc, Internal<Name>, ()>>>;
type DoResult<Loc, Name> = Option<Arc<process::Expression<Loc, Internal<Name>, ()>>>;

impl<Loc: Clone, Name: Clone + Hash + Eq> Expression<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Expression<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Let(loc, name, annotation, expression, body) => {
                let expression = expression.compile()?;
                let body = body.compile()?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    original(annotation),
                    (),
                    Arc::new(process::Process::Let(
                        loc.clone(),
                        Internal::Original(name.clone()),
                        None,
                        (),
                        expression,
                        Arc::new(process::Process::Do(
                            loc.clone(),
                            Internal::Result(None),
                            (),
                            process::Command::Link(body),
                        )),
                    )),
                ))
            }

            Self::Do(loc, process, expression) => {
                let expression = expression.compile()?;
                let body = process.compile(None, Some(expression))?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    body,
                ))
            }

            Self::Fork(loc, channel, annotation, process) => Arc::new(process::Expression::Fork(
                loc.clone(),
                Captures::new(),
                Internal::Original(channel.clone()),
                original(annotation),
                (),
                process.compile(None, None)?,
            )),

            Self::Construction(loc, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    process,
                ))
            }

            Self::Application(loc, name, Apply::Noop(_)) => Arc::new(
                process::Expression::Reference(loc.clone(), Internal::Original(name.clone()), ()),
            ),

            Self::Application(loc, name, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Expression::Fork(
                    loc.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    Arc::new(process::Process::Let(
                        loc.clone(),
                        Internal::Object(None),
                        None,
                        (),
                        Arc::new(process::Expression::Reference(
                            loc.clone(),
                            Internal::Original(name.clone()),
                            (),
                        )),
                        process,
                    )),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Construct<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(loc, argument, construct) => {
                let argument = argument.compile()?;
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Send(argument, process),
                ))
            }

            Self::Receive(loc, parameter, annotation, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Receive(
                        Internal::Original(parameter.clone()),
                        original(annotation),
                        process,
                    ),
                ))
            }

            Self::Choose(loc, chosen, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
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
                    Internal::Result(None),
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Break(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Result(None),
                (),
                process::Command::Break,
            )),

            Self::Begin(loc, label, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Begin(Some(Internal::Result(label.clone())), process),
                ))
            }

            Self::Loop(loc, label) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Result(None),
                (),
                process::Command::Loop(Some(Internal::Result(label.clone()))),
            )),
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> ConstructBranch<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Receive(loc, parameter, annotation, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Receive(
                        Internal::Original(parameter.clone()),
                        original(annotation),
                        process,
                    ),
                ))
            }
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Apply<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Noop(loc) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Result(None),
                (),
                process::Command::Link(Arc::new(process::Expression::Reference(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                ))),
            )),

            Self::Send(loc, expression, apply) => {
                let expression = expression.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Send(expression, process),
                ))
            }

            Self::Choose(loc, chosen, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
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
                    Internal::Object(None),
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Begin(loc, label, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Begin(Some(Internal::Object(label.clone())), process),
                ))
            }

            Self::Loop(loc, label) => Arc::new(process::Process::Do(
                loc.clone(),
                Internal::Object(None),
                (),
                process::Command::Loop(Some(Internal::Object(label.clone()))),
            )),
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> ApplyBranch<Loc, Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Then(loc, name, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Let(
                    loc.clone(),
                    Internal::Original(name.clone()),
                    None,
                    (),
                    Arc::new(process::Expression::Reference(
                        loc.clone(),
                        Internal::Object(None),
                        (),
                    )),
                    Arc::new(process::Process::Do(
                        loc.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::Link(expression),
                    )),
                ))
            }

            Self::Receive(loc, parameter, annotation, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Receive(
                        Internal::Original(parameter.clone()),
                        original(annotation),
                        process,
                    ),
                ))
            }

            Self::Continue(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Continue(Arc::new(process::Process::Do(
                        loc.clone(),
                        Internal::Result(None),
                        (),
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
        do_result: DoResult<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        Ok(match self {
            Self::Let(loc, name, annotation, expression, process) => {
                Arc::new(process::Process::Let(
                    loc.clone(),
                    Internal::Original(name.clone()),
                    original(annotation),
                    (),
                    expression.compile()?,
                    process.compile(pass, do_result)?,
                ))
            }

            Self::Command(name, command) => command.compile(name, pass, do_result)?,

            Self::Pass(loc) => match pass {
                Some(process) => process,
                None => Err(CompileError::PassNotPossible(loc.clone()))?,
            },

            Self::Telltypes(loc) => Arc::new(process::Process::Telltypes(loc.clone())),

            Self::Noop(loc) => match do_result {
                Some(expression) => Arc::new(process::Process::Do(
                    loc.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                )),
                None => Err(CompileError::MustEndProcess(loc.clone()))?,
            },
        })
    }
}

impl<Loc: Clone, Name: Clone + Hash + Eq> Command<Loc, Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Loc, Name>,
        do_result: DoResult<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass, do_result)?,

            Self::Link(loc, expression) => {
                if do_result.is_some() {
                    return Err(CompileError::CannotEndInDoExpression(loc.clone()));
                }
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(loc, argument, command) => {
                let argument = argument.compile()?;
                let process = command.compile(object_name, pass, do_result)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Send(argument, process),
                ))
            }

            Self::Receive(loc, parameter, annotation, command) => {
                let process = command.compile(object_name, pass, do_result)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Receive(
                        Internal::Original(parameter.clone()),
                        original(annotation),
                        process,
                    ),
                ))
            }

            Self::Choose(loc, chosen, command) => {
                let process = command.compile(object_name, pass, do_result)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(loc, CommandBranches(process_branches), optional_process) => {
                let pass = match optional_process {
                    Some(process) => Some(process.compile(pass, do_result.clone())?),
                    None => pass,
                };

                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, process_branch) in process_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(process_branch.compile(
                        object_name,
                        pass.clone(),
                        do_result.clone(),
                    )?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Break(loc) => {
                if do_result.is_some() {
                    return Err(CompileError::CannotEndInDoExpression(loc.clone()));
                }
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Break,
                ))
            }

            Self::Continue(loc, process) => {
                let process = process.compile(pass, do_result)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Continue(process),
                ))
            }

            Self::Begin(loc, label, command) => {
                let process = command.compile(object_name, pass, do_result)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Begin(label.clone().map(Internal::Original), process),
                ))
            }

            Self::Loop(loc, label) => Arc::new(process::Process::Do(
                loc.clone(),
                object_internal,
                (),
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
        do_result: DoResult<Loc, Name>,
    ) -> Result<Arc<process::Process<Loc, Internal<Name>, ()>>, CompileError<Loc>> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass, do_result)?,

            Self::Receive(loc, parameter, annotation, branch) => {
                let process = branch.compile(object_name, pass, do_result)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Receive(
                        Internal::Original(parameter.clone()),
                        original(annotation),
                        process,
                    ),
                ))
            }

            Self::Continue(loc, process) => {
                let process = process.compile(pass, do_result)?;
                Arc::new(process::Process::Do(
                    loc.clone(),
                    object_internal,
                    (),
                    process::Command::Continue(process),
                ))
            }
        })
    }
}

fn original<Loc: Clone, Name: Clone + Eq + Hash>(
    annotation: &Option<Type<Loc, Name>>,
) -> Option<Type<Loc, Internal<Name>>> {
    annotation
        .clone()
        .map(|t| t.map_names(&mut Internal::Original))
}
