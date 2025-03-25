use std::fmt::Display;
use std::hash::Hash;
use std::str::FromStr;
use std::sync::Arc;
use indexmap::IndexMap;
use crate::par::language::{CompileError, Pass};
use crate::par::location::Span;
use crate::par::process;
use crate::par::process::Captures;
use crate::par::types::Type;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name {
    pub string: String,
}

impl From<String> for Name {
    fn from(string: String) -> Self {
        Self { string }
    }
}
impl FromStr for Name {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Name::from(s.to_owned()))
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

#[derive(Clone, Debug)]
pub struct Program<Name, Expr> {
    pub type_defs: Vec<TypeDef<Name>>,
    pub declarations: Vec<Declaration<Name>>,
    pub definitions: Vec<Definition<Name, Expr>>,
}

#[derive(Clone, Debug)]
pub struct TypeDef<Name> {
    pub span: Span,
    pub name: Name,
    pub params: Vec<Name>,
    pub typ: Type<Name>,
}

#[derive(Clone, Debug)]
pub struct Declaration<Name> {
    pub span: Span,
    pub name: Name,
    pub typ: Type<Name>,
}

#[derive(Clone, Debug)]
pub struct Definition<Name, Expr> {
    pub span: Span,
    pub name: Name,
    pub expression: Expr,
}

impl<Name, Expr> Default for Program<Name, Expr> {
    fn default() -> Self {
        Self {
            type_defs: Default::default(),
            declarations: Default::default(),
            definitions: Default::default(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Pattern<Name> {
    Name(Span, Name, Option<Type<Name>>),
    Receive(Span, Box<Self>, Box<Self>),
    Continue(Span),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Expression<Name> {
    Reference(Span, Name),
    Let {
        span: Span,
        pattern: Pattern<Name>,
        expression: Box<Self>,
        then: Box<Self>,
    },
    Do {
        span: Span,
        process: Box<Process<Name>>,
        expression: Box<Self>,
    },
    Fork {
        span: Span,
        channel: Name,
        annotation: Option<Type<Name>>,
        process: Box<Process<Name>>,
    },
    Construction(Span, Construct<Name>),
    Application(Span, Box<Self>, Apply<Name>),
}

#[derive(Clone, Debug)]
pub enum Construct<Name> {
    /// wraps an expression
    Then(Span, Box<Expression<Name>>),
    Send(Span, Box<Expression<Name>>, Box<Self>),
    Receive(Span, Pattern<Name>, Box<Self>),
    /// constructs an either type
    Choose(Span, Name, Box<Self>),
    /// constructs a choice type
    Either(Span, ConstructBranches<Name>),
    /// ! (unit)
    Break(Span),
    Begin { span: Span, unfounded: bool, label: Option<Name>, then: Box<Self> },
    Loop(Span, Option<Name>),
    SendType(Span, Type<Name>, Box<Self>),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct ConstructBranches<Name>(pub IndexMap<Name, ConstructBranch<Name>>);

#[derive(Clone, Debug)]
pub enum ConstructBranch<Name> {
    Then(Span, Expression<Name>),
    Receive(Span, Pattern<Name>, Box<Self>),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Apply<Name> {
    Noop(Span),
    Send(Span, Box<Expression<Name>>, Box<Self>),
    Choose(Span, Name, Box<Self>),
    Either(Span, ApplyBranches<Name>),
    Begin { span: Span, unfounded: bool, label: Option<Name>, then: Box<Self> },
    Loop(Span, Option<Name>),
    SendType(Span, Type<Name>, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct ApplyBranches<Name>(pub IndexMap<Name, ApplyBranch<Name>>);

#[derive(Clone, Debug)]
pub enum ApplyBranch<Name> {
    Then(Span, Name, Expression<Name>),
    Receive(Span, Pattern<Name>, Box<Self>),
    Continue(Span, Expression<Name>),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Process<Name> {
    Let { span: Span, pattern: Pattern<Name>, value: Expression<Name>, then: Box<Self> },
    Command(Name, Command<Name>),
    Telltypes(Span, Box<Self>),
    Noop(Span),
}

#[derive(Clone, Debug)]
pub enum Command<Name> {
    Then(Box<Process<Name>>),
    Link(Span, Box<Expression<Name>>),
    Send(Span, Box<Expression<Name>>, Box<Self>),
    Receive(Span, Pattern<Name>, Box<Self>),
    Choose(Span, Name, Box<Self>),
    Either(
        Span,
        CommandBranches<Name>,
        Option<Box<Process<Name>>>,
    ),
    Break(Span),
    Continue(Span, Box<Process<Name>>),
    Begin { span: Span, unfounded: bool, label: Option<Name>, then: Box<Self> },
    Loop(Span, Option<Name>),
    SendType(Span, Type<Name>, Box<Self>),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct CommandBranches<Name>(pub IndexMap<Name, CommandBranch<Name>>);

#[derive(Clone, Debug)]
pub enum CommandBranch<Name> {
    Then(Process<Name>),
    Receive(Span, Pattern<Name>, Box<Self>),
    Continue(Span, Process<Name>),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Internal<Name> {
    Original(Name),
    Result(Option<Name>),
    Object(Option<Name>),
    Match(usize),
}

impl<Name: From<String>> From<String> for Internal<Name> {
    fn from(value: String) -> Self {
        Self::Original(Name::from(value))
    }
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
            Self::Match(level) => write!(f, "#match{}", level),
        }
    }
}

impl<Name: Clone + Hash + Eq> Pattern<Name> {
    pub fn compile_let(
        &self,
        span: &Span,
        expression: Arc<process::Expression<Internal<Name>, ()>>,
        process: Arc<process::Process<Internal<Name>, ()>>,
    ) -> Arc<process::Process<Internal<Name>, ()>> {
        if let Self::Name(_, name, annotation) = self {
            return Arc::new(process::Process::Let(
                span.clone(),
                Internal::Original(name.clone()),
                original(annotation),
                (),
                expression,
                process,
            ));
        }
        Arc::new(process::Process::Let(
            span.clone(),
            Internal::Match(0),
            self.annotation(),
            (),
            expression,
            self.compile_helper(0, process),
        ))
    }

    pub fn compile_receive(
        &self,
        level: usize,
        span: &Span,
        subject: &Internal<Name>,
        process: Arc<process::Process<Internal<Name>, ()>>,
    ) -> Arc<process::Process<Internal<Name>, ()>> {
        if let Self::Name(_, name, annotation) = self {
            return Arc::new(process::Process::Do(
                span.clone(),
                subject.clone(),
                (),
                process::Command::Receive(
                    Internal::Original(name.clone()),
                    original(annotation),
                    process,
                ),
            ));
        }
        Arc::new(process::Process::Do(
            span.clone(),
            subject.clone(),
            (),
            process::Command::Receive(
                Internal::Match(level),
                self.annotation(),
                self.compile_helper(level, process),
            ),
        ))
    }

    fn compile_helper(
        &self,
        level: usize,
        process: Arc<process::Process<Internal<Name>, ()>>,
    ) -> Arc<process::Process<Internal<Name>, ()>> {
        match self {
            Self::Name(span, name, annotation) => Arc::new(process::Process::Let(
                span.clone(),
                Internal::Original(name.clone()),
                original(annotation),
                (),
                Arc::new(process::Expression::Reference(
                    span.clone(),
                    Internal::Match(level),
                    (),
                )),
                process,
            )),

            Self::Receive(span, first, rest) => first.compile_receive(
                level + 1,
                span,
                &Internal::Match(level),
                rest.compile_helper(level, process),
            ),

            Self::Continue(span) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Match(level),
                (),
                process::Command::Continue(process),
            )),

            Self::ReceiveType(span, parameter, rest) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Match(level),
                (),
                process::Command::ReceiveType(
                    Internal::Original(parameter.clone()),
                    rest.compile_helper(level, process),
                ),
            )),
        }
    }

    fn annotation(&self) -> Option<Type<Internal<Name>>> {
        match self {
            Self::Name(_, _, annotation) => original(annotation),
            Self::Receive(span, first, rest) => {
                let first = first.annotation()?;
                let rest = rest.annotation()?;
                Some(Type::Send(span.clone(), Box::new(first), Box::new(rest)))
            }
            Self::Continue(span) => Some(Type::Break(span.clone())),
            Self::ReceiveType(span, parameter, rest) => {
                let rest = rest.annotation()?;
                Some(Type::SendType(
                    span.clone(),
                    Internal::Original(parameter.clone()),
                    Box::new(rest),
                ))
            }
        }
    }
}

impl<Name: Clone + Hash + Eq> Expression<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Expression<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Reference(span, name) => Arc::new(process::Expression::Reference(
                span.clone(),
                Internal::Original(name.clone()),
                (),
            )),

            Self::Let(span, pattern, expression, body) => {
                let expression = expression.compile()?;
                let body = body.compile()?;
                Arc::new(process::Expression::Fork(
                    span.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    pattern.compile_let(
                        span,
                        expression,
                        Arc::new(process::Process::Do(
                            span.clone(),
                            Internal::Result(None),
                            (),
                            process::Command::Link(body),
                        )),
                    ),
                ))
            }

            Self::Do(span, process, expression) => {
                let expression = expression.compile()?;
                let body = process.compile(Some(Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))))?;
                Arc::new(process::Expression::Fork(
                    span.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    body,
                ))
            }

            Self::Fork(span, channel, annotation, process) => Arc::new(process::Expression::Fork(
                span.clone(),
                Captures::new(),
                Internal::Original(channel.clone()),
                original(annotation),
                (),
                process.compile(None)?,
            )),

            Self::Construction(span, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Expression::Fork(
                    span.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    process,
                ))
            }

            Self::Application(_, expr, Apply::Noop(_)) => expr.compile()?,

            Self::Application(span, expr, apply) => {
                let expr = expr.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Expression::Fork(
                    span.clone(),
                    Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    Arc::new(process::Process::Let(
                        span.clone(),
                        Internal::Object(None),
                        None,
                        (),
                        expr,
                        process,
                    )),
                ))
            }
        })
    }
}

impl<Name: Clone + Hash + Eq> Construct<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Then(span, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(span, argument, construct) => {
                let argument = argument.compile()?;
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Send(argument, process),
                ))
            }

            Self::Receive(span, pattern, construct) => {
                let process = construct.compile()?;
                pattern.compile_receive(0, span, &Internal::Result(None), process)
            }

            Self::Choose(span, chosen, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(span, ConstructBranches(construct_branches)) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, construct_branch) in construct_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(construct_branch.compile()?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Break(span) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Result(None),
                (),
                process::Command::Break,
            )),

            Self::Begin { span, unfounded, label, then: construct } => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Begin(
                        *unfounded,
                        Some(Internal::Result(label.clone())),
                        process,
                    ),
                ))
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Result(None),
                (),
                process::Command::Loop(Some(Internal::Result(label.clone()))),
            )),

            Self::SendType(span, argument, construct) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::SendType(argument, process),
                ))
            }

            Self::ReceiveType(span, parameter, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Name: Clone + Hash + Eq> ConstructBranch<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Then(span, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Receive(span, pattern, branch) => {
                let process = branch.compile()?;
                pattern.compile_receive(0, span, &Internal::Result(None), process)
            }

            Self::ReceiveType(span, parameter, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Name: Clone + Hash + Eq> Apply<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Noop(span) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Result(None),
                (),
                process::Command::Link(Arc::new(process::Expression::Reference(
                    span.clone(),
                    Internal::Object(None),
                    (),
                ))),
            )),

            Self::Send(span, expression, apply) => {
                let expression = expression.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Send(expression, process),
                ))
            }

            Self::Choose(span, chosen, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(span, ApplyBranches(expression_branches)) => {
                let mut branches = Vec::new();
                let mut processes = Vec::new();
                for (branch_name, expression_branch) in expression_branches {
                    branches.push(Internal::Original(branch_name.clone()));
                    processes.push(expression_branch.compile()?);
                }
                let branches = Arc::from(branches);
                let processes = Box::from(processes);
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Begin { span, unfounded, label, then: apply } => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Begin(
                        *unfounded,
                        Some(Internal::Object(label.clone())),
                        process,
                    ),
                ))
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Object(None),
                (),
                process::Command::Loop(Some(Internal::Object(label.clone()))),
            )),

            Self::SendType(span, argument, apply) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = apply.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::SendType(argument, process),
                ))
            }
        })
    }
}

impl<Name: Clone + Hash + Eq> ApplyBranch<Name> {
    pub fn compile(
        &self,
    ) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Then(span, name, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Let(
                    span.clone(),
                    Internal::Original(name.clone()),
                    None,
                    (),
                    Arc::new(process::Expression::Reference(
                        span.clone(),
                        Internal::Object(None),
                        (),
                    )),
                    Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::Link(expression),
                    )),
                ))
            }

            Self::Receive(span, pattern, branch) => {
                let process = branch.compile()?;
                pattern.compile_receive(0, span, &Internal::Object(None), process)
            }

            Self::Continue(span, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::Continue(Arc::new(process::Process::Do(
                        span.clone(),
                        Internal::Result(None),
                        (),
                        process::Command::Link(expression),
                    ))),
                ))
            }

            Self::ReceiveType(span, parameter, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Object(None),
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Name: Clone + Hash + Eq> Process<Name> {
    pub fn compile(
        &self,
        pass: Pass<Name>,
    ) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Let { span, pattern, value: expression, then: process } => {
                pattern.compile_let(span, expression.compile()?, process.compile(pass)?)
            }

            Self::Command(name, command) => command.compile(name, pass)?,

            Self::Telltypes(span, process) => Arc::new(process::Process::Telltypes(
                span.clone(),
                process.compile(pass)?,
            )),

            Self::Noop(span) => match pass {
                Some(process) => process,
                None => Err(CompileError::MustEndProcess(span.clone()))?,
            },
        })
    }
}

impl<Name: Clone + Hash + Eq> Command<Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Name>,
    ) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass)?,

            Self::Link(span, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(span, argument, command) => {
                let argument = argument.compile()?;
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Send(argument, process),
                ))
            }

            Self::Receive(span, pattern, command) => {
                let process = command.compile(object_name, pass)?;
                pattern.compile_receive(0, span, &object_internal, process)
            }

            Self::Choose(span, chosen, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Choose(Internal::Original(chosen.clone()), process),
                ))
            }

            Self::Either(span, CommandBranches(process_branches), optional_process) => {
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
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Match(branches, processes),
                ))
            }

            Self::Break(span) => Arc::new(process::Process::Do(
                span.clone(),
                object_internal,
                (),
                process::Command::Break,
            )),

            Self::Continue(span, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Continue(process),
                ))
            }

            Self::Begin { span, unfounded, label, then: command } => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Begin(
                        *unfounded,
                        label.clone().map(Internal::Original),
                        process,
                    ),
                ))
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do(
                span.clone(),
                object_internal,
                (),
                process::Command::Loop(label.clone().map(Internal::Original)),
            )),

            Self::SendType(span, argument, command) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::SendType(argument, process),
                ))
            }

            Self::ReceiveType(span, parameter, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

impl<Name: Clone + Hash + Eq> CommandBranch<Name> {
    pub fn compile(
        &self,
        object_name: &Name,
        pass: Pass<Name>,
    ) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        let object_internal = Internal::Original(object_name.clone());

        Ok(match self {
            Self::Then(process) => process.compile(pass)?,

            Self::Receive(span, pattern, branch) => {
                let process = branch.compile(object_name, pass)?;
                pattern.compile_receive(0, span, &object_internal, process)
            }

            Self::Continue(span, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::Continue(process),
                ))
            }

            Self::ReceiveType(span, parameter, branch) => {
                let process = branch.compile(object_name, pass)?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    object_internal,
                    (),
                    process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
                ))
            }
        })
    }
}

fn original<Name: Clone + Eq + Hash>(
    annotation: &Option<Type<Name>>,
) -> Option<Type<Internal<Name>>> {
    annotation
        .clone()
        .map(|t| t.map_names(&mut Internal::Original))
}
