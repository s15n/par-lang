// why not rename this file to ast.rs?

use std::{fmt::Display, hash::Hash, sync::Arc};

use super::{
    process::{self, Captures},
    types::Type,
};
use crate::location::{Point, Span, Spanning};
use indexmap::{IndexMap, IndexSet};

#[derive(Clone, Debug)]
pub struct Name {
    pub span: Span,
    pub string: String,
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
    pub typ: TypeNode<Name>,
}

#[derive(Clone, Debug)]
pub struct Declaration<Name> {
    pub span: Span,
    pub name: Name,
    pub typ: TypeNode<Name>,
}

#[derive(Clone, Debug)]
pub struct Definition<Name, Expr> {
    pub span: Span,
    pub name: Name,
    pub expression: Expr,
}

#[derive(Clone, Debug)]
pub enum TypeNode<Name> {
    Chan(Span, Box<Self>),
    /// type variable
    Var(Span, Name),
    /// named type
    Name(Span, Name, Vec<Type<Name>>),
    Send(Span, Box<Self>, Box<Self>),
    Receive(Span, Box<Self>, Box<Self>),
    Either(Span, IndexMap<Name, Self>),
    Choice(Span, IndexMap<Name, Self>),
    /// ! (unit)
    Break(Span),
    /// ? (bottom)
    Continue(Span),
    Recursive {
        span: Span,
        /*
        The ascendents of the type (denoted by the names of the respective loop points):
        If you `begin` on a `recursive`, and it expands, so its `self`s get replaced by new
        `recursive`s, these new `recursive`s will have as their *ascendent* the original `recursive`.
        This is for totality checking.
         */
        asc: IndexSet<Option<Name>>,
        label: Option<Name>,
        body: Box<Self>,
    },
    Iterative {
        span: Span,
        asc: IndexSet<Option<Name>>,
        label: Option<Name>,
        body: Box<Self>,
    },
    Self_(Span, Option<Name>),
    SendType(Span, Name, Box<Self>),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Pattern<Name> {
    Name(Span, Name, Option<TypeNode<Name>>),
    Receive(Span, Box<Self>, Box<Self>),
    Continue(Span),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Expression<Name> {
    Reference(Span, Name),
    Grouped(Span, Box<Self>),
    Let {
        span: Span,
        pattern: Pattern<Name>,
        expression: Box<Self>,
        then: Box<Self>,
    },
    Do {
        span: Span,
        process: Box<Process<Name>>,
        then: Box<Self>,
    },
    Fork {
        span: Span,
        channel: Name,
        annotation: Option<TypeNode<Name>>,
        process: Box<Process<Name>>,
    },
    Construction(Construct<Name>),
    Application(Span, Box<Self>, Apply<Name>),
}

#[derive(Clone, Debug)]
pub enum Construct<Name> {
    /// wraps an expression
    Then(Box<Expression<Name>>),
    Send(Span, Box<Expression<Name>>, Box<Self>),
    Receive(Span, Pattern<Name>, Box<Self>),
    /// constructs an either type
    Choose(Span, Name, Box<Self>),
    /// constructs a choice type
    Either(Span, ConstructBranches<Name>),
    /// ! (unit)
    Break(Span),
    Begin {
        span: Span,
        unfounded: bool,
        label: Option<Name>,
        then: Box<Self>,
    },
    Loop(Span, Option<Name>),
    SendType(Span, TypeNode<Name>, Box<Self>),
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
    Noop(Point),
    Send(Span, Box<Expression<Name>>, Box<Self>),
    Choose(Span, Name, Box<Self>),
    Either(Span, ApplyBranches<Name>),
    Begin {
        span: Span,
        unfounded: bool,
        label: Option<Name>,
        then: Box<Self>,
    },
    Loop(Span, Option<Name>),
    SendType(Span, TypeNode<Name>, Box<Self>),
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

// span doesn't include the "then" process
#[derive(Clone, Debug)]
pub enum Process<Name> {
    Let {
        span: Span,
        pattern: Pattern<Name>,
        value: Box<Expression<Name>>,
        then: Box<Self>,
    },
    Command(Name, Command<Name>),
    Telltypes(Span, Box<Self>),
    Noop(Point),
}

#[derive(Clone, Debug)]
pub enum Command<Name> {
    Then(Box<Process<Name>>),
    Link(Span, Box<Expression<Name>>),
    Send(Span, Expression<Name>, Box<Self>),
    Receive(Span, Pattern<Name>, Box<Self>),
    Choose(Span, Name, Box<Self>),
    Either(Span, CommandBranches<Name>, Option<Box<Process<Name>>>),
    Break(Span),
    Continue(Span, Box<Process<Name>>),
    Begin {
        span: Span,
        unfounded: bool,
        label: Option<Name>,
        then: Box<Self>,
    },
    Loop(Span, Option<Name>),
    SendType(Span, TypeNode<Name>, Box<Self>),
    ReceiveType(Span, Name, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct CommandBranches<Name>(pub IndexMap<Name, CommandBranch<Name>>);

#[derive(Clone, Debug)]
pub enum CommandBranch<Name> {
    Then(Span, Process<Name>),
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

impl Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state)
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for Name {}

impl PartialOrd for Name {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.string.partial_cmp(&other.string)
    }
}

impl Ord for Name {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.string.cmp(&other.string)
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl Internal<Name> {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Original(name) | Self::Result(Some(name)) | Self::Object(Some(name)) => {
                Some(name.span.clone())
            }

            _ => None,
        }
    }
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

impl<Name, Expr> Default for Program<Name, Expr> {
    fn default() -> Self {
        Self {
            type_defs: Vec::new(),
            declarations: Vec::new(),
            definitions: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompileError {
    MustEndProcess(Span),
}

impl Spanning for CompileError {
    fn span(&self) -> Span {
        match self {
            CompileError::MustEndProcess(span) => span.clone(),
        }
    }
}

impl CompileError {
    pub fn message(&self) -> &str {
        match self {
            CompileError::MustEndProcess(_) => "process must end",
        }
    }
}

type Pass<Name> = Option<Arc<process::Process<Internal<Name>, ()>>>;

impl<Name: Clone + Hash + Eq> Pattern<Name> {
    pub fn compile_let(
        &self,
        loc: &Span,
        expression: Arc<process::Expression<Internal<Name>, ()>>,
        process: Arc<process::Process<Internal<Name>, ()>>,
    ) -> Arc<process::Process<Internal<Name>, ()>> {
        if let Self::Name(_, name, annotation) = self {
            return Arc::new(process::Process::Let {
                span: loc.clone(),
                name: Internal::Original(name.clone()),
                annotation: original(annotation),
                typ: (),
                value: expression,
                then: process,
            });
        }
        Arc::new(process::Process::Let {
            span: loc.clone(),
            name: Internal::Match(0),
            annotation: self.annotation(),
            typ: (),
            value: expression,
            then: self.compile_helper(0, process),
        })
    }

    pub fn compile_receive(
        &self,
        level: usize,
        loc: &Span,
        subject: &Internal<Name>,
        process: Arc<process::Process<Internal<Name>, ()>>,
    ) -> Arc<process::Process<Internal<Name>, ()>> {
        if let Self::Name(_, name, annotation) = self {
            return Arc::new(process::Process::Do {
                span: loc.clone(),
                name: subject.clone(),
                typ: (),
                command: process::Command::Receive(
                    Internal::Original(name.clone()),
                    original(annotation),
                    process,
                ),
            });
        }
        Arc::new(process::Process::Do {
            span: loc.clone(),
            name: subject.clone(),
            typ: (),
            command: process::Command::Receive(
                Internal::Match(level),
                self.annotation(),
                self.compile_helper(level, process),
            ),
        })
    }

    fn compile_helper(
        &self,
        level: usize,
        process: Arc<process::Process<Internal<Name>, ()>>,
    ) -> Arc<process::Process<Internal<Name>, ()>> {
        match self {
            Self::Name(loc, name, annotation) => Arc::new(process::Process::Let {
                span: loc.clone(),
                name: Internal::Original(name.clone()),
                annotation: original(annotation),
                typ: (),
                value: Arc::new(process::Expression::Reference(
                    loc.clone(),
                    Internal::Match(level),
                    (),
                )),
                then: process,
            }),

            Self::Receive(loc, first, rest) => first.compile_receive(
                level + 1,
                loc,
                &Internal::Match(level),
                rest.compile_helper(level, process),
            ),

            Self::Continue(loc) => Arc::new(process::Process::Do {
                span: loc.clone(),
                name: Internal::Match(level),
                typ: (),
                command: process::Command::Continue(process),
            }),

            Self::ReceiveType(loc, parameter, rest) => Arc::new(process::Process::Do {
                span: loc.clone(),
                name: Internal::Match(level),
                typ: (),
                command: process::Command::ReceiveType(
                    Internal::Original(parameter.clone()),
                    rest.compile_helper(level, process),
                ),
            }),
        }
    }

    fn annotation(&self) -> Option<TypeNode<Internal<Name>>> {
        match self {
            Self::Name(_, _, annotation) => original(annotation),
            Self::Receive(loc, first, rest) => {
                let first = first.annotation()?;
                let rest = rest.annotation()?;
                Some(Type::Send(loc.clone(), Box::new(first), Box::new(rest)))
            }
            Self::Continue(loc) => Some(Type::Break(loc.clone())),
            Self::ReceiveType(loc, parameter, rest) => {
                let rest = rest.annotation()?;
                Some(Type::SendType(
                    loc.clone(),
                    Internal::Original(parameter.clone()),
                    Box::new(rest),
                ))
            }
        }
    }
}

impl<Name> Spanning for Pattern<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Name(span, _, _)
            | Self::Continue(span)
            | Self::Receive(span, _, _)
            | Self::ReceiveType(span, _, _) => span.clone(),
        }
    }
}

impl<Name: Clone + Hash + Eq> Expression<Name> {
    pub fn compile(&self) -> Result<Arc<process::Expression<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Reference(loc, name) => Arc::new(process::Expression::Reference(
                loc.clone(),
                Internal::Original(name.clone()),
                (),
            )),

            Self::Grouped(_, expression) => expression.compile()?,

            Self::Let {
                span,
                pattern,
                expression,
                then: body,
            } => {
                let expression = expression.compile()?;
                let body = body.compile()?;
                Arc::new(process::Expression::Fork {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: Internal::Result(None),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: pattern.compile_let(
                        span,
                        expression,
                        Arc::new(process::Process::Do {
                            span: span.clone(),
                            name: Internal::Result(None),
                            typ: (),
                            command: process::Command::Link(body),
                        }),
                    ),
                })
            }

            Self::Do {
                span,
                process,
                then: expression,
            } => {
                let expression = expression.compile()?;
                let body = process.compile(Some(Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::Link(expression),
                })))?;
                Arc::new(process::Expression::Fork {
                    span: span.clone(),
                    captures: Captures::new(),
                    chan_name: Internal::Result(None),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: body,
                })
            }

            Self::Fork {
                span,
                channel,
                annotation,
                process,
            } => Arc::new(process::Expression::Fork {
                span: span.clone(),
                captures: Captures::new(),
                chan_name: Internal::Original(channel.clone()),
                chan_annotation: original(annotation),
                chan_type: (),
                expr_type: (),
                process: process.compile(None)?,
            }),

            Self::Construction(construct) => {
                let process = construct.compile()?;
                Arc::new(process::Expression::Fork {
                    span: construct.span().clone(),
                    captures: Captures::new(),
                    chan_name: Internal::Result(None),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process,
                })
            }

            Self::Application(_, expr, Apply::Noop(_)) => expr.compile()?,

            Self::Application(loc, expr, apply) => {
                let expr = expr.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Expression::Fork {
                    span: loc.clone(),
                    captures: Captures::new(),
                    chan_name: Internal::Result(None),
                    chan_annotation: None,
                    chan_type: (),
                    expr_type: (),
                    process: Arc::new(process::Process::Let {
                        span: loc.clone(),
                        name: Internal::Object(None),
                        annotation: None,
                        typ: (),
                        value: expr,
                        then: process,
                    }),
                })
            }
        })
    }
}

impl<Name> Spanning for Expression<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Reference(span, _)
            | Self::Grouped(span, _)
            | Self::Let { span, .. }
            | Self::Do { span, .. }
            | Self::Fork { span, .. }
            | Self::Application(span, _, _) => span.clone(),

            Self::Construction(construction) => construction.span(),
        }
    }
}

impl<Name: Clone + Hash + Eq> Construct<Name> {
    pub fn compile(&self) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Then(expression) => {
                let span = expression.span().clone();
                let expression = expression.compile()?;
                Arc::new(process::Process::Do {
                    span: span,
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            Self::Send(loc, argument, construct) => {
                let argument = argument.compile()?;
                let process = construct.compile()?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::Send(argument, process),
                })
            }

            Self::Receive(loc, pattern, construct) => {
                let process = construct.compile()?;
                pattern.compile_receive(0, loc, &Internal::Result(None), process)
            }

            Self::Choose(span, chosen, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::Choose(Internal::Original(chosen.clone()), process),
                })
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
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::Match(branches, processes),
                })
            }

            Self::Break(loc) => Arc::new(process::Process::Do {
                span: loc.clone(),
                name: Internal::Result(None),
                typ: (),
                command: process::Command::Break,
            }),

            Self::Begin {
                span,
                unfounded,
                label,
                then: construct,
            } => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::Begin {
                        unfounded: *unfounded,
                        label: Some(Internal::Result(label.clone())),
                        captures: Captures::new(),
                        body: process,
                    },
                })
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: Internal::Result(None),
                typ: (),
                command: process::Command::Loop(Some(Internal::Result(label.clone())), Captures::new()),
            }),

            Self::SendType(loc, argument, construct) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = construct.compile()?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::SendType(argument, process),
                })
            }

            Self::ReceiveType(loc, parameter, construct) => {
                let process = construct.compile()?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::ReceiveType(
                        Internal::Original(parameter.clone()),
                        process,
                    ),
                })
            }
        })
    }
}

impl<Name> Spanning for Construct<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Send(span, _, _)
            | Self::Receive(span, _, _)
            | Self::Choose(span, _, _)
            | Self::Either(span, _)
            | Self::Break(span)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendType(span, _, _)
            | Self::ReceiveType(span, _, _) => span.clone(),

            Self::Then(expression) => expression.span(),
        }
    }
}

impl<Name: Clone + Hash + Eq> ConstructBranch<Name> {
    pub fn compile(&self) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Then(loc, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            Self::Receive(loc, pattern, branch) => {
                let process = branch.compile()?;
                pattern.compile_receive(0, loc, &Internal::Result(None), process)
            }

            Self::ReceiveType(loc, parameter, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::ReceiveType(
                        Internal::Original(parameter.clone()),
                        process,
                    ),
                })
            }
        })
    }
}

impl<Name> Spanning for ConstructBranch<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Then(span, _) | Self::Receive(span, _, _) | Self::ReceiveType(span, _, _) => {
                span.clone()
            }
        }
    }
}

impl<Name: Clone + Hash + Eq> Apply<Name> {
    pub fn compile(&self) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Noop(point) => {
                let span = point.point_span();
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Result(None),
                    typ: (),
                    command: process::Command::Link(Arc::new(process::Expression::Reference(
                        span.clone(),
                        Internal::Object(None),
                        (),
                    ))),
                })
            }

            Self::Send(loc, expression, apply) => {
                let expression = expression.compile()?;
                let process = apply.compile()?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: Internal::Object(None),
                    typ: (),
                    command: process::Command::Send(expression, process),
                })
            }

            Self::Choose(span, chosen, apply) => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Object(None),
                    typ: (),
                    command: process::Command::Choose(Internal::Original(chosen.clone()), process),
                })
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
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Object(None),
                    typ: (),
                    command: process::Command::Match(branches, processes),
                })
            }

            Self::Begin {
                span,
                unfounded,
                label,
                then: apply,
            } => {
                let process = apply.compile()?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Object(None),
                    typ: (),
                    command: process::Command::Begin {
                        unfounded: *unfounded,
                        label: Some(Internal::Object(label.clone())),
                        captures: Captures::new(),
                        body: process,
                    },
                })
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: Internal::Object(None),
                typ: (),
                command: process::Command::Loop(Some(Internal::Object(label.clone())), Captures::new()),
            }),

            Self::SendType(loc, argument, apply) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = apply.compile()?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: Internal::Object(None),
                    typ: (),
                    command: process::Command::SendType(argument, process),
                })
            }
        })
    }
}

impl Spanning for Apply<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Send(span, _, _)
            | Self::Choose(span, _, _)
            | Self::Either(span, _)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendType(span, _, _) => span.clone(),

            Self::Noop(point) => point.point_span(),
        }
    }
}

impl<Name: Clone + Hash + Eq> ApplyBranch<Name> {
    pub fn compile(&self) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Then(span, name, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Let {
                    span: span.clone(),
                    name: Internal::Original(name.clone()),
                    annotation: None,
                    typ: (),
                    value: Arc::new(process::Expression::Reference(
                        span.clone(),
                        Internal::Object(None),
                        (),
                    )),
                    then: Arc::new(process::Process::Do {
                        span: span.clone(),
                        name: Internal::Result(None),
                        typ: (),
                        command: process::Command::Link(expression),
                    }),
                })
            }

            Self::Receive(loc, pattern, branch) => {
                let process = branch.compile()?;
                pattern.compile_receive(0, loc, &Internal::Object(None), process)
            }

            Self::Continue(span, expression) => {
                let expression = expression.compile()?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: Internal::Object(None),
                    typ: (),
                    command: process::Command::Continue(Arc::new(process::Process::Do {
                        span: span.clone(),
                        name: Internal::Result(None),
                        typ: (),
                        command: process::Command::Link(expression),
                    })),
                })
            }

            Self::ReceiveType(loc, parameter, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: Internal::Object(None),
                    typ: (),
                    command: process::Command::ReceiveType(
                        Internal::Original(parameter.clone()),
                        process,
                    ),
                })
            }
        })
    }
}

impl<Name> Spanning for ApplyBranch<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Then(span, _, _)
            | Self::Receive(span, _, _)
            | Self::Continue(span, _)
            | Self::ReceiveType(span, _, _) => span.clone(),
        }
    }
}

impl<Name: Clone + Hash + Eq> Process<Name> {
    pub fn compile(
        &self,
        pass: Pass<Name>,
    ) -> Result<Arc<process::Process<Internal<Name>, ()>>, CompileError> {
        Ok(match self {
            Self::Let {
                span,
                pattern,
                value,
                then,
            } => pattern.compile_let(span, value.compile()?, then.compile(pass)?),

            Self::Command(name, command) => command.compile(name, pass)?,

            Self::Telltypes(loc, process) => Arc::new(process::Process::Telltypes(
                loc.clone(),
                process.compile(pass)?,
            )),

            Self::Noop(point) => match pass {
                Some(process) => process,
                None => Err(CompileError::MustEndProcess(point.point_span()))?,
            },
        })
    }
}

impl<Name> Spanning for Process<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Let { span, .. } | Self::Telltypes(span, _) => span.clone(),

            Self::Command(_, command) => command.span(),
            Self::Noop(point) => point.point_span(),
        }
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
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::Link(expression),
                })
            }

            Self::Send(loc, argument, command) => {
                let argument = argument.compile()?;
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::Send(argument, process),
                })
            }

            Self::Receive(loc, pattern, command) => {
                let process = command.compile(object_name, pass)?;
                pattern.compile_receive(0, loc, &object_internal, process)
            }

            Self::Choose(span, chosen, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::Choose(Internal::Original(chosen.clone()), process),
                })
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
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::Match(branches, processes),
                })
            }

            Self::Break(span) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: object_internal,
                typ: (),
                command: process::Command::Break,
            }),

            Self::Continue(span, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::Continue(process),
                })
            }

            Self::Begin {
                span,
                unfounded,
                label,
                then: command,
            } => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::Begin {
                        unfounded: *unfounded,
                        label: label.clone().map(Internal::Original),
                        captures: Captures::new(),
                        body: process,
                    },
                })
            }

            Self::Loop(span, label) => Arc::new(process::Process::Do {
                span: span.clone(),
                name: object_internal,
                typ: (),
                command: process::Command::Loop(label.clone().map(Internal::Original), Captures::new()),
            }),

            Self::SendType(loc, argument, command) => {
                let argument = argument.clone().map_names(&mut Internal::Original);
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::SendType(argument, process),
                })
            }

            Self::ReceiveType(loc, parameter, command) => {
                let process = command.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::ReceiveType(
                        Internal::Original(parameter.clone()),
                        process,
                    ),
                })
            }
        })
    }
}

impl<Name> Spanning for Command<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Link(span, _)
            | Self::Send(span, _, _)
            | Self::Receive(span, _, _)
            | Self::Choose(span, _, _)
            | Self::Either(span, _, _)
            | Self::Break(span)
            | Self::Continue(span, _)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendType(span, _, _)
            | Self::ReceiveType(span, _, _) => span.clone(),

            Self::Then(process) => process.span(),
        }
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
            Self::Then(_, process) => process.compile(pass)?,

            Self::Receive(loc, pattern, branch) => {
                let process = branch.compile(object_name, pass)?;
                pattern.compile_receive(0, loc, &object_internal, process)
            }

            Self::Continue(span, process) => {
                let process = process.compile(pass)?;
                Arc::new(process::Process::Do {
                    span: span.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::Continue(process),
                })
            }

            Self::ReceiveType(loc, parameter, branch) => {
                let process = branch.compile(object_name, pass)?;
                Arc::new(process::Process::Do {
                    span: loc.clone(),
                    name: object_internal,
                    typ: (),
                    command: process::Command::ReceiveType(
                        Internal::Original(parameter.clone()),
                        process,
                    ),
                })
            }
        })
    }
}

impl<Name> Spanning for CommandBranch<Name> {
    fn span(&self) -> Span {
        match self {
            Self::Then(span, _)
            | Self::Receive(span, _, _)
            | Self::Continue(span, _)
            | Self::ReceiveType(span, _, _) => span.clone(),
        }
    }
}

fn original<Name: Clone + Eq + Hash>(
    annotation: &Option<TypeNode<Name>>,
) -> Option<TypeNode<Internal<Name>>> {
    annotation
        .clone()
        .map(|t| t.map_names(&mut Internal::Original))
}
