use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;
use indexmap::IndexMap;
use crate::par::language::{CompileError, Pass};
use crate::par::lexer::Token;
use crate::par::location::{Span, Spanning};
use crate::par::process;
use crate::par::types::Type;

//#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Debug, Clone)]
pub struct Name {
    pub span: Span,
    pub string: String,
}

/*impl From<String> for Name {
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
*/
impl From<&Token<'_>> for Name {
    fn from(token: &Token) -> Self {
        Self {
            span: token.span,
            string: token.raw.to_owned()
        }
    }
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
    Receive(Span, Vec<Self>, Box<Self>),
    Continue(Span),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

impl<Name> Spanning for Pattern<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Name(span, _, _)
            | Self::Continue(span)
            | Self::Receive(span, _, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),
        }
    }
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
        annotation: Option<Type<Name>>,
        process: Box<Process<Name>>,
    },
    Construction(Construct<Name>),
    Application(Span, Box<Self>, Apply<Name>),
}

impl<Name> Spanning for Expression<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Reference(span, _)
            | Self::Grouped(span, _)
            | Self::Let { span, .. }
            | Self::Do { span, .. }
            | Self::Fork { span, .. }
            | Self::Application(span, _, _)
            => span.clone(),

            Self::Construction(construction) => construction.span(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Construct<Name> {
    /// wraps an expression
    Then(Box<Expression<Name>>),
    Send(Span, Vec<Expression<Name>>, Box<Self>),
    Receive(Span, Vec<Pattern<Name>>, Box<Self>),
    /// constructs an either type
    Choose(Span, Name, Box<Self>),
    /// constructs a choice type
    Either(Span, ConstructBranches<Name>),
    /// ! (unit)
    Break(Span),
    Begin { span: Span, unfounded: bool, label: Option<Name>, then: Box<Self> },
    Loop(Span, Option<Name>),
    SendTypes(Span, Vec<Type<Name>>, Box<Self>),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

impl<Name> Spanning for Construct<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Send(span, _, _)
            | Self::Receive(span, _, _)
            | Self::Choose(span, _, _)
            | Self::Either(span, _)
            | Self::Break(span)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendTypes(span, _, _)
            | Self::ReceiveTypes(span, _, _)
            => span.clone(),

            Self::Then(expression) => expression.span(),
        }
    }
}

pub fn curry_patterns<T, Name>(
    complete_span: &Span,
    patterns: &Vec<Pattern<Name>>,
    then: T,
    construct: impl Fn(T, &Pattern<Name>) -> T,
    construct_final: impl Fn(&Span, T, &Pattern<Name>) -> T,
) -> T {
    let mut patterns = patterns.into_iter();
    let first = patterns.next().unwrap();
    let result = patterns.rfold(then, construct);
    construct_final(complete_span, result, first)
}

#[derive(Clone, Debug)]
pub struct ConstructBranches<Name>(pub IndexMap<Name, ConstructBranch<Name>>);

#[derive(Clone, Debug)]
pub enum ConstructBranch<Name> {
    Then(Span, Expression<Name>),
    Receive(Span, Vec<Pattern<Name>>, Box<Self>),
    ReceiveTypes(Span, Vec<Name>, Box<Self>),
}

impl<Name> Spanning for ConstructBranch<Name> {
    fn span(&self) -> Span {
        todo!()
    }
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

impl Spanning for Apply<Name> {
    fn span(&self) -> Span {
        match self {
            | Self::Noop(span)
            | Self::Send(span, _, _)
            | Self::Choose(span, _, _)
            | Self::Either(span, _)
            | Self::Begin { span, .. }
            | Self::Loop(span, _)
            | Self::SendType(span, _, _)
            => span.clone(),
        }
    }
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

            Self::Receive(span, patterns, rest) => todo!(),
            /*first.compile_receive(
                level + 1,
                span,
                &Internal::Match(level),
                rest.compile_helper(level, process),
            ),*/

            Self::Continue(span) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Match(level),
                (),
                process::Command::Continue(process),
            )),

            Self::ReceiveTypes(span, params, rest) => Arc::new(process::Process::Do(
                span.clone(),
                Internal::Match(level),
                (),
                /*params.into_iter().rev().fold(process::Command::ReceiveType(
                    Internal::Original(parameter.clone()),
                    rest.compile_helper(level, process),
                ), |rest, parameter| {
                    process::Command::ReceiveType(
                        Internal::Original(parameter.clone()),
                        rest.compile_helper(level, process),
                    )
                })*/
                todo!()
            )),
        }
    }

    fn annotation(&self) -> Option<Type<Internal<Name>>> {
        match self {
            Self::Name(_, _, annotation) => original(annotation),
            Self::Receive(span, patterns, rest) => {
                todo!()
                //let first = first.annotation()?;
                //let rest = rest.annotation()?;
                //Some(Type::Send(span.clone(), Box::new(first), Box::new(rest)))
            }
            Self::Continue(span) => Some(Type::Break(span.clone())),
            Self::ReceiveTypes(span, params, rest) => {
                let rest = rest.annotation()?;
                Some(Type::SendTypes(
                    span.clone(),
                    params.into_iter().map(Name::clone).map(Internal::Original).collect(),
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

            Self::Grouped(_, expression) => expression.compile()?,

            Self::Let { span, pattern, expression, then: body} => {
                let expression = expression.compile()?;
                let body = body.compile()?;
                Arc::new(process::Expression::Fork(
                    span.clone(),
                    process::Captures::new(),
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

            Self::Do { span, process, then: expression } => {
                let expression = expression.compile()?;
                let body = process.compile(Some(Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))))?;
                Arc::new(process::Expression::Fork(
                    span.clone(),
                    process::Captures::new(),
                    Internal::Result(None),
                    None,
                    (),
                    body,
                ))
            }

            Self::Fork { span, channel, annotation, process} => Arc::new(process::Expression::Fork(
                span.clone(),
                process::Captures::new(),
                Internal::Original(channel.clone()),
                original(annotation),
                (),
                process.compile(None)?,
            )),

            Self::Construction(construct) => {
                let process = construct.compile()?;
                Arc::new(process::Expression::Fork(
                    construct.span().clone(),
                    process::Captures::new(),
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
                    process::Captures::new(),
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
            Self::Then(expression) => {
                let span = expression.span().clone();
                let expression = expression.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    process::Command::Link(expression),
                ))
            }

            Self::Send(span, args, construct) => {
                todo!()
                //let argument = argument.compile()?;
                //let process = construct.compile()?;
                //Arc::new(process::Process::Do(
                //    span.clone(),
                //    Internal::Result(None),
                //    (),
                //    process::Command::Send(argument, process),
                //))
            }

            Self::Receive(span, patterns, construct) => {
                let process = construct.compile()?;
                patterns.into_iter().rfold(
                    process,
                    |result, pattern| {
                        pattern.compile_receive(0, &span, &Internal::Result(None), result)
                    },
                )
                /*let span_end = process.span();
                curry_patterns(
                    span,
                    patterns,
                    process,
                    |result, pattern| {
                        let span: Span = (pattern.span()..=span_end).into();
                        pattern.compile_receive(0, &span, &Internal::Result(None), result)
                    },
                    |span, result, pattern| {
                        pattern.compile_receive(0, span, &Internal::Result(None), result)
                    },
                )
                patterns.into_iter().rfold(
                    process,
                    |result, pattern| {
                        let span: Span = (pattern.span()..=span_end).into();
                        pattern.compile_receive(0, &span, &Internal::Result(None), result)
                    },
                )
                 */
                //pattern.compile_receive(0, span, &Internal::Result(None), process)
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

            Self::SendTypes(span, args, construct) => {
                todo!()
                //let args = args.into_iter().map(|arg| arg.map_names(&mut Internal::Original));
                //let process = construct.compile()?;
                //Arc::new(process::Process::Do(
                //    span.clone(),
                //    Internal::Result(None),
                //    (),
                //    args.into_iter().rev().fold(process, |process, arg| {
                //        process::Command::SendType(arg, process)
                //    }),
                //))
            }

            Self::ReceiveTypes(span, params, construct) => {
                let process = construct.compile()?;
                todo!()
                //Arc::new(process::Process::Do(
                //    span.clone(),
                //    Internal::Result(None),
                //    (),
                //    params.into_iter().rev().fold(process, |process, parameter| {
                //        process::Command::ReceiveType(Internal::Original(parameter.clone()), process)
                //    }),
                //))
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

            Self::Receive(span, patterns, branch) => {
                let process = branch.compile()?;
                todo!() //pattern.compile_receive(0, span, &Internal::Result(None), process)
            }

            Self::ReceiveTypes(span, params, branch) => {
                let process = branch.compile()?;
                Arc::new(process::Process::Do(
                    span.clone(),
                    Internal::Result(None),
                    (),
                    todo!(),
                    //process::Command::ReceiveType(Internal::Original(parameter.clone()), process),
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
