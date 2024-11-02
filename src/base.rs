#![allow(dead_code)]

use std::{collections::BTreeMap, sync::Arc};

#[derive(Clone, Debug)]
pub enum RuntimeError<I, X> {
    DoesNotExist(I),
    AlreadyDefined(I),
    Unused(Context<I, X>),

    CannotLog(Value<I, X>),
    CannotLink(Value<I, X>, Value<I, X>),
    CannotBreakTo(Value<I, X>),
    CannotContinueFrom(Value<I, X>),
    CannotSendTo(Value<I, X>),
    CannotReceiveFrom(Value<I, X>),

    MissingCase(Value<I, X>, I),
    CannotSelectFrom(Value<I, X>, Vec<I>),
}

#[derive(Clone, Debug)]
pub enum Expression<I> {
    Ref(I),
    Fork(Capture<I>, I, Arc<Process<I>>),
}

#[derive(Clone, Debug)]
pub enum Process<I> {
    Let(I, Arc<Expression<I>>, Arc<Self>),
    Link(I, Arc<Expression<I>>),
    Do(I, Command<I>),
}

#[derive(Clone, Debug)]
pub enum Command<I> {
    Break,
    Continue(Arc<Process<I>>),
    Send(Arc<Expression<I>>, Arc<Process<I>>),
    Receive(I, Arc<Process<I>>),
    Select(I, Arc<Process<I>>),
    Case(Vec<(I, Arc<Process<I>>)>, Option<Arc<Process<I>>>),
}

impl<I: Clone> Command<I> {
    fn cannot<X>(&self, value: Value<I, X>) -> RuntimeError<I, X> {
        match self {
            Command::Break => RuntimeError::CannotBreakTo(value),
            Command::Continue(_) => RuntimeError::CannotContinueFrom(value),
            Command::Send(_, _) => RuntimeError::CannotSendTo(value),
            Command::Receive(_, _) => RuntimeError::CannotReceiveFrom(value),
            Command::Select(branch, _) => RuntimeError::MissingCase(value, branch.clone()),
            Command::Case(branches, _) => RuntimeError::CannotSelectFrom(
                value,
                branches
                    .into_iter()
                    .map(|(branch, _)| branch)
                    .cloned()
                    .collect(),
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value<I, X> {
    Suspend(Context<I, X>, I, Arc<Process<I>>),
    External(X),
}

#[derive(Clone, Debug)]
pub struct Capture<I> {
    pub variables: Vec<I>,
}

#[derive(Clone, Debug, Default)]
pub struct Context<I, X> {
    pub statics: BTreeMap<I, Arc<Expression<I>>>,
    pub variables: BTreeMap<I, Vec<Value<I, X>>>,
}

impl<I, X> Context<I, X> {
    pub fn empty() -> Self {
        Self {
            statics: BTreeMap::new(),
            variables: BTreeMap::new(),
        }
    }
}

impl<I: Clone + Ord, X> Context<I, X> {
    fn set(&mut self, name: &I, value: Value<I, X>) -> Result<(), RuntimeError<I, X>> {
        Ok(self.variables.entry(name.clone()).or_default().push(value))
    }

    fn get(&mut self, name: &I) -> Result<Value<I, X>, RuntimeError<I, X>> {
        if let Some(value) = self.variables.get_mut(name).and_then(|v| v.pop()) {
            if self.variables.get(name).filter(|v| !v.is_empty()).is_none() {
                self.variables.remove(name);
            }
            return Ok(value);
        }
        if let Some(definition) = self.statics.get(name) {
            return Ok(evaluate(
                Context {
                    statics: self.statics.clone(),
                    variables: BTreeMap::new(),
                },
                Arc::clone(definition),
            )?
            .1);
        }
        Err(RuntimeError::DoesNotExist(name.clone()))
    }

    fn extract(&mut self, capture: &Capture<I>) -> Result<Context<I, X>, RuntimeError<I, X>> {
        let mut captured = Context {
            statics: self.statics.clone(),
            variables: BTreeMap::new(),
        };
        for name in &capture.variables {
            let Some(value) = self.variables.remove(name) else {
                return Err(RuntimeError::DoesNotExist(name.clone()));
            };
            if let Some(_) = captured.variables.insert(name.clone(), value) {
                return Err(RuntimeError::AlreadyDefined(name.clone()));
            }
        }
        Ok(captured)
    }
}

pub mod notation {
    use super::*;

    pub fn ref_<I>(name: I) -> Expression<I> {
        Expression::Ref(name)
    }

    pub fn fork_<I, const N: usize>(cap: [I; N], chan: I, proc: Process<I>) -> Expression<I> {
        Expression::Fork(
            Capture {
                variables: Vec::from(cap),
            },
            chan,
            Arc::new(proc),
        )
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

    pub fn case_<I, const N: usize>(
        id: I,
        branches: [(I, Process<I>); N],
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

    pub fn case_exhaust_<I, const N: usize>(id: I, branches: [(I, Process<I>); N]) -> Process<I> {
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
}

pub fn evaluate<I: Clone + Ord, X>(
    mut context: Context<I, X>,
    expression: Arc<Expression<I>>,
) -> Result<(Context<I, X>, Value<I, X>), RuntimeError<I, X>> {
    match expression.as_ref() {
        Expression::Ref(name) => {
            let value = context.get(name)?;
            Ok((context, value))
        }

        Expression::Fork(capture, channel, process) => {
            let captured = context.extract(capture)?;
            Ok((
                context,
                Value::Suspend(captured, channel.clone(), Arc::clone(process)),
            ))
        }
    }
}

pub fn step<I: Clone + Ord, X>(
    mut context: Context<I, X>,
    process: Arc<Process<I>>,
) -> Result<(Context<I, X>, Arc<Process<I>>), RuntimeError<I, X>> {
    match process.as_ref() {
        Process::Let(name, expression, process) => {
            let (mut context, value) = evaluate(context, Arc::clone(expression))?;
            context.set(name, value)?;
            Ok((context, Arc::clone(process)))
        }

        Process::Link(name, expression) => {
            let left = context.get(name)?;
            let (context, right) = evaluate(context, Arc::clone(expression))?;
            if !context.variables.is_empty() {
                return Err(RuntimeError::Unused(context));
            }
            drop(context);
            match (left, right) {
                (Value::Suspend(mut context, name, process), value)
                | (value, Value::Suspend(mut context, name, process)) => {
                    context.set(&name, value)?;
                    Ok((context, process))
                }

                (left, right) => Err(RuntimeError::CannotLink(left, right)),
            }
        }

        Process::Do(name, command) => {
            let target = context.get(name)?;
            let Value::Suspend(mut target_context, channel, target_process) = target else {
                return Err(command.cannot(target));
            };
            let target_command = match target_process.as_ref() {
                Process::Do(head, command) if head == &channel => command,
                _ => {
                    target_context.set(&channel, Value::Suspend(context, name.clone(), process))?;
                    return Ok((target_context, target_process));
                }
            };

            match (
                (context, name, command),
                (target_context, &channel, target_command),
            ) {
                (
                    (break_context, _, Command::Break),
                    (continue_context, _, Command::Continue(then)),
                )
                | (
                    (continue_context, _, Command::Continue(then)),
                    (break_context, _, Command::Break),
                ) => {
                    if !break_context.variables.is_empty() {
                        return Err(RuntimeError::Unused(break_context));
                    }
                    drop(break_context);
                    Ok((continue_context, Arc::clone(then)))
                }

                (
                    (send_context, name, Command::Send(argument, after_send)),
                    (mut receive_context, channel, Command::Receive(parameter, after_receive)),
                )
                | (
                    (mut receive_context, channel, Command::Receive(parameter, after_receive)),
                    (send_context, name, Command::Send(argument, after_send)),
                ) => {
                    let (mut send_context, argument) =
                        evaluate(send_context, Arc::clone(argument))?;
                    receive_context.set(parameter, argument)?;
                    send_context.set(
                        name,
                        Value::Suspend(receive_context, channel.clone(), Arc::clone(after_receive)),
                    )?;
                    Ok((send_context, Arc::clone(after_send)))
                }

                (
                    (mut select_context, name, Command::Select(selected, after_select)),
                    (mut handle_context, channel, Command::Case(branches, otherwise)),
                )
                | (
                    (mut handle_context, channel, Command::Case(branches, otherwise)),
                    (mut select_context, name, Command::Select(selected, after_select)),
                ) => {
                    for (branch, then) in branches {
                        if selected == branch {
                            select_context.set(
                                name,
                                Value::Suspend(handle_context, channel.clone(), Arc::clone(then)),
                            )?;
                            return Ok((select_context, Arc::clone(after_select)));
                        }
                    }
                    let Some(otherwise) = otherwise else {
                        return Err(RuntimeError::MissingCase(
                            Value::Suspend(
                                handle_context,
                                channel.clone(),
                                Arc::new(Process::Do(
                                    name.clone(),
                                    Command::Case(branches.clone(), None),
                                )),
                            ),
                            selected.clone(),
                        ));
                    };
                    handle_context.set(
                        channel,
                        Value::Suspend(
                            select_context,
                            name.clone(),
                            Arc::new(Process::Do(
                                name.clone(),
                                Command::Select(selected.clone(), Arc::clone(after_select)),
                            )),
                        ),
                    )?;
                    Ok((handle_context, Arc::clone(otherwise)))
                }

                (_, (target_context, channel, command)) => Err(command.cannot(Value::Suspend(
                    target_context,
                    channel.clone(),
                    Arc::new(Process::Do(channel.clone(), command.clone())),
                ))),
            }
        }
    }
}

impl<I: std::fmt::Display> std::fmt::Display for Expression<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Ref(name) => write!(f, "{}", name),
            Expression::Fork(_, name, process) => write!(f, "{}{{ {} }}", name, process),
        }
    }
}

impl<I: std::fmt::Display> std::fmt::Display for Process<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Process::Let(name, definition, process) => {
                write!(f, "let {} = {}; {}", name, definition, process)
            }
            Process::Link(left, right) => write!(f, "{} <> {}", left, right),
            Process::Do(name, Command::Break) => write!(f, "{}()", name),
            Process::Do(name, Command::Continue(process)) => write!(f, "{}[]; {}", name, process),
            Process::Do(name, Command::Send(argument, process)) => {
                write!(f, "{}({}); {}", name, argument, process)
            }
            Process::Do(name, Command::Receive(parameter, process)) => {
                write!(f, "{}[{}]; {}", name, parameter, process)
            }
            Process::Do(name, Command::Select(branch, process)) => {
                write!(f, "{}.{}; {}", name, branch, process)
            }
            Process::Do(name, Command::Case(branches, otherwise)) => {
                write!(f, "{}.case{{ ", name)?;
                for (branch, process) in branches {
                    write!(f, "{} => {{ {} }} ", branch, process)?;
                }
                write!(f, "}}")?;
                if let Some(otherwise) = otherwise {
                    write!(f, "; {}", otherwise)?;
                }
                Ok(())
            }
        }
    }
}

impl<I: std::fmt::Display, X: std::fmt::Debug> std::fmt::Display
    for Value<I, X>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Suspend(_, name, process) => write!(f, "{}{{ {} }}", name, process),
            Value::External(x) => write!(f, "#{{{:?}}}", x),
        }
    }
}
