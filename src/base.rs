#![allow(dead_code)]

use im::{OrdMap, Vector};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub enum RuntimeError<X: Clone + Ord> {
    DoesNotExist(X),
    AlreadyDefined(X),
    Unused(Context<X>),

    CannotLog(Value<X>),
    CannotLink(Value<X>, Value<X>),
    CannotBreakTo(Value<X>),
    CannotContinueFrom(Value<X>),
    CannotSendTo(Value<X>),
    CannotReceiveFrom(Value<X>),

    MissingCase(Value<X>, X),
    CannotSelectFrom(Value<X>, Vector<X>),
}

#[derive(Clone, Debug)]
pub enum Expression<X: Clone> {
    Tag(X),
    Ref(X),
    Fork(Capture<X>, X, Arc<Process<X>>),
}

#[derive(Clone, Debug)]
pub enum Process<X: Clone> {
    Halt,
    Log(Arc<Expression<X>>, Arc<Self>),
    Let(X, Arc<Expression<X>>, Arc<Self>),
    Link(X, Arc<Expression<X>>),
    Do(X, Command<X>),
}

#[derive(Clone, Debug)]
pub enum Command<X: Clone> {
    Break,
    Continue(Arc<Process<X>>),
    Send(Arc<Expression<X>>, Arc<Process<X>>),
    Receive(X, Arc<Process<X>>),
    Exhaust,
    Select(X, Arc<Process<X>>),
    Case(Vector<(X, Arc<Process<X>>)>, Arc<Process<X>>),
}

impl<X: Clone + Ord> Command<X> {
    fn cannot(&self, value: Value<X>) -> RuntimeError<X> {
        match self {
            Command::Break => RuntimeError::CannotBreakTo(value),
            Command::Continue(_) => RuntimeError::CannotContinueFrom(value),
            Command::Send(_, _) => RuntimeError::CannotSendTo(value),
            Command::Receive(_, _) => RuntimeError::CannotReceiveFrom(value),
            Command::Exhaust => RuntimeError::CannotSelectFrom(value, Vector::new()),
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
pub enum Value<X: Clone + Ord> {
    Tag(X),
    Suspend(Context<X>, X, Arc<Process<X>>),
}

#[derive(Clone, Debug)]
pub struct Capture<X: Clone> {
    pub variables: Vector<X>,
}

#[derive(Clone, Debug, Default)]
pub struct Context<X: Clone + Ord> {
    pub statics: OrdMap<X, Arc<Expression<X>>>,
    pub variables: OrdMap<X, Vector<Value<X>>>,
}

impl<X: Clone + Ord> Context<X> {
    pub fn empty() -> Self {
        Self {
            statics: OrdMap::new(),
            variables: OrdMap::new(),
        }
    }
}

pub struct Machine<X> {
    pub log: Box<dyn FnMut(X)>,
}

impl<X: Ord + Clone> Context<X> {
    fn set(&mut self, name: &X, value: Value<X>) -> Result<(), RuntimeError<X>> {
        Ok(self
            .variables
            .entry(name.clone())
            .or_default()
            .push_back(value))
    }

    fn get(&mut self, name: &X) -> Result<Value<X>, RuntimeError<X>> {
        if let Some(value) = self.variables.get_mut(name).and_then(|v| v.pop_back()) {
            if self.variables.get(name).filter(|v| !v.is_empty()).is_none() {
                self.variables.remove(name);
            }
            return Ok(value);
        }
        if let Some(definition) = self.statics.get(name) {
            return Ok(evaluate(
                Context {
                    statics: self.statics.clone(),
                    variables: OrdMap::new(),
                },
                Arc::clone(definition),
            )?
            .1);
        }
        Err(RuntimeError::DoesNotExist(name.clone()))
    }

    fn extract(&mut self, capture: &Capture<X>) -> Result<Context<X>, RuntimeError<X>> {
        let mut captured = Context {
            statics: self.statics.clone(),
            variables: OrdMap::new(),
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

    pub fn tag_<X: Clone>(tag: X) -> Expression<X> {
        Expression::Tag(tag)
    }

    pub fn ref_<X: Clone>(name: X) -> Expression<X> {
        Expression::Ref(name)
    }

    pub fn fork_<X: Clone>(cap: &[X], chan: X, proc: Process<X>) -> Expression<X> {
        Expression::Fork(
            Capture {
                variables: Vector::from(cap),
            },
            chan,
            Arc::new(proc),
        )
    }

    pub fn halt_<X: Clone>() -> Process<X> {
        Process::Halt
    }

    pub fn log_<X: Clone>(expr: Expression<X>, then: Process<X>) -> Process<X> {
        Process::Log(Arc::new(expr), Arc::new(then))
    }

    pub fn let_<X: Clone>(name: X, def: Expression<X>, then: Process<X>) -> Process<X> {
        Process::Let(name, Arc::new(def), Arc::new(then))
    }

    pub fn link_<X: Clone>(name: X, expr: Expression<X>) -> Process<X> {
        Process::Link(name, Arc::new(expr))
    }

    pub fn break_<X: Clone>(id: X) -> Process<X> {
        Process::Do(id, Command::Break)
    }

    pub fn continue_<X: Clone>(id: X, then: Process<X>) -> Process<X> {
        Process::Do(id, Command::Continue(Arc::new(then)))
    }

    pub fn send_<X: Clone>(id: X, arg: Expression<X>, then: Process<X>) -> Process<X> {
        Process::Do(id, Command::Send(Arc::new(arg), Arc::new(then)))
    }

    pub fn receive_<X: Clone>(id: X, param: X, then: Process<X>) -> Process<X> {
        Process::Do(id, Command::Receive(param, Arc::new(then)))
    }

    pub fn exhaust_<X: Clone>(id: X) -> Process<X> {
        Process::Do(id, Command::Exhaust)
    }

    pub fn select_<X: Clone>(id: X, branch: X, then: Process<X>) -> Process<X> {
        Process::Do(id, Command::Select(branch, Arc::new(then)))
    }

    pub fn case_<X: Clone>(
        id: X,
        branches: &[(X, Process<X>)],
        otherwise: Process<X>,
    ) -> Process<X> {
        Process::Do(
            id,
            Command::Case(
                branches
                    .into_iter()
                    .map(|(branch, then)| (branch.clone(), Arc::new(then.clone())))
                    .collect(),
                Arc::new(otherwise),
            ),
        )
    }

    pub fn case_exhaust_<X: Clone>(id: X, branches: &[(X, Process<X>)]) -> Process<X> {
        case_(id.clone(), branches, exhaust_(id))
    }
}

pub fn evaluate<X: Ord + Clone>(
    mut context: Context<X>,
    expression: Arc<Expression<X>>,
) -> Result<(Context<X>, Value<X>), RuntimeError<X>> {
    match expression.as_ref() {
        Expression::Tag(tag) => Ok((context, Value::Tag(tag.clone()))),

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

impl<X: Ord + Clone> Machine<X> {
    pub fn step(
        &mut self,
        mut context: Context<X>,
        process: Arc<Process<X>>,
    ) -> Result<(Context<X>, Arc<Process<X>>), RuntimeError<X>> {
        match process.as_ref() {
            Process::Halt => {
                if !context.variables.is_empty() {
                    return Err(RuntimeError::Unused(context));
                }
                Ok((context, process))
            }

            Process::Log(expression, process) => {
                let (context, value) = evaluate(context, Arc::clone(expression))?;
                let Value::Tag(tag) = value else {
                    return Err(RuntimeError::CannotLog(value));
                };
                (self.log)(tag);
                Ok((context, Arc::clone(process)))
            }

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
                        target_context
                            .set(&channel, Value::Suspend(context, name.clone(), process))?;
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
                            Value::Suspend(
                                receive_context,
                                channel.clone(),
                                Arc::clone(after_receive),
                            ),
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
                                    Value::Suspend(
                                        handle_context,
                                        channel.clone(),
                                        Arc::clone(then),
                                    ),
                                )?;
                                return Ok((select_context, Arc::clone(after_select)));
                            }
                        }
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
}

impl<X: Clone + Ord + std::fmt::Display> std::fmt::Display for Expression<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Tag(tag) => write!(f, "\"{}\"", tag),
            Expression::Ref(name) => write!(f, "{}", name),
            Expression::Fork(_, name, process) => write!(f, "{}{{ {} }}", name, process),
        }
    }
}

impl<X: Clone + Ord + std::fmt::Display> std::fmt::Display for Process<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Process::Log(expression, process) => write!(f, "@log({}); {}", expression, process),
            Process::Let(name, definition, process) => {
                write!(f, "let {} = {}; {}", name, definition, process)
            }
            Process::Link(left, right) => write!(f, "{} <> {}", left, right),
            Process::Halt => write!(f, "~"),
            Process::Do(name, Command::Break) => write!(f, "{}()", name),
            Process::Do(name, Command::Continue(process)) => write!(f, "{}[]; {}", name, process),
            Process::Do(name, Command::Send(argument, process)) => {
                write!(f, "{}({}); {}", name, argument, process)
            }
            Process::Do(name, Command::Receive(parameter, process)) => {
                write!(f, "{}[{}]; {}", name, parameter, process)
            }
            Process::Do(name, Command::Exhaust) => {
                write!(f, "{}/0", name)
            }
            Process::Do(name, Command::Select(branch, process)) => {
                write!(f, "{}.{}; {}", name, branch, process)
            }
            Process::Do(name, Command::Case(branches, otherwise)) => {
                write!(f, "{}.case{{ ", name)?;
                for (branch, process) in branches {
                    write!(f, "{} => {{ {} }} ", branch, process)?;
                }
                write!(f, "}}; {}", otherwise)
            }
        }
    }
}

impl<X: Clone + Ord + std::fmt::Display> std::fmt::Display for Value<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Suspend(_, name, process) => write!(f, "{}{{ {} }}", name, process),
            Value::Tag(tag) => write!(f, "\"{}\"", tag),
        }
    }
}
