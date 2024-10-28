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

    Exhausted(Context<X>, Value<X>),
    CannotHandle(Context<X>, X),
    CannotSelectOn(Value<X>, X),
}

#[derive(Clone, Debug)]
pub enum Expression<X: Clone> {
    Tag(X),

    Static(X),
    Variable(X),
    Let(X, Arc<Self>, Arc<Self>),
    Fork(Capture<X>, X, Arc<Process<X>>),

    Break,
    Continue(Capture<X>, Arc<Process<X>>),
    Send(Arc<Self>, Arc<Self>),
    Receive(Capture<X>, X, Arc<Self>),

    Exhaust,
    Select(X, Arc<Self>),
    Handle(Capture<X>, X, Arc<Self>, Arc<Self>),
}

#[derive(Clone, Debug)]
pub enum Process<X: Clone> {
    Halt,

    Log(Arc<Expression<X>>, Arc<Self>),

    Let(X, Arc<Expression<X>>, Arc<Self>),
    Link(X, Arc<Expression<X>>),

    Break(X),
    Continue(X, Arc<Self>),
    Send(X, Arc<Expression<X>>, Arc<Self>),
    Receive(X, X, Arc<Self>),

    Exhaust(X),
    Select(X, X, Arc<Self>),
    Handle(X, X, Arc<Self>, Arc<Self>),
}

#[derive(Clone, Debug)]
pub enum Value<X: Clone + Ord> {
    Suspend(Context<X>, X, Arc<Process<X>>),

    Tag(X),

    Break,
    Continue(Context<X>, Arc<Process<X>>),
    Send(Box<Self>, Box<Self>),
    Receive(Context<X>, X, Arc<Expression<X>>),

    Exhaust,
    Select(X, Box<Self>),
    Handle(Context<X>, X, Arc<Expression<X>>, Arc<Expression<X>>),
}

#[derive(Clone, Debug)]
pub struct Capture<X: Clone> {
    variables: Vector<X>,
}

#[derive(Clone, Debug, Default)]
pub struct Context<X: Clone + Ord> {
    pub statics: OrdMap<X, Arc<Expression<X>>>,
    pub variables: OrdMap<X, Value<X>>,
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
        match self.variables.insert(name.clone(), value) {
            Some(_) => Err(RuntimeError::AlreadyDefined(name.clone())),
            None => Ok(()),
        }
    }

    fn get(&mut self, name: &X) -> Result<Value<X>, RuntimeError<X>> {
        match self.variables.remove(name) {
            Some(value) => Ok(value),
            None => Err(RuntimeError::DoesNotExist(name.clone())),
        }
    }

    fn get_static(&mut self, name: &X) -> Result<Arc<Expression<X>>, RuntimeError<X>> {
        match self.statics.get(name) {
            Some(definition) => Ok(Arc::clone(definition)),
            None => Err(RuntimeError::DoesNotExist(name.clone())),
        }
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

    pub fn static_<X: Clone>(name: X) -> Expression<X> {
        Expression::Static(name)
    }

    pub fn var_<X: Clone>(name: X) -> Expression<X> {
        Expression::Variable(name)
    }

    pub fn let_in_<X: Clone>(name: X, def: Expression<X>, exp: Expression<X>) -> Expression<X> {
        Expression::Let(name, Arc::new(def), Arc::new(exp))
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

    pub fn break_<X: Clone>() -> Expression<X> {
        Expression::Break
    }

    pub fn continue_<X: Clone>(cap: &[X], proc: Process<X>) -> Expression<X> {
        Expression::Continue(
            Capture {
                variables: Vector::from(cap),
            },
            Arc::new(proc),
        )
    }

    pub fn send_<X: Clone>(arg: Expression<X>, then: Expression<X>) -> Expression<X> {
        Expression::Send(Arc::new(arg), Arc::new(then))
    }

    pub fn receive_<X: Clone>(cap: &[X], param: X, body: Expression<X>) -> Expression<X> {
        Expression::Receive(
            Capture {
                variables: Vector::from(cap),
            },
            param,
            Arc::new(body),
        )
    }

    pub fn exhaust_<X: Clone>() -> Expression<X> {
        Expression::Exhaust
    }

    pub fn select_<X: Clone>(branch: X, exp: Expression<X>) -> Expression<X> {
        Expression::Select(branch, Arc::new(exp))
    }

    pub fn handle_<X: Clone>(cap: &[X], branch: X, mch: Expression<X>, oth: Expression<X>) -> Expression<X> {
        Expression::Handle(
            Capture {
                variables: Vector::from(cap),
            },
            branch,
            Arc::new(mch),
            Arc::new(oth),
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

    pub fn break_to_<X: Clone>(id: X) -> Process<X> {
        Process::Break(id)
    }

    pub fn continue_from_<X: Clone>(id: X, then: Process<X>) -> Process<X> {
        Process::Continue(id, Arc::new(then))
    }

    pub fn send_to_<X: Clone>(id: X, arg: Expression<X>, then: Process<X>) -> Process<X> {
        Process::Send(id, Arc::new(arg), Arc::new(then))
    }

    pub fn receive_from_<X: Clone>(id: X, param: X, then: Process<X>) -> Process<X> {
        Process::Receive(id, param, Arc::new(then))
    }

    pub fn exhaust_in_<X: Clone>(id: X) -> Process<X> {
        Process::Exhaust(id)
    }

    pub fn select_on_<X: Clone>(id: X, branch: X, then: Process<X>) -> Process<X> {
        Process::Select(id, branch, Arc::new(then))
    }

    pub fn handle_in_<X: Clone>(id: X, branch: X, mch: Process<X>, oth: Process<X>) -> Process<X> {
        Process::Handle(id, branch, Arc::new(mch), Arc::new(oth))
    }
}

pub fn evaluate<X: Ord + Clone>(
    mut context: Context<X>,
    expression: Arc<Expression<X>>,
) -> Result<(Context<X>, Value<X>), RuntimeError<X>> {
    match expression.as_ref() {
        Expression::Tag(tag) => Ok((context, Value::Tag(tag.clone()))),

        Expression::Static(name) => {
            let definition = context.get_static(name)?;
            let statics = context.statics.clone();
            Ok((context, evaluate(Context {
                statics,
                variables: OrdMap::new(),
            }, definition)?.1))
        }

        Expression::Variable(name) => {
            let value = context.get(name)?;
            Ok((context, value))
        }

        Expression::Let(name, definition, expression) => {
            let (mut context, value) = evaluate(context, Arc::clone(definition))?;
            context.set(name, value)?;
            evaluate(context, Arc::clone(expression))
        }

        Expression::Fork(capture, channel, process) => {
            let captured = context.extract(capture)?;
            Ok((
                context,
                Value::Suspend(captured, channel.clone(), Arc::clone(process)),
            ))
        }

        Expression::Break => Ok((context, Value::Break)),

        Expression::Continue(capture, process) => {
            let captured = context.extract(capture)?;
            Ok((context, Value::Continue(captured, Arc::clone(process))))
        }

        Expression::Send(argument, continuation) => {
            let (context, argument) = evaluate(context, argument.clone())?;
            let (context, continuation) = evaluate(context, continuation.clone())?;
            Ok((
                context,
                Value::Send(Box::new(argument), Box::new(continuation)),
            ))
        }

        Expression::Receive(capture, parameter, body) => {
            let captured = context.extract(capture)?;
            Ok((
                context,
                Value::Receive(captured, parameter.clone(), Arc::clone(body)),
            ))
        }

        Expression::Exhaust => {
            Ok((context, Value::Exhaust))
        }

        Expression::Select(branch, expression) => {
            let (context, value) = evaluate(context, Arc::clone(expression))?;
            Ok((context, Value::Select(branch.clone(), Box::new(value))))
        }

        Expression::Handle(capture, branch, matching, otherwise) => {
            let captured = context.extract(capture)?;
            Ok((
                context,
                Value::Handle(captured, branch.clone(), Arc::clone(matching), Arc::clone(otherwise)),
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
                    (Value::Break, Value::Continue(context, process))
                    | (Value::Continue(context, process), Value::Break) => Ok((context, process)),

                    (
                        Value::Send(argument, continuation),
                        Value::Receive(mut context, parameter, body),
                    )
                    | (
                        Value::Receive(mut context, parameter, body),
                        Value::Send(argument, continuation),
                    ) => {
                        context.set(name, *continuation)?;
                        context.set(&parameter, *argument)?;
                        Ok((context, Arc::new(Process::Link(name.clone(), body))))
                    }

                    (
                        Value::Select(selected, value),
                        Value::Handle(mut context, branch, matching, otherwise),
                    )
                    | (
                        Value::Handle(mut context, branch, matching, otherwise),
                        Value::Select(selected, value),
                    ) => {
                        context.set(name, *value)?;
                        Ok((context, if branch == selected {
                            Arc::new(Process::Link(name.clone(), matching))
                        } else {
                            Arc::new(Process::Link(name.clone(), otherwise))
                        }))
                    }

                    (Value::Suspend(mut context, name, process), value)
                    | (value, Value::Suspend(mut context, name, process)) => {
                        context.set(&name, value)?;
                        Ok((context, process))
                    }

                    (left, right) => Err(RuntimeError::CannotLink(left, right)),
                }
            }

            Process::Break(name) => {
                let value = context.get(name)?;
                if !context.variables.is_empty() {
                    return Err(RuntimeError::Unused(context));
                }
                drop(context);
                match value {
                    Value::Continue(context, process) => Ok((context, process)),
                    Value::Suspend(mut context, name, process) => {
                        context.set(&name, Value::Break)?;
                        Ok((context, process))
                    }
                    _ => Err(RuntimeError::CannotBreakTo(value)),
                }
            }

            Process::Continue(name, process) => {
                let value = context.get(name)?;
                match value {
                    Value::Break => Ok((context, Arc::clone(process))),
                    Value::Suspend(mut inner_context, inner_name, inner_process) => {
                        let suspended = Value::Continue(context, Arc::clone(process));
                        inner_context.set(&inner_name, suspended)?;
                        Ok((inner_context, inner_process))
                    }
                    _ => Err(RuntimeError::CannotContinueFrom(value)),
                }
            }

            Process::Send(identity, argument, process) => {
                let target = context.get(identity)?;
                let (mut context, argument) = evaluate(context, Arc::clone(argument))?;
                match target {
                    Value::Receive(mut inner_context, parameter, body) => {
                        inner_context.set(&parameter, argument)?;
                        let (empty_context, result) = evaluate(inner_context, body)?;
                        if !empty_context.variables.is_empty() {
                            return Err(RuntimeError::Unused(empty_context));
                        }
                        context.set(identity, result)?;
                        Ok((context, Arc::clone(process)))
                    }

                    Value::Suspend(mut inner_context, inner_name, inner_process) => {
                        let suspended = Value::Send(
                            Box::new(argument),
                            Box::new(Value::Suspend(
                                context,
                                identity.clone(),
                                Arc::clone(process),
                            )),
                        );
                        inner_context.set(&inner_name, suspended)?;
                        Ok((inner_context, inner_process))
                    }

                    _ => Err(RuntimeError::CannotSendTo(target)),
                }
            }

            Process::Receive(identity, parameter, process) => {
                let source = context.get(identity)?;
                match source {
                    Value::Send(argument, continuation) => {
                        context.set(parameter, *argument)?;
                        context.set(identity, *continuation)?;
                        Ok((context, Arc::clone(process)))
                    }

                    Value::Suspend(mut inner_context, inner_name, inner_process) => {
                        let mut capture = Capture {
                            variables: context.variables.keys().map(X::clone).collect(),
                        };
                        capture.variables.push_back(parameter.clone());
                        let suspended = Value::Receive(
                            context,
                            parameter.clone(),
                            Arc::new(Expression::Fork(
                                capture,
                                identity.clone(),
                                Arc::clone(process),
                            )),
                        );
                        inner_context.set(&inner_name, suspended)?;
                        Ok((inner_context, inner_process))
                    }

                    _ => Err(RuntimeError::CannotReceiveFrom(source)),
                }
            }

            Process::Exhaust(identity) => {
                let value = context.get(identity)?;
                Err(RuntimeError::Exhausted(context, value))
            }

            Process::Select(identity, selected, process) => {
                let handler = context.get(identity)?;
                match handler {
                    Value::Exhaust => Err(RuntimeError::CannotHandle(context, selected.clone())),

                    Value::Handle(inner_context, branch, matching, otherwise) => {
                        let (empty_context, new_value) = if &branch == selected {
                            evaluate(inner_context, matching)?
                        } else {
                            evaluate(inner_context, otherwise)?
                        };
                        if !empty_context.variables.is_empty() {
                            return Err(RuntimeError::Unused(empty_context));
                        }
                        context.set(identity, new_value)?;
                        Ok((context, Arc::clone(process)))
                    }

                    Value::Suspend(mut inner_context, inner_name, inner_process) => {
                        let suspended = Value::Select(
                            selected.clone(),
                            Box::new(Value::Suspend(
                                context,
                                identity.clone(),
                                Arc::clone(process),
                            )),
                        );
                        inner_context.set(&inner_name, suspended)?;
                        Ok((inner_context, inner_process))
                    }

                    _ => Err(RuntimeError::CannotSelectOn(handler, selected.clone())),
                }
            }

            Process::Handle(identity, branch, matching, otherwise) => {
                let selector = context.get(identity)?;
                match selector {
                    Value::Select(selected, value) => {
                        if branch == &selected {
                            context.set(identity, *value)?;
                            Ok((context, Arc::clone(matching)))
                        } else {
                            context.set(identity, Value::Select(selected, value))?;
                            Ok((context, Arc::clone(otherwise)))
                        }
                    }

                    Value::Suspend(mut inner_context, inner_name, inner_process) => {
                        let capture = Capture {
                            variables: context.variables.keys().cloned().collect(),
                        };
                        let suspended = Value::Handle(
                            context,
                            branch.clone(),
                            Arc::new(Expression::Fork(
                                capture.clone(),
                                identity.clone(),
                                Arc::clone(matching),
                            )),
                            Arc::new(Expression::Fork(
                                capture,
                                identity.clone(),
                                Arc::clone(otherwise),
                            )),
                        );
                        inner_context.set(&inner_name, suspended)?;
                        Ok((inner_context, inner_process))
                    }

                    _ => Err(RuntimeError::CannotHandle(context, branch.clone())),
                }
            }
        }
    }
}

impl<X: Clone + Ord + std::fmt::Display> std::fmt::Display for Expression<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Tag(tag) => write!(f, "\"{}\"", tag),
            Expression::Static(name) => write!(f, "@{}", name),
            Expression::Variable(name) => write!(f, "{}", name),
            Expression::Let(name, definition, expression) => write!(f, "let {} = {} in {}", name, definition, expression),
            Expression::Fork(_, name, process) => write!(f, "{}{{ {} }}", name, process),
            Expression::Break => write!(f, "()"),
            Expression::Continue(_, process) => write!(f, "{{ {} }}", process),
            Expression::Send(argument, continuation) => write!(f, "({}){}", argument, continuation),
            Expression::Receive(_, parameter, body) => write!(f, "[{}]{}", parameter, body),
            Expression::Exhaust => write!(f, "/0"),
            Expression::Select(branch, expression) => write!(f, ":{} {}", branch, expression),
            Expression::Handle(_, branch, matching, otherwise) => write!(f, "/{} {} {}", branch, matching, otherwise),
        }
    }
}

impl<X: Clone + Ord + std::fmt::Display> std::fmt::Display for Process<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Process::Log(expression, process) => write!(f, "@log({}); {}", expression, process),
            Process::Let(name, definition, process) => write!(f, "let {} = {}; {}", name, definition, process),
            Process::Link(left, right) => write!(f, "{} <-> {}", left, right),
            Process::Halt => write!(f, "~"),
            Process::Break(name) => write!(f, "{}()", name),
            Process::Continue(name, process) => write!(f, "{}[]; {}", name, process),
            Process::Send(name, argument, process) => write!(f, "{}({}); {}", name, argument, process),
            Process::Receive(name, parameter, process) => write!(f, "{}[{}]; {}", name, parameter, process),
            Process::Exhaust(name) => write!(f, "{}/0", name),
            Process::Select(name, branch, process) => write!(f, "{}:{}; {}", name, branch, process),
            Process::Handle(name, branch, matching, otherwise) => write!(f, "{}/{}{{ {} }}; {}", name, branch, matching, otherwise),
        }
    }
}

impl<X: Clone + Ord + std::fmt::Display> std::fmt::Display for Value<X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Suspend(_, name, process) => write!(f, "{}{{ {} }}", name, process),
            Value::Tag(tag) => write!(f, "\"{}\"", tag),
            Value::Break => write!(f, "()"),
            Value::Continue(_, process) => write!(f, "{{ {} }}", process),
            Value::Send(argument, continuation) => write!(f, "({}){}", argument, continuation),
            Value::Receive(_, parameter, body) => write!(f, "[{}]{}", parameter, body),
            Value::Exhaust => write!(f, "/0"),
            Value::Select(branch, value) => write!(f, ":{} {}", branch, value),
            Value::Handle(_, branch, matching, otherwise) => write!(f, "/{} {} {}", branch, matching, otherwise),
        }
    }
}
