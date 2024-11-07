use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use crate::print::print_context;

#[derive(Clone, Debug)]
pub enum RuntimeError<I, X> {
    DoesNotExist(I),
    AlreadyDefined(I),
    Unused(Context<I, X>),

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

impl<I: Ord, X: Clone + Ord> Value<I, X> {
    fn value_state(&self) -> ValueState<X> {
        match self {
            Value::Suspend(context, channel, process) => match process.as_ref() {
                Process::Do(subject, _) if subject == channel => ValueState::HeadNormal,
                Process::Do(subject, Command::Case(_, _) | Command::Receive(_, _)) => {
                    if let Some(Value::External(ext)) = context.variables.get(subject) {
                        ValueState::BlockingOn(BTreeSet::from([ext.clone()]))
                    } else if let Some(ext) = context.state.blocking_on.get(subject) {
                        ValueState::BlockingOn(ext.clone())
                    } else {
                        ValueState::Unnormalized
                    }
                }
                _ => ValueState::Unnormalized,
            },
            Value::External(_) => ValueState::HeadNormal,
        }
    }

    fn context_state(&self) -> Option<&ContextState<I, X>> {
        match self {
            Value::Suspend(context, _, _) => Some(&context.state),
            Value::External(_) => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Request<I, X> {
    Break,
    Continue,
    Send(Value<I, X>),
    Receive,
    Select(I),
    Case(Vec<I>, Option<()>),
}

impl<I, X> Request<I, X> {
    pub fn is_blocking(&self) -> bool {
        match self {
            Self::Receive | Self::Case(_, _) => true,
            _ => false,
        }
    }
}

pub enum Response<I, X> {
    Receive(Value<I, X>),
    Case(Option<I>),
}

#[derive(Clone, Debug)]
pub struct Capture<I> {
    pub variables: BTreeSet<I>,
}

#[derive(Clone, Debug)]
pub enum ValueState<X> {
    Unnormalized,
    HeadNormal,
    BlockingOn(BTreeSet<X>),
}

#[derive(Clone, Debug)]
pub struct Context<I, X> {
    pub statics: BTreeMap<I, Arc<Expression<I>>>,
    pub variables: BTreeMap<I, Value<I, X>>,
    state: ContextState<I, X>,
}

#[derive(Clone, Debug)]
struct ContextState<I, X> {
    unnormalized: BTreeSet<I>,
    blocking_on: BTreeMap<I, BTreeSet<X>>,
}

impl<I, X> ContextState<I, X> {
    fn new() -> Self {
        Self {
            unnormalized: BTreeSet::new(),
            blocking_on: BTreeMap::new(),
        }
    }
}

impl<I, X> Context<I, X> {
    pub fn new() -> Self {
        Self {
            statics: BTreeMap::new(),
            variables: BTreeMap::new(),
            state: ContextState::new(),
        }
    }
}

impl<I: Clone + Ord, X: Clone + Ord> Context<I, X> {
    pub fn set(&mut self, name: &I, value: Value<I, X>) -> Result<(), RuntimeError<I, X>> {
        if let Some(_) = self.variables.remove(name) {
            return Err(RuntimeError::AlreadyDefined(name.clone()));
        }
        if let Some(context_state) = value.context_state() {
            if !context_state.unnormalized.is_empty() {
                self.state.unnormalized.insert(name.clone());
            }
            for (_, exts) in &context_state.blocking_on {
                self.state
                    .blocking_on
                    .entry(name.clone())
                    .or_default()
                    .extend(exts.iter().cloned());
            }
        }
        match value.value_state() {
            ValueState::Unnormalized => {
                self.state.unnormalized.insert(name.clone());
            }
            ValueState::BlockingOn(exts) => {
                self.state
                    .blocking_on
                    .entry(name.clone())
                    .or_default()
                    .extend(exts);
            }
            _ => (),
        }
        self.variables.insert(name.clone(), value);
        Ok(())
    }

    pub fn get(&mut self, name: &I) -> Result<Value<I, X>, RuntimeError<I, X>> {
        if let Some(value) = self.variables.remove(name) {
            self.state.unnormalized.remove(name);
            self.state.blocking_on.remove(name);
            return Ok(value);
        }
        if let Some(definition) = self.statics.get(name) {
            return Ok(evaluate(
                Context {
                    statics: self.statics.clone(),
                    variables: BTreeMap::new(),
                    state: ContextState::new(),
                },
                Arc::clone(definition),
            )?
            .1);
        }
        Err(RuntimeError::DoesNotExist(name.clone()))
    }

    pub fn extract(&mut self, capture: &Capture<I>) -> Result<Context<I, X>, RuntimeError<I, X>> {
        let mut captured = Context {
            statics: self.statics.clone(),
            variables: BTreeMap::new(),
            state: ContextState::new(),
        };
        for name in &capture.variables {
            if !self.variables.contains_key(name) && self.statics.contains_key(name) {
                continue;
            }
            captured.set(name, self.get(name)?)?;
        }
        Ok(captured)
    }

    pub fn get_unblocked(&self, responses: &BTreeMap<X, Response<I, X>>) -> Option<I> {
        let available = responses.keys().cloned().collect::<BTreeSet<_>>();
        self.state.unnormalized.first().cloned().or_else(|| {
            self.state
                .blocking_on
                .iter()
                .filter(|&(_, exts)| !exts.is_disjoint(&available))
                .map(|(name, _)| name)
                .cloned()
                .next()
        })
    }
}

pub struct Running<I, X> {
    pub context: Context<I, X>,
    pub process: Arc<Process<I>>,
}

impl<I, X> Running<I, X> {
    pub fn some(context: Context<I, X>, process: Arc<Process<I>>) -> Option<Self> {
        Some(Self { context, process })
    }
}

pub fn run_to_suspension<I: Clone + Ord + std::fmt::Display, X: Clone + Ord + std::fmt::Display>(
    mut running: Running<I, X>,
    responses: &mut BTreeMap<X, Response<I, X>>,
) -> Result<(Option<Running<I, X>>, Vec<(X, Request<I, X>)>), RuntimeError<I, X>> {
    let mut requests = Vec::new();
    let mut head_normal = false;

    loop {
        let mut buf = String::new();
        let _ = print_context(&mut buf, &running.context, 0);
        println!("{}\n{}", buf, running.process);
        println!("---");

        if !head_normal {
            loop {
                let (new_running, request) = step(running, responses)?;
                let block = match request {
                    Some((ext, req)) => {
                        requests.push((ext, req.clone()));
                        req.is_blocking()
                    }
                    None => false,
                };
                running = match new_running {
                    Some(new) => new,
                    None => return Ok((None, requests)),
                };
                if block {
                    break;
                }
            }
        }

        let Some(unblocked) = running.context.get_unblocked(responses) else {
            return Ok((Some(running), requests));
        };

        let Ok(value) = running.context.get(&unblocked) else {
            unreachable!();
        };
        head_normal = matches!(value.value_state(), ValueState::HeadNormal);
        let Value::Suspend(mut context, channel, process) = value else {
            unreachable!();
        };

        context.set(
            &channel,
            Value::Suspend(running.context, unblocked, running.process),
        )?;
        running = Running { context, process };
    }
}

pub fn evaluate<I: Clone + Ord, X: Clone + Ord>(
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

pub fn step<I: Clone + Ord, X: Clone + Ord>(
    Running {
        mut context,
        process,
    }: Running<I, X>,
    responses: &mut BTreeMap<X, Response<I, X>>,
) -> Result<(Option<Running<I, X>>, Option<(X, Request<I, X>)>), RuntimeError<I, X>> {
    match process.as_ref() {
        Process::Let(name, expression, process) => {
            let (mut context, value) = evaluate(context, Arc::clone(expression))?;
            context.set(name, value)?;
            Ok((Running::some(context, Arc::clone(process)), None))
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
                    Ok((Running::some(context, process), None))
                }

                (left, right) => Err(RuntimeError::CannotLink(left, right)),
            }
        }

        Process::Do(name, command) => {
            let target = context.get(name)?;

            if let Value::External(ext) = target {
                return match command {
                    Command::Break => {
                        if !context.variables.is_empty() {
                            return Err(RuntimeError::Unused(context));
                        }
                        drop(context);
                        Ok((None, Some((ext, Request::Break))))
                    }

                    Command::Continue(then) => Ok((
                        Running::some(context, Arc::clone(then)),
                        Some((ext, Request::Continue)),
                    )),

                    Command::Send(argument, after_send) => {
                        let (mut context, argument) = evaluate(context, Arc::clone(argument))?;
                        context.set(name, Value::External(ext.clone()))?;
                        Ok((
                            Running::some(context, Arc::clone(after_send)),
                            Some((ext, Request::Send(argument))),
                        ))
                    }

                    Command::Receive(parameter, after_receive) => match responses.remove(&ext) {
                        Some(Response::Receive(argument)) => {
                            context.set(parameter, argument)?;
                            context.set(name, Value::External(ext))?;
                            Ok((Running::some(context, Arc::clone(after_receive)), None))
                        }
                        Some(_) => Err(command.cannot(Value::External(ext))),
                        None => {
                            context.set(name, Value::External(ext.clone()))?;
                            Ok((
                                Running::some(context, Arc::clone(&process)),
                                Some((ext, Request::Receive)),
                            ))
                        }
                    },

                    Command::Select(selected, after_select) => {
                        context.set(name, Value::External(ext.clone()))?;
                        Ok((
                            Running::some(context, Arc::clone(after_select)),
                            Some((ext, Request::Select(selected.clone()))),
                        ))
                    }

                    Command::Case(branches, otherwise) => match responses.remove(&ext) {
                        Some(Response::Case(selected)) => {
                            for (branch, then) in branches {
                                if selected.as_ref() == Some(branch) {
                                    context.set(name, Value::External(ext))?;
                                    return Ok((Running::some(context, Arc::clone(then)), None));
                                }
                            }
                            let Some(otherwise) = otherwise else {
                                return Err(command.cannot(Value::External(ext)));
                            };
                            context.set(name, Value::External(ext))?;
                            Ok((Running::some(context, Arc::clone(otherwise)), None))
                        }
                        Some(_) => Err(command.cannot(Value::External(ext))),
                        None => {
                            context.set(name, Value::External(ext.clone()))?;
                            Ok((
                                Running::some(context, Arc::clone(&process)),
                                Some((
                                    ext,
                                    Request::Case(
                                        branches.iter().map(|(branch, _)| branch.clone()).collect(),
                                        otherwise.as_ref().map(|_| ()),
                                    ),
                                )),
                            ))
                        }
                    },
                };
            }

            let Value::Suspend(mut target_context, channel, target_process) = target else {
                return Err(command.cannot(target));
            };
            let target_command = match target_process.as_ref() {
                Process::Do(head, command) if head == &channel => command,
                _ => {
                    target_context.set(&channel, Value::Suspend(context, name.clone(), process))?;
                    return Ok((Running::some(target_context, target_process), None));
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
                    Ok((Running::some(continue_context, Arc::clone(then)), None))
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
                    Ok((Running::some(send_context, Arc::clone(after_send)), None))
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
                            return Ok((
                                Running::some(select_context, Arc::clone(after_select)),
                                None,
                            ));
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
                    Ok((Running::some(handle_context, Arc::clone(otherwise)), None))
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
            Expression::Fork(capture, name, process) => {
                let names = capture
                    .variables
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "{}[{}]{{ {} }}", name, names, process)
            }
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

impl<I: std::fmt::Display, X: std::fmt::Display> std::fmt::Display for Value<I, X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Suspend(context, name, process) => {
                let names = context
                    .variables
                    .keys()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "{}[{}]{{ {} }}", name, names, process)
            }
            Value::External(x) => write!(f, "{}", x),
        }
    }
}

impl<I: std::fmt::Display, X: std::fmt::Display> std::fmt::Display for RuntimeError<I, X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::DoesNotExist(name) => write!(f, "{} does not exist", name),
            RuntimeError::AlreadyDefined(name) => write!(f, "{} is already defined", name),
            RuntimeError::Unused(context) => write!(
                f,
                "{} was not used",
                context
                    .variables
                    .values()
                    .next()
                    .expect("no unused variables")
            ),
            RuntimeError::CannotLink(left, right) => {
                write!(f, "cannot link\n--> {}\n--> {}", left, right)
            }
            RuntimeError::CannotBreakTo(value) => write!(f, "cannot break to\n--> {}", value),
            RuntimeError::CannotContinueFrom(value) => {
                write!(f, "cannot continue from\n--> {}", value)
            }
            RuntimeError::CannotSendTo(value) => write!(f, "cannot send to\n--> {}", value),
            RuntimeError::CannotReceiveFrom(value) => {
                write!(f, "cannot receive from\n--> {}", value)
            }
            RuntimeError::MissingCase(value, branch) => {
                write!(f, "missing case {} in\n--> {}", branch, value)
            }
            RuntimeError::CannotSelectFrom(value, branches) => {
                write!(f, "cannot select from [")?;
                for (i, branch) in branches.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", branch)?;
                }
                write!(f, "] in\n--> {}", value)
            }
        }
    }
}
