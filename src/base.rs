use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

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

    ExternalEscaped(X),
}

#[derive(Clone, Debug)]
pub enum Expression<I> {
    Ref(I),
    Fork(RefCell<Capture<I>>, I, Arc<Process<I>>),
    String(Arc<str>),
}

impl<I: Clone + Ord> Expression<I> {
    pub fn fix_captures(&self) -> BTreeSet<I> {
        match self {
            Self::Ref(name) => BTreeSet::from([name.clone()]),
            Self::Fork(capture, bound, process) => {
                let mut free = process.fix_captures();
                free.remove(bound);
                capture.borrow_mut().variables = free.clone();
                free
            }
            Self::String(_) => BTreeSet::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Process<I> {
    Let(I, Arc<Expression<I>>, Arc<Self>),
    Link(I, Arc<Expression<I>>),
    Do(I, Command<I>),
}

impl<I: Clone + Ord> Process<I> {
    pub fn fix_captures(&self) -> BTreeSet<I> {
        match self {
            Self::Let(bound, expression, process) => {
                let mut free = process.fix_captures();
                free.remove(bound);
                free.extend(expression.fix_captures());
                free
            }
            Self::Link(name, expression) => {
                let mut free = expression.fix_captures();
                free.insert(name.clone());
                free
            }
            Self::Do(name, Command::Break) => BTreeSet::from([name.clone()]),
            Self::Do(name, Command::Continue(process)) => {
                let mut free = process.fix_captures();
                free.insert(name.clone());
                free
            }
            Self::Do(name, Command::Send(expression, process)) => {
                let mut free = expression.fix_captures();
                free.extend(process.fix_captures());
                free.insert(name.clone());
                free
            }
            Self::Do(name, Command::Receive(bound, process)) => {
                let mut free = process.fix_captures();
                free.remove(bound);
                free.insert(name.clone());
                free
            }
            Self::Do(name, Command::Select(_, process)) => {
                let mut free = process.fix_captures();
                free.insert(name.clone());
                free
            }
            Self::Do(name, Command::Case(branches)) => {
                let mut free = BTreeSet::new();
                for (_, process) in branches {
                    free.extend(process.fix_captures());
                }
                free.insert(name.clone());
                free
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Command<I> {
    Break,
    Continue(Arc<Process<I>>),
    Send(Arc<Expression<I>>, Arc<Process<I>>),
    Receive(I, Arc<Process<I>>),
    Select(I, Arc<Process<I>>),
    Case(Vec<(I, Arc<Process<I>>)>),
}

impl<I: Clone> Command<I> {
    fn cannot<X>(&self, value: Value<I, X>) -> RuntimeError<I, X> {
        match self {
            Self::Break => RuntimeError::CannotBreakTo(value),
            Self::Continue(_) => RuntimeError::CannotContinueFrom(value),
            Self::Send(_, _) => RuntimeError::CannotSendTo(value),
            Self::Receive(_, _) => RuntimeError::CannotReceiveFrom(value),
            Self::Select(branch, _) => RuntimeError::MissingCase(value, branch.clone()),
            Self::Case(branches) => RuntimeError::CannotSelectFrom(
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
    String(String),
}

impl<I: Clone + Ord, X> Value<I, X> {
    pub fn fix_captures(&self) {
        match self {
            Self::Suspend(context, _, process) => {
                context.fix_captures();
                process.fix_captures();
            }
            _ => (),
        }
    }
}

impl<I: Clone + Ord, X: Clone + Ord> Value<I, X> {
    fn value_state(&self) -> ValueState<I, X> {
        match self {
            Self::Suspend(context, channel, process) => match process.as_ref() {
                Process::Do(subject, _) if subject == channel => ValueState::HeadNormal,
                Process::Do(subject, command) => {
                    let mut requesting = context
                        .state
                        .requesting
                        .get(subject)
                        .cloned()
                        .unwrap_or_else(|| BTreeMap::new());
                    if let Some(Value::External(external)) = context.variables.get(subject) {
                        match command {
                            Command::Receive(_, _) => {
                                requesting.insert(external.clone(), Request::Receive);
                            }
                            Command::Case(branches) => {
                                requesting.insert(
                                    external.clone(),
                                    Request::Case(
                                        branches.iter().map(|(branch, _)| branch.clone()).collect(),
                                    ),
                                );
                            }
                            _ => (),
                        }
                    }
                    if requesting.is_empty() {
                        ValueState::Unnormalized
                    } else {
                        ValueState::Requesting(requesting)
                    }
                }
                _ => ValueState::Unnormalized,
            },
            _ => ValueState::HeadNormal,
        }
    }

    fn context_state(&self) -> Option<&ContextState<I, X>> {
        match self {
            Value::Suspend(context, _, _) => Some(&context.state),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Action<I, X> {
    Break,
    Continue,
    Send(Value<I, X>),
    Receive,
    Select(I),

    #[allow(unused)]
    Case(Vec<I>),
}

impl<I, X> Action<I, X> {
    pub fn is_requesting(&self) -> bool {
        match self {
            Self::Receive | Self::Case(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Request<I> {
    Receive,
    Case(Vec<I>),
}

#[derive(Clone, Debug)]
pub enum Response<I, X> {
    Receive(Value<I, X>),
    Case(I),
}

#[derive(Clone, Debug)]
pub struct Capture<I> {
    pub variables: BTreeSet<I>,
}

impl<I> Default for Capture<I> {
    fn default() -> Self {
        Self {
            variables: BTreeSet::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ValueState<I, X> {
    Unnormalized,
    HeadNormal,
    Requesting(BTreeMap<X, Request<I>>),
}

#[derive(Clone, Debug)]
pub struct Context<I, X> {
    pub statics: BTreeMap<I, Arc<Expression<I>>>,
    pub variables: BTreeMap<I, Value<I, X>>,
    pub state: ContextState<I, X>,
}

#[derive(Clone, Debug)]
pub struct ContextState<I, X> {
    pub unnormalized: BTreeSet<I>,
    pub requesting: BTreeMap<I, BTreeMap<X, Request<I>>>,
}

impl<I, X> ContextState<I, X> {
    fn new() -> Self {
        Self {
            unnormalized: BTreeSet::new(),
            requesting: BTreeMap::new(),
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

impl<I: Clone + Ord, X> Context<I, X> {
    pub fn fix_captures(&self) {
        for (_, definition) in &self.statics {
            definition.fix_captures();
        }
        for (_, value) in &self.variables {
            value.fix_captures();
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
            for (_, requests) in &context_state.requesting {
                self.state
                    .requesting
                    .entry(name.clone())
                    .or_default()
                    .extend(
                        requests
                            .iter()
                            .map(|(external, request)| (external.clone(), request.clone())),
                    );
            }
        }
        match value.value_state() {
            ValueState::Unnormalized => {
                self.state.unnormalized.insert(name.clone());
            }
            ValueState::Requesting(requests) => {
                self.state
                    .requesting
                    .entry(name.clone())
                    .or_default()
                    .extend(requests);
            }
            _ => (),
        }
        self.variables.insert(name.clone(), value);
        Ok(())
    }

    pub fn get(&mut self, name: &I) -> Result<Value<I, X>, RuntimeError<I, X>> {
        if let Some(value) = self.variables.remove(name) {
            self.state.unnormalized.remove(name);
            self.state.requesting.remove(name);
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
        self.state.unnormalized.first().cloned().or_else(|| {
            self.state
                .requesting
                .iter()
                .filter(|&(_, requests)| {
                    responses
                        .keys()
                        .any(|external| requests.contains_key(external))
                })
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

pub fn run_to_suspension<I: Clone + Ord, X: Clone + Ord>(
    mut running: Running<I, X>,
    responses: &mut BTreeMap<X, Response<I, X>>,
) -> Result<(Option<Running<I, X>>, Vec<(X, Action<I, X>)>), RuntimeError<I, X>> {
    let mut actions = Vec::new();
    let mut head_normal = false;

    loop {
        if !head_normal {
            loop {
                let (new_running, action) = step(running, responses)?;
                let block = match action {
                    Some((ext, act)) => {
                        actions.push((ext, act.clone()));
                        act.is_requesting()
                    }
                    None => false,
                };
                running = match new_running {
                    Some(new) => new,
                    None => return Ok((None, actions)),
                };
                if block {
                    break;
                }
            }
        }

        let Some(unblocked) = running.context.get_unblocked(responses) else {
            return Ok((Some(running), actions));
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
            let captured = context.extract(&capture.borrow())?;
            Ok((
                context,
                Value::Suspend(captured, channel.clone(), Arc::clone(process)),
            ))
        }

        Expression::String(literal) => Ok((context, Value::String(literal.to_string()))),
    }
}

pub fn step<I: Clone + Ord, X: Clone + Ord>(
    Running {
        mut context,
        process,
    }: Running<I, X>,
    responses: &mut BTreeMap<X, Response<I, X>>,
) -> Result<(Option<Running<I, X>>, Option<(X, Action<I, X>)>), RuntimeError<I, X>> {
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
                        Ok((None, Some((ext, Action::Break))))
                    }

                    Command::Continue(then) => Ok((
                        Running::some(context, Arc::clone(then)),
                        Some((ext, Action::Continue)),
                    )),

                    Command::Send(argument, after_send) => {
                        let (mut context, argument) = evaluate(context, Arc::clone(argument))?;
                        context.set(name, Value::External(ext.clone()))?;
                        Ok((
                            Running::some(context, Arc::clone(after_send)),
                            Some((ext, Action::Send(argument))),
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
                                Some((ext, Action::Receive)),
                            ))
                        }
                    },

                    Command::Select(selected, after_select) => {
                        context.set(name, Value::External(ext.clone()))?;
                        Ok((
                            Running::some(context, Arc::clone(after_select)),
                            Some((ext, Action::Select(selected.clone()))),
                        ))
                    }

                    Command::Case(branches) => match responses.remove(&ext) {
                        Some(Response::Case(selected)) => {
                            for (branch, then) in branches {
                                if &selected == branch {
                                    context.set(name, Value::External(ext))?;
                                    return Ok((Running::some(context, Arc::clone(then)), None));
                                }
                            }
                            Err(command.cannot(Value::External(ext)))
                        }
                        Some(_) => Err(command.cannot(Value::External(ext))),
                        None => {
                            context.set(name, Value::External(ext.clone()))?;
                            Ok((
                                Running::some(context, Arc::clone(&process)),
                                Some((
                                    ext,
                                    Action::Case(
                                        branches.iter().map(|(branch, _)| branch.clone()).collect(),
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
                    (handle_context, channel, Command::Case(branches)),
                )
                | (
                    (handle_context, channel, Command::Case(branches)),
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
                    Err(RuntimeError::MissingCase(
                        Value::Suspend(
                            handle_context,
                            channel.clone(),
                            Arc::new(Process::Do(name.clone(), Command::Case(branches.clone()))),
                        ),
                        selected.clone(),
                    ))
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
            Self::Ref(name) => write!(f, "{}", name),
            Self::Fork(_, name, process) => write!(f, "{} {{ {} }}", name, process),
            Self::String(literal) => write!(f, "{:?}", literal),
        }
    }
}

impl<I: std::fmt::Display> std::fmt::Display for Process<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(name, definition, process) => {
                write!(f, "let {} = {}; {}", name, definition, process)
            }
            Self::Link(left, right) => write!(f, "{} <> {}", left, right),
            Self::Do(name, Command::Break) => write!(f, "{}()", name),
            Self::Do(name, Command::Continue(process)) => write!(f, "{}[]; {}", name, process),
            Self::Do(name, Command::Send(argument, process)) => {
                write!(f, "{}({}); {}", name, argument, process)
            }
            Self::Do(name, Command::Receive(parameter, process)) => {
                write!(f, "{}[{}]; {}", name, parameter, process)
            }
            Self::Do(name, Command::Select(branch, process)) => {
                write!(f, "{}.{}; {}", name, branch, process)
            }
            Self::Do(name, Command::Case(branches)) => {
                write!(f, "{}.case {{ ", name)?;
                for (branch, process) in branches {
                    write!(f, "{} => {{ {} }} ", branch, process)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl<I: std::fmt::Display, X: std::fmt::Display> std::fmt::Display for Value<I, X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Suspend(_, name, process) => write!(f, "{} {{ {} }}", name, process),
            Self::External(x) => write!(f, "{}", x),
            Self::String(s) => write!(f, "{:?}", s),
        }
    }
}

impl<I: std::fmt::Display, X: std::fmt::Display> std::fmt::Display for RuntimeError<I, X> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DoesNotExist(name) => write!(f, "{} does not exist", name),
            Self::AlreadyDefined(name) => write!(f, "{} is already defined", name),
            Self::Unused(context) => write!(
                f,
                "{} was not used",
                context
                    .variables
                    .values()
                    .next()
                    .expect("no unused variables")
            ),
            Self::CannotLink(left, right) => {
                write!(f, "cannot link\n--> {}\n--> {}", left, right)
            }
            Self::CannotBreakTo(value) => write!(f, "cannot break to\n--> {}", value),
            Self::CannotContinueFrom(value) => {
                write!(f, "cannot continue from\n--> {}", value)
            }
            Self::CannotSendTo(value) => write!(f, "cannot send to\n--> {}", value),
            Self::CannotReceiveFrom(value) => {
                write!(f, "cannot receive from\n--> {}", value)
            }
            Self::MissingCase(value, branch) => {
                write!(f, "missing case {} in\n--> {}", branch, value)
            }
            Self::CannotSelectFrom(value, branches) => {
                write!(f, "cannot select from [")?;
                for (i, branch) in branches.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", branch)?;
                }
                write!(f, "] in\n--> {}", value)
            }
            Self::ExternalEscaped(external) => write!(f, "external {} escaped", external),
        }
    }
}
