use std::collections::BTreeMap;

use crate::base::{
    run_to_suspension, Action, Command, Context, Process, Request, Response, Running, RuntimeError,
    Value,
};

pub struct Environment<I> {
    pub histories: BTreeMap<External, Vec<Event<I, External>>>,
    pub responses: BTreeMap<External, Response<I, External>>,
    pub runnings: Vec<Running<I, External>>,
    pub primary: External,
    next: External,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct External {
    id: u64,
}

#[derive(Clone, Debug)]
pub enum Event<I, X> {
    Break,
    Continue,
    Send(X),
    Message(String),
    Receive(X),
    Select(I),
    Case(I),
}

impl<I: Clone + Ord> Environment<I> {
    pub fn new(
        mut context: Context<I, External>,
        program: &I,
    ) -> Result<Self, RuntimeError<I, External>> {
        let program = context.get(program)?;
        let Value::Suspend(mut context, channel, process) = program else {
            unreachable!();
        };
        let primary = External { id: 0 };
        let next = External { id: 1 };
        context.set(&channel, Value::External(primary.clone()))?;
        Ok(Self {
            histories: BTreeMap::new(),
            responses: BTreeMap::new(),
            runnings: Vec::from([Running { context, process }]),
            primary,
            next,
        })
    }

    fn generate(&mut self) -> External {
        let next_id = self.next.id + 1;
        std::mem::replace(&mut self.next, External { id: next_id })
    }

    pub fn get_requests(&self) -> BTreeMap<External, Request<I>> {
        let mut requests = BTreeMap::new();
        for running in &self.runnings {
            for (_, requesting) in &running.context.state.requesting {
                requests.extend(requesting.iter().map(|(k, v)| (k.clone(), v.clone())));
            }
            if let Process::Do(subject, command) = &*running.process {
                if let Some(Value::External(external)) = running.context.variables.get(subject) {
                    match command {
                        Command::Receive(_, _) => {
                            requests.insert(external.clone(), Request::Receive);
                        }
                        Command::Case(branches, otherwise) => {
                            requests.insert(
                                external.clone(),
                                Request::Case(
                                    branches.iter().map(|(branch, _)| branch.clone()).collect(),
                                    otherwise.is_some(),
                                ),
                            );
                        }
                        _ => (),
                    }
                }
            }
        }
        requests
    }

    pub fn respond(&mut self, external: External, response: Response<I, External>) {
        match &response {
            Response::Receive(Value::External(ext)) => {
                self.histories
                    .entry(external.clone())
                    .or_default()
                    .push(Event::Receive(ext.clone()));
            }
            Response::Case(Some(selected)) => {
                self.histories
                    .entry(external.clone())
                    .or_default()
                    .push(Event::Case(selected.clone()));
            }
            _ => (),
        }
        self.responses.insert(external.clone(), response);
    }

    pub fn run_to_suspension(&mut self) -> Result<(), RuntimeError<I, External>> {
        loop {
            self.run_all()?;

            let mut responded = false;
            for (external, request) in self.get_requests() {
                if matches!(request, Request::Receive) {
                    let new_external = self.generate();
                    self.respond(external, Response::Receive(Value::External(new_external)));
                    responded = true;
                }
            }
            if !responded {
                return Ok(());
            }
        }
    }

    pub fn run_all(&mut self) -> Result<(), RuntimeError<I, External>> {
        let mut pending = std::mem::replace(&mut self.runnings, Vec::new());
        while let Some(running) = pending.pop() {
            let (new_running, requests) = run_to_suspension(running, &mut self.responses)?;
            self.runnings.extend(new_running.into_iter());
            for (external, request) in requests {
                match request {
                    Action::Break => {
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Break);
                    }
                    Action::Continue => {
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Continue);
                    }
                    Action::Send(Value::Suspend(mut context, channel, process)) => {
                        let new_external = self.generate();
                        context.set(&channel, Value::External(new_external.clone()))?;
                        pending.push(Running { context, process });
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Send(new_external));
                    }
                    Action::Send(Value::External(escaped)) => {
                        return Err(RuntimeError::ExternalEscaped(escaped))
                    }
                    Action::Send(Value::String(message)) => {
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Message(message));
                    }
                    Action::Receive => (),
                    Action::Select(selected) => {
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Select(selected));
                    }
                    Action::Case(_, _) => (),
                }
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for External {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{{{}}}", self.id)
    }
}
