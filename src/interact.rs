use std::collections::BTreeMap;

use crate::base::{run_to_suspension, Context, Request, Response, Running, RuntimeError, Value};

pub struct Environment<I> {
    pub histories: BTreeMap<External, Vec<Event<I, External>>>,
    pub blockers: BTreeMap<External, Blocker<I>>,
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
pub enum Blocker<I> {
    Receive,
    Case(Vec<I>, Option<()>),
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
            blockers: BTreeMap::new(),
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
        self.blockers.remove(&external);
        self.responses.insert(external.clone(), response);
    }

    pub fn run_to_suspension(&mut self) -> Result<(), RuntimeError<I, External>> {
        loop {
            self.run_all()?;
            let mut receives = self
                .blockers
                .iter()
                .filter(|&(_, b)| matches!(b, Blocker::Receive))
                .map(|(ext, _)| ext.clone())
                .collect::<Vec<_>>();
            if receives.is_empty() {
                return Ok(());
            }
            for external in receives.drain(..) {
                let ext = self.generate();
                self.respond(external, Response::Receive(Value::External(ext)));
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
                    Request::Break => {
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Break);
                    }
                    Request::Continue => {
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Continue);
                    }
                    Request::Send(Value::Suspend(mut context, channel, process)) => {
                        let new_external = self.generate();
                        context.set(&channel, Value::External(new_external.clone()))?;
                        pending.push(Running { context, process });
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Send(new_external));
                    }
                    Request::Send(Value::External(escaped)) => {
                        return Err(RuntimeError::ExternalEscaped(escaped))
                    }
                    Request::Send(Value::String(message)) => {
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Message(message));
                    }
                    Request::Receive => {
                        self.blockers.insert(external.clone(), Blocker::Receive);
                    }
                    Request::Select(selected) => {
                        self.histories
                            .entry(external)
                            .or_default()
                            .push(Event::Select(selected));
                    }
                    Request::Case(branches, otherwise) => {
                        self.blockers
                            .insert(external.clone(), Blocker::Case(branches, otherwise));
                    }
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
