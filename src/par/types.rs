use indexmap::IndexMap;
use std::{hash::Hash, sync::Arc};

#[derive(Clone, Debug)]
pub enum Error<Loc, Name> {
    NameNotDefined(Loc, Name),
    ShadowedObligation(Loc, Name),
    UnfulfilledObligations(Loc, Vec<Name>),
    IncompatibleOperations(Type<Loc, Name>, Type<Loc, Name>),
    NoSuchLoopPoint(Loc, Option<Name>),
}

#[derive(Clone, Debug)]
pub enum Type<Loc, Name> {
    Send(Loc, Arc<Self>, Arc<Self>),
    Receive(Loc, Arc<Self>, Arc<Self>),
    Either(Loc, IndexMap<Name, Arc<Self>>),
    Branch(Loc, IndexMap<Name, Arc<Self>>),
    Break(Loc),
    Continue(Loc),
    Recursive(Loc, Option<Name>, Arc<Self>),
    Iterative(Loc, Option<Name>, Arc<Self>),
    Self_(Loc, Option<Name>),
    Loop(Loc, Option<Name>),
}

pub struct Context<Loc, Name> {
    variables: IndexMap<Name, Type<Loc, Name>>,
}

impl<Loc, Name> Context<Loc, Name>
where
    Loc: Clone + Eq + Hash,
    Name: Clone + Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            variables: IndexMap::new(),
        }
    }

    pub fn split(&self) -> Self {
        Self {
            variables: IndexMap::new(),
        }
    }

    pub fn get_variable(&self, name: &Name) -> Option<&Type<Loc, Name>> {
        self.variables.get(name)
    }

    pub fn get(&self, loc: &Loc, name: &Name) -> Result<&Type<Loc, Name>, Error<Loc, Name>> {
        match self.get_variable(name) {
            Some(typ) => Ok(typ),
            None => Err(Error::NameNotDefined(loc.clone(), name.clone())),
        }
    }

    pub fn put(
        &mut self,
        loc: &Loc,
        name: Name,
        value: Type<Loc, Name>,
    ) -> Result<(), Error<Loc, Name>> {
        if let Some(_) = self.variables.get(&name) {
            return Err(Error::ShadowedObligation(loc.clone(), name));
        }
        self.variables.insert(name, value);
        Ok(())
    }
}
