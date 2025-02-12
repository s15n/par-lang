use indexmap::IndexMap;
use std::{
    collections::HashSet,
    fmt::{self, Display, Write},
    hash::Hash,
    sync::Arc,
};

use super::process::{Captures, Command, Expression, Process};

#[derive(Clone, Debug)]
pub enum TypeError<Loc, Name> {
    NameNotDefined(Loc, Name),
    ShadowedObligation(Loc, Name),
    TypeMustBeKnownAtThisPoint(Loc, Name),
    ParameterTypeMustBeKnown(Loc, Name, Name),
    CannotAssignFromTo(Loc, Type<Loc, Name>, Type<Loc, Name>),
    UnfulfilledObligations(Loc, Vec<Name>),
    InvalidOperation(Operation<Loc, Name>, Type<Loc, Name>),
    InvalidBranch(Loc, Name, Type<Loc, Name>),
    MissingBranch(Loc, Name, Type<Loc, Name>),
    RedundantBranch(Loc, Name, Type<Loc, Name>),
    TypesCannotBeUnified(Type<Loc, Name>, Type<Loc, Name>),
    NoSuchLoopPoint(Loc, Option<Name>),
    LoopVariableNotPreserved(Loc, Name),
    LoopVariableChangedType(Loc, Name, Type<Loc, Name>, Type<Loc, Name>),
    Telltypes(Loc, IndexMap<Name, Type<Loc, Name>>),
}

#[derive(Clone, Debug)]
pub enum Operation<Loc, Name> {
    Send(Loc),
    Receive(Loc),
    Choose(Loc, Name),
    Match(Loc, Arc<[Name]>),
    Break(Loc),
    Continue(Loc),
    Begin(Loc, Option<Name>),
    Loop(Loc, Option<Name>),
}

#[derive(Clone, Debug)]
pub enum Type<Loc, Name> {
    Send(Loc, Box<Self>, Box<Self>),
    Receive(Loc, Box<Self>, Box<Self>),
    Either(Loc, IndexMap<Name, Self>),
    Choice(Loc, IndexMap<Name, Self>),
    Break(Loc),
    Continue(Loc),
    Recursive(Loc, Option<Name>, Box<Self>),
    Iterative(Loc, Option<Name>, Box<Self>),
    Self_(Loc, Option<Name>),
    Loop(Loc, Option<Name>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum TypePoint<Name> {
    Self_(Option<Name>),
    Loop(Option<Name>),
}

impl<Loc, Name: Eq + Hash> Type<Loc, Name> {
    pub fn map_names<N: Eq + Hash>(self, f: &mut impl FnMut(Name) -> N) -> Type<Loc, N> {
        match self {
            Self::Send(loc, t, u) => {
                Type::Send(loc, Box::new(t.map_names(f)), Box::new(u.map_names(f)))
            }
            Self::Receive(loc, t, u) => {
                Type::Receive(loc, Box::new(t.map_names(f)), Box::new(u.map_names(f)))
            }
            Self::Either(loc, branches) => Type::Either(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (f(branch), typ.map_names(f)))
                    .collect(),
            ),
            Self::Choice(loc, branches) => Type::Choice(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (f(branch), typ.map_names(f)))
                    .collect(),
            ),
            Self::Break(loc) => Type::Break(loc),
            Self::Continue(loc) => Type::Continue(loc),
            Self::Recursive(loc, label, body) => {
                Type::Recursive(loc, map_label(label, f), Box::new(body.map_names(f)))
            }
            Self::Iterative(loc, label, body) => {
                Type::Iterative(loc, map_label(label, f), Box::new(body.map_names(f)))
            }
            Self::Self_(loc, label) => Type::Self_(loc, map_label(label, f)),
            Self::Loop(loc, label) => Type::Loop(loc, map_label(label, f)),
        }
    }
}

fn map_label<Name, N>(label: Option<Name>, f: &mut impl FnMut(Name) -> N) -> Option<N> {
    label.map(f)
}

impl<Loc: Clone, Name: Clone + Eq + Hash> Type<Loc, Name> {
    pub fn check_subtype(
        &self,
        loc: &Loc,
        u: &Type<Loc, Name>,
    ) -> Result<(), TypeError<Loc, Name>> {
        if !self.is_subtype_of(u, &HashSet::new()) {
            return Err(TypeError::CannotAssignFromTo(
                loc.clone(),
                self.clone(),
                u.clone(),
            ));
        }
        Ok(())
    }

    fn is_subtype_of(
        &self,
        other: &Self,
        ind: &HashSet<(TypePoint<Name>, TypePoint<Name>)>,
    ) -> bool {
        match (self, other) {
            (Self::Send(_, t1, u1), Self::Send(_, t2, u2)) => {
                t1.is_subtype_of(t2, ind) && u1.is_subtype_of(u2, ind)
            }
            (Self::Receive(_, t1, u1), Self::Receive(_, t2, u2)) => {
                t2.is_subtype_of(t1, ind) && u1.is_subtype_of(u2, ind)
            }
            (Self::Either(_, branches1), Self::Either(_, branches2)) => {
                branches1.iter().all(|(branch, t1)| {
                    branches2
                        .get(branch)
                        .map(|t2| t1.is_subtype_of(t2, ind))
                        .unwrap_or(false)
                })
            }
            (Self::Choice(_, branches1), Self::Choice(_, branches2)) => {
                branches2.iter().all(|(branch, t2)| {
                    branches1
                        .get(branch)
                        .map(|t1| t1.is_subtype_of(t2, ind))
                        .unwrap_or(false)
                })
            }
            (Self::Break(_), Self::Break(_)) => true,
            (Self::Continue(_), Self::Continue(_)) => true,

            (Self::Recursive(_, label1, body1), Self::Recursive(_, label2, body2)) => {
                let mut ind = ind.clone();
                ind.insert((
                    TypePoint::Self_(label1.clone()),
                    TypePoint::Self_(label2.clone()),
                ));
                body1.is_subtype_of(body2, &ind)
            }
            (typ, Self::Recursive(_, label, body)) => {
                typ.is_subtype_of(&Self::expand_recursive(label, body), ind)
            }
            (Self::Iterative(_, label1, body1), Self::Iterative(_, label2, body2)) => {
                let mut ind = ind.clone();
                ind.insert((
                    TypePoint::Loop(label1.clone()),
                    TypePoint::Loop(label2.clone()),
                ));
                body1.is_subtype_of(body2, &ind)
            }
            (Self::Iterative(_, label, body), typ) => {
                Self::expand_iterative(label, body).is_subtype_of(typ, ind)
            }

            (Self::Self_(_, label1), Self::Self_(_, label2)) => ind.contains(&(
                TypePoint::Self_(label1.clone()),
                TypePoint::Self_(label2.clone()),
            )),
            (Self::Loop(_, label1), Self::Loop(_, label2)) => ind.contains(&(
                TypePoint::Loop(label1.clone()),
                TypePoint::Loop(label2.clone()),
            )),

            _ => false,
        }
    }

    pub fn dual(&self) -> Self {
        match self {
            Self::Send(loc, t, u) => Self::Receive(loc.clone(), t.clone(), Box::new(u.dual())),
            Self::Receive(loc, t, u) => Self::Send(loc.clone(), t.clone(), Box::new(u.dual())),
            Self::Either(loc, branches) => Self::Choice(
                loc.clone(),
                branches
                    .iter()
                    .map(|(branch, t)| (branch.clone(), t.dual()))
                    .collect(),
            ),
            Self::Choice(loc, branches) => Self::Either(
                loc.clone(),
                branches
                    .iter()
                    .map(|(branch, t)| (branch.clone(), t.dual()))
                    .collect(),
            ),
            Self::Break(loc) => Self::Continue(loc.clone()),
            Self::Continue(loc) => Self::Break(loc.clone()),
            Self::Recursive(loc, label, t) => {
                Self::Iterative(loc.clone(), label.clone(), Box::new(t.dual()))
            }
            Self::Iterative(loc, label, t) => {
                Self::Recursive(loc.clone(), label.clone(), Box::new(t.dual()))
            }
            Self::Self_(loc, label) => Self::Loop(loc.clone(), label.clone()),
            Self::Loop(loc, label) => Self::Self_(loc.clone(), label.clone()),
        }
    }

    pub fn expand_recursive(label: &Option<Name>, body: &Self) -> Self {
        body.clone().expand_recursive_helper(label, body)
    }

    fn expand_recursive_helper(self, top_label: &Option<Name>, top_body: &Self) -> Self {
        match self {
            Self::Send(loc, t, u) => Self::Send(
                loc,
                Box::new(t.expand_recursive_helper(top_label, top_body)),
                Box::new(u.expand_recursive_helper(top_label, top_body)),
            ),
            Self::Receive(loc, t, u) => Self::Receive(
                loc,
                Box::new(t.expand_recursive_helper(top_label, top_body)),
                Box::new(u.expand_recursive_helper(top_label, top_body)),
            ),
            Self::Either(loc, branches) => Self::Either(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (branch, typ.expand_recursive_helper(top_label, top_body)))
                    .collect(),
            ),
            Self::Choice(loc, branches) => Self::Choice(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (branch, typ.expand_recursive_helper(top_label, top_body)))
                    .collect(),
            ),
            Self::Break(loc) => Self::Break(loc),
            Self::Continue(loc) => Self::Break(loc),
            Self::Recursive(loc, label, body) => {
                if &label == top_label {
                    Self::Recursive(loc, label, body)
                } else {
                    Self::Recursive(
                        loc,
                        label,
                        Box::new(body.expand_recursive_helper(top_label, top_body)),
                    )
                }
            }
            Self::Iterative(loc, label, body) => Self::Iterative(
                loc,
                label,
                Box::new(body.expand_recursive_helper(top_label, top_body)),
            ),
            Self::Self_(loc, label) => {
                if &label == top_label {
                    Self::Recursive(loc, label, Box::new(top_body.clone()))
                } else {
                    Self::Self_(loc, label)
                }
            }
            Self::Loop(loc, label) => Self::Loop(loc, label),
        }
    }

    pub fn expand_iterative(label: &Option<Name>, body: &Self) -> Self {
        body.clone().expand_iterative_helper(label, body)
    }

    fn expand_iterative_helper(self, top_label: &Option<Name>, top_body: &Self) -> Self {
        match self {
            Self::Send(loc, t, u) => Self::Send(
                loc,
                Box::new(t.expand_iterative_helper(top_label, top_body)),
                Box::new(u.expand_iterative_helper(top_label, top_body)),
            ),
            Self::Receive(loc, t, u) => Self::Receive(
                loc,
                Box::new(t.expand_iterative_helper(top_label, top_body)),
                Box::new(u.expand_iterative_helper(top_label, top_body)),
            ),
            Self::Either(loc, branches) => Self::Either(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (branch, typ.expand_iterative_helper(top_label, top_body)))
                    .collect(),
            ),
            Self::Choice(loc, branches) => Self::Choice(
                loc,
                branches
                    .into_iter()
                    .map(|(branch, typ)| (branch, typ.expand_iterative_helper(top_label, top_body)))
                    .collect(),
            ),
            Self::Break(loc) => Self::Break(loc),
            Self::Continue(loc) => Self::Break(loc),
            Self::Recursive(loc, label, body) => Self::Recursive(
                loc,
                label,
                Box::new(body.expand_iterative_helper(top_label, top_body)),
            ),
            Self::Iterative(loc, label, body) => {
                if &label == top_label {
                    Self::Iterative(loc, label, body)
                } else {
                    Self::Iterative(
                        loc,
                        label,
                        Box::new(body.expand_iterative_helper(top_label, top_body)),
                    )
                }
            }
            Self::Self_(loc, label) => Self::Self_(loc, label),
            Self::Loop(loc, label) => {
                if &label == top_label {
                    Self::Iterative(loc, label, Box::new(top_body.clone()))
                } else {
                    Self::Loop(loc, label)
                }
            }
        }
    }

    pub fn unify(self, other: Self) -> Result<Self, TypeError<Loc, Name>> {
        Ok(match (self, other) {
            (Self::Send(loc, t1, u1), Self::Send(_, t2, u2)) => {
                Self::Send(loc, Box::new(t1.unify(*t2)?), Box::new(u1.unify(*u2)?))
            }
            (Self::Receive(loc, t1, u1), Self::Receive(_, t2, u2)) => {
                Self::Receive(loc, Box::new(t1.unify(*t2)?), Box::new(u1.unify(*u2)?))
            }
            (Self::Either(loc, branches1), Self::Either(_, branches2)) => {
                let mut branches = branches1;
                for (branch, typ2) in branches2 {
                    let typ = match branches.shift_remove(&branch) {
                        Some(typ1) => typ1.unify(typ2)?,
                        None => typ2,
                    };
                    branches.insert(branch, typ);
                }
                Self::Either(loc, branches)
            }
            (Self::Choice(loc, branches1), Self::Choice(_, branches2)) => {
                let mut branches = branches1;
                for (branch, typ2) in branches2 {
                    let typ = match branches.shift_remove(&branch) {
                        Some(typ1) => typ1.unify(typ2)?,
                        None => typ2,
                    };
                    branches.insert(branch, typ);
                }
                Self::Choice(loc, branches)
            }
            (Self::Break(loc), Self::Break(_)) => Self::Break(loc),
            (Self::Continue(loc), Self::Continue(_)) => Self::Continue(loc),
            (typ1, typ2) => return Err(TypeError::TypesCannotBeUnified(typ1, typ2)), //TODO: recursive & iterative
        })
    }

    pub fn unify_vec(
        loc: &Loc,
        name: &Name,
        mut types: Vec<Self>,
    ) -> Result<Self, TypeError<Loc, Name>> {
        let Some(mut typ) = types.pop() else {
            return Err(TypeError::TypeMustBeKnownAtThisPoint(
                loc.clone(),
                name.clone(),
            ));
        };
        while let Some(typ1) = types.pop() {
            typ = typ1.unify(typ)?;
        }
        Ok(typ)
    }
}

#[derive(Clone, Debug)]
pub struct Context<Loc, Name> {
    variables: IndexMap<Name, Type<Loc, Name>>,
    loop_points: IndexMap<Option<Name>, (Name, Arc<IndexMap<Name, Type<Loc, Name>>>)>,
}

impl<Loc, Name> Context<Loc, Name>
where
    Loc: std::fmt::Debug + Clone + Eq + Hash,
    Name: Clone + Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            variables: IndexMap::new(),
            loop_points: IndexMap::new(),
        }
    }

    pub fn split(&self) -> Self {
        Self {
            variables: IndexMap::new(),
            loop_points: self.loop_points.clone(),
        }
    }

    pub fn get_variable(&mut self, name: &Name) -> Option<Type<Loc, Name>> {
        self.variables.shift_remove(name)
    }

    pub fn get(&mut self, loc: &Loc, name: &Name) -> Result<Type<Loc, Name>, TypeError<Loc, Name>> {
        match self.get_variable(name) {
            Some(typ) => Ok(typ),
            None => Err(TypeError::NameNotDefined(loc.clone(), name.clone())),
        }
    }

    pub fn put(
        &mut self,
        loc: &Loc,
        name: Name,
        typ: Type<Loc, Name>,
    ) -> Result<(), TypeError<Loc, Name>> {
        if let Some(_) = self.variables.get(&name) {
            return Err(TypeError::ShadowedObligation(loc.clone(), name));
        }
        self.variables.insert(name, typ);
        Ok(())
    }

    pub fn capture(
        &mut self,
        inference_subject: Option<&Name>,
        cap: &Captures<Loc, Name>,
        target: &mut Self,
    ) -> Result<(), TypeError<Loc, Name>> {
        for (name, loc) in &cap.names {
            if Some(name) == inference_subject {
                return Err(TypeError::TypeMustBeKnownAtThisPoint(
                    loc.clone(),
                    name.clone(),
                ));
            }
            let value = match self.get_variable(name) {
                Some(value) => value,
                None => continue,
            };
            target.put(loc, name.clone(), value)?;
        }
        Ok(())
    }

    pub fn obligations(&self) -> impl Iterator<Item = &Name> {
        self.variables.iter().map(|(name, _)| name)
    }

    pub fn check_process(
        &mut self,
        process: &Process<Loc, Name, ()>,
    ) -> Result<Arc<Process<Loc, Name, Type<Loc, Name>>>, TypeError<Loc, Name>> {
        match process {
            Process::Let(loc, name, annotation, (), expression, process) => {
                let (expression, typ) = match annotation {
                    Some(annotated_type) => (
                        self.check_expression(None, expression, annotated_type)?,
                        annotated_type.clone(),
                    ),
                    None => self.infer_expression(None, expression)?,
                };
                self.put(loc, name.clone(), typ.clone())?;
                let process = self.check_process(process)?;
                Ok(Arc::new(Process::Let(
                    loc.clone(),
                    name.clone(),
                    annotation.clone(),
                    typ,
                    expression,
                    process,
                )))
            }

            Process::Do(loc, object, (), command) => {
                let typ = match self.get_variable(object) {
                    Some(typ) => typ,
                    None => return Err(TypeError::NameNotDefined(loc.clone(), object.clone())),
                };

                let (command, _) =
                    self.check_command(loc, object, &typ, command, &mut |context, process| {
                        Ok((context.check_process(process)?, vec![]))
                    })?;

                Ok(Arc::new(Process::Do(
                    loc.clone(),
                    object.clone(),
                    typ,
                    command,
                )))
            }

            Process::Telltypes(loc) => {
                return Err(TypeError::Telltypes(loc.clone(), self.variables.clone()))
            }
        }
    }

    fn check_command(
        &mut self,
        loc: &Loc,
        object: &Name,
        typ: &Type<Loc, Name>,
        command: &Command<Loc, Name, ()>,
        analyze_process: &mut impl FnMut(
            &mut Self,
            &Process<Loc, Name, ()>,
        ) -> Result<
            (
                Arc<Process<Loc, Name, Type<Loc, Name>>>,
                Vec<Type<Loc, Name>>,
            ),
            TypeError<Loc, Name>,
        >,
    ) -> Result<(Command<Loc, Name, Type<Loc, Name>>, Vec<Type<Loc, Name>>), TypeError<Loc, Name>>
    {
        if let Type::Iterative(_, top_label, body) = typ {
            return self.check_command(
                loc,
                object,
                &Type::expand_iterative(top_label, body),
                command,
                analyze_process,
            );
        }

        Ok(match command {
            Command::Link(expression) => {
                let expression = self.check_expression(None, expression, &typ.dual())?;
                self.cannot_have_obligations(loc)?;
                (Command::Link(expression), vec![])
            }

            Command::Send(argument, process) => {
                let Type::Receive(_, argument_type, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        Operation::Send(loc.clone()),
                        typ.clone(),
                    ));
                };
                let argument = self.check_expression(None, argument, &argument_type)?;
                self.put(loc, object.clone(), *then_type.clone())?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Send(argument, process), inferred_types)
            }

            Command::Receive(parameter, annotation, process) => {
                let Type::Send(_, parameter_type, then_type) = typ else {
                    return Err(TypeError::InvalidOperation(
                        Operation::Receive(loc.clone()),
                        typ.clone(),
                    ));
                };
                if let Some(annotated_type) = annotation {
                    parameter_type.check_subtype(loc, annotated_type)?;
                }
                self.put(loc, parameter.clone(), *parameter_type.clone())?;
                self.put(loc, object.clone(), *then_type.clone())?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (
                    Command::Receive(parameter.clone(), annotation.clone(), process),
                    inferred_types,
                )
            }

            Command::Choose(chosen, process) => {
                let Type::Choice(_, branches) = typ else {
                    return Err(TypeError::InvalidOperation(
                        Operation::Choose(loc.clone(), chosen.clone()),
                        typ.clone(),
                    ));
                };
                let Some(branch_type) = branches.get(chosen) else {
                    return Err(TypeError::InvalidBranch(
                        loc.clone(),
                        chosen.clone(),
                        typ.clone(),
                    ));
                };
                self.put(loc, object.clone(), branch_type.clone())?;
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Choose(chosen.clone(), process), inferred_types)
            }

            Command::Match(branches, processes) => {
                let Type::Either(_, required_branches) = typ else {
                    return Err(TypeError::InvalidOperation(
                        Operation::Match(loc.clone(), Arc::clone(branches)),
                        typ.clone(),
                    ));
                };
                if let Some(missing) = required_branches
                    .keys()
                    .find(|&branch| !branches.contains(branch))
                {
                    return Err(TypeError::MissingBranch(
                        loc.clone(),
                        missing.clone(),
                        typ.clone(),
                    ));
                }

                let original_context = self.clone();
                let mut typed_processes = Vec::new();
                let mut inferred_types = Vec::new();

                for (branch, process) in branches.iter().zip(processes.iter()) {
                    *self = original_context.clone();

                    let Some(branch_type) = required_branches.get(branch) else {
                        return Err(TypeError::RedundantBranch(
                            loc.clone(),
                            branch.clone(),
                            typ.clone(),
                        ));
                    };
                    self.put(loc, object.clone(), branch_type.clone())?;
                    let (process, inferred_type) = analyze_process(self, process)?;
                    typed_processes.push(process);
                    inferred_types.extend(inferred_type);
                }

                (
                    Command::Match(Arc::clone(branches), Box::from(typed_processes)),
                    inferred_types,
                )
            }

            Command::Break => {
                let Type::Continue(_) = typ else {
                    return Err(TypeError::InvalidOperation(
                        Operation::Break(loc.clone()),
                        typ.clone(),
                    ));
                };
                self.cannot_have_obligations(loc)?;
                (Command::Break, vec![])
            }

            Command::Continue(process) => {
                let Type::Break(_) = typ else {
                    return Err(TypeError::InvalidOperation(
                        Operation::Continue(loc.clone()),
                        typ.clone(),
                    ));
                };
                let (process, inferred_types) = analyze_process(self, process)?;
                (Command::Continue(process), inferred_types)
            }

            Command::Begin(label, process) => {
                let Type::Recursive(_, typ_label, typ_body) = typ else {
                    return Err(TypeError::InvalidOperation(
                        Operation::Begin(loc.clone(), label.clone()),
                        typ.clone(),
                    ));
                };
                let expanded = Type::expand_recursive(typ_label, typ_body);

                self.put(loc, object.clone(), expanded)?;
                self.loop_points.insert(
                    label.clone(),
                    (object.clone(), Arc::new(self.variables.clone())),
                );
                let (process, inferred_types) = analyze_process(self, process)?;

                let inferred_iterative = inferred_types
                    .into_iter()
                    .map(|body| Type::Iterative(loc.clone(), label.clone(), Box::new(body)))
                    .collect();

                (Command::Begin(label.clone(), process), inferred_iterative)
            }

            Command::Loop(label) => {
                let Type::Recursive(_, typ_label, typ_body) = typ else {
                    return Err(TypeError::InvalidOperation(
                        Operation::Loop(loc.clone(), label.clone()),
                        typ.clone(),
                    ));
                };
                let expanded = Type::expand_recursive(typ_label, typ_body);

                let Some((driver, variables)) = self.loop_points.get(label).cloned() else {
                    return Err(TypeError::NoSuchLoopPoint(loc.clone(), label.clone()));
                };
                self.put(loc, driver.clone(), expanded)?;

                for (var, type_at_begin) in variables.as_ref() {
                    let Some(current_type) = self.get_variable(var) else {
                        return Err(TypeError::LoopVariableNotPreserved(
                            loc.clone(),
                            var.clone(),
                        ));
                    };
                    if !current_type.is_subtype_of(type_at_begin, &HashSet::new()) {
                        return Err(TypeError::LoopVariableChangedType(
                            loc.clone(),
                            var.clone(),
                            current_type,
                            type_at_begin.clone(),
                        ));
                    }
                }
                self.cannot_have_obligations(loc)?;

                let inferred_loop = Type::Loop(loc.clone(), label.clone());

                (Command::Loop(label.clone()), vec![inferred_loop])
            }
        })
    }

    pub fn infer_process(
        &mut self,
        process: &Process<Loc, Name, ()>,
        subject: &Name,
    ) -> Result<(Arc<Process<Loc, Name, Type<Loc, Name>>>, Type<Loc, Name>), TypeError<Loc, Name>>
    {
        match process {
            Process::Let(loc, name, annotation, (), expression, process) => {
                let (expression, typ) = match annotation {
                    Some(annotated_type) => (
                        self.check_expression(Some(subject), expression, annotated_type)?,
                        annotated_type.clone(),
                    ),
                    None => self.infer_expression(Some(subject), expression)?,
                };
                self.put(loc, name.clone(), typ.clone())?;
                let (process, subject_type) = self.infer_process(process, subject)?;
                Ok((
                    Arc::new(Process::Let(
                        loc.clone(),
                        name.clone(),
                        annotation.clone(),
                        typ,
                        expression,
                        process,
                    )),
                    subject_type,
                ))
            }

            Process::Do(loc, object, (), command) => {
                if object == subject {
                    let (command, typ) = self.infer_command(loc, subject, command)?;
                    return Ok((
                        Arc::new(Process::Do(
                            loc.clone(),
                            object.clone(),
                            typ.clone(),
                            command,
                        )),
                        typ,
                    ));
                }

                let typ = match self.get_variable(object) {
                    Some(typ) => typ,
                    None => return Err(TypeError::NameNotDefined(loc.clone(), object.clone())),
                };

                let (command, inferred_types) =
                    self.check_command(loc, object, &typ, command, &mut |context, process| {
                        let (process, typ) = context.infer_process(process, subject)?;
                        Ok((process, vec![typ]))
                    })?;

                Ok((
                    Arc::new(Process::Do(loc.clone(), object.clone(), typ, command)),
                    Type::unify_vec(loc, object, inferred_types)?,
                ))
            }

            Process::Telltypes(loc) => {
                return Err(TypeError::Telltypes(loc.clone(), self.variables.clone()))
            }
        }
    }

    pub fn infer_command(
        &mut self,
        loc: &Loc,
        subject: &Name,
        command: &Command<Loc, Name, ()>,
    ) -> Result<(Command<Loc, Name, Type<Loc, Name>>, Type<Loc, Name>), TypeError<Loc, Name>> {
        Ok(match command {
            Command::Link(expression) => {
                let (expression, typ) = self.infer_expression(Some(subject), expression)?;
                (Command::Link(expression), typ.dual())
            }

            Command::Send(argument, process) => {
                let (argument, arg_type) = self.infer_expression(Some(subject), argument)?;
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::Send(argument, process),
                    Type::Receive(loc.clone(), Box::new(arg_type), Box::new(then_type)),
                )
            }

            Command::Receive(parameter, annotation, process) => {
                let Some(param_type) = annotation else {
                    return Err(TypeError::ParameterTypeMustBeKnown(
                        loc.clone(),
                        subject.clone(),
                        parameter.clone(),
                    ));
                };
                self.put(loc, parameter.clone(), param_type.clone())?;
                let (process, then_type) = self.infer_process(process, subject)?;
                (
                    Command::Receive(parameter.clone(), annotation.clone(), process),
                    Type::Send(
                        loc.clone(),
                        Box::new(param_type.clone()),
                        Box::new(then_type),
                    ),
                )
            }

            Command::Choose(chosen, process) => {
                let (process, typ) = self.infer_process(process, subject)?;
                (
                    Command::Choose(chosen.clone(), process),
                    Type::Choice(loc.clone(), IndexMap::from([(chosen.clone(), typ)])),
                )
            }

            Command::Match(branches, processes) => {
                let original_context = self.clone();
                let mut typed_processes = Vec::new();
                let mut branch_types = IndexMap::new();

                for (branch, process) in branches.iter().zip(processes.iter()) {
                    *self = original_context.clone();
                    let (process, typ) = self.infer_process(process, subject)?;
                    typed_processes.push(process);
                    branch_types.insert(branch.clone(), typ);
                }

                (
                    Command::Match(Arc::clone(branches), Box::from(typed_processes)),
                    Type::Either(loc.clone(), branch_types),
                )
            }

            Command::Break => {
                self.cannot_have_obligations(loc)?;
                (Command::Break, Type::Continue(loc.clone()))
            }

            Command::Continue(process) => {
                let process = self.check_process(process)?;
                (Command::Continue(process), Type::Break(loc.clone()))
            }

            Command::Begin(label, process) => {
                self.loop_points.insert(
                    label.clone(),
                    (subject.clone(), Arc::new(self.variables.clone())),
                );
                let (process, body) = self.infer_process(process, subject)?;
                (
                    Command::Begin(label.clone(), process),
                    Type::Recursive(loc.clone(), label.clone(), Box::new(body)),
                )
            }

            Command::Loop(label) => {
                let Some((driver, variables)) = self.loop_points.get(label).cloned() else {
                    return Err(TypeError::NoSuchLoopPoint(loc.clone(), label.clone()));
                };
                if &driver != subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        loc.clone(),
                        subject.clone(),
                    ));
                }

                for (var, type_at_begin) in variables.as_ref() {
                    let Some(current_type) = self.get_variable(var) else {
                        return Err(TypeError::LoopVariableNotPreserved(
                            loc.clone(),
                            var.clone(),
                        ));
                    };
                    if !current_type.is_subtype_of(type_at_begin, &HashSet::new()) {
                        return Err(TypeError::LoopVariableChangedType(
                            loc.clone(),
                            var.clone(),
                            current_type,
                            type_at_begin.clone(),
                        ));
                    }
                }
                self.cannot_have_obligations(loc)?;

                (
                    Command::Loop(label.clone()),
                    Type::Self_(loc.clone(), label.clone()),
                )
            }
        })
    }

    pub fn check_expression(
        &mut self,
        inference_subject: Option<&Name>,
        expression: &Expression<Loc, Name, ()>,
        target_type: &Type<Loc, Name>,
    ) -> Result<Arc<Expression<Loc, Name, Type<Loc, Name>>>, TypeError<Loc, Name>> {
        match expression {
            Expression::Reference(loc, name, ()) => {
                if Some(name) == inference_subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        loc.clone(),
                        name.clone(),
                    ));
                }
                let typ = self.get(loc, name)?;
                typ.check_subtype(loc, target_type)?;
                Ok(Arc::new(Expression::Reference(
                    loc.clone(),
                    name.clone(),
                    typ.clone(),
                )))
            }

            Expression::Fork(loc, captures, channel, annotation, (), process) => {
                let target_dual = target_type.dual();
                let typ = match annotation {
                    Some(annotated_type) => {
                        annotated_type.check_subtype(loc, &target_dual)?;
                        annotated_type.clone()
                    }
                    None => target_dual,
                };
                let mut context = self.split();
                self.capture(inference_subject, captures, &mut context)?;
                context.put(loc, channel.clone(), typ.clone())?;
                let process = context.check_process(process)?;
                Ok(Arc::new(Expression::Fork(
                    loc.clone(),
                    captures.clone(),
                    channel.clone(),
                    annotation.clone(),
                    typ,
                    process,
                )))
            }
        }
    }

    pub fn infer_expression(
        &mut self,
        inference_subject: Option<&Name>,
        expression: &Expression<Loc, Name, ()>,
    ) -> Result<(Arc<Expression<Loc, Name, Type<Loc, Name>>>, Type<Loc, Name>), TypeError<Loc, Name>>
    {
        match expression {
            Expression::Reference(loc, name, ()) => {
                if Some(name) == inference_subject {
                    return Err(TypeError::TypeMustBeKnownAtThisPoint(
                        loc.clone(),
                        name.clone(),
                    ));
                }
                let typ = self.get(loc, name)?;
                Ok((
                    Arc::new(Expression::Reference(
                        loc.clone(),
                        name.clone(),
                        typ.clone(),
                    )),
                    typ.clone(),
                ))
            }

            Expression::Fork(loc, captures, channel, annotation, (), process) => {
                let mut context = self.split();
                self.capture(inference_subject, captures, &mut context)?;
                let (process, typ) = match annotation {
                    Some(typ) => {
                        context.put(loc, channel.clone(), typ.clone())?;
                        (context.check_process(process)?, typ.clone())
                    }
                    None => context.infer_process(process, channel)?,
                };
                let dual = typ.dual();
                Ok((
                    Arc::new(Expression::Fork(
                        loc.clone(),
                        captures.clone(),
                        channel.clone(),
                        annotation.clone(),
                        typ,
                        process,
                    )),
                    dual,
                ))
            }
        }
    }

    pub fn cannot_have_obligations(&mut self, loc: &Loc) -> Result<(), TypeError<Loc, Name>> {
        if self.obligations().any(|_| true) {
            return Err(TypeError::UnfulfilledObligations(
                loc.clone(),
                self.obligations().cloned().collect(),
            ));
        }
        Ok(())
    }
}

impl<Loc, Name: Display> Type<Loc, Name> {
    pub fn pretty(&self, f: &mut impl Write, indent: usize) -> fmt::Result {
        match self {
            Type::Send(_, arg, then) => {
                write!(f, "(")?;
                arg.pretty(f, indent)?;
                write!(f, ") ")?;
                then.pretty(f, indent)
            }

            Type::Receive(_, param, then) => {
                write!(f, "[")?;
                param.pretty(f, indent)?;
                write!(f, "] ")?;
                then.pretty(f, indent)
            }

            Type::Either(_, branches) => {
                write!(f, "either {{")?;
                for (branch, typ) in branches {
                    indentation(f, indent + 1)?;
                    write!(f, ".{} ", branch)?;
                    typ.pretty(f, indent + 1)?;
                }
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Type::Choice(_, branches) => {
                write!(f, "{{")?;
                for (branch, typ) in branches {
                    indentation(f, indent + 1)?;
                    write!(f, "{} => ", branch)?;
                    typ.pretty(f, indent + 1)?;
                }
                indentation(f, indent)?;
                write!(f, "}}")
            }

            Type::Break(_) => write!(f, "!"),
            Type::Continue(_) => write!(f, "?"),

            Type::Recursive(_, label, body) => {
                write!(f, "recursive ")?;
                if let Some(label) = label {
                    write!(f, "@{} ", label)?;
                }
                body.pretty(f, indent)
            }

            Type::Iterative(_, label, body) => {
                write!(f, "iterative ")?;
                if let Some(label) = label {
                    write!(f, "@{} ", label)?;
                }
                body.pretty(f, indent)
            }

            Type::Self_(_, label) => {
                write!(f, "self")?;
                if let Some(label) = label {
                    write!(f, " @{}", label)?;
                }
                Ok(())
            }

            Type::Loop(_, label) => {
                write!(f, "loop")?;
                if let Some(label) = label {
                    write!(f, " @{}", label)?;
                }
                Ok(())
            }
        }
    }
}

fn indentation(f: &mut impl Write, indent: usize) -> fmt::Result {
    write!(f, "\n")?;
    for _ in 0..indent {
        write!(f, "  ")?;
    }
    Ok(())
}
