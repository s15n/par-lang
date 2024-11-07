use std::{cmp::Ordering, collections::BTreeSet, sync::Arc};

use pest::{error::LineColLocation, iterators::Pair, Parser};
use pest_derive::Parser;

use crate::base::{Capture, Command, Context, Expression, Process};

#[derive(Clone, Debug)]
pub struct Name {
    pub string: String,
    pub location: Location,
}

impl Name {
    pub fn of<S: AsRef<str>>(s: S) -> Arc<Self> {
        Arc::new(Self {
            string: String::from(s.as_ref()),
            location: Location { line: 0, column: 0 },
        })
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub message: String,
    pub location: Location,
}

#[derive(Clone, Debug)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Parser)]
#[grammar = "par.pest"]
pub struct Par;

pub fn parse_program<X>(source: &str) -> Result<Context<Arc<Name>, X>, ParseError> {
    let mut context = Context::new();
    for pair in Par::parse(Rule::program, source)?
        .next()
        .unwrap()
        .into_inner()
    {
        if pair.as_rule() == Rule::EOI {
            continue;
        }
        let mut pairs = pair.into_inner();

        let (p1, p2) = (pairs.next().unwrap(), pairs.next().unwrap());
        let expr = parse_expression(p2, &mut BTreeSet::new())?;
        let name = parse_name(p1)?;

        if let Some(_) = context.statics.insert(name.clone(), expr) {
            return Err(ParseError {
                message: format!("\"{}\" is already defined", name.string),
                location: name.location.clone(),
            });
        }
    }
    Ok(context)
}

fn parse_name(pair: Pair<'_, Rule>) -> Result<Arc<Name>, ParseError> {
    Ok(Arc::new(pair.into()))
}

fn parse_expression(
    pair: Pair<'_, Rule>,
    free: &mut BTreeSet<Arc<Name>>,
) -> Result<Arc<Expression<Arc<Name>>>, ParseError> {
    let pair = pair.into_inner().next().unwrap();

    match pair.as_rule() {
        Rule::fork => {
            let mut pairs = pair.into_inner();

            let (p1, p2) = (pairs.next().unwrap(), pairs.next().unwrap());
            let process = parse_process(p2, free)?;
            let object = parse_name(p1)?;
            free.remove(&object);

            Ok(Arc::new(Expression::Fork(
                Capture {
                    variables: free.clone(),
                },
                object,
                process,
            )))
        }
        Rule::reference => {
            let name = Arc::new(pair.into());
            free.remove(&name);
            free.replace(name.clone());
            Ok(Arc::new(Expression::Ref(name)))
        }
        _ => unreachable!(),
    }
}

fn parse_process(
    pair: Pair<'_, Rule>,
    free: &mut BTreeSet<Arc<Name>>,
) -> Result<Arc<Process<Arc<Name>>>, ParseError> {
    let pair = pair.into_inner().next().unwrap();
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    match rule {
        Rule::p_let => {
            let (p1, p2, p3) = (
                pairs.next().unwrap(),
                pairs.next().unwrap(),
                pairs.next().unwrap(),
            );
            let proc = parse_process(p3, free)?;
            let mut expr_free = BTreeSet::new();
            let expr = parse_expression(p2, &mut expr_free)?;
            let name = parse_name(p1)?;
            free.remove(&name);
            free.extend(expr_free);
            Ok(Arc::new(Process::Let(name, expr, proc)))
        }
        Rule::p_link => {
            let (p1, p2) = (pairs.next().unwrap(), pairs.next().unwrap());
            let argument = parse_expression(p2, free)?;
            let subject = parse_name(p1)?;
            free.replace(subject.clone());
            Ok(Arc::new(Process::Link(subject, argument)))
        }
        Rule::p_break => {
            let subject = parse_name(pairs.next().unwrap())?;
            free.replace(subject.clone());
            Ok(Arc::new(Process::Do(subject, Command::Break)))
        }
        Rule::p_continue => {
            let (p1, p2) = (pairs.next().unwrap(), pairs.next().unwrap());
            let then = parse_process(p2, free)?;
            let subject = parse_name(p1)?;
            free.replace(subject.clone());
            Ok(Arc::new(Process::Do(subject, Command::Continue(then))))
        }
        Rule::p_send => {
            let (p1, p2, p3) = (
                pairs.next().unwrap(),
                pairs.next().unwrap(),
                pairs.next().unwrap(),
            );
            let then = parse_process(p3, free)?;
            let mut arg_free = BTreeSet::new();
            let argument = parse_expression(p2, &mut arg_free)?;
            let subject = parse_name(p1)?;
            free.replace(subject.clone());
            free.extend(arg_free);
            Ok(Arc::new(Process::Do(
                subject,
                Command::Send(argument, then),
            )))
        }
        Rule::p_receive => {
            let (p1, p2, p3) = (
                pairs.next().unwrap(),
                pairs.next().unwrap(),
                pairs.next().unwrap(),
            );
            let then = parse_process(p3, free)?;
            let parameter = parse_name(p2)?;
            let subject = parse_name(p1)?;
            free.remove(&parameter);
            free.replace(subject.clone());
            Ok(Arc::new(Process::Do(
                subject,
                Command::Receive(parameter, then),
            )))
        }
        Rule::p_select => {
            let (p1, p2, p3) = (
                pairs.next().unwrap(),
                pairs.next().unwrap(),
                pairs.next().unwrap(),
            );
            let then = parse_process(p3, free)?;
            let branch = parse_name(p2)?;
            let subject = parse_name(p1)?;
            free.replace(subject.clone());
            Ok(Arc::new(Process::Do(
                subject,
                Command::Select(branch, then),
            )))
        }
        Rule::p_case => {
            let subject = parse_name(pairs.next().unwrap())?;
            let pair = pairs.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::p_branches);

            let mut branches = Vec::new();
            let mut free_in_branches = Vec::new();

            for mut pairs in pair.into_inner().map(Pair::into_inner) {
                let (p1, p2) = (pairs.next().unwrap(), pairs.next().unwrap());

                let mut branch_free = free.clone();
                let process = parse_process(p2, &mut branch_free)?;
                free_in_branches.push(branch_free);

                let branch = parse_name(p1)?;
                branches.push((branch, process));
            }

            let otherwise = match pairs.next() {
                Some(pair) => {
                    let mut other_free = free.clone();
                    let process = parse_process(pair, &mut other_free)?;
                    free_in_branches.push(other_free);
                    Some(process)
                }
                None => None,
            };

            free.extend(free_in_branches.into_iter().flatten());
            free.replace(subject.clone());

            Ok(Arc::new(Process::Do(
                subject,
                Command::Case(branches, otherwise),
            )))
        }
        _ => unreachable!(),
    }
}

impl From<Pair<'_, Rule>> for Location {
    fn from(value: Pair<'_, Rule>) -> Self {
        let (line, column) = value.line_col();
        Self { line, column }
    }
}

impl From<Pair<'_, Rule>> for Name {
    fn from(value: Pair<'_, Rule>) -> Self {
        Self {
            string: value.as_str().to_string(),
            location: value.into(),
        }
    }
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(value: pest::error::Error<Rule>) -> Self {
        Self {
            message: value.to_string(),
            location: match value.line_col {
                LineColLocation::Pos((line, column)) => Location { line, column },
                LineColLocation::Span((line, column), _) => Location { line, column },
            },
        }
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl PartialOrd for Name {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Name {
    fn cmp(&self, other: &Self) -> Ordering {
        self.string.cmp(&other.string)
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Name {}
