use std::{cell::RefCell, cmp::Ordering, sync::Arc};

use pest::{
    error::LineColLocation,
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

use crate::base::{Capture, Command, Context, Expression, Process};

#[derive(Clone, Debug)]
pub struct Name {
    pub string: String,
    pub location: Location,
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub message: String,

    #[allow(unused)]
    pub location: Location,
}

#[allow(unused)]
#[derive(Clone, Debug)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Parser)]
#[grammar = "par.pest"]
pub struct Par;

pub fn parse_program<X: std::fmt::Display>(
    source: &str,
) -> Result<Context<Arc<Name>, X>, ParseError> {
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
        let name = parse_name(&mut pairs)?;
        let expr = parse_expression(&mut pairs)?;
        if let Some(_) = context.statics.insert(name.clone(), expr) {
            return Err(ParseError {
                message: format!("\"{}\" is already defined", name.string),
                location: name.location.clone(),
            });
        }
    }
    context.fix_captures();
    Ok(context)
}

fn parse_name(pairs: &mut Pairs<'_, Rule>) -> Result<Arc<Name>, ParseError> {
    let pair = pairs.next().unwrap();
    Ok(Arc::new(pair.into()))
}

fn parse_expression(pairs: &mut Pairs<'_, Rule>) -> Result<Arc<Expression<Arc<Name>>>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::fork => {
            let mut pairs = pair.into_inner();
            let object = parse_name(&mut pairs)?;
            let process = parse_process(&mut pairs, None)?;
            Ok(Arc::new(Expression::Fork(
                RefCell::new(Capture::default()),
                object,
                process,
            )))
        }
        Rule::reference => {
            let name = Arc::new(pair.into());
            Ok(Arc::new(Expression::Ref(name)))
        }
        Rule::string => {
            let slice = pair.as_str();
            let literal = Arc::from(&slice[1..slice.len() - 1]);
            Ok(Arc::new(Expression::String(literal)))
        }
        _ => unreachable!(),
    }
}

fn parse_process(
    pairs: &mut Pairs<'_, Rule>,
    pass: Option<Arc<Process<Arc<Name>>>>,
) -> Result<Arc<Process<Arc<Name>>>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let span = pair.as_span();
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    match rule {
        Rule::p_let => {
            let name = parse_name(&mut pairs)?;
            let expr = parse_expression(&mut pairs)?;
            let proc = parse_process(&mut pairs, pass)?;
            Ok(Arc::new(Process::Let(name, expr, proc)))
        }
        Rule::p_link => {
            let subject = parse_name(&mut pairs)?;
            let argument = parse_expression(&mut pairs)?;
            Ok(Arc::new(Process::Link(subject, argument)))
        }
        Rule::p_break => {
            let subject = parse_name(&mut pairs)?;
            Ok(Arc::new(Process::Do(subject, Command::Break)))
        }
        Rule::p_continue => {
            let subject = parse_name(&mut pairs)?;
            let then = parse_process(&mut pairs, pass)?;
            Ok(Arc::new(Process::Do(subject, Command::Continue(then))))
        }
        Rule::p_send => {
            let subject = parse_name(&mut pairs)?;
            let argument = parse_expression(&mut pairs)?;
            let then = parse_process(&mut pairs, pass)?;
            Ok(Arc::new(Process::Do(
                subject,
                Command::Send(argument, then),
            )))
        }
        Rule::p_receive => {
            let subject = parse_name(&mut pairs)?;
            let parameter = parse_name(&mut pairs)?;
            let then = parse_process(&mut pairs, pass)?;
            Ok(Arc::new(Process::Do(
                subject,
                Command::Receive(parameter, then),
            )))
        }
        Rule::p_select => {
            let subject = parse_name(&mut pairs)?;
            let branch = parse_name(&mut pairs)?;
            let then = parse_process(&mut pairs, pass)?;
            Ok(Arc::new(Process::Do(
                subject,
                Command::Select(branch, then),
            )))
        }
        Rule::p_case => {
            let subject = parse_name(&mut pairs)?;
            let pair = pairs.next().unwrap();
            assert_eq!(pair.as_rule(), Rule::p_branches);

            let mut pass = pass;
            if let Some(pair) = pairs.next() {
                pass = Some(parse_process(&mut Pairs::single(pair), pass)?);
            }

            let mut branches = Vec::new();
            for mut pairs in pair.into_inner().map(Pair::into_inner) {
                let branch = parse_name(&mut pairs)?;
                let process = parse_process(&mut pairs, pass.clone())?;
                branches.push((branch, process));
            }
            Ok(Arc::new(Process::Do(subject, Command::Case(branches))))
        }
        Rule::p_pass => match pass {
            Some(process) => Ok(process),
            None => Err(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::ParsingError {
                    positives: vec![Rule::process],
                    negatives: vec![],
                },
                span,
            ))?,
        },

        Rule::sugar_oneof => {
            use crate::notation::*;

            let subject = parse_name(&mut pairs)?;
            let branches = pairs
                .next()
                .unwrap()
                .into_inner()
                .map(|pair| parse_name(&mut Pairs::single(pair)))
                .collect::<Result<Vec<_>, _>>()?;
            let then = parse_process(&mut pairs, pass)?;

            // subject.oneof(yes no);
            // -->
            // let subject = result {
            //   subject.case {
            //     yes => { result.yes; subject[]; result() }
            //     no  => { result.no;  subject[]; result() }
            //   }
            // }

            let result = Arc::new(Name {
                string: "result".to_string(),
                location: subject.location.clone(),
            });

            Ok(Arc::new(let_(
                subject.clone(),
                fork_(
                    result.clone(),
                    case_(
                        subject.clone(),
                        branches.into_iter().map(|branch| {
                            (
                                branch.clone(),
                                select_(
                                    result.clone(),
                                    branch,
                                    continue_(subject.clone(), break_(result.clone())),
                                ),
                            )
                        }),
                    ),
                ),
                (*then).clone(),
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
