use std::fmt::Display;

use indexmap::IndexMap;
use pest::{
    error::LineColLocation,
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

use crate::language::{
    Apply, ApplyBranch, ApplyBranches, Command, CommandBranch, CommandBranches, Construct,
    ConstructBranch, ConstructBranches, Expression, Process,
};

#[derive(Parser)]
#[grammar = "par.pest"]
pub struct Par;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Loc {
    Code { line: usize, column: usize },
    External,
}

impl Default for Loc {
    fn default() -> Self {
        Self::External
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Code { line, column } => write!(f, "{}:{}", line, column),
            Self::External => write!(f, "#:#"),
        }
    }
}

impl Loc {
    pub fn from(pair: &Pair<Rule>) -> Loc {
        let (line, column) = pair.line_col();
        Loc::Code { line, column }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Name {
    pub string: String,
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub location: Loc,
}

pub fn parse_program(source: &str) -> Result<IndexMap<Name, Expression<Loc, Name>>, ParseError> {
    let mut pairs = match Par::parse(Rule::program, source) {
        Ok(mut pairs) => pairs.next().unwrap().into_inner(),
        Err(error) => {
            return Err(ParseError {
                location: match error.line_col {
                    LineColLocation::Pos((line, column))
                    | LineColLocation::Span((line, column), _) => Loc::Code { line, column },
                },
            })
        }
    };

    let mut definitions = IndexMap::new();
    while let Some(pair) = pairs.next() {
        if pair.as_rule() == Rule::definition {
            let mut pairs = pair.into_inner();
            let (_, name) = parse_name(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            definitions.insert(name, expression);
        }
    }
    Ok(definitions)
}

fn parse_name(pairs: &mut Pairs<'_, Rule>) -> Result<(Loc, Name), ParseError> {
    let pair = pairs.next().unwrap();
    Ok((
        Loc::from(&pair),
        Name {
            string: String::from(pair.as_str()),
        },
    ))
}

fn parse_expression(pairs: &mut Pairs<'_, Rule>) -> Result<Expression<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::expr_let => {
            let mut pairs = pair.into_inner();
            let (_, name) = parse_name(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            let body = parse_expression(&mut pairs)?;
            Ok(Expression::Let(
                loc,
                name,
                Box::new(expression),
                Box::new(body),
            ))
        }

        Rule::expr_do => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            Ok(Expression::Do(loc, Box::new(process), Box::new(expression)))
        }

        Rule::expr_fork => {
            let mut pairs = pair.into_inner();
            let (_, name) = parse_name(&mut pairs)?;
            let process = parse_process(&mut pairs)?;
            Ok(Expression::Fork(loc, name, Box::new(process)))
        }

        Rule::construction => Ok(Expression::Construction(
            loc,
            parse_construct(&mut Pairs::single(pair))?,
        )),

        Rule::application => {
            let mut pairs = pair.into_inner();
            let (loc, object_name) = parse_name(&mut pairs)?;
            let apply = parse_apply(&mut pairs)?;
            Ok(Expression::Application(loc, object_name, apply))
        }

        _ => unreachable!(),
    }
}

fn parse_construct(pairs: &mut Pairs<'_, Rule>) -> Result<Construct<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::cons_then => {
            let mut pairs = Pairs::single(pair);
            let expression = parse_expression(&mut pairs)?;
            Ok(Construct::Then(loc, Box::new(expression)))
        }

        Rule::cons_send => {
            let mut pairs = pair.into_inner();
            let argument = parse_expression(&mut pairs)?;
            let construct = parse_construct(&mut pairs)?;
            Ok(Construct::Send(
                loc,
                Box::new(argument),
                Box::new(construct),
            ))
        }

        Rule::cons_receive => {
            let mut pairs = pair.into_inner();
            let (_, parameter) = parse_name(&mut pairs)?;
            let construct = parse_construct(&mut pairs)?;
            Ok(Construct::Receive(loc, parameter, Box::new(construct)))
        }

        Rule::cons_choose => {
            let mut pairs = pair.into_inner();
            let (_, chosen) = parse_name(&mut pairs)?;
            let construct = parse_construct(&mut pairs)?;
            Ok(Construct::Choose(loc, chosen, Box::new(construct)))
        }

        Rule::cons_either => {
            let mut pairs = pair.into_inner();
            let mut branches = IndexMap::new();
            while let Some(pair) = pairs.next() {
                let (_, name) = parse_name(&mut Pairs::single(pair))?;
                let branch = parse_construct_branch(&mut pairs)?;
                branches.insert(name, branch);
            }
            Ok(Construct::Either(loc, ConstructBranches(branches)))
        }

        Rule::cons_break => Ok(Construct::Break(loc)),

        Rule::cons_begin => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            let construct = parse_construct(&mut pairs)?;
            Ok(Construct::Begin(loc, label, Box::new(construct)))
        }

        Rule::cons_loop => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            Ok(Construct::Loop(loc, label))
        }

        _ => unreachable!(),
    }
}

fn parse_construct_branch(
    pairs: &mut Pairs<'_, Rule>,
) -> Result<ConstructBranch<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::cons_branch_then => {
            let mut pairs = pair.into_inner();
            let expression = parse_expression(&mut pairs)?;
            Ok(ConstructBranch::Then(loc, expression))
        }

        Rule::cons_branch_receive => {
            let mut pairs = pair.into_inner();
            let (_, parameter) = parse_name(&mut pairs)?;
            let branch = parse_construct_branch(&mut pairs)?;
            Ok(ConstructBranch::Receive(loc, parameter, Box::new(branch)))
        }

        _ => unreachable!(),
    }
}

fn parse_apply(pairs: &mut Pairs<'_, Rule>) -> Result<Apply<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::apply_noop => Ok(Apply::Noop(loc)),

        Rule::apply_send => {
            let mut pairs = pair.into_inner();
            let argument = parse_expression(&mut pairs)?;
            let then = parse_apply(&mut pairs)?;
            Ok(Apply::Send(loc, Box::new(argument), Box::new(then)))
        }

        Rule::apply_choose => {
            let mut pairs = pair.into_inner();
            let (_, chosen) = parse_name(&mut pairs)?;
            let then = parse_apply(&mut pairs)?;
            Ok(Apply::Choose(loc, chosen, Box::new(then)))
        }

        Rule::apply_either => {
            let mut pairs = pair.into_inner();
            let mut branches = IndexMap::new();
            while let Some(pair) = pairs.next() {
                let (_, name) = parse_name(&mut Pairs::single(pair))?;
                let branch = parse_apply_branch(&mut pairs)?;
                branches.insert(name, branch);
            }
            Ok(Apply::Either(loc, ApplyBranches(branches)))
        }

        Rule::apply_begin => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            let then = parse_apply(&mut pairs)?;
            Ok(Apply::Begin(loc, label, Box::new(then)))
        }

        Rule::apply_loop => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            Ok(Apply::Loop(loc, label))
        }

        _ => unreachable!(),
    }
}

fn parse_apply_branch(pairs: &mut Pairs<'_, Rule>) -> Result<ApplyBranch<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::apply_branch_then => {
            let mut pairs = pair.into_inner();
            let (_, name) = parse_name(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            Ok(ApplyBranch::Then(loc, name, expression))
        }

        Rule::apply_branch_receive => {
            let mut pairs = pair.into_inner();
            let (_, parameter) = parse_name(&mut pairs)?;
            let branch = parse_apply_branch(&mut pairs)?;
            Ok(ApplyBranch::Receive(loc, parameter, Box::new(branch)))
        }

        Rule::apply_branch_continue => {
            let mut pairs = pair.into_inner();
            let expression = parse_expression(&mut pairs)?;
            Ok(ApplyBranch::Continue(loc, expression))
        }

        _ => unreachable!(),
    }
}

fn parse_process(pairs: &mut Pairs<'_, Rule>) -> Result<Process<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::proc_let => {
            let mut pairs = pair.into_inner();
            let (_, name) = parse_name(&mut pairs)?;
            let expression = parse_expression(&mut pairs)?;
            let process = parse_process(&mut pairs)?;
            Ok(Process::Let(
                loc,
                name,
                Box::new(expression),
                Box::new(process),
            ))
        }

        Rule::proc_pass => Ok(Process::Pass(loc)),

        Rule::command => {
            let mut pairs = pair.into_inner();
            let (_, object_name) = parse_name(&mut pairs)?;
            let command = parse_command(&mut pairs)?;
            Ok(Process::Command(object_name, command))
        }

        Rule::proc_noop => Ok(Process::Noop(loc)),

        _ => unreachable!(),
    }
}

fn parse_command(pairs: &mut Pairs<'_, Rule>) -> Result<Command<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::cmd_then => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(Command::Then(Box::new(process)))
        }

        Rule::cmd_link => {
            let mut pairs = pair.into_inner();
            let expression = parse_expression(&mut pairs)?;
            Ok(Command::Link(loc, Box::new(expression)))
        }

        Rule::cmd_send => {
            let mut pairs = pair.into_inner();
            let argument = parse_expression(&mut pairs)?;
            let command = parse_command(&mut pairs)?;
            Ok(Command::Send(loc, Box::new(argument), Box::new(command)))
        }

        Rule::cmd_receive => {
            let mut pairs = pair.into_inner();
            let (_, parameter) = parse_name(&mut pairs)?;
            let command = parse_command(&mut pairs)?;
            Ok(Command::Receive(loc, parameter, Box::new(command)))
        }

        Rule::cmd_choose => {
            let mut pairs = pair.into_inner();
            let (_, chosen) = parse_name(&mut pairs)?;
            let command = parse_command(&mut pairs)?;
            Ok(Command::Choose(loc, chosen, Box::new(command)))
        }

        Rule::cmd_either => {
            let mut pairs = pair.into_inner();
            let branches = parse_command_branches(&mut pairs)?;
            let optional_process = match pairs.next() {
                Some(pair) => Some(parse_process(&mut Pairs::single(pair))?),
                None => None,
            };
            Ok(Command::Either(
                loc,
                branches,
                optional_process.map(Box::new),
            ))
        }

        Rule::cmd_break => Ok(Command::Break(loc)),

        Rule::cmd_continue => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(Command::Continue(loc, Box::new(process)))
        }

        Rule::cmd_begin => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            let command = parse_command(&mut pairs)?;
            Ok(Command::Begin(loc, label, Box::new(command)))
        }

        Rule::cmd_loop => {
            let mut pairs = pair.into_inner();
            let label = parse_loop_label(&mut pairs)?;
            Ok(Command::Loop(loc, label))
        }

        _ => unreachable!(),
    }
}

fn parse_command_branches(
    pairs: &mut Pairs<'_, Rule>,
) -> Result<CommandBranches<Loc, Name>, ParseError> {
    let mut pairs = pairs.next().unwrap().into_inner();

    let mut branches = IndexMap::new();
    while let Some(pair) = pairs.next() {
        let (_, name) = parse_name(&mut Pairs::single(pair))?;
        let branch = parse_command_branch(&mut pairs)?;
        branches.insert(name, branch);
    }
    Ok(CommandBranches(branches))
}

fn parse_command_branch(
    pairs: &mut Pairs<'_, Rule>,
) -> Result<CommandBranch<Loc, Name>, ParseError> {
    let pair = pairs.next().unwrap().into_inner().next().unwrap();
    let loc = Loc::from(&pair);

    match pair.as_rule() {
        Rule::cmd_branch_then => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(CommandBranch::Then(process))
        }

        Rule::cmd_branch_receive => {
            let mut pairs = pair.into_inner();
            let (_, parameter) = parse_name(&mut pairs)?;
            let branch = parse_command_branch(&mut pairs)?;
            Ok(CommandBranch::Receive(loc, parameter, Box::new(branch)))
        }

        Rule::cmd_branch_continue => {
            let mut pairs = pair.into_inner();
            let process = parse_process(&mut pairs)?;
            Ok(CommandBranch::Continue(loc, process))
        }

        _ => unreachable!(),
    }
}

fn parse_loop_label(pairs: &mut Pairs<Rule>) -> Result<Option<Name>, ParseError> {
    match pairs.next().unwrap().into_inner().next() {
        Some(pair) => Ok(Some(parse_name(&mut Pairs::single(pair))?.1)),
        None => Ok(None),
    }
}
