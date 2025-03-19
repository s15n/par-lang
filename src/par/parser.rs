use super::{
    language::{
        Apply, ApplyBranch, ApplyBranches, Command, CommandBranch, CommandBranches, Construct,
        ConstructBranch, ConstructBranches, Expression, Pattern, Process,
    },
    lexer::{Input, Token, TokenKind},
    parse::{Loc, Name, Program},
    types::Type,
};
use indexmap::IndexMap;
use winnow::{
    combinator::{
        alt, cut_err, delimited, empty, not, opt, peek, preceded, repeat, separated, terminated,
        trace,
    },
    error::{
        AddContext, ContextError, ErrMode, ModalError, ParserError, StrContext, StrContextValue,
    },
    stream::{Accumulate, Compare, Stream, StreamIsPartial},
    token::any,
    Parser,
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct MyError<C = StrContext> {
    context: Vec<(usize, ContextError<C>)>,
}
impl<C> MyError<C> {
    fn eof_offset(&self) -> usize {
        self.context.iter().map(|x| x.0).min().unwrap_or(usize::MAX)
    }
}
pub type Error_ = MyError;
pub type Error = ErrMode<Error_>;
impl<I: Stream, C: core::fmt::Debug> ParserError<I> for MyError<C> {
    type Inner = Self;

    fn from_input(input: &I) -> Self {
        Self {
            context: vec![(input.eof_offset(), ContextError::from_input(input))],
        }
    }
    fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
        Ok(self)
    }
    fn append(self, _input: &I, _token_start: &<I as Stream>::Checkpoint) -> Self {
        // // TODO try merging based on length
        // (_input.eof_offset() < self.eof_offset());
        self
    }
    fn or(mut self, other: Self) -> Self {
        // dbg!(&self.context, &other.context);
        self.context.extend(other.context);
        self
    }
}
impl<I: Stream, C> AddContext<I, C> for MyError<C> {
    fn add_context(
        mut self,
        input: &I,
        token_start: &<I as Stream>::Checkpoint,
        context: C,
    ) -> Self {
        let new_context = |context| {
            (
                input.eof_offset(),
                ContextError::new().add_context(input, token_start, context),
            )
        };
        if self.context.is_empty() {
            self.context.push(new_context(context));
            return self;
        }
        let last = self.context.pop().unwrap();
        if last.0 != input.eof_offset() {
            self.context.push(new_context(context));
            return self;
        }
        let last = (
            last.0.min(input.eof_offset()),
            last.1.add_context(input, token_start, context),
        );
        self.context.push(last);
        self
    }
}

pub type Result<O, E = MyError> = core::result::Result<O, ErrMode<E>>;

/// Token with additional context of expecting the `token` value
pub fn t<'i, I, E>(token: &'static str) -> impl Parser<I, I::Slice, E>
where
    I: Stream + StreamIsPartial + for<'s> Compare<&'s str>,
    E: AddContext<I, StrContext> + ParserError<I>,
{
    move |input: &mut I| {
        token
            .context(StrContext::Expected(StrContextValue::StringLiteral(token)))
            .parse_next(input)
    }
}

/// Like regular `preceded` but cuts if `parser` after `ignored` fails, assuming that it should be unambiguous.
pub fn commit_after<Input, Ignored, Output, Error, IgnoredParser, ParseNext>(
    mut ignored: IgnoredParser,
    parser: ParseNext,
) -> impl Parser<Input, Output, Error>
where
    Input: Stream,
    Error: ParserError<Input> + ModalError,
    IgnoredParser: Parser<Input, Ignored, Error>,
    ParseNext: Parser<Input, Output, Error>,
{
    let mut parser = cut_err(parser);
    trace("commit_after", move |input: &mut Input| {
        let _ = ignored.parse_next(input)?;
        parser.parse_next(input)
    })
}

pub fn comment<I, E>() -> impl Parser<I, I::Slice, E>
where
    I: Stream + StreamIsPartial + for<'s> Compare<&'s str>,
    E: ParserError<I>,
{
    // TODO can we add /* */ as accepted syntax?
    // delimited(("/*"), parser, ("*/"))
    preceded("//", repeat(0.., (not("\n"), any)).map(|()| ())).take()
    // .context(StrContext::Label("comment"))
}

fn keyword<I>() -> impl Parser<I, I::Slice, Error>
where
    I: Stream + StreamIsPartial + for<'s> Compare<&'s str>,
{
    alt((
        "type",
        "dec",
        "def",
        "chan",
        "let",
        "do",
        "in",
        "pass",
        "begin",
        "loop",
        "telltypes",
        "either",
        "recursive",
        "iterative",
        "self",
    ))
    .context(StrContext::Label("keyword"))
}

pub fn with_loc<'a, O, E>(
    mut parser: impl Parser<Input<'a>, O, E>,
) -> impl Parser<Input<'a>, (O, Loc), E>
where
    E: ParserError<Input<'a>> + ModalError,
{
    move |input: &mut Input<'a>| -> core::result::Result<(O, Loc), E> {
        let tok: &Token<'_> = peek(any).parse_next(input)?;
        let out = parser.parse_next(input)?;
        Ok((out, tok.loc.clone()))
    }
}
pub fn with_span<'a, O, E>(
    mut parser: impl Parser<Input<'a>, O, E>,
) -> impl Parser<Input<'a>, (O, core::ops::Range<usize>), E>
where
    E: ParserError<Input<'a>>,
{
    move |input: &mut Input<'a>| -> core::result::Result<(O, core::ops::Range<usize>), E> {
        let last = input.last().cloned();
        let start = peek(any).parse_next(input)?.span.start;
        let out = parser.parse_next(input)?;
        let end = peek::<_, &Token, E, _>(any)
            .parse_next(input)
            .unwrap_or(&last.unwrap()) // if input now empty, use that last token.
            .span
            .end;
        Ok((out, start..end))
    }
}

pub fn name<'s>(input: &mut Input<'s>) -> Result<Name> {
    preceded(not(keyword()), TokenKind::Ident.parse_to::<Name>())
        .context(StrContext::Expected(StrContextValue::CharLiteral('_')))
        .context(StrContext::Expected(StrContextValue::Description(
            "alphanumeric",
        )))
        .context(StrContext::Expected(StrContextValue::Description(
            "non-keyword",
        )))
        .context(StrContext::Label("name"))
        .parse_next(input)
}

pub fn program(
    input: Input,
) -> std::result::Result<
    Program<Name, Expression<Loc, Name>>,
    winnow::error::ParseError<Input, Error_>,
> {
    enum Either<A, B, C> {
        A(A),
        B(B),
        C(C),
    }

    repeat(
        0..,
        alt((
            type_def.map(Either::A),
            declaration.map(Either::B),
            definition.map(Either::C),
        ))
        .context(StrContext::Label("item")),
    )
    .fold(Program::default, |mut acc, item| {
        match item {
            Either::A((name, (params, typ))) => {
                acc.type_defs.insert(name, (params, typ));
            }
            Either::B((name, typ)) => {
                acc.declarations.insert(name, Some(typ));
            }
            Either::C((name, typ, expression)) => {
                if let Some(typ) = typ {
                    acc.declarations.insert(name.clone(), Some(typ));
                }
                acc.declarations.entry(name.clone()).or_insert(None);
                acc.definitions.insert(name, expression);
            }
        };
        acc
    })
    .parse(input)
}

pub fn type_def(input: &mut Input) -> Result<(Name, (Vec<Name>, Type<Loc, Name>))> {
    commit_after(t("type"), (name, type_params, t("="), typ))
        .map(|(name, type_params, _, typ)| (name, (type_params, typ)))
        .context(StrContext::Label("type definition"))
        .parse_next(input)
}

pub fn declaration(input: &mut Input) -> Result<(Name, Type<Loc, Name>)> {
    commit_after(t("dec"), (name, t(":"), typ))
        .map(|(name, _, typ)| (name, typ))
        .context(StrContext::Label("declaration"))
        .parse_next(input)
}

pub fn definition(
    input: &mut Input,
) -> Result<(Name, Option<Type<Loc, Name>>, Expression<Loc, Name>)> {
    commit_after(t("def"), (name, annotation, t("="), expression))
        .map(|(name, annotation, _, expression)| (name, annotation, expression))
        .context(StrContext::Label("definition"))
        .parse_next(input)
}

fn list<P, I, O>(item: P) -> impl Parser<I, Vec<O>, Error> + use<P, I, O>
where
    P: Parser<I, O, Error>,
    I: Stream + StreamIsPartial + for<'a> Compare<&'a str>,
    Vec<O>: Accumulate<O>,
{
    terminated(separated(1.., item, t(",")), opt(t(",")))
}

fn typ(input: &mut Input) -> Result<Type<Loc, Name>> {
    // TODO, use `dispatch` to choose alternate based on peek prefix.
    // This should also help error messages.
    // let s = dispatch! {peek(take::<usize, Input, Error>(2usize));
    // "<>" => "<>"
    // };
    alt((
        typ_name,
        typ_chan,
        typ_either,
        typ_choice,
        typ_break,
        typ_continue,
        typ_recursive,
        typ_iterative,
        typ_self,
        typ_send_type,
        typ_send, // try after send_type so matching `(` is unambiguous
        typ_recv_type,
        typ_receive, // try after recv_type so matching `[` is unambiguous
    ))
    .context(StrContext::Label("type"))
    .parse_next(input)
}

fn typ_name(input: &mut Input) -> Result<Type<Loc, Name>> {
    trace(
        "typ_name",
        with_loc((name, type_args)).map(|((name, typ_args), loc)| Type::Name(loc, name, typ_args)),
    )
    .parse_next(input)
}

fn typ_chan(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(
        t("chan"),
        typ.context(StrContext::Label("chan type")),
    ))
    .map(|(typ, loc)| Type::Chan(Loc::from(loc), Box::new(typ)))
    .parse_next(input)
}

fn typ_send(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(t("("), (terminated(list(typ), t(")")), typ)))
        .map(|((args, then), span)| {
            args.into_iter().rev().fold(then, |then, arg| {
                Type::Send(Loc::from(span.clone()), Box::new(arg), Box::new(then))
            })
        })
        .parse_next(input)
}

fn typ_receive(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(t("["), (terminated(list(typ), t("]")), typ)))
        .map(|((args, then), span)| {
            args.into_iter().rev().fold(then, |then, arg| {
                Type::Receive(Loc::from(span.clone()), Box::new(arg), Box::new(then))
            })
        })
        .parse_next(input)
}

fn typ_either(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(
        t("either"),
        (
            t("{"),
            repeat(0.., (t("."), name, typ, opt(t(",")))).fold(
                || IndexMap::new(),
                |mut branches, (_, name, typ, _)| {
                    branches.insert(name, typ);
                    branches
                },
            ),
            t("}"),
        ),
    ))
    .map(|((_, branches, _), span)| Type::Either(Loc::from(span), branches))
    .parse_next(input)
}

fn typ_choice(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(
        t("{"),
        terminated(
            repeat(0.., ((t(".")), name, typ_branch, opt(t(",")))).fold(
                || IndexMap::new(),
                |mut branches, (_, name, typ, _)| {
                    branches.insert(name, typ);
                    branches
                },
            ),
            t("}"),
        ),
    ))
    .map(|(branches, span)| Type::Choice(Loc::from(span), branches))
    .parse_next(input)
}

fn typ_break(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(t("!"))
        .map(|(_, span)| Type::Break(Loc::from(span)))
        .parse_next(input)
}

fn typ_continue(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(t("?"))
        .map(|(_, span)| Type::Continue(Loc::from(span)))
        .parse_next(input)
}

fn typ_recursive(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(t("recursive"), (loop_label, typ)))
        .map(|((label, typ), loc)| Type::Recursive(Loc::from(loc), label, Box::new(typ)))
        .parse_next(input)
}

fn typ_iterative<'s>(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(
        t("iterative"),
        (loop_label, typ).context(StrContext::Label("iterative type body")),
    ))
    .map(|((name, typ), span)| Type::Iterative(Loc::from(span), name, Box::new(typ)))
    .parse_next(input)
}

fn typ_self<'s>(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(
        t("self"),
        loop_label.context(StrContext::Label("self type loop label")),
    ))
    .map(|(label, span)| Type::Self_(Loc::from(span), label))
    .parse_next(input)
}

fn typ_send_type<'s>(input: &mut Input) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(
        (t("("), t("type")),
        (
            list(name).context(StrContext::Label("list of type names to send")),
            t(")"),
            typ,
        ),
    ))
    .map(|((names, _, typ), span)| {
        names.into_iter().rev().fold(typ, |body, name| {
            Type::SendType(Loc::from(span.clone()), name, Box::new(body))
        })
    })
    .parse_next(input)
}

fn typ_recv_type<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(
        (t("["), t("type")),
        (
            list(name).context(StrContext::Label("list of type names to receive")),
            t("]"),
            typ,
        ),
    ))
    .map(|((names, _, typ), span)| {
        names.into_iter().rev().fold(typ, |body, name| {
            Type::ReceiveType(Loc::from(span.clone()), name, Box::new(body))
        })
    })
    .parse_next(input)
}

fn type_params<'s>(input: &mut Input) -> Result<Vec<Name>> {
    opt(delimited(t("<"), list(name), t(">"))) // TODO should be able to use `<` to improve error message
        .map(Option::unwrap_or_default)
        .parse_next(input)
}

fn type_args<'s>(input: &mut Input) -> Result<Vec<Type<Loc, Name>>> {
    opt(delimited(t("<"), list(typ), t(">"))) // TODO should be able to use `<` to improve error message
        .map(Option::unwrap_or_default)
        .parse_next(input)
}

fn typ_branch<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    // try recv_type first so `(` is unambiguous on `typ_branch_received`
    alt((typ_branch_then, typ_branch_recv_type, typ_branch_receive)).parse_next(input)
}

fn typ_branch_then<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    commit_after(t("=>"), typ).parse_next(input)
}

fn typ_branch_receive<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    with_loc(commit_after(t("("), (list(typ), t(")"), typ_branch)))
        .map(|((args, _, then), span)| {
            args.into_iter().rev().fold(then, |acc, arg| {
                Type::Receive(Loc::from(span.clone()), Box::new(arg), Box::new(acc))
            })
        })
        .parse_next(input)
}

fn typ_branch_recv_type<'s>(input: &mut Input<'s>) -> Result<Type<Loc, Name>> {
    with_loc(preceded(
        (t("("), t("type")),
        cut_err((list(name), t(")"), typ_branch)),
    ))
    .map(|((names, _, body), span)| {
        names.into_iter().rev().fold(body, |acc, name| {
            Type::ReceiveType(Loc::from(span.clone()), name, Box::new(acc))
        })
    })
    .parse_next(input)
}

fn annotation(input: &mut Input) -> Result<Option<Type<Loc, Name>>> {
    opt(commit_after(t(":"), typ)).parse_next(input)
}

// pattern           = { pattern_name | pattern_receive | pattern_continue | pattern_recv_type }
fn pattern(input: &mut Input) -> Result<Pattern<Loc, Name>> {
    alt((
        pattern_name,
        pattern_receive_type,
        pattern_receive,
        pattern_continue,
    ))
    .parse_next(input)
}

fn pattern_name(input: &mut Input) -> Result<Pattern<Loc, Name>> {
    with_loc((name, annotation))
        .map(|((name, annotation), loc)| Pattern::Name(loc, name, annotation))
        .parse_next(input)
}

fn pattern_receive(input: &mut Input) -> Result<Pattern<Loc, Name>> {
    with_loc(commit_after(t("("), (list(pattern), t(")"), pattern)))
        .map(|((patterns, _, mut rest), loc)| {
            for pattern in patterns.into_iter().rev() {
                rest = Pattern::Receive(loc.clone(), Box::new(pattern), Box::new(rest));
            }
            rest
        })
        .parse_next(input)
}

fn pattern_continue(input: &mut Input) -> Result<Pattern<Loc, Name>> {
    with_loc(t("!"))
        .map(|(_, loc)| Pattern::Continue(loc))
        .parse_next(input)
}

fn pattern_receive_type(input: &mut Input) -> Result<Pattern<Loc, Name>> {
    with_loc(commit_after(
        (t("("), t("type")),
        (list(name), t(")"), pattern),
    ))
    .map(|((names, _, mut rest), loc)| {
        for name in names.into_iter().rev() {
            rest = Pattern::ReceiveType(loc.clone(), name, Box::new(rest));
        }
        rest
    })
    .parse_next(input)
}

fn expression(input: &mut Input) -> Result<Expression<Loc, Name>> {
    alt((
        expr_let,
        expr_do,
        expr_fork,
        application,
        with_loc(construction).map(|(cons, loc)| Expression::Construction(loc, cons)),
        delimited(t("{"), expression, t("}")),
    ))
    .context(StrContext::Label("expression"))
    .parse_next(input)
}

fn expr_let(input: &mut Input) -> Result<Expression<Loc, Name>> {
    with_loc(commit_after(
        t("let"),
        (pattern, t("="), expression, t("in"), expression),
    ))
    .map(|((pattern, _, expression, _, body), loc)| {
        Expression::Let(loc, pattern, Box::new(expression), Box::new(body))
    })
    .parse_next(input)
}

fn expr_do(input: &mut Input) -> Result<Expression<Loc, Name>> {
    with_loc(commit_after(
        t("do"),
        (t("{"), process, (t("}"), t("in")), expression),
    ))
    .map(|((_, process, _, expression), loc)| {
        Expression::Do(loc, Box::new(process), Box::new(expression))
    })
    .parse_next(input)
}

fn expr_fork(input: &mut Input) -> Result<Expression<Loc, Name>> {
    commit_after(
        t("chan"),
        (with_loc(name), annotation, t("{"), process, t("}")),
    )
    .map(|((name, loc), annotation, _, process, _)| {
        Expression::Fork(loc, name, annotation, Box::new(process))
    })
    .parse_next(input)
}

fn construction(input: &mut Input) -> Result<Construct<Loc, Name>> {
    alt((
        cons_begin,
        cons_loop,
        cons_then,
        cons_choose,
        cons_either,
        cons_break,
        cons_send_type,
        cons_send,
        cons_recv_type,
        cons_receive,
    ))
    .context(StrContext::Label("construction"))
    .parse_next(input)
}

fn cons_then(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(alt((
        expr_fork,
        expr_let,
        expr_do,
        application,
        delimited(t("{"), expression, t("}")),
    )))
    .map(|(expr, loc)| Construct::Then(loc, Box::new(expr)))
    .parse_next(input)
}

fn cons_send(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(commit_after(
        t("("),
        (list(expression), t(")"), construction),
    ))
    .map(|((arguments, _, mut construct), loc)| {
        for argument in arguments.into_iter().rev() {
            construct = Construct::Send(loc.clone(), Box::new(argument), Box::new(construct));
        }
        construct
    })
    .parse_next(input)
}

fn cons_receive(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(commit_after(t("["), (list(pattern), t("]"), construction)))
        .map(|((patterns, _, mut construct), loc)| {
            for pattern in patterns.into_iter().rev() {
                construct = Construct::Receive(loc.clone(), pattern, Box::new(construct));
            }
            construct
        })
        .parse_next(input)
}

fn cons_choose(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(commit_after(t("."), (name, construction)))
        .map(|((chosen, construct), loc)| Construct::Choose(loc, chosen, Box::new(construct)))
        .parse_next(input)
}

fn cons_either(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(commit_after(
        t("{"),
        (
            repeat(0.., (t("."), name, cons_branch, opt(t(",")))).fold(
                || IndexMap::new(),
                |mut branches, (_, name, branch, _)| {
                    branches.insert(name, branch);
                    branches
                },
            ),
            t("}"),
        ),
    ))
    .map(|((branches, _), loc)| Construct::Either(loc, ConstructBranches(branches)))
    .parse_next(input)
}

fn cons_break(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(t("!"))
        .map(|(_, loc)| Construct::Break(loc))
        .parse_next(input)
}

fn cons_begin(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(commit_after(t("begin"), (loop_label, construction)))
        .map(|((label, construct), loc)| (Construct::Begin(loc, label, Box::new(construct))))
        .parse_next(input)
}

fn cons_loop(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(commit_after(t("loop"), loop_label))
        .map(|(label, loc)| (Construct::Loop(loc, label)))
        .parse_next(input)
}

fn cons_send_type(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(commit_after(
        (t("("), t("type")),
        (list(typ), t(")"), construction),
    ))
    .map(|((names, _, mut construct), loc)| {
        for name in names.into_iter().rev() {
            construct = Construct::SendType(loc.clone(), name, Box::new(construct));
        }
        construct
    })
    .parse_next(input)
}

fn cons_recv_type(input: &mut Input) -> Result<Construct<Loc, Name>> {
    with_loc(commit_after(
        (t("["), t("type")),
        (list(name), t("]"), construction),
    ))
    .map(|((names, _, mut construct), loc)| {
        for name in names.into_iter().rev() {
            construct = Construct::ReceiveType(loc.clone(), name, Box::new(construct));
        }
        construct
    })
    .parse_next(input)
}

fn cons_branch(input: &mut Input) -> Result<ConstructBranch<Loc, Name>> {
    alt((cons_branch_then, cons_branch_recv_type, cons_branch_receive)).parse_next(input)
}

fn cons_branch_then(input: &mut Input) -> Result<ConstructBranch<Loc, Name>> {
    with_loc(commit_after(t("=>"), expression))
        .map(|(expression, loc)| ConstructBranch::Then(loc, expression))
        .parse_next(input)
}

fn cons_branch_receive(input: &mut Input) -> Result<ConstructBranch<Loc, Name>> {
    with_loc(commit_after(t("("), (list(pattern), t(")"), cons_branch)))
        .map(|((patterns, _, mut branch), loc)| {
            for pattern in patterns.into_iter().rev() {
                branch = ConstructBranch::Receive(loc.clone(), pattern, Box::new(branch));
            }
            branch
        })
        .parse_next(input)
}

fn cons_branch_recv_type(input: &mut Input) -> Result<ConstructBranch<Loc, Name>> {
    with_loc(commit_after(
        (t("("), t("type")),
        (list(name), t(")"), cons_branch),
    ))
    .map(|((names, _, mut branch), loc)| {
        for name in names.into_iter().rev() {
            branch = ConstructBranch::ReceiveType(loc.clone(), name, Box::new(branch));
        }
        branch
    })
    .parse_next(input)
}

fn application(input: &mut Input) -> Result<Expression<Loc, Name>> {
    with_loc((
        alt((
            with_loc(name).map(|(name, loc)| Expression::Reference(loc, name)),
            delimited(t("{"), expression, t("}")),
        )),
        apply,
    ))
    .map(|((expr, apply), loc)| Expression::Application(loc, Box::new(expr), apply))
    .context(StrContext::Label("application"))
    .parse_next(input)
}

fn apply(input: &mut Input) -> Result<Apply<Loc, Name>> {
    alt((
        apply_begin,
        apply_loop,
        apply_choose,
        apply_either,
        apply_send_type,
        apply_send,
        apply_noop,
    ))
    .parse_next(input)
}

fn apply_send(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(commit_after(t("("), (list(expression), t(")"), apply)))
        .map(|((arguments, _, mut apply), loc)| {
            for argument in arguments.into_iter().rev() {
                apply = Apply::Send(loc.clone(), Box::new(argument), Box::new(apply));
            }
            apply
        })
        .parse_next(input)
}

fn apply_choose(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(commit_after(t("."), (name, apply)))
        .map(|((chosen, then), loc)| Apply::Choose(loc, chosen, Box::new(then)))
        .parse_next(input)
}

fn apply_either(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(commit_after(
        t("{"),
        (
            repeat(0.., (t("."), name, apply_branch, opt(t(",")))).fold(
                || IndexMap::new(),
                |mut branches, (_, name, branch, _)| {
                    branches.insert(name, branch);
                    branches
                },
            ),
            t("}"),
        ),
    ))
    .map(|((branches, _), loc)| Apply::Either(loc, ApplyBranches(branches)))
    .parse_next(input)
}

fn apply_begin(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(commit_after(t("begin"), (loop_label, apply)))
        .map(|((label, then), loc)| Apply::Begin(loc, label, Box::new(then)))
        .parse_next(input)
}

fn apply_loop(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(commit_after(t("loop"), loop_label))
        .map(|(label, loc)| Apply::Loop(loc, label))
        .parse_next(input)
}

fn apply_send_type(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(commit_after(
        (t("("), t("type")),
        (list(typ), t(")"), apply),
    ))
    .map(|((types, _, mut apply), loc)| {
        for typ in types.into_iter().rev() {
            apply = Apply::SendType(loc.clone(), typ, Box::new(apply));
        }
        apply
    })
    .parse_next(input)
}

fn apply_noop(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(empty)
        .map(|((), loc)| Apply::Noop(loc))
        .parse_next(input)
}

fn apply_branch(input: &mut Input) -> Result<ApplyBranch<Loc, Name>> {
    alt((
        apply_branch_then,
        apply_branch_recv_type,
        apply_branch_receive,
        apply_branch_continue,
    ))
    .parse_next(input)
}

fn apply_branch_then(input: &mut Input) -> Result<ApplyBranch<Loc, Name>> {
    (with_loc(name), cut_err((t("=>"), expression)))
        .map(|((name, loc), (_, expression))| ApplyBranch::Then(loc, name, expression))
        .parse_next(input)
}

fn apply_branch_receive(input: &mut Input) -> Result<ApplyBranch<Loc, Name>> {
    with_loc(commit_after(t("("), (list(pattern), t(")"), apply_branch)))
        .map(|((patterns, _, mut branch), loc)| {
            for pattern in patterns.into_iter().rev() {
                branch = ApplyBranch::Receive(loc.clone(), pattern, Box::new(branch));
            }
            branch
        })
        .parse_next(input)
}

fn apply_branch_continue(input: &mut Input) -> Result<ApplyBranch<Loc, Name>> {
    with_loc(commit_after(t("!"), (t("=>"), expression)))
        .map(|((_, expression), loc)| ApplyBranch::Continue(loc, expression))
        .parse_next(input)
}

fn apply_branch_recv_type(input: &mut Input) -> Result<ApplyBranch<Loc, Name>> {
    with_loc(commit_after(
        (t("("), t("type")),
        (list(name), t(")"), apply_branch),
    ))
    .map(|((names, _, mut branch), loc)| {
        for name in names.into_iter().rev() {
            branch = ApplyBranch::ReceiveType(loc.clone(), name, Box::new(branch))
        }
        branch
    })
    .parse_next(input)
}

fn process(input: &mut Input) -> Result<Process<Loc, Name>> {
    alt((proc_let, proc_pass, proc_telltypes, command, proc_noop))
        .context(StrContext::Label("process"))
        .parse_next(input)
}

fn proc_let(input: &mut Input) -> Result<Process<Loc, Name>> {
    with_loc(commit_after(
        t("let"),
        (pattern, t("="), expression, process),
    ))
    .map(|((pattern, _, expression, process), loc)| {
        Process::Let(loc, pattern, Box::new(expression), Box::new(process))
    })
    .parse_next(input)
}

fn proc_pass(input: &mut Input) -> Result<Process<Loc, Name>> {
    with_loc(t("pass"))
        .map(|(_, loc)| Process::Pass(loc))
        .parse_next(input)
}

fn proc_telltypes(input: &mut Input) -> Result<Process<Loc, Name>> {
    with_loc(commit_after(t("telltypes"), process))
        .map(|(process, loc)| Process::Telltypes(loc, Box::new(process)))
        .parse_next(input)
}

fn proc_noop(input: &mut Input) -> Result<Process<Loc, Name>> {
    with_loc(empty)
        .map(|((), loc)| Process::Noop(loc))
        .parse_next(input)
}

fn command(input: &mut Input) -> Result<Process<Loc, Name>> {
    (name, cmd)
        .map(|(name, cmd)| Process::Command(name, cmd))
        .parse_next(input)
}

fn cmd(input: &mut Input) -> Result<Command<Loc, Name>> {
    alt((
        cmd_link,
        cmd_choose,
        cmd_either,
        cmd_break,
        cmd_continue,
        cmd_begin,
        cmd_loop,
        cmd_send_type,
        cmd_send,
        cmd_recv_type,
        cmd_receive,
        cmd_then,
    ))
    .context(StrContext::Label("command"))
    .parse_next(input)
}

fn cmd_then(input: &mut Input) -> Result<Command<Loc, Name>> {
    process
        .map(|x| Command::Then(Box::new(x)))
        .parse_next(input)
}

fn cmd_link(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(t("<>"), expression))
        .map(|(expression, loc)| Command::Link(loc, Box::new(expression)))
        .parse_next(input)
}

fn cmd_send(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(t("("), (list(expression), t(")"), cmd)))
        .map(|((expressions, _, mut cmd), loc)| {
            for expression in expressions.into_iter().rev() {
                cmd = Command::Send(loc.clone(), Box::new(expression), Box::new(cmd));
            }
            cmd
        })
        .parse_next(input)
}

fn cmd_receive(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(t("["), (list(pattern), t("]"), cmd)))
        .map(|((patterns, _, mut cmd), loc)| {
            for pattern in patterns.into_iter().rev() {
                cmd = Command::Receive(loc.clone(), pattern, Box::new(cmd));
            }
            cmd
        })
        .parse_next(input)
}

fn cmd_choose(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(t("."), (name, cmd)))
        .map(|((name, cmd), loc)| Command::Choose(loc, name, Box::new(cmd)))
        .parse_next(input)
}

fn cmd_either(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(
        t("{"),
        (cmd_branches, t("}"), opt(pass_process)),
    ))
    .map(|((branches, _, pass_process), loc)| {
        Command::Either(loc, branches, pass_process.map(Box::new))
    })
    .parse_next(input)
}

fn cmd_break(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(t("!"))
        .map(|(_, loc)| Command::Break(loc))
        .parse_next(input)
}

fn cmd_continue(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc((t("?"), process))
        .map(|((_, process), loc)| Command::Continue(loc, Box::new(process)))
        .parse_next(input)
}

fn cmd_begin(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(t("begin"), (loop_label, cmd)))
        .map(|((label, cmd), loc)| Command::Begin(loc, label, Box::new(cmd)))
        .parse_next(input)
}

fn cmd_loop(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(t("loop"), loop_label))
        .map(|(label, loc)| Command::Loop(loc, label))
        .parse_next(input)
}

fn cmd_send_type(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after((t("("), t("type")), (list(typ), t(")"), cmd)))
        .map(|((types, _, mut cmd), loc)| {
            for typ in types.into_iter().rev() {
                cmd = Command::SendType(loc.clone(), typ, Box::new(cmd));
            }
            cmd
        })
        .parse_next(input)
}

fn cmd_recv_type(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after((t("["), t("type")), (list(name), t("]"), cmd)))
        .map(|((names, _, mut cmd), loc)| {
            for name in names.into_iter().rev() {
                cmd = Command::ReceiveType(loc.clone(), name, Box::new(cmd));
            }
            cmd
        })
        .parse_next(input)
}

fn pass_process(input: &mut Input) -> Result<Process<Loc, Name>> {
    alt((proc_let, proc_pass, proc_telltypes, command)).parse_next(input)
}

fn cmd_branches(input: &mut Input) -> Result<CommandBranches<Loc, Name>> {
    repeat(0.., (t("."), name, cmd_branch))
        .fold(
            || IndexMap::new(),
            |mut branches, (_, name, branch)| {
                branches.insert(name, branch);
                branches
            },
        )
        .map(CommandBranches)
        .parse_next(input)
}

fn cmd_branch(input: &mut Input) -> Result<CommandBranch<Loc, Name>> {
    alt((
        cmd_branch_then,
        cmd_branch_continue,
        cmd_branch_recv_type,
        cmd_branch_receive,
    ))
    .parse_next(input)
}

fn cmd_branch_then(input: &mut Input) -> Result<CommandBranch<Loc, Name>> {
    commit_after(t("=>"), (t("{"), process, t("}")))
        .map(|(_, process, _)| CommandBranch::Then(process))
        .parse_next(input)
}

fn cmd_branch_receive(input: &mut Input) -> Result<CommandBranch<Loc, Name>> {
    with_loc(commit_after(t("("), (list(pattern), t(")"), cmd_branch)))
        .map(|((patterns, _, mut branch), loc)| {
            for pattern in patterns.into_iter().rev() {
                branch = CommandBranch::Receive(loc.clone(), pattern, Box::new(branch));
            }
            branch
        })
        .parse_next(input)
}

fn cmd_branch_continue(input: &mut Input) -> Result<CommandBranch<Loc, Name>> {
    with_loc(commit_after(t("!"), (t("=>"), t("{"), process, t("}"))))
        .map(|((_, _, process, _), loc)| CommandBranch::Continue(loc, process))
        .parse_next(input)
}

fn cmd_branch_recv_type(input: &mut Input) -> Result<CommandBranch<Loc, Name>> {
    with_loc(commit_after(
        (t("("), t("type")),
        (list(name), t(")"), cmd_branch),
    ))
    .map(|((names, _, mut branch), loc)| {
        for name in names.into_iter().rev() {
            branch = CommandBranch::ReceiveType(loc.clone(), name, Box::new(branch));
        }
        branch
    })
    .parse_next(input)
}

fn loop_label<'s>(input: &mut Input<'s>) -> Result<Option<Name>> {
    opt(preceded(t(":"), name)).parse_next(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::par::lexer::lex;
    use winnow::stream::TokenSlice;

    #[test]
    fn test_list() {
        let mut p = list("ab");
        assert_eq!(p.parse("ab"), Ok(vec!["ab"]));
        assert_eq!(p.parse("ab,ab,ab"), Ok(vec!["ab", "ab", "ab"]));
        assert_eq!(p.parse("ab,ab,ab,"), Ok(vec!["ab", "ab", "ab"]));
        assert!(p.parse("ab,ab,ab,,").is_err());
        assert!(p.parse("ba").is_err());
        let toks = lex::<Error>("ab_12,asd, asdf3").unwrap();
        let toks = TokenSlice::new(&toks);
        {
            assert_eq!(
                list(name).parse(toks),
                Ok(vec![
                    Name {
                        string: "ab_12".to_owned()
                    },
                    Name {
                        string: "asd".to_owned()
                    },
                    Name {
                        string: "asdf3".to_owned()
                    }
                ])
            );
        }
    }
    #[test]
    fn test_loop_label() {
        let toks = lex::<Error>(":one").unwrap();
        let toks = TokenSlice::new(&toks);
        assert_eq!(
            with_span(loop_label).parse(toks),
            Ok((
                Some(Name {
                    string: "one".to_owned()
                }),
                0..4
            ))
        );
    }

    #[test]
    fn t() {
        let toks = lex::<Error>(include_str!("../../examples/semigroup_queue.par")).unwrap();
        let toks = TokenSlice::new(&toks);
        match program(toks) {
            Ok(x) => eprintln!("{x:?}"),
            Err(e) => {
                eprintln!("{}", e.inner());
                eprintln!("{:?}", e.into_inner().context().collect::<Vec<_>>())
            }
        }
    }
}
