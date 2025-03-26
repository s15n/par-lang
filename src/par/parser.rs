use super::{
    ast::{Construct, ConstructBranch, ConstructBranches, Declaration, Definition, Expression, Name, Pattern, Program, TypeDef},
    lexer::{tokenize, Input, Token, TokenKind},
    types::Type,
};
use crate::par::location::{Span, Spanning};
use indexmap::IndexMap;
use miette::{SourceOffset, SourceSpan};
use std::sync::Arc;
use std::str::FromStr;
use winnow::token::literal;
use winnow::{
    combinator::{
        alt, cut_err, not, opt, preceded, repeat, separated, terminated,
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
// impl<C> MyError<C> {
//     fn eof_offset(&self) -> usize {
//         self.context.iter().map(|x| x.0).min().unwrap_or(usize::MAX)
//     }
// }
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
        self
    }
    fn or(mut self, other: Self) -> Self {
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
fn t<'i, E>(kind: TokenKind) -> impl Parser<Input<'i>, &'i Token<'i>, E>
where
    E: AddContext<Input<'i>, StrContext> + ParserError<Input<'i>>,
{
    literal(kind)
        .context(StrContext::Expected(StrContextValue::StringLiteral(kind.expected())))
        .map(|t: &[Token]| &t[0])
}

/// Like `t` for but for `n` tokens.
macro_rules! tn {
    ($($t:expr),+) => {
        ($($t),+).context(StrContext::Expected(StrContextValue::Description(
            stringify!($($t),+),
        )))
    };
}


fn commit_after<Input, Prefix, Output, Error, PrefixParser, ParseNext>(
    prefix: PrefixParser,
    parser: ParseNext,
) -> impl Parser<Input, (Prefix, Output), Error>
where
    Input: Stream,
    Error: ParserError<Input> + ModalError,
    PrefixParser: Parser<Input, Prefix, Error>,
    ParseNext: Parser<Input, Output, Error>,
{
    trace("commit_after", (
        prefix,
        cut_err(parser),
    ))
}

/// Like `commit_after` but the prefix is optional and only cuts if the prefix is `Some`.
/// Also returns the prefix.
fn opt_commit_after<Input, Prefix, Output, Error, PrefixParser, ParseNext>(
    prefix: PrefixParser,
    mut parser: ParseNext,
) -> impl Parser<Input, (Option<Prefix>, Output), Error>
where
    Input: Stream,
    Error: ParserError<Input> + ModalError,
    PrefixParser: Parser<Input, Prefix, Error>,
    ParseNext: Parser<Input, Output, Error>,
{
    let mut prefix = opt(prefix);
    trace("opt_commit_after", move |input: &mut Input| {
        let prefix = prefix.parse_next(input)?;
        if prefix.is_some() {
            let n = cut_err(parser.by_ref()).parse_next(input)?;
            Ok((prefix, n))
        } else {
            let n = parser.parse_next(input)?;
            Ok((prefix, n))
        }
    })
}

pub fn comment<'s, E>() -> impl Parser<&'s str, &'s str, E>
where
    E: ParserError<&'s str>,
{
    // below should be a valid block comment
    /* /* */ */
    // So have to consider nested comments
    let comment_block_rest = move |input: &mut &'s str| -> core::result::Result<(), E> {
        let mut nesting = 0;
        loop {
            let next_2 = match input.len() {
                0 => break Ok(()),
                1 => break Err(ParserError::from_input(input)),
                _ => &input.as_bytes()[..2],
            };
            match next_2 {
                s @ b"/*" => {
                    nesting += 1;
                    *input = &input[s.len()..];
                }
                s @ b"*/" if nesting > 0 => {
                    nesting -= 1;
                    *input = &input[s.len()..];
                }
                s @ b"*/" => {
                    *input = &input[s.len()..];
                    break Ok(());
                }
                _ => {
                    let mut it = input.chars();
                    it.next(); // skip a char
                    *input = it.as_str();
                }
            }
        }
    };
    alt((
        preceded("//", repeat(0.., (not("\n"), any)).map(|()| ())),
        // ending `*/` is optional so lexer doesn't panic if unclosed
        preceded("/*", comment_block_rest).map(|()| ()),
    ))
    // .context(StrContext::Label("comment"))
    .take()
}

/*
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
        "begin",
        "unfounded",
        "loop",
        "telltypes",
        "either",
        "recursive",
        "iterative",
        "self",
    ))
    .context(StrContext::Label("keyword"))
}
*/

/*fn with_loc<'a, O, E>(
    mut parser: impl Parser<Input<'a>, O, E>,
) -> impl Parser<Input<'a>, (O, Loc), E>
where
    E: ParserError<Input<'a>> + ModalError,
{
    move |input: &mut Input<'a>| -> core::result::Result<(O, Loc), E> {
        let loc = match peek(any::<_, E>).parse_next(input) {
            Ok(x) => x.loc.clone(),
            Err(e) => {
                let checkpoint = input.checkpoint();
                input.reset_to_start();
                let Some(last) = input.last() else {
                    return Err(e);
                };
                let res = match last.loc {
                    Loc::Code { line, column } => Loc::Code {
                        line,
                        column: column + 1,
                    },
                    Loc::External => Loc::External,
                };
                input.reset(&checkpoint);
                res
            }
        };
        let out = parser.parse_next(input)?;
        Ok((out, loc))
    }
}*/
/*
#[allow(dead_code)]
fn with_loc<'a, O, E>(
    mut parser: impl Parser<Input<'a>, O, E>,
) -> impl Parser<Input<'a>, (O, Loc), E>
where
    E: ParserError<Input<'a>>,
{
    move |input: &mut Input<'a>| -> core::result::Result<(O, Loc), E> {
        let last = input.last().cloned();
        let start_loc = peek(any).parse_next(input)?.loc.clone();
        let start_span = match start_loc {
            Loc::Code { span, .. } => span,
            Loc::External => unreachable!("external loc in code"),
        };
        let out = parser.parse_next(input)?;
        let end_loc = peek::<_, &Token, E, _>(any)
            .parse_next(input)
            .unwrap_or(&last.unwrap()) // if input now empty, use that last token.
            .loc
            .clone();
        let (end_span, file, line, column) = match end_loc {
            Loc::Code { span, file, line, column } => (span, file, line, column),
            Loc::External => unreachable!("external loc in code"),
        };

        Ok((out, Loc::Code {
            line,
            column: column + 1,
            span: start_span.start..end_span.end,
            file,
        }))
    }
}
*/

fn name(input: &mut Input) -> Result<Name> {
    TokenKind::Identifier.parse_to::<Name>()
        .context(StrContext::Expected(StrContextValue::CharLiteral('_')))
        .context(StrContext::Expected(StrContextValue::Description(
            "alphabetic",
        )))
        .context(StrContext::Label("name"))
        .parse_next(input)
}

struct ProgramParseError {
    offset: usize,
    error: Error_,
}
impl ProgramParseError {
    fn offset(&self) -> usize {
        self.offset
    }
    fn inner(&self) -> &Error_ {
        &self.error
    }
}
fn program(
    mut input: Input,
) -> std::result::Result<Program<Name, Expression<Name>>, ProgramParseError> {
    pub enum Item<Name, Expr> {
        TypeDef(TypeDef<Name>),
        Declaration(Declaration<Name>),
        Definition(Definition<Name, Expr>, Option<Type<Name>>),
    }

    let parser = repeat(
        0..,
        alt((
            type_def.map(Item::TypeDef),
            declaration.map(Item::Declaration),
            definition.map(|(def, typ)| Item::Definition(def, typ)),
        ))
        .context(StrContext::Label("item")),
    )
    .fold(Program::default, |mut acc, item| {
        match item {
            Item::TypeDef(type_def) => {
                acc.type_defs.push(type_def);
            }
            Item::Declaration(dec) => {
                acc.declarations.push(dec);
            }
            Item::Definition(Definition { span, name, expression }, annotation) => {
                if let Some(typ) = annotation {
                    acc.declarations.push(Declaration { span: span.clone(), name: name.clone(), typ });
                }
                acc.definitions.push(Definition { span, name, expression });
            }
        };
        acc
    });

    let start = input.checkpoint();
    (
        parser,
        winnow::combinator::eof
            .context(StrContext::Expected(StrContextValue::StringLiteral("type")))
            .context(StrContext::Expected(StrContextValue::StringLiteral("dec")))
            .context(StrContext::Expected(StrContextValue::StringLiteral("def")))
            .context(StrContext::Expected(StrContextValue::Description(
                "end of file",
            ))),
    )
        .parse_next(&mut input)
        .map(|(x, _eof)| x)
        .map_err(|e| {
            let e = e.into_inner().unwrap_or_else(|_err| {
                panic!("complete parsers should not report `ErrMode::Incomplete(_)`")
            });

            ProgramParseError {
                offset: winnow::stream::Offset::offset_from(&input, &start),
                error: ParserError::append(e, &input, &start),
            }
        })
}

#[derive(Debug, Clone, miette::Diagnostic)]
#[diagnostic(severity(Error))]
pub struct SyntaxError {
    #[label]
    source_span: SourceSpan,
    // Generate these with the miette! macro.
    #[related]
    related: Arc<[miette::ErrReport]>,
    #[help]
    help: String,

    span: Span,
    message: String,
}

impl SyntaxError {
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl Spanning for SyntaxError {
    fn span(&self) -> Span {
        self.span
    }
}

impl core::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "Syntax error.".fmt(f)
    }
}
impl core::error::Error for SyntaxError {}

pub fn set_miette_hook() {
    _ = miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(true)
                .color(false)
                // .context_lines(1)
                // .with_cause_chain()
                .build(),
        )
    }));
}

pub fn parse_program(
    input: &str
) -> std::result::Result<Program<Name, Expression<Name>>, SyntaxError> {
    let toks = tokenize(&input);
    let e = match program(Input::new(&toks)) {
        Ok(x) => return Ok(x),
        Err(e) => e,
    };
    // Empty input doesn't error so this won't panic.
    let error_tok = toks.get(e.offset()).unwrap_or(toks.last().unwrap()).clone();
    //let Loc::Code { span: error_tok_span, .. } = &error_tok.loc else { unreachable!("Error token not in code") };
    let error_tok_span = error_tok.span.clone();
    let source_span = SourceSpan::new(SourceOffset::from(error_tok_span.start.offset), {
        match error_tok_span.len() {
            // miette unicode format for 1 length span is a hard-to-notice line, so don't set length to 1.
            x if x == 1 => 0,
            x => x,
        }
    });
    Err(SyntaxError {
        span: error_tok_span,
        message: "Syntax error.".to_string(),

        source_span,
        related: {
            /*let crate::par::parse::ParseError {
                msg,
                location: Loc::Code { span, file },
            } = (match crate::par::parse::parse_program(&input) {
                Ok(_) => unreachable!("pest parser didn't error but combinator parser did"),
                Err(e) => e,
            });
            else {
                unreachable!("parse error location not `Code`")
            };*/
            let msg = "Some related error";
            Arc::from(vec![miette::miette!(
                labels = vec![miette::LabeledSpan::new_with_span(
                    None,
                    source_span,
                )],
                help = msg,
                "pest error"
            )])
        },
        help: e
            .inner()
            .context
            .iter()
            .map(|x| x.1.to_string().chars().chain(['\n']).collect::<String>())
            .collect::<String>(),
    })
}

fn type_def(input: &mut Input) -> Result<TypeDef<Name>> {
    /*commit_after(t("type"), (with_loc(name), type_params, t("="), typ))
        .map(|((name, loc), type_params, _, typ)| (loc, name, type_params, typ))
        .context(StrContext::Label("type definition"))
        .parse_next(input)*/
    commit_after(
        t(TokenKind::Type),
        (
            name,
            type_params,
            t(TokenKind::Eq),
            typ,
        )
    )
        .map(|(pre, (name, (_, params), _, typ))| {
            let span = (pre.span..=typ.span()).into();
            TypeDef {
                name,
                params,
                typ,
                span,
            }
        })
        .context(StrContext::Label("type definition"))
        .parse_next(input)
}

fn declaration(input: &mut Input) -> Result<Declaration<Name>> {
    commit_after(
        t(TokenKind::Dec),
        (
            name,
            t(TokenKind::Colon),
            typ
        )
    )
        .map(|(pre, (name, _, typ))| {
            let span = (pre.span..=typ.span()).into();
            Declaration {
                name,
                typ,
                span,
            }
        })
        .context(StrContext::Label("declaration"))
        .parse_next(input)
}

fn definition(
    input: &mut Input,
) -> Result<(Definition<Name, Expression<Name>>, Option<Type<Name>>)> {
    commit_after(
        t(TokenKind::Def),
        (
            name,
            annotation,
            t(TokenKind::Eq),
            expression
        )
    )
        .map(|(pre, (name, annotation, _, expression))| {
            let span = (pre.span..=expression.span()).into();
            (Definition {
                name,
                expression,
                span,
            }, annotation)
        })
        .context(StrContext::Label("definition"))
        .parse_next(input)
}

fn list<'i, P, O>(item: P) -> impl Parser<Input<'i>, Vec<O>, Error> + use<'i, P, O>
//fn list<P, I, O>(item: P) -> impl Parser<I, Vec<O>, Error> + use<P, I, O>
where
    P: Parser<Input<'i>, O, Error>,
    //I: Stream + StreamIsPartial + for<'a> Compare<&'a str>,
    Vec<O>: Accumulate<O>,
{
    terminated(separated(1.., item, t(TokenKind::Comma)), opt(t(TokenKind::Comma)))
}

fn branches_body<'i, P, O>(
    branch: P,
) -> impl Parser<Input<'i>, (Span, IndexMap<Name, O>), Error> + use<'i, P, O>
where
    P: Parser<Input<'i>, O, Error>,
{
    commit_after(
        t(TokenKind::LCrl),
        (
            repeat(0.., (t(TokenKind::Dot), name, cut_err(branch), opt(t(TokenKind::Comma)))).fold(
                || IndexMap::new(),
                |mut branches, (_, name, branch, _)| {
                    branches.insert(name, branch);
                    branches
                },
            ),
            t(TokenKind::RCrl),
        ),
    )
        .map(|(pre, (branches, last))| {
            let span = (pre.span..=last.span).into();
            (span, branches)
        })
        .context(StrContext::Label("either/choice branches"))
}

fn typ(input: &mut Input) -> Result<Type<Name>> {
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

fn typ_name(input: &mut Input) -> Result<Type<Name>> {
    trace(
        "typ_name",
        (t(TokenKind::Identifier), type_args).map(|(name_token, (typ_args_span, typ_args))| {
            let span = (name_token.span..=typ_args_span).into();
            let name = Name::from_str(name_token.raw).unwrap();
            Type::Name(span, name, typ_args)
        })
    )
    .parse_next(input)
}

fn typ_chan(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::Chan),
        typ.context(StrContext::Label("chan type")),
    )
        .map(|(pre, typ)| {
            let span = (pre.span..=typ.span()).into();
            Type::Chan(span, Box::new(typ))
        })
        .parse_next(input)
}

fn typ_send(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::LPar),
        (
            terminated(list(typ), t(TokenKind::RPar)),
            typ
        )
    )
        .map(|(pre, (args, then))| {
            let span = (pre.span..=then.span()).into();
            Type::Send(span, args, Box::from(then))
        })
        .parse_next(input)
}

fn typ_receive(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::LBkt),
        (
            terminated(list(typ), t(TokenKind::RBkt)),
            typ
        )
    )
        .map(|(pre, (args, then))| {
            let span = (pre.span..=then.span()).into();
            Type::Receive(span, args, Box::from(then))
        })
        .parse_next(input)
}

fn typ_either(input: &mut Input) -> Result<Type<Name>> {
    commit_after(t(TokenKind::Either), branches_body(typ))
        .map(|(pre, (branches_span, branches))| {
            let span = (pre.span..=branches_span).into();
            Type::Either(span, branches)
        })
        .parse_next(input)
}

fn typ_choice(input: &mut Input) -> Result<Type<Name>> {
    branches_body(typ_branch)
        .map(|(span, branches)|
            Type::Choice(span, branches)
        )
        .parse_next(input)
}

fn typ_break(input: &mut Input) -> Result<Type<Name>> {
    t(TokenKind::Bang)
        .map(|token| Type::Break(token.span))
        .parse_next(input)
}

fn typ_continue(input: &mut Input) -> Result<Type<Name>> {
    t(TokenKind::Quest)
        .map(|token| Type::Break(token.span))
        .parse_next(input)
}

fn typ_recursive(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::Recursive),
        (loop_label, typ)
    )
        .map(|(pre, (label, typ))| {
            let span = (pre.span..=typ.span()).into();
            Type::Recursive {
                span,
                asc: Default::default(),
                label: label.map(|(_, label)| label),
                body: Box::from(typ)
            }
        })
        .parse_next(input)
}

fn typ_iterative<'s>(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::Iterative),
        (loop_label, typ)
    )
        .map(|(pre, (label, typ))| {
            let span = (pre.span..=typ.span()).into();
            Type::Recursive {
                span,
                asc: Default::default(),
                label: label.map(|(_, label)| label),
                body: Box::new(typ)
            }
        })
        .parse_next(input)
}

fn typ_self<'s>(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::Self_),
        loop_label.context(StrContext::Label("self type loop label")),
    )
        .map(|(pre, label)| {
            match label {
                Some((label_span, label)) => {
                    let span = (pre.span..=label_span).into();
                    Type::Self_(span, Some(label))
                },
                None => Type::Self_(pre.span, None),
            }
        })
        .parse_next(input)
}

fn typ_send_type<'s>(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        tn!(TokenKind::LPar, TokenKind::Type),
        (
            list(name).context(StrContext::Label("list of type names to send")),
            t(TokenKind::RPar),
            typ
        )
    )
        .map(|((pre, _), (args, _, then))| {
            let span = (pre.span..=then.span()).into();
            Type::SendTypes(span, args, Box::new(then))
        })
        .parse_next(input)
}

fn typ_recv_type<'s>(input: &mut Input<'s>) -> Result<Type<Name>> {
    commit_after(
        tn!(TokenKind::LBkt, TokenKind::Type),
        (
            list(name).context(StrContext::Label("list of type names to send")),
            t(TokenKind::RBkt),
            typ
        )
    )
        .map(|((pre, _), (args, _, then))| {
            let span = (pre.span..=then.span()).into();
            Type::ReceiveTypes(span, args, Box::new(then))
        })
        .parse_next(input)
}

fn type_params<'s>(input: &mut Input) -> Result<(Span, Vec<Name>)> {
    // TODO should be able to use `<` to improve error message
    opt((t(TokenKind::LAng), list(name), t(TokenKind::RAng)))
        .map(|opt| match opt {
            Some((pre, names, suf)) => {
                let span = (pre.span..=suf.span).into();
                (span, names)
            }
            None => (Span::default(), Vec::default()),
        })
        .parse_next(input)
}

fn type_args<'s>(input: &mut Input) -> Result<(Span, Vec<Type<Name>>)> {
    // TODO should be able to use `<` to improve error message
    opt((t(TokenKind::LAng), list(typ), t(TokenKind::RAng)))
        .map(|opt| match opt {
            Some((pre, types, suf)) => {
                let span = (pre.span..=suf.span).into();
                (span, types)
            }
            None => (Span::default(), Vec::default()),
        })
        .parse_next(input)
}

fn typ_branch(input: &mut Input) -> Result<Type<Name>> {
    // try recv_type first so `(` is unambiguous on `typ_branch_received`
    alt((typ_branch_then, typ_branch_recv_type, typ_branch_receive)).parse_next(input)
}

fn typ_branch_then(input: &mut Input) -> Result<Type<Name>> {
    commit_after(t(TokenKind::Arrow), typ)
        .map(|(_, typ)| typ)
        .parse_next(input)
}

fn typ_branch_receive(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        t(TokenKind::LPar),
        (list(typ), t(TokenKind::RPar), typ_branch)
    )
        .map(|(pre, (args, _, then))| {
            let span = (pre.span..=then.span()).into();
            Type::Receive(span, args, Box::new(then))
        })
        .parse_next(input)
}

fn typ_branch_recv_type(input: &mut Input) -> Result<Type<Name>> {
    commit_after(
        tn!(TokenKind::LPar, TokenKind::Type),
        (list(name), t(TokenKind::RPar), typ_branch)
    )
        .map(|((pre, _), (args, _, then))| {
            let span = (pre.span..=then.span()).into();
            Type::ReceiveTypes(span, args, Box::new(then))
        })
        .parse_next(input)
}

fn annotation(input: &mut Input) -> Result<Option<Type<Name>>> {
    opt(commit_after(t(TokenKind::Colon), typ))
        .map(|opt| opt.map(|(_, typ)| typ))
        .parse_next(input)
}

// pattern           = { pattern_name | pattern_receive | pattern_continue | pattern_recv_type }
fn pattern(input: &mut Input) -> Result<Pattern<Name>> {
    alt((
        pattern_name,
        /*pattern_receive_type,
        pattern_receive,
        pattern_continue,*/
    ))
        .parse_next(input)
}

fn pattern_name(input: &mut Input) -> Result<Pattern<Name>> {
    (t(TokenKind::Identifier), annotation)
        .map(|(name_token, annotation)| {
            let span = match &annotation {
                Some(typ) => (name_token.span..=typ.span()).into(),
                None => name_token.span,
            };
            Pattern::Name(span, Name::from(name_token), annotation)
        })
        .parse_next(input)
}

/*fn pattern_receive(input: &mut Input) -> Result<Pattern<Name>> {
    commit_after(
        t(TokenKind::LPar),
        (list(pattern), t(TokenKind::RPar), pattern)
    )
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
        tn!("(", "type"),
        (list(name), t(")"), pattern),
    ))
    .map(|((names, _, mut rest), loc)| {
        for name in names.into_iter().rev() {
            rest = Pattern::ReceiveType(loc.clone(), name, Box::new(rest));
        }
        rest
    })
    .parse_next(input)
}*/

fn expression(input: &mut Input) -> Result<Expression<Name>> {
    alt((
        expr_let,
        //expr_do,
        //expr_fork,
        //application,
        //with_loc(construction).map(|(cons, loc)| Expression::Construction(loc, cons)),
        construction.map(Expression::Construction),
        expr_grouped,
    ))
    .context(StrContext::Label("expression"))
    .parse_next(input)
}

fn expr_grouped(input: &mut Input) -> Result<Expression<Name>> {
    (
        t(TokenKind::LCrl),
        expression,
        t(TokenKind::RCrl)
    ).map(|(pre, expr, suf)| {
        Expression::Grouped(
            (pre.span..=suf.span).into(),
            Box::new(expr),
        )
    })
        .parse_next(input)
}

fn expr_let(input: &mut Input) -> Result<Expression<Name>> {
    commit_after(
        t(TokenKind::Let),
        (
            pattern,
            t(TokenKind::Eq),
            expression,
            t(TokenKind::In),
            expression
        ),
    )
        .map(|(pre, (pattern, _, expression, _, body))| {
            let span = (pre.span..=body.span()).into();
            Expression::Let {
                span,
                pattern,
                expression: Box::new(expression),
                then: Box::new(body)
            }
        })
        .parse_next(input)
}
/*
fn expr_do(input: &mut Input) -> Result<Expression<Name>> {
    commit_after(
        t(TokenKind::Do),
        (
            t(TokenKind::LCrl),
            process,
            (t(TokenKind::RCrl), t(TokenKind::In)),
            expression
        ),
    )
    .map(|(pre, (_, process, _, expression))| {
        let span = (pre.span..=expression.span()).into();
        Expression::Do {
            span,
            process: Box::new(process),
            then: Box::new(expression)
        }
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
}*/

fn construction(input: &mut Input) -> Result<Construct<Name>> {
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

fn cons_then(input: &mut Input) -> Result<Construct<Name>> {
    alt((
        //expr_fork,
        expr_let,
        //expr_do,
        //application,
        expr_grouped,
    ))
    .map(|expr| Construct::Then(Box::new(expr)))
    .parse_next(input)
}

fn cons_send(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(
        t(TokenKind::LPar),
        (list(expression), t(TokenKind::RPar), construction),
    )
    .map(|(pre, (args, _, then))| {
        let span = (pre.span..=then.span()).into();
        Construct::Send(span, args, Box::from(then))
    })
    .parse_next(input)
}

fn cons_receive(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(
        t(TokenKind::LBkt),
        (list(pattern), t(TokenKind::RBkt), construction)
    )
        .map(|(pre, (patterns, _, then))| {
            let span = (pre.span..=then.span()).into();
            Construct::Receive(span, patterns, Box::from(then))
        })
        .parse_next(input)
}

fn cons_choose(input: &mut Input) -> Result<Construct<Name>> {
    // Note this can't be a commit_after because its possible that this is not a choose construction, and instead a branch of an either.
    (
        t(TokenKind::Dot),
        (name, construction)
    )
        .map(|(pre, (chosen, then))| {
            let span = (pre.span..=then.span()).into();
            Construct::Choose(span, chosen, Box::new(then))
        })
        .parse_next(input)
}

fn cons_either(input: &mut Input) -> Result<Construct<Name>> {
    branches_body(cons_branch)
        .map(|(span, branches)| Construct::Either(span, ConstructBranches(branches)))
        .parse_next(input)
}

fn cons_break(input: &mut Input) -> Result<Construct<Name>> {
    t(TokenKind::Bang)
        .map(|token| Construct::Break(token.span))
        .parse_next(input)
}

fn cons_begin(input: &mut Input) -> Result<Construct<Name>> {
    opt_commit_after(
        t(TokenKind::Unfounded),
        commit_after(
            t(TokenKind::Begin),
            (loop_label, construction)
        ),
    )
    .map(|(unfounded, (begin, (label, then)))| {
        let span = match unfounded {
            Some(prefix) => (prefix.span..=then.span()).into(),
            None => (begin.span..=then.span()).into(),
        };
        Construct::Begin {
            span,
            unfounded: unfounded.is_some(),
            label: label.map(|(_, label)| label),
            then: Box::from(then)
        }
    })
    .parse_next(input)
}

fn cons_loop(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(t(TokenKind::Loop), loop_label)
        .map(|(loop_, label)| {
            match label {
                Some((label_span, label)) => {
                    let span = (loop_.span..=label_span).into();
                    Construct::Loop(span, Some(label))
                },
                None => Construct::Loop(loop_.span, None),
            }
        })
        .parse_next(input)
}

fn cons_send_type(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(
        tn!(TokenKind::LPar, TokenKind::Type),
        (list(typ), t(TokenKind::RPar), construction),
    )
    .map(|((pre, _), (types, _, then))| {
        let span = (pre.span..=then.span()).into();
        Construct::SendTypes(span, types, Box::from(then))
    })
    .parse_next(input)
}

fn cons_recv_type(input: &mut Input) -> Result<Construct<Name>> {
    commit_after(
        tn!(TokenKind::LBkt, TokenKind::Type),
        (list(name), t(TokenKind::RBkt), construction),
    )
        .map(|((pre, _), (names, _, then))| {
            let span = (pre.span..=then.span()).into();
            Construct::ReceiveTypes(span, names, Box::new(then))
        })
        .parse_next(input)
}

fn cons_branch(input: &mut Input) -> Result<ConstructBranch<Name>> {
    alt((
        cons_branch_then, cons_branch_recv_type, cons_branch_receive
    )).parse_next(input)
}

fn cons_branch_then(input: &mut Input) -> Result<ConstructBranch<Name>> {
    commit_after(t(TokenKind::Arrow), expression)
        .map(|(pre, expression)| {
            let span = (pre.span..=expression.span()).into();
            ConstructBranch::Then(span, expression)
        })
        .parse_next(input)
}

fn cons_branch_receive(input: &mut Input) -> Result<ConstructBranch<Name>> {
    commit_after(
        t(TokenKind::LPar),
        (list(pattern), t(TokenKind::RPar), cons_branch)
    )
        .map(|(pre, (patterns, _, branch))| {
            let span = (pre.span..=branch.span()).into();
            ConstructBranch::Receive(span, patterns, Box::new(branch))
        })
        .parse_next(input)
}

fn cons_branch_recv_type(input: &mut Input) -> Result<ConstructBranch<Name>> {
    commit_after(
        tn!(TokenKind::LPar, TokenKind::Type),
        (list(name), t(TokenKind::RPar), cons_branch)
    )
        .map(|((pre, _), (names, _, branch))| {
            let span = (pre.span..=branch.span()).into();
            ConstructBranch::ReceiveTypes(span, names, Box::from(branch))
        })
        .parse_next(input)
}
/*
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
    with_loc(branches_body(apply_branch))
        .map(|(branches, loc)| Apply::Either(loc, ApplyBranches(branches)))
        .parse_next(input)
}

fn apply_begin(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(opt_commit_after(
        t("unfounded"),
        commit_after(t("begin"), (loop_label, apply)),
    ))
    .map(|((unfounded, (label, then)), loc)| {
        Apply::Begin(loc, unfounded.is_some(), label, Box::new(then))
    })
    .parse_next(input)
}

fn apply_loop(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(commit_after(t("loop"), loop_label))
        .map(|(label, loc)| Apply::Loop(loc, label))
        .parse_next(input)
}

fn apply_send_type(input: &mut Input) -> Result<Apply<Loc, Name>> {
    with_loc(commit_after(tn!("(", "type"), (list(typ), t(")"), apply)))
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
        tn!("(", "type"),
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

 */
/*
fn process(input: &mut Input) -> Result<Process<Loc, Name>> {
    alt((proc_let, proc_telltypes, command, proc_noop))
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
    with_loc((
        branches_body(cmd_branch).map(CommandBranches),
        opt(pass_process),
    ))
    .map(|((branches, pass_process), loc)| {
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
    with_loc(opt_commit_after(
        t("unfounded"),
        commit_after(t("begin"), (loop_label, cmd)),
    ))
    .map(|((unfounded, (label, cmd)), loc)| {
        Command::Begin(loc, unfounded.is_some(), label, Box::new(cmd))
    })
    .parse_next(input)
}

fn cmd_loop(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(t("loop"), loop_label))
        .map(|(label, loc)| Command::Loop(loc, label))
        .parse_next(input)
}

fn cmd_send_type(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(tn!("(", "type"), (list(typ), t(")"), cmd)))
        .map(|((types, _, mut cmd), loc)| {
            for typ in types.into_iter().rev() {
                cmd = Command::SendType(loc.clone(), typ, Box::new(cmd));
            }
            cmd
        })
        .parse_next(input)
}

fn cmd_recv_type(input: &mut Input) -> Result<Command<Loc, Name>> {
    with_loc(commit_after(tn!("[", "type"), (list(name), t("]"), cmd)))
        .map(|((names, _, mut cmd), loc)| {
            for name in names.into_iter().rev() {
                cmd = Command::ReceiveType(loc.clone(), name, Box::new(cmd));
            }
            cmd
        })
        .parse_next(input)
}

fn pass_process(input: &mut Input) -> Result<Process<Loc, Name>> {
    alt((proc_let, proc_telltypes, command)).parse_next(input)
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
        tn!("(", "type"),
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

 */

fn loop_label(input: &mut Input) -> Result<Option<(Span, Name)>> {
    opt((
        t(TokenKind::Colon),
        t(TokenKind::Identifier)
    ))
        .map(|opt| {
            opt.map(|(pre, name_token)| {
                let span = (pre.span..=name_token.span).into();
                let name = Name::from_str(name_token.raw).unwrap();
                (span, name)
            })
        })
        .parse_next(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::par::lexer::tokenize;

    #[test]
    fn test_list() {
        let mut p = list("ab");
        assert_eq!(p.parse("ab").unwrap(), vec!["ab"]);
        assert_eq!(p.parse("ab,ab,ab").unwrap(), vec!["ab", "ab", "ab"]);
        assert_eq!(p.parse("ab,ab,ab,").unwrap(), vec!["ab", "ab", "ab"]);
        assert!(p.parse("ab,ab,ab,,").is_err());
        assert!(p.parse("ba").is_err());
        let toks = tokenize("ab_12,asd, asdf3");
        let toks = Input::new(&toks);
        {
            assert_eq!(
                list(name).parse(toks).unwrap(),
                vec![
                    Name {
                        string: "ab_12".to_owned()
                    },
                    Name {
                        string: "asd".to_owned()
                    },
                    Name {
                        string: "asdf3".to_owned()
                    }
                ]
            );
        }
    }
    /*#[test]
    fn test_loop_label() {
        let toks = lex(":one");
        let toks = Input::new(&toks);
        assert_eq!(
            with_span(loop_label).parse(toks).unwrap(),
            (
                Some(Name {
                    string: "one".to_owned()
                }),
                0..4
            )
        );
    }*/ // todo

    /*#[test]
    fn test1() {
        let input = include_str!("../../examples/sample.par");
        let res = parse_program(input);
        match res {
            Ok(new) => {
                let old = crate::par::parse::parse_program(input).unwrap();
                eprintln!("old: {:?}", old);
                eprintln!("\n---\n");
                eprintln!("new: {:?}", new);
                // assert_eq!(format!("{:?}", old), format!("{:?}", new))
            }
            Err(e) => {
                set_miette_hook();
                let e = miette::Report::from(e).with_source_code(input);
                eprintln!("{e:?}");
            }
        }
    }*/
}
