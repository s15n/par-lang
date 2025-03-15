use super::{parse::Loc, parser::comment};
use core::{ops::Range, str::FromStr};
use winnow::{
    combinator::{alt, peek},
    error::{ContextError as Error, ParserError},
    stream::{
        Checkpoint, Compare, CompareResult, Location, Offset, ParseSlice, Stream, TokenSlice,
    },
    token::{any, literal, take_while},
    LocatingSlice, Parser, Result,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    LParen,
    RParen,
    LCurly,
    RCurly,
    LAngle,
    RAngle,
    LBrack,
    RBrack,
    Colon,
    Ident,
    Comma,
    Dot,
    Arrow,
    Equal,
    Bang,
    Quest,
    Link,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<'i> {
    pub kind: TokenKind,
    pub raw: &'i str,
    pub loc: super::parse::Loc,
    pub span: Range<usize>,
}
impl PartialEq<TokenKind> for Token<'_> {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}
impl From<TokenKind> for &'static str {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LCurly => "{",
            TokenKind::RCurly => "}",
            TokenKind::LBrack => "[",
            TokenKind::RBrack => "]",
            TokenKind::LAngle => "<",
            TokenKind::RAngle => ">",
            TokenKind::Colon => ":",
            TokenKind::Ident => "",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Arrow => "=>",
            TokenKind::Equal => "=",
            TokenKind::Bang => "!",
            TokenKind::Quest => "?",
            TokenKind::Link => "<>",
        }
    }
}

impl PartialEq<str> for Token<'_> {
    fn eq(&self, other: &str) -> bool {
        <&str>::from(self.kind) == other
            || (matches!(self.kind, TokenKind::Ident) && self.raw == other)
    }
}
impl PartialEq<&str> for Token<'_> {
    fn eq(&self, &other: &&str) -> bool {
        <&str>::from(self.kind) == other
            || (matches!(self.kind, TokenKind::Ident) && self.raw == other)
    }
}

impl<'i, E> Parser<Tokens<'i>, &'i Token<'i>, E> for TokenKind
where
    E: ParserError<Tokens<'i>>,
{
    fn parse_next(&mut self, input: &mut Tokens<'i>) -> Result<&'i Token<'i>, E> {
        literal(*self).parse_next(input).map(|t| &t[0])
    }
}
// impl<'i, E> Parser<Tokens<'i>, Token<'i>, E> for TokenKind
// where
//     E: ParserError<Tokens<'i>>,
// {
//     fn parse_next(&mut self, input: &mut Tokens<'i>) -> Result<Token<'i>, E> {
//         literal(*self).parse_next(input).map(|t| t[0].clone())
//     }
// }

impl<'a, T: FromStr> ParseSlice<T> for Token<'a> {
    fn parse_slice(&self) -> Option<T> {
        self.raw.parse().ok()
    }
}
impl<'a, T: FromStr> ParseSlice<T> for &Token<'a> {
    fn parse_slice(&self) -> Option<T> {
        self.raw.parse().ok()
    }
}

// #[derive(Debug)]
// pub struct Input<'a> {
//     initial: &'a [Token<'a>],
//     input: &'a [Token<'a>],
// }
// impl<'a> Offset<&'a [Token<'a>]> for Input<'a> {
//     fn offset_from(&self, start: &&'a [Token<'a>]) -> usize {
//         start.offset_from(self.0)
//     }
// }
// impl<'a> Offset<Token<'a>> for Token<'a> {
//     fn offset_from(&self, start: &Token<'a>) -> usize {
//         start.loc
//     }
// }
// impl<'a> Offset<Checkpoint<&'a [Token<'a>], &'a [Token<'a>]>> for Input<'a> {
//     fn offset_from(&self, start: &Checkpoint<&'a [Token<'a>], &'a [Token<'a>]>) -> usize {
//         self.input.offset_from(start)
//     }
// }
// impl<'a> Stream for Input<'a> {
//     type Token = Token<'a>;
//     type Slice = &'a [Token<'a>];
//     type IterOffsets = std::iter::Enumerate<core::iter::Cloned<std::slice::Iter<'a, Token<'a>>>>;
//     type Checkpoint = Checkpoint<&'a [Token<'a>], &'a [Token<'a>]>;

//     fn iter_offsets(&self) -> Self::IterOffsets {
//         self.input.iter_offsets()
//     }

//     fn eof_offset(&self) -> usize {
//         self.input.eof_offset()
//     }

//     fn next_token(&mut self) -> Option<Self::Token> {
//         self.input.next_token()
//     }

//     fn peek_token(&self) -> Option<Self::Token> {
//         self.input.peek_token()
//     }

//     fn offset_for<P>(&self, predicate: P) -> Option<usize>
//     where
//         P: Fn(Self::Token) -> bool,
//     {
//         self.input.offset_for(predicate)
//     }

//     fn offset_at(&self, tokens: usize) -> std::result::Result<usize, winnow::error::Needed> {
//         self.input.offset_at(tokens)
//     }

//     fn next_slice(&mut self, offset: usize) -> Self::Slice {
//         self.input.next_slice(offset)
//     }

//     fn peek_slice(&self, offset: usize) -> Self::Slice {
//         self.input.peek_slice(offset)
//     }

//     fn checkpoint(&self) -> Self::Checkpoint {
//         self.input.checkpoint()
//     }

//     fn reset(&mut self, checkpoint: &Self::Checkpoint) {
//         self.input.reset(checkpoint);
//     }

//     fn raw(&self) -> &dyn core::fmt::Debug {
//         &self.input as &dyn core::fmt::Debug
//     }
// }
// impl<'a> winnow::stream::StreamIsPartial for Input<'a> {
//     type PartialState = ();

//     fn complete(&mut self) -> Self::PartialState {
//         self.input.complete()
//     }

//     fn restore_partial(&mut self, state: Self::PartialState) {
//         self.input.restore_partial(state);
//     }

//     fn is_partial_supported() -> bool {
//         false
//     }
// }
// impl<'a> Location for Input<'a> {
//     fn previous_token_end(&self) -> usize {
//         self.input.offset_from(&self.initial)
//     }

//     fn current_token_start(&self) -> usize {
//         self.input.offset_from(&self.initial)
//     }
// }
// impl Compare<&'_ str> for Input<'_> {
//     fn compare(&self, t: &str) -> CompareResult {
//         match self.input.first().map(|tok| tok.raw == t) {
//             Some(true) => CompareResult::Ok(1),
//             None | Some(false) => CompareResult::Error,
//         }
//     }
// }

pub type Tokens<'i> = TokenSlice<'i, Token<'i>>;
pub type Input<'a> = Tokens<'a>;

pub fn lex<'s, Error>(input: &'s str) -> Result<Vec<Token<'s>>, Error>
where
    Error: ParserError<&'s str> + std::fmt::Debug,
{
    let mut input = input;
    let input = &mut input;
    let mut row = 0;
    let mut last_newline = input.len();
    let mut tokens = Vec::new();
    let mut idx = 0;
    while let Ok(c) = peek(any::<&str, Error>).parse_next(input) {
        let column = last_newline - input.len(); // starting column
        let Some((raw, kind)) = (match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                let ident = take_while(
                    0..,
                    |c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
                )
                .take()
                .parse_next(input)?;
                Some((ident, TokenKind::Ident))
            }
            '\n' => {
                let _ = any::<&str, Error>.parse_next(input);
                row += 1;
                last_newline = input.len();
                None
            }
            ' ' | '\t' | '\r' => {
                let _ = any::<&str, Error>.parse_next(input);
                None
            }
            ':' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::Colon))
            }
            '[' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::LBrack))
            }
            ']' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::RBrack))
            }
            '(' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::LParen))
            }
            ')' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::RParen))
            }
            '{' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::LCurly))
            }
            '}' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::RCurly))
            }
            '<' => Some(
                alt((
                    "<>".map(|raw| (raw, TokenKind::Link)),
                    "<".map(|raw| (raw, TokenKind::LAngle)),
                ))
                .parse_next(input)?,
            ),
            '>' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::RAngle))
            }
            '/' => {
                let _ = comment().parse_next(input)?;
                None
            }
            ',' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::Comma))
            }
            '.' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::Dot))
            }
            '=' => Some(
                alt((
                    ("=>").map(|raw| (raw, TokenKind::Arrow)),
                    ("=").map(|raw| (raw, TokenKind::Equal)),
                ))
                .parse_next(input)?,
            ),
            '!' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::Bang))
            }
            '?' => {
                let raw = any::<&str, Error>.take().parse_next(input)?;
                Some((raw, TokenKind::Quest))
            }
            _ => return Err(ParserError::from_input(dbg!(input))),
        }) else {
            idx += 1;
            continue;
        };
        tokens.push(Token {
            kind,
            raw,
            loc: Loc::Code { line: row, column },
            span: idx..idx + raw.len(),
        });
        idx += raw.len();
    }
    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tok() {
        let tokens = lex::<Error>(&mut "({[< ><>]}):abc_123: a\nab");
        assert_eq!(
            tokens
                .as_ref()
                .map(|x| x.into_iter().map(|x| x.kind).collect()),
            Ok(vec![
                TokenKind::LParen,
                TokenKind::LCurly,
                TokenKind::LBrack,
                TokenKind::LAngle,
                TokenKind::RAngle,
                TokenKind::Link,
                TokenKind::RBrack,
                TokenKind::RCurly,
                TokenKind::RParen,
                TokenKind::Colon,
                TokenKind::Ident,
                TokenKind::Colon,
                TokenKind::Ident,
                TokenKind::Ident,
            ])
        );
        _ = dbg!(tokens);
    }
}
