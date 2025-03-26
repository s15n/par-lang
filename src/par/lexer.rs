use super::{parser::comment};
use crate::par::location::{Point, Span};
use core::{ops::Range, str::FromStr};
use miette::{SourceOffset, SourceSpan};
use winnow::{
    combinator::{alt, peek},
    error::{EmptyError, ParserError},
    stream::{ParseSlice, TokenSlice},
    token::{any, literal, take_while},
    Parser, Result,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    LPar,
    RPar,
    LCrl,
    RCrl,
    LBkt,
    RBkt,
    LAng,
    RAng,

    Colon,
    Comma,
    Dot,
    Eq,
    Arrow,
    Bang,
    Quest,
    Link,

    Identifier,
    Begin,
    Chan,
    Dec,
    Def,
    Do,
    Either,
    In,
    Iterative,
    Let,
    Loop,
    Recursive,
    Self_,
    Type,
    Unfounded,

    Unknown,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'i> {
    pub kind: TokenKind,
    pub raw: &'i str,
    pub span: Span,
}
// More useful in winnow debug view
// impl core::fmt::Debug for Token<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         format!("{}", self.raw).fmt(f)
//         // f.debug_struct("Token").field("kind", &self.kind).field("raw", &self.raw).field("loc", &self.loc).field("span", &self.span).finish()
//     }
// }
impl PartialEq<TokenKind> for Token<'_> {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}

impl TokenKind {
    pub fn expected(&self) -> &'static str {
        match self {
            TokenKind::LPar => "(",
            TokenKind::RPar => ")",
            TokenKind::LCrl => "{",
            TokenKind::RCrl => "}",
            TokenKind::LBkt => "[",
            TokenKind::RBkt => "]",
            TokenKind::LAng => "<",
            TokenKind::RAng => ">",

            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Eq => "=",
            TokenKind::Arrow => "=>",
            TokenKind::Bang => "!",
            TokenKind::Quest => "?",
            TokenKind::Link => "<>",

            TokenKind::Identifier => "identifier",
            TokenKind::Begin => "begin",
            TokenKind::Chan => "chan",
            TokenKind::Dec => "dec",
            TokenKind::Def => "def",
            TokenKind::Do => "do",
            TokenKind::Either => "either",
            TokenKind::In => "in",
            TokenKind::Iterative => "iterative",
            TokenKind::Let => "let",
            TokenKind::Loop => "loop",
            TokenKind::Recursive => "recursive",
            TokenKind::Self_ => "self",
            TokenKind::Type => "type",
            TokenKind::Unfounded => "unfounded",

            TokenKind::Unknown => "???",
        }
    }
}
/*
impl From<TokenKind> for &'static str {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::LPar => "(",
            TokenKind::RPar => ")",
            TokenKind::LCrl => "{",
            TokenKind::RCrl => "}",
            TokenKind::LBkt => "[",
            TokenKind::RBkt => "]",
            TokenKind::LAng => "<",
            TokenKind::RAng => ">",

            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Equal => "=",
            TokenKind::Arrow => "=>",
            TokenKind::Bang => "!",
            TokenKind::Quest => "?",
            TokenKind::Link => "<>",

            TokenKind::Identifier => "<id>",
            TokenKind::Begin => "begin",
            TokenKind::Chan => "chan",
            TokenKind::Dec => "dec",
            TokenKind::Def => "def",
            TokenKind::Do => "do",
            TokenKind::Either => "either",
            TokenKind::In => "in",
            TokenKind::Iterative => "iterative",
            TokenKind::Let => "let",
            TokenKind::Loop => "loop",
            TokenKind::Recursive => "recursive",
            TokenKind::Self_ => "self",
            TokenKind::Type => "type",
            TokenKind::Unfounded => "unfounded",

            TokenKind::Unknown => "<unknown>",
        }
    }
}
*/

impl PartialEq<str> for Token<'_> {
    fn eq(&self, other: &str) -> bool {
        self.raw == other
    }
}
impl PartialEq<&str> for Token<'_> {
    fn eq(&self, &other: &&str) -> bool {
        self.raw == other
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

pub type Tokens<'i> = TokenSlice<'i, Token<'i>>;
pub type Input<'a> = Tokens<'a>;

pub fn tokenize<'s>(input: &'s str) -> Vec<Token<'s>> {
    lex::<'s, EmptyError>(input).expect("lexing failed")
}

fn lex<'s, E>(input: &'s str) -> Result<Vec<Token<'s>>, E>
where
    E: ParserError<&'s str>,
{
    let mut input = input;
    let input = &mut input;

    let mut row = 0;
    let mut last_newline = input.len();
    let mut tokens = Vec::new();
    let mut idx = 0;

    let mut any = any::<&str, E>;

    while let Ok(c) = peek(any).parse_next(input) {
        let column = last_newline - input.len(); // starting column
        let Some((raw, kind)) = (match c {
            'a'..='z' | 'A'..='Z' | '_' => {
                let raw = take_while(
                    0..,
                    |c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
                )
                .take()
                .parse_next(input)?;
                let kind = match raw {
                    "begin" => TokenKind::Begin,
                    "chan" => TokenKind::Chan,
                    "dec" => TokenKind::Dec,
                    "def" => TokenKind::Def,
                    "do" => TokenKind::Do,
                    "either" => TokenKind::Either,
                    "in" => TokenKind::In,
                    "iterative" => TokenKind::Iterative,
                    "let" => TokenKind::Let,
                    "loop" => TokenKind::Loop,
                    "recursive" => TokenKind::Recursive,
                    "self" => TokenKind::Self_,
                    "type" => TokenKind::Type,
                    "unfounded" => TokenKind::Unfounded,
                    _ => TokenKind::Identifier,
                };
                Some((raw, kind))
            }
            '\n' => {
                let _ = any.parse_next(input);
                row += 1;
                last_newline = input.len();
                idx += 1;
                None
            }
            ' ' | '\t' | '\r' => {
                let _ = any.parse_next(input);
                idx += 1;
                None
            }
            ':' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::Colon))
            }
            '[' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::LBkt))
            }
            ']' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::RBkt))
            }
            '(' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::LPar))
            }
            ')' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::RPar))
            }
            '{' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::LCrl))
            }
            '}' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::RCrl))
            }
            '<' => Some(
                alt((
                    "<>".map(|raw| (raw, TokenKind::Link)),
                    "<".map(|raw| (raw, TokenKind::LAng)),
                ))
                .parse_next(input)?,
            ),
            '>' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::RAng))
            }
            '/' => {
                let (is_comment, raw) = alt((
                    comment().take().map(|x| (true, x)),
                    any.take().map(|x| (false, x)),
                ))
                .parse_next(input)?;
                if is_comment {
                    idx += raw.len();
                    None
                } else {
                    Some((raw, TokenKind::Unknown))
                }
            }
            ',' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::Comma))
            }
            '.' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::Dot))
            }
            '=' => Some(
                alt((
                    ("=>").map(|raw| (raw, TokenKind::Arrow)),
                    ("=").map(|raw| (raw, TokenKind::Eq)),
                ))
                .parse_next(input)?,
            ),
            '!' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::Bang))
            }
            '?' => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::Quest))
            }
            _ => {
                let raw = any.take().parse_next(input)?;
                Some((raw, TokenKind::Unknown))
            }
        }) else {
            continue;
        };
        let start = Point {
            offset: idx,
            row,
            column,
        };
        idx += raw.len();
        let end = Point {
            offset: idx,
            row,
            column: column + raw.len(),
        };
        tokens.push(Token {
            kind,
            raw,
            span: Span { start, end },
        });
    }
    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tok() {
        let tokens = tokenize(&mut "({[< ><>]}):abc_123: a\nab");
        assert_eq!(
            tokens.iter().map(|x| x.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::LPar,
                TokenKind::LCrl,
                TokenKind::LBkt,
                TokenKind::LAng,
                TokenKind::RAng,
                TokenKind::Link,
                TokenKind::RBkt,
                TokenKind::RCrl,
                TokenKind::RPar,
                TokenKind::Colon,
                TokenKind::Identifier,
                TokenKind::Colon,
                TokenKind::Identifier,
                TokenKind::Identifier,
            ]
        );
        eprintln!("{:#?}", tokens);
    }
}
