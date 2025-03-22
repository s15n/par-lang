use super::types::Type;
use core::{fmt::Display, str::FromStr};

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

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name {
    pub string: String,
}

impl From<String> for Name {
    fn from(string: String) -> Self {
        Self { string }
    }
}
impl FromStr for Name {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Name::from(s.to_owned()))
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

#[derive(Clone, Debug)]
pub struct Program<Loc, Name, Expr> {
    pub type_defs: Vec<(Loc, Name, Vec<Name>, Type<Loc, Name>)>,
    pub declarations: Vec<(Loc, Name, Type<Loc, Name>)>,
    pub definitions: Vec<(Loc, Name, Expr)>,
}

impl<Name, Expr> Default for Program<Loc, Name, Expr> {
    fn default() -> Self {
        Self {
            type_defs: Default::default(),
            declarations: Default::default(),
            definitions: Default::default(),
        }
    }
}
