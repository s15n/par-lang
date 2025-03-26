use std::ops::RangeInclusive;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Default, Hash)]
pub struct Point {
    // 0-based
    pub offset: usize,
    // 0-based
    pub row: usize,
    // 0-based
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(Default, Hash)]
pub struct Span {
    pub start: Point,
    pub end: Point,
}

pub trait Spanning {
    fn span(&self) -> Span;
}

impl Span {
    pub fn len(&self) -> usize {
        self.end.offset - self.start.offset
    }
}

impl From<RangeInclusive<Span>> for Span {
    fn from(range: RangeInclusive<Span>) -> Span {
        Span {
            start: range.start().start,
            end: range.end().end,
        }
    }
}