use lsp_types::{self as lsp};
use crate::location::Span;

impl Into<lsp::Range> for Span {
    fn into(self) -> lsp::Range {
        lsp::Range {
            start: lsp::Position {
                line: self.start.row as u32,
                character: self.start.column as u32,
            },
            end: lsp::Position {
                line: self.end.row as u32,
                character: self.end.column as u32,
            },
        }
    }
}

impl Into<lsp::Range> for &Span {
    fn into(self) -> lsp::Range {
        self.clone().into()
    }
}