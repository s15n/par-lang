use lsp_types::{self as lsp};
use crate::location::{Point, Span};

impl Into<lsp::Range> for Span {
    fn into(self) -> lsp::Range {
        lsp::Range {
            start: self.start.into(),
            end: self.end.into(),
        }
    }
}

impl Into<lsp::Range> for &Span {
    fn into(self) -> lsp::Range {
        self.clone().into()
    }
}

impl Into<lsp::Position> for Point {
    fn into(self) -> lsp::Position {
        lsp::Position {
            line: self.row as u32,
            character: self.column as u32,
        }
    }
}

// todo: are these macros the best way to go?

macro_rules! running_constants {
    ($($x:ident),*) => {
        running_constants!(0 => $($x),*);
    };
    ($index:expr => $first:ident, $($x:ident),*) => {
        pub const $first: u32 = $index;
        running_constants!($index + 1 => $($x),*);
    };
    ($index:expr => $last:ident) => {
        pub const $last: u32 = $index;
        #[allow(non_upper_case_globals)]
        pub const count: usize = $index + 1;
    };
}

macro_rules! semantic_token_legend {
    ($variant:ident($lsp:ident): $($x:ident),*) => {
        pub const $variant: [
            lsp::$lsp;
            $variant::count
        ] = [
            $(lsp::$lsp::$x,)*
        ];

        #[allow(non_snake_case)]
        pub mod $variant {
            running_constants!($($x),*);
        }
    };
}

semantic_token_legend!(SEMANTIC_TOKEN_TYPES(SemanticTokenType): TYPE);
semantic_token_legend!(SEMANTIC_TOKEN_MODIFIERS(SemanticTokenModifier): DECLARATION, DEFINITION);