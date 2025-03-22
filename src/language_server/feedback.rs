use std::collections::HashMap;
use std::fmt::format;
use lsp_types::{self as lsp, Uri};
use miette::Diagnostic;
use crate::language_server::instance::CompileError;
use crate::par::parse::Loc;

pub struct Feedback {
    diagnostics: HashMap<Uri, Vec<lsp::Diagnostic>>,
}

impl Feedback {
    pub fn new() -> Feedback {
        Self {
            diagnostics: HashMap::new(),
        }
    }

    pub fn diagnostics(&self) -> &HashMap<Uri, Vec<lsp::Diagnostic>> {
        &self.diagnostics
    }

    pub fn add_diagnostic(&mut self, uri: Uri, diagnostic: lsp::Diagnostic) {
        self.diagnostics.entry(uri).or_default().push(diagnostic);
    }
}

pub struct FeedbackBookKeeper {
    feedback: Feedback,
}

impl FeedbackBookKeeper {
    pub fn new() -> FeedbackBookKeeper {
        Self {
            feedback: Feedback::new(),
        }
    }

    /// The last feedback with empty diagnostics
    /// for all URIs, so that the client can clear
    pub fn cleanup(&mut self) -> &mut Feedback {
        let mut feedback = Feedback::new();
        let last_feedback = std::mem::replace(&mut self.feedback, feedback);
        for (uri, diagnostics) in last_feedback.diagnostics.into_iter() {
            if !diagnostics.is_empty() {
                self.feedback.diagnostics.entry(uri).or_default();
            }
        }
        &mut self.feedback
    }

    pub fn diagnostics(&self) -> &HashMap<Uri, Vec<lsp::Diagnostic>> {
        self.feedback.diagnostics()
    }
}

pub fn diagnostic_for_error(err: &CompileError) -> lsp::Diagnostic {
    use crate::playground::Error;

    let (loc, message) = match err {
        CompileError::Compile(Error::Parse(err)) => (err.loc(), err.message().to_string()),
        CompileError::Compile(Error::Compile(err)) => (err.loc(), err.message().to_string()),
        CompileError::Compile(Error::Type(err))
        | CompileError::Types(err)
        => (err.loc(), "Type Error".to_string()),
        CompileError::Compile(Error::Runtime(_)) => {
            unreachable!("Runtime error at compile time")
        },
    };
    let range = match loc {
        Loc::Code { line, column, span, ..} => lsp::Range {
            start: lsp::Position {
                line: *line as u32 - 1,
                character: *column as u32 - 1,
            },
            end: lsp::Position {
                line: *line as u32 - 1,
                character: (*column + span.len()) as u32 - 1,
            }
        },
        Loc::External => {
            tracing::warn!("External error location");
            lsp::Range {
                start: lsp::Position { line: 0, character: 0, },
                end: lsp::Position { line: 0, character: 0, }
            }
        }
    };
    lsp::Diagnostic {
        range,
        severity: Some(lsp::DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: None,
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}