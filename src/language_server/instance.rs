use std::fmt::Display;
use lsp_types::{self as lsp, Uri};
use crate::par::ast::{Declaration, Definition, Internal, Name, TypeDef};
use crate::par::lexer::Token;
use crate::par::location::Span;
use crate::par::types::TypeError;
use crate::playground::Compiled;
use super::io::IO;

#[derive(Debug, Clone)]
pub enum CompileError {
    Compile(crate::playground::Error),
    Types(TypeError<Internal<Name>>),
}
pub type CompileResult = Result<Compiled, CompileError>;

pub struct Instance {
    uri: Uri,
    dirty: bool,
    compiled: Option<CompileResult>,
    io: IO,
}

impl Instance {
    pub fn new(uri: Uri, io: IO) -> Instance {
        Self {
            uri,
            dirty: true,
            compiled: None,
            io
        }
    }

    pub fn handle_hover(&self, params: &lsp::HoverParams) -> Option<lsp::Hover> {
        tracing::info!("Handling hover request with params: {:?}", params);

        let pos = params.text_document_position_params.position;

        let payload = match &self.compiled {
            Some(Ok(compiled)) => {
                let mut message: Option<String> = Some(format!("{}:{}", pos.line, pos.character));

                let mut inside_item = false;

                for TypeDef { span, name, .. } in &compiled.program.type_defs {
                    if !is_inside(pos, span) {
                        continue;
                    }
                    inside_item = true;
                    message = Some(format!("Type: {}", name.to_string()));
                    break;
                }

                if !inside_item {
                    for Declaration { span, name, typ } in &compiled.program.declarations {
                        if !is_inside(pos, span) {
                            continue;
                        }
                        inside_item = true;
                        let mut msg = format!("Declaration: {}: ", name.to_string());
                        let indent = msg.len();
                        typ.pretty(&mut msg, indent + 1).unwrap();
                        message = Some(msg);
                        break;
                    }
                }

                if !inside_item {
                    for Definition { span, name, .. } in &compiled.program.definitions {
                        if !is_inside(pos, span) {
                            continue;
                        }
                        inside_item = true;
                        message = Some(format!("Declaration: {}", name.to_string()));
                        break;
                    }
                }

                if let Some(message) = message {
                    message
                } else {
                    format!("Compiled:\n{}", compiled.pretty.clone())
                }
            },
            Some(Err(e)) => format!("Compiled error: {:?}", e),
            None => "Not compiled".to_string(),
        };

        let hover = lsp::Hover {
            contents: lsp::HoverContents::Scalar(
                lsp::MarkedString::String(payload)
            ),
            range: None,
        };
        Some(hover)
    }

    pub fn compile(&mut self) -> Result<(), CompileError> {
        if !self.dirty {
            tracing::debug!("No changes to compile");
            return Ok(());
        }
        let code = self.io.read(&self.uri);

        // todo: progress reporting
        let mut compiled = stacker::grow(32 * 1024 * 1024, || {
            Compiled::from_string(&code.unwrap())
        }).map_err(|err| CompileError::Compile(err));
        match compiled {
            Ok(Compiled { checked: Err(err), .. }) => {
                compiled = Err(CompileError::Types(err));
            }
            _ => {}
        }

        let result = match &compiled {
            Ok(_) => {
                self.dirty = false;
                tracing::info!("Compilation successful");
                Ok(())
            }
            Err(err) => {
                tracing::info!("Compilation failed");
                Err(err.clone())
            }
        };

        self.compiled = Some(compiled);

        result
    }

    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }
}

fn is_inside(pos: lsp::Position, span: &Span) -> bool {
    let pos_row = pos.line as usize;
    let pos_column = pos.character as usize;

    let start = span.start;
    let end = span.end;

    !(pos_row < start.row || pos_row > end.row)
        && !(pos_row == start.row && pos_column < start.column)
        && !(pos_row == end.row && pos_column > end.column)
}