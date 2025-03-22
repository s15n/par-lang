use lsp_types::{self as lsp, Uri};
use crate::par::language::Internal;
use crate::par::parse::{Loc, Name};
use crate::par::types::TypeError;
use crate::playground::Compiled;
use super::io::IO;

#[derive(Debug, Clone)]
pub enum CompileError {
    Compile(crate::playground::Error),
    Types(TypeError<Loc, Internal<Name>>),
}
pub type CompileResult = Result<Compiled, CompileError>;

pub struct Instance {
    uri: Uri,
    dirty: bool,
    compiled: Option<CompileResult>,
}

impl Instance {
    pub fn new(uri: Uri) -> Instance {
        Self {
            uri,
            dirty: true,
            compiled: None,
        }
    }

    pub fn handle_hover(&self, params: &lsp::HoverParams) -> Option<lsp::Hover> {
        tracing::info!("Handling hover request with params: {:?}", params);

        let payload = match &self.compiled {
            Some(Ok(compiled)) => format!("Compiled:\n{}", compiled.pretty.clone()),
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

    pub fn compile(&mut self, io: &IO) -> Result<(), CompileError> {
        if !self.dirty {
            tracing::debug!("No changes to compile");
            return Ok(());
        }
        let code = io.read(&self.uri);

        // todo: progress reporting
        let mut compiled = stacker::grow(32 * 1024 * 1024, || {
            Compiled::from_string(code, self.uri.to_string())
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