use lsp_types::{self as lsp, Uri};
use crate::playground::Compiled;
use super::io::IO;

pub struct Instance {
    uri: Uri,
    dirty: bool,
    compiled: Option<Result<Compiled, crate::playground::Error>>,
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
            _ => "Compilation failed".to_string(),
        };

        let hover = lsp::Hover {
            contents: lsp::HoverContents::Scalar(
                lsp::MarkedString::String(payload)
            ),
            range: None,
        };
        Some(hover)
    }

    pub fn compile(&mut self, io: &IO) {
        if !self.dirty {
            tracing::debug!("No changes to compile");
            return;
        }
        let code = io.read(&self.uri);
        self.compiled = stacker::grow(32 * 1024 * 1024, || {
            Some(Compiled::from_string(code))
        });
        let ok = self.compiled.as_ref().unwrap().is_ok();
        if ok {
            self.dirty = false;
            tracing::info!("Compilation successful");
        } else {
            tracing::info!("Compilation failed");
        }
    }

    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }
}