use std::collections::HashMap;
use lsp_types::Uri;

pub struct IO {
    //compilation: Compilation,
    virtual_files: VirtualFiles,
}

impl IO {
    pub fn new() -> IO {
        Self {
            //compilation: Compilation::new(),
            virtual_files: VirtualFiles::new(),
        }
    }

    pub fn update_file(&mut self, uri: &Uri, content: String) {
        self.virtual_files.update_file(uri, content);
    }

    pub fn read(&self, uri: &Uri) -> &str {
        self.virtual_files.read(uri)
    }

    /*pub fn compile(&mut self) {
        self.compilation.compile();
    }

    pub fn compiled(&self) -> Option<&Result<crate::playground::Compiled, crate::playground::Error>> {
        self.compilation.compiled()
    }*/
}

struct VirtualFiles {
    files: HashMap<Uri, String>,
}

impl VirtualFiles {
    fn new() -> VirtualFiles {
        Self {
            files: HashMap::new(),
        }
    }

    fn update_file(&mut self, uri: &Uri, content: String) {
        self.files.insert(uri.clone(), content);
    }

    fn read(&self, uri: &Uri) -> &str {
        let content = self.files.get(uri);
        match content {
            Some(content) => content.as_str(),
            None => "",
        }
    }
}