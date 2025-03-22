use std::collections::HashMap;
use lsp_server::{Connection};
use lsp_types::{self as lsp, InitializeParams, Uri};
use crate::language_server::feedback::{diagnostic_for_error, Feedback, FeedbackBookKeeper};
use crate::language_server::instance::Instance;
use super::{io::IO};

type Instances = HashMap<Uri, Instance>;

pub struct LanguageServer<'c> {
    connection: &'c Connection,
    initialize_params: InitializeParams,
    instances: Instances,
    feedback: FeedbackBookKeeper,
    io: IO,
}

impl<'c> LanguageServer<'c> {
    pub fn new(
        connection: &'c Connection
    ) -> LanguageServer {
        let initialize_params = initialize_lsp(connection);
        Self {
            connection,
            initialize_params,
            instances: HashMap::new(),
            feedback: FeedbackBookKeeper::new(),
            io: IO::new(),
        }
    }

    pub fn run(&mut self) {
        loop {
            let message = self.connection.receiver.recv().expect("Receiving message failed");
            tracing::debug!("Received message: {:?}", message);

            match message {
                lsp_server::Message::Request(r) if self.should_shutdown(&r) => break,
                lsp_server::Message::Request(r) => self.handle_request(r),
                lsp_server::Message::Response(r) => self.handle_response(r),
                lsp_server::Message::Notification(n) => self.handle_notification(n),
            }
        }
    }

    fn handle_request(&mut self, request: lsp_server::Request) {
        use lsp::request::{HoverRequest, Request};

        let request_id = request.id.clone();
        let method = request.method.as_str();
        let payload = match method {
            HoverRequest::METHOD => {
                let params = extract_request::<HoverRequest>(request);
                //self.handle_hover(params)
                self.handle_request_instance(
                    &params.text_document_position_params.text_document.uri,
                    |instance, io| instance.handle_hover(&params, io)
                )
            }
            _ => {
                tracing::warn!("Unhandled request: {:?}", request);
                return
            }
        };

        let response = lsp_server::Response::new_ok(request_id, payload);
        tracing::debug!("Responding {:?}", response);
        self.connection.sender
            .send(lsp_server::Message::Response(response))
            .expect("Sending response failed");

        self.publish_feedback();
    }

    fn handle_response(&self, response: lsp_server::Response) {
        tracing::warn!("Unhandled response: {:?}", response);
    }

    fn handle_notification(&mut self, notification: lsp_server::Notification) {
        use lsp::notification::{DidChangeTextDocument, DidOpenTextDocument, Notification};

        let method = notification.method.as_str();
        match method {
            DidOpenTextDocument::METHOD => {
                let params = extract_notification::<DidOpenTextDocument>(notification);
                self.cache_file(
                    &params.text_document.uri,
                    params.text_document.text,
                );
            }
            DidChangeTextDocument::METHOD => {
                let params = extract_notification::<DidChangeTextDocument>(notification);
                if let Some(last_change) = params.content_changes.into_iter().last() {
                    self.cache_file(
                        &params.text_document.uri,
                        last_change.text,
                    );
                }
            }
            _ => {
                tracing::warn!("Unhandled notification: {:?}", notification);
                return
            }
        }
    }

    fn should_shutdown(&self, request: &lsp_server::Request) -> bool {
        self.connection.handle_shutdown(request).expect("Protocol error while handling shutdown")
    }

    fn handle_request_instance<R>(
        &mut self,
        uri: &Uri,
        handler: impl FnOnce(&mut Instance, &IO) -> R
    ) -> R {
        let instance = instance_for(&mut self.instances, uri);

        let compile_result = instance.compile(&self.io);
        let response = handler(instance, &self.io);

        let mut feedback = self.feedback.cleanup();
        match compile_result {
            Ok(_) => { /* warnings */ },
            Err(err) => {
                feedback.add_diagnostic(uri.clone(), diagnostic_for_error(&err));
            }
        };

        response
    }

    fn publish_feedback(&mut self) {
        use lsp::notification::{PublishDiagnostics, Notification};

        for (uri, diagnostics) in self.feedback.diagnostics() {
            let params = lsp_types::PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostics.clone(),
                version: None,
            };
            let notification = lsp_server::Notification {
                method: PublishDiagnostics::METHOD.to_string(),
                params: serde_json::to_value(params).unwrap(),
            };

            self.connection.sender
                .send(lsp_server::Message::Notification(notification))
                .expect("Sending notification failed");
        }
    }

    fn cache_file(&mut self, uri: &Uri, text: String) {
        tracing::info!("Caching file: {:?}", uri);
        self.io.update_file(uri, text);
        instance_for(&mut self.instances, uri).mark_dirty();
    }
}

fn initialize_lsp(connection: &Connection) -> InitializeParams {
    let server_capabilities = lsp::ServerCapabilities {
        text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(lsp::TextDocumentSyncKind::FULL)),
        hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
        ..lsp::ServerCapabilities::default()
    };
    let server_capabilities_json = serde_json::to_value(&server_capabilities).unwrap();
    let result_json = connection
        .initialize(server_capabilities_json)
        .expect("Initializing LSP failed");
    let result: InitializeParams = serde_json::from_value(result_json).unwrap();
    tracing::debug!("Initialized LSP with params: {:?}", result);
    result
}

fn instance_for<'a>(instances: &'a mut Instances, uri: &'a Uri) -> &'a mut Instance {
    instances.entry(uri.clone()).or_insert_with(|| {
        Instance::new(uri.clone())
    })
}

fn extract_request<R>(request: lsp_server::Request) -> R::Params
where
    R: lsp::request::Request,
{
    let (_, params) = request
        .extract::<R::Params>(R::METHOD)
        .expect(format!("Could not extract request {}", R::METHOD).as_str());
    params
}

fn extract_notification<N>(notification: lsp_server::Notification) -> N::Params
where
    N: lsp::notification::Notification,
{
    notification
        .extract::<N::Params>(N::METHOD)
        .expect(format!("Could not extract notification {}", N::METHOD).as_str())
}