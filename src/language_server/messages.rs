use lsp_server::Connection;

pub struct Message {

}

pub struct MessageBuffer {
    messages: Vec<Message>
}

impl MessageBuffer {
    pub fn new() -> Self {
        Self {
            messages: Vec::new()
        }
    }

    pub fn receive(&mut self, connection: &Connection) {
        let message = connection.receiver.recv().expect("Receiving message failed");
    }
}