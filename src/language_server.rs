pub mod language_server_main;
mod instance;
mod io;
mod messages;
mod server;
mod feedback;

pub const URI_PLAYGROUND: &str = "playground://main";
//pub const URI_TEST: Uri = Uri::from_str("playground://test").unwrap();