#![allow(dead_code)]

use std::sync::Arc;

use base::{Context, Machine, Process, notation::*};
use im::OrdMap;
use print::print_context;

mod base;
mod print;

fn main() {
    let mut machine = Machine {
        log: Box::new(|tag: &'static str| println!("LOG: {}", tag)),
    };
    /*let mut context = Context::empty();
    let mut process = Arc::new(merger());*/
    let (mut context, mut process) = logger();

    loop {
        println!("{}", process);

        if matches!(process.as_ref(), Process::Halt) {
            break;
        }

        let mut w = String::new();
        let _ = print_context(&mut w, &context, 0);
        println!("{}", w);
        println!("\n-----");

        (context, process) = match machine.step(context, process) {
            Err(e) => break println!("ERROR: {:?}", e),
            Ok(p) => p,
        };

        println!("-----");
    }
}

fn atm() -> Process<&'static str> {
    let_("client", fork_(&[], "atm",
        send_to_("atm", tag_("Alice"),
        send_to_("atm", tag_("Withdraw"),
        receive_from_("atm", "money",
        log_(var_("money"),
        continue_from_("atm",
        halt_())))))),
    receive_from_("client", "account",
    receive_from_("client", "request",
    log_(var_("account"),
    send_to_("client", tag_("Bank"),
    log_(var_("request"),
    break_to_("client")))))))
}

fn merger() -> Process<&'static str> {
    let_("left", fork_(&[], "parent",
        let_("left", fork_(&[], "parent",
            send_to_("parent", tag_("A"),
            send_to_("parent", tag_("X"),
            break_to_("parent")))),
        let_("right", fork_(&[], "parent",
            send_to_("parent", tag_("B"),
            break_to_("parent"))),
        receive_from_("left", "value",
        receive_from_("left", "secret",
        send_to_("parent", var_("value"),
        send_to_("parent", var_("secret"),
        continue_from_("left",
        link_("parent", var_("right")))))))))),
    let_("right", fork_(&[], "parent",
        receive_from_("parent", "secret",
        let_("left", fork_(&[], "parent",
            receive_from_("parent", "secret",
            send_to_("parent", tag_("C"),
            log_(var_("secret"),
            break_to_("parent"))))),
        let_("right", fork_(&[], "parent",
            send_to_("parent", tag_("D"),
            break_to_("parent"))),
        send_to_("left", var_("secret"),
        receive_from_("left", "value",
        continue_from_("left",
        send_to_("parent", var_("value"),
        link_("parent", var_("right")))))))))),
    receive_from_("left", "p",
    receive_from_("left", "secret",
    receive_from_("left", "q",
    send_to_("right", var_("secret"),
    receive_from_("right", "r",
    receive_from_("right", "s",
    continue_from_("left",
    continue_from_("right",
    log_(var_("p"),
    log_(var_("q"),
    log_(var_("r"),
    log_(var_("s"),
    halt_()))))))))))))))
}

fn logger() -> (Context<&'static str>, Arc<Process<&'static str>>) {
    let logger = fork_(&[], "feed",
        handle_in_("feed", "done", break_to_("feed"),
        handle_in_("feed", "next",
        receive_from_("feed", "msg",
        log_(var_("msg"),
        link_("feed", static_("logger")))),
        exhaust_in_("feed"))));

    let feeder = let_("lg", static_("logger"),
        select_on_("lg", "next",
        send_to_("lg", tag_("A"),
        select_on_("lg", "next",
        send_to_("lg", tag_("B"),
        select_on_("lg", "next",
        send_to_("lg", tag_("C"),
        select_on_("lg", "done",
        continue_from_("lg",
        halt_())))))))));

    (Context {
        statics: OrdMap::from(vec![("logger", Arc::new(logger))]),
        variables: OrdMap::new(),
    }, Arc::new(feeder))
}
