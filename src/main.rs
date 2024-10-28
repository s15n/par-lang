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
    let mut context = Context::empty();
    let mut process = Arc::new(merger());
    //let (mut context, mut process) = logger();

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
        send_("atm", tag_("Alice"),
        send_("atm", tag_("Withdraw"),
        receive_("atm", "money",
        log_(var_("money"),
        continue_("atm",
        halt_())))))),
    receive_("client", "account",
    receive_("client", "request",
    log_(var_("account"),
    send_("client", tag_("Bank"),
    log_(var_("request"),
    break_("client")))))))
}

fn merger() -> Process<&'static str> {
    let_("left", fork_(&[], "parent",
        let_("left", fork_(&[], "parent",
            send_("parent", tag_("A"),
            send_("parent", tag_("X"),
            break_("parent")))),
        let_("right", fork_(&[], "parent",
            send_("parent", tag_("B"),
            break_("parent"))),
        receive_("left", "value",
        receive_("left", "secret",
        send_("parent", var_("value"),
        send_("parent", var_("secret"),
        continue_("left",
        link_("parent", var_("right")))))))))),
    let_("right", fork_(&[], "parent",
        receive_("parent", "secret",
        let_("left", fork_(&[], "parent",
            receive_("parent", "secret",
            send_("parent", tag_("C"),
            log_(var_("secret"),
            break_("parent"))))),
        let_("right", fork_(&[], "parent",
            send_("parent", tag_("D"),
            break_("parent"))),
        send_("left", var_("secret"),
        receive_("left", "value",
        continue_("left",
        send_("parent", var_("value"),
        link_("parent", var_("right")))))))))),
    receive_("left", "p",
    receive_("left", "secret",
    receive_("left", "q",
    send_("right", var_("secret"),
    receive_("right", "r",
    receive_("right", "s",
    continue_("left",
    continue_("right",
    log_(var_("p"),
    log_(var_("q"),
    log_(var_("r"),
    log_(var_("s"),
    halt_()))))))))))))))
}

fn logger() -> (Context<&'static str>, Arc<Process<&'static str>>) {
    let logger = fork_(&[], "feed",
        handle_("feed", "done", break_("feed"),
        handle_("feed", "next",
        receive_("feed", "msg",
        log_(var_("msg"),
        link_("feed", static_("logger")))),
        exhaust_("feed"))));

    let feeder = let_("lg", static_("logger"),
        select_("lg", "next",
        send_("lg", tag_("A"),
        select_("lg", "next",
        send_("lg", tag_("B"),
        select_("lg", "next",
        send_("lg", tag_("C"),
        select_("lg", "done",
        continue_("lg",
        halt_())))))))));

    (Context {
        statics: OrdMap::from(vec![("logger", Arc::new(logger))]),
        variables: OrdMap::new(),
    }, Arc::new(feeder))
}
