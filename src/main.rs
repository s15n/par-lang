#![allow(dead_code)]

use std::sync::Arc;

use base::{Context, Machine, Process, notation::*};
use im::OrdMap;
use parse::parse_program;
use print::print_context;

mod base;
mod print;
mod parse;

fn main() {
    match parse_program(CODE) {
        Err(e) => println!("{:?}", e),
        Ok(context) => {
            for (name, def) in context.statics {
                println!("define {} = {}\n", name, def);
            }
        }
    }
    /*let mut machine = Machine {
        log: Box::new(|tag: &'static str| println!("LOG: {}", tag)),
    };
    let mut context = Context::empty();
    let mut process = Arc::new(merger());
    let (mut context, mut process) = stack();

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
    }*/
}

fn atm() -> Process<&'static str> {
    let_("client", fork_(&[], "atm",
        send_("atm", tag_("Alice"),
        send_("atm", tag_("Withdraw"),
        receive_("atm", "money",
        log_(ref_("money"),
        continue_("atm",
        halt_())))))),
    receive_("client", "account",
    receive_("client", "request",
    log_(ref_("account"),
    send_("client", tag_("Bank"),
    log_(ref_("request"),
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
        send_("parent", ref_("value"),
        send_("parent", ref_("secret"),
        continue_("left",
        link_("parent", ref_("right")))))))))),
    let_("right", fork_(&[], "parent",
        receive_("parent", "secret",
        let_("left", fork_(&[], "parent",
            receive_("parent", "secret",
            send_("parent", tag_("C"),
            log_(ref_("secret"),
            break_("parent"))))),
        let_("right", fork_(&[], "parent",
            send_("parent", tag_("D"),
            break_("parent"))),
        send_("left", ref_("secret"),
        receive_("left", "value",
        continue_("left",
        send_("parent", ref_("value"),
        link_("parent", ref_("right")))))))))),
    receive_("left", "p",
    receive_("left", "secret",
    receive_("left", "q",
    send_("right", ref_("secret"),
    receive_("right", "r",
    receive_("right", "s",
    continue_("left",
    continue_("right",
    log_(ref_("p"),
    log_(ref_("q"),
    log_(ref_("r"),
    log_(ref_("s"),
    halt_()))))))))))))))
}

fn logger() -> (Context<&'static str>, Arc<Process<&'static str>>) {
    let logger = fork_(&[], "feed",
        case_exhaust_("feed", &[
            ("done",
                break_("feed")),
            ("next",
                receive_("feed", "msg",
                log_(ref_("msg"),
                link_("feed", ref_("logger")))))]));

    let feeder = let_("lg", ref_("logger"),
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

fn stack() -> (Context<&'static str>, Arc<Process<&'static str>>) {
    let bot = fork_(&[], "items",
        case_exhaust_("items", &[
            ("pop",
                select_("items", "empty",
                break_("items"))),
            ("push",
                let_("above", ref_("top"),
                send_("above", ref_("bot"),
                link_("items", ref_("above")))))]));

    let top = fork_(&[], "items",
        receive_("items", "below",
        receive_("items", "pushed",
        case_exhaust_("items", &[
            ("pop",
                select_("items", "item",
                send_("items", ref_("pushed"),
                link_("items", ref_("below"))))),
            ("push",
                let_("above", ref_("top"),
                let_("self", ref_("top"),
                send_("self", ref_("below"),
                send_("self", ref_("pushed"),
                send_("above", ref_("self"),
                link_("items", ref_("above"))))))))]))));

    let program = let_("stack", ref_("bot"),
        select_("stack", "push",
        send_("stack", tag_("A"),
        select_("stack", "pop",
        case_exhaust_("stack", &[("item",
        receive_("stack", "value",
        log_(ref_("value"),
        select_("stack", "push",
        send_("stack", tag_("B"),
        select_("stack", "push",
        send_("stack", tag_("C"),
        select_("stack", "push",
        send_("stack", tag_("D"),
        select_("stack", "pop",
        case_exhaust_("stack", &[("item",
        receive_("stack", "value",
        log_(ref_("value"),
        select_("stack", "pop",
        case_exhaust_("stack", &[("item",
        receive_("stack", "value",
        log_(ref_("value"),
        select_("stack", "pop",
        case_exhaust_("stack", &[("item",
        receive_("stack", "value",
        log_(ref_("value"),
        select_("stack", "pop",
        case_exhaust_("stack", &[("empty",
        continue_("stack",
        halt_()))])))))])))))])))))])))))))))))])))));

    (Context {
        statics: OrdMap::from(vec![
            ("bot", Arc::new(bot)),
            ("top", Arc::new(top)),
        ]),
        variables: OrdMap::new(),
    }, Arc::new(program))
}

static CODE: &'static str = r#"
define drained = items {
    items.case {
        pop => {
            items.empty;
            items()
        }
        push => {
            let above = stacked;
            above(drained);
            items <> above
        }
    }
}

define stacked = items {
    items[under];
    items[top];
    items.case {
        pop => {
            items.item;
            items(top);
            items <> under
        }
        push => {
            let above = stacked;
            let self = stacked;
            self(under);
            self(top);
            above(self);
            items <> above
        }
    }
}
"#;
