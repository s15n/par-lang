#![allow(dead_code)]

use parse::parse_program;

mod base;
mod interact;
mod parse;
mod print;

fn main() {
    match parse_program::<()>(CODE) {
        Err(e) => println!("{:?}", e),
        Ok(context) => {
            for (name, def) in context.statics {
                println!("define {} = {}\n", name, def);
            }
        }
    }
}

static CODE: &str = r#"
define yes_or_no = ask {
    ask[value];
    value.case {
        yes => { ask.yes; ask() }
        no  => { ask.no; ask() }
    }
}

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
