use std::fmt;

use crate::base::{Context, Value};

#[allow(unused)]
pub fn print_context<I: fmt::Display, X: fmt::Display>(
    w: &mut impl fmt::Write,
    context: &Context<I, X>,
    level: usize,
) -> std::fmt::Result {
    for (i, (name, value)) in context.variables.iter().enumerate() {
        if i > 0 {
            write!(w, "\n")?;
        }
        indent(w, level)?;
        write!(w, "{} = ", name)?;
        print_value(w, value, level)?;
    }
    Ok(())
}

pub fn print_value<I: fmt::Display, X: fmt::Display>(
    w: &mut impl fmt::Write,
    value: &Value<I, X>,
    level: usize,
) -> std::fmt::Result {
    write!(w, "{}", value)?;
    match value {
        Value::Suspend(context, _, _) if !context.variables.is_empty() => {
            write!(w, "\n")?;
            print_context(w, context, level + 1)?;
        }
        _ => (),
    }
    Ok(())
}

pub fn pretty(w: &mut impl fmt::Write, code: &str, level: usize) -> fmt::Result {
    let next_semicolon = str::find(code, ";");
    let next_open = str::find(code, "{");
    let next_close = str::find(code, "}");
    let closest = [next_semicolon, next_open, next_close]
        .into_iter()
        .flatten()
        .min();

    //indent(w, level)?;

    if closest.is_some() && closest == next_semicolon {
        let semicolon = closest.unwrap();
        //write!(w, "\n")?;
        indent(w, level)?;
        write!(w, "{};\n", &code[..semicolon].trim())?;
        return pretty(w, &code[semicolon + 1..].trim(), level);
    }
    if closest.is_some() && closest == next_open {
        let open = closest.unwrap();
        //write!(w, "\n")?;
        indent(w, level)?;
        write!(w, "{} {{\n", &code[..1.max(open) - 1].trim())?;
        return pretty(w, &code[open + 1..].trim(), level + 1);
    }
    if closest.is_some() && closest == next_close {
        let close = closest.unwrap();
        let until = &code[..1.max(close) - 1].trim();
        if !until.is_empty() {
            //write!(w, "\n")?;
            indent(w, level)?;
            write!(w, "{}\n", until)?;
        }
        //write!(w, "\n")?;
        indent(w, 1.max(level) - 1)?;
        write!(w, "}}\n")?;
        return pretty(w, &code[close + 1..], 1.max(level) - 1);
    }

    write!(w, "{}\n", code)
}

fn indent(w: &mut impl fmt::Write, level: usize) -> fmt::Result {
    for _ in 0..level {
        write!(w, "  ")?;
    }
    Ok(())
}
