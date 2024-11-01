use crate::base::{Context, Value};

pub fn print_context<X: Clone + Ord + std::fmt::Debug + std::fmt::Display>(
    w: &mut impl std::fmt::Write,
    context: &Context<X>,
    level: usize,
) -> std::fmt::Result {
    for (name, values) in &context.variables {
        for value in values {
            write!(w, "\n")?;
            indent(w, level)?;
            write!(w, "{} = ", name)?;
            print_value(w, value, level)?;
        }
    }
    Ok(())
}

pub fn print_value<X: Clone + Ord + std::fmt::Debug + std::fmt::Display>(
    w: &mut impl std::fmt::Write,
    value: &Value<X>,
    level: usize,
) -> std::fmt::Result {
    write!(w, "{}", value)?;
    match value {
        Value::Suspend(context, _, _) => {
            print_context(w, context, level + 1)?;
        }
        _ => (),
    }
    Ok(())
}

fn indent(w: &mut impl std::fmt::Write, level: usize) -> std::fmt::Result {
    for _ in 0..level {
        write!(w, "  ")?;
    }
    Ok(())
}
