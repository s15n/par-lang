fn test_equivalence(
    code: &str,
    equal_names: &[(bool, (&str, Option<&str>), (&str, Option<&str>))],
) -> Result<(), ()> {
    let mut code = String::from(code);
    for (idx, (should_check, (l, lt), (r, rt))) in equal_names.iter().enumerate() {
        if let Some(lt) = lt {
            code.push_str(&format!("declare left_{idx} : {lt}\n"));
        }
        if let Some(rt) = rt {
            code.push_str(&format!("declare right_{idx} : {rt}\n"));
        }
        code.push_str(&format!(
            "define left_{idx} = {l}\n define right_{idx} = {r}\n"
        ));
    }
    let program = crate::playground::Compiled::from_string(&code);
    let ic_compiled = program.unwrap().checked.unwrap().ic_compiled.unwrap();
    for (idx, (should_check, _, _)) in equal_names.iter().enumerate() {
        let left = ic_compiled
            .get_with_name(&format!("left_{idx}").into())
            .unwrap();
        let right = ic_compiled
            .get_with_name(&format!("right_{idx}").into())
            .unwrap();
        let mut net = ic_compiled.create_net();
        assert!(*should_check == crate::icombs::are_equivalent(&mut net, left, right))
    }
    Ok(())
}

#[test]
fn main() {
    test_equivalence(
        r#"
type Bool = either { .true! .false! }
declare true: Bool
declare false: Bool
define true = .true!
define false = .false!
declare not: [Bool] Bool
define not = [x: Bool] x {
  true? => .false!
  false? => .true!
}
declare not_comm: [Bool] Bool
define not_comm = [x] x {
  false? => .true!
  true? => .false!
}

declare bool_id: [Bool] Bool
define bool_id = [x: Bool] x {
  true? => .true!
  false? => .false!
}

declare compose: [[Bool] Bool] [[Bool] Bool] [Bool] Bool
define compose = [f] [g] [x] f(g(x))

  "#,
        &[
            (true, ("true", None), ("true", None)),
            (false, ("true", None), ("false", None)),
            (true, ("not", None), ("not_comm", None)),
            (true, ("not(true)", None), ("false", None)),
            (true, ("true", None), ("compose(not)(not)(true)", None)),
        ],
    )
    .unwrap()
}
