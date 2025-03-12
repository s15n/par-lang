use std::{
    ops::BitAnd,
    sync::{Arc, Mutex},
};

use futures::{
    channel::oneshot::channel,
    future::{join, BoxFuture},
    task::{Spawn, SpawnExt},
    FutureExt,
};

use crate::{
    icombs::{
        readback::{ReadbackResult, SharedState},
        Net,
    },
    par::{
        language::Internal,
        parse::{Loc, Name, Program},
        process::Expression,
        types::Type,
    },
    playground,
    readback::{NameRequiredTraits, ReadbackState},
    spawn::TokioSpawn,
};
type Prog<Name> = Arc<Program<Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>>;

#[derive(Debug, Default, Clone)]
pub enum ReadbackPattern<Name: Clone> {
    #[default]
    Break,
    Continue,
    Send(Box<ReadbackPattern<Name>>, Box<ReadbackPattern<Name>>),
    Receive(Box<ReadbackPattern<Name>>, Box<ReadbackPattern<Name>>),
    Either(Name, Box<ReadbackPattern<Name>>),
    // After choosing this, the rest of the pattern should match this.
    Choice(Name, Box<ReadbackPattern<Name>>),
    Any,
}

#[derive(Debug)]
pub struct TestingResult {
    checked: bool,
}

impl From<bool> for TestingResult {
    fn from(checked: bool) -> Self {
        TestingResult { checked }
    }
}

impl BitAnd for TestingResult {
    type Output = TestingResult;
    fn bitand(self, rhs: Self) -> Self::Output {
        (self.checked & rhs.checked).into()
    }
}

impl FromIterator<TestingResult> for TestingResult {
    fn from_iter<T: IntoIterator<Item = TestingResult>>(iter: T) -> Self {
        iter.into_iter().fold(false.into(), |a, b| a & b)
    }
}

impl From<(TestingResult, TestingResult)> for TestingResult {
    fn from((a, b): (TestingResult, TestingResult)) -> Self {
        a & b
    }
}

// testing works by matching each readback result against a pattern.

pub struct TestingState<Name: NameRequiredTraits> {
    pub shared: SharedState,
    pub spawner: Arc<dyn Spawn + Send + Sync>,
    pub net: Arc<Mutex<Net>>,
    pub prog: Prog<Name>,
}

impl<Name: NameRequiredTraits> TestingState<Name> {
    fn matches(
        &self,
        result: ReadbackResult<Name>,
        pattern: ReadbackPattern<Name>,
    ) -> BoxFuture<TestingResult> {
        async move {
            match result {
                ReadbackResult::Break => matches!(pattern, ReadbackPattern::Break).into(),
                ReadbackResult::Continue => matches!(pattern, ReadbackPattern::Continue).into(),
                ReadbackResult::Send(left, right) => {
                    if let ReadbackPattern::Send(lp, rp) = pattern {
                        join(
                            self.clone().matches(*left, *lp),
                            self.clone().matches(*right, *rp),
                        )
                        .await
                        .into()
                    } else {
                        false.into()
                    }
                }
                ReadbackResult::Receive(left, right) => {
                    if let ReadbackPattern::Receive(lp, rp) = pattern {
                        join(
                            self.clone().matches(*left, *lp),
                            self.clone().matches(*right, *rp),
                        )
                        .await
                        .into()
                    } else {
                        false.into()
                    }
                }
                ReadbackResult::Either(name, payload) => {
                    let ReadbackPattern::Either(pat_name, pattern_payload) = pattern else {
                        return false.into();
                    };
                    if name != pat_name {
                        return false.into();
                    }
                    self.clone().matches(*payload, *pattern_payload).await
                }
                ReadbackResult::Choice(ctx, options) => {
                    let ReadbackPattern::Choice(name, pattern_payload) = pattern else {
                        return false.into();
                    };
                    let payload = self.shared.choose_choice(name, ctx, options, &self.spawner);
                    self.matches(ReadbackResult::Halted(payload), *pattern_payload)
                        .await
                }
                ReadbackResult::Halted(mut tree) => {
                    tree.ty = crate::readback::prepare_type_for_readback(
                        &self.prog,
                        tree.ty,
                        &Default::default(),
                    );
                    let res = self.shared.read_with_type(tree).await;
                    self.matches(res, pattern).await
                }

                ReadbackResult::Variable(_) => todo!(),
                _ => todo!(),
            }
        }
        .boxed()
    }
}

pub struct TestCase {
    code: String,
    ty: String,
    pattern: ReadbackPattern<Internal<Name>>,
}

pub struct TestFamily {
    name: String,
    prelude: String,
    tests: Vec<TestCase>,
}

#[tokio::test]
async fn test_whole_programs() -> Result<(), String> {
    let mut failed = false;
    let test_families = vec![TestFamily {
        name: "hello world".to_owned(),
        prelude: r#"
            type Bool  = either { .true!, .false! }
            type Option<T>  = either { .some T, .none! }
            dec true: Bool
            def true = .true!
            dec false: Bool
            def false = .false!

            dec not: [Bool] Bool
            def not = [x] x {
                true? => .false!
                false? => .true!
            }
        "#
        .to_owned(),
        tests: vec![TestCase {
            code: "not(false)".to_owned(),
            ty: "Bool".to_owned(),
            pattern: ReadbackPattern::Either(
                "false".to_owned().into(),
                Box::new(ReadbackPattern::Break),
            ),
        }],
    }];
    for i in test_families {
        eprintln!("testing family: {}", i.name);
        use core::fmt::Write;
        // First, build the source code
        let mut source = String::new();
        write!(&mut source, "{}", i.prelude).unwrap();
        for (idx, j) in i.tests.iter().enumerate() {
            write!(&mut source, "dec test_{}: {}\n", idx, j.ty).unwrap();
            write!(&mut source, "def test_{} = {}\n", idx, j.code).unwrap();
        }
        let compiled = playground::Compiled::from_string(&source).unwrap();
        let checked = compiled.checked.unwrap();
        let ic_compiled = checked.ic_compiled.unwrap();
        let prog = (checked.program.clone());
        let mut spawner = Arc::new(TokioSpawn);
        let mut net = Arc::new(Mutex::new(ic_compiled.create_net()));
        let mut shared = SharedState::with_net(net.clone());
        let (_tx, rx) = channel();
        spawner.spawn(shared.create_net_reducer(rx)).unwrap();
        let mut state = TestingState {
            shared,
            spawner,
            net: net.clone(),
            prog: prog.clone(),
        };
        for (idx, j) in i.tests.iter().enumerate() {
            let def_name = format!("test_{}", idx);
            let def_name = Internal::Original(def_name.into());
            let mut tree = ic_compiled.get_with_name(&def_name).unwrap();
            net.lock().unwrap().freshen_variables(&mut tree);
            let testing_result = state
                .matches(
                    ReadbackResult::Halted(
                        tree.with_type(
                            prog.declarations
                                .get(&def_name)
                                .unwrap()
                                .as_ref()
                                .unwrap()
                                .clone(),
                        ),
                    ),
                    j.pattern.clone(),
                )
                .await;
            if testing_result.checked {
                eprintln!("  ok : `{}`", j.code);
            } else {
                eprintln!("  err: `{}`", j.code);
                failed = true;
            }
        }
    }
    if failed {
        Err("Some tests failed!".to_string())
    } else {
        Ok(())
    }
}
