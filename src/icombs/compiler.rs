use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

use crate::par::parse::Loc;
use indexmap::IndexMap;
use std::hash::Hash;

use crate::par::{
    language::Internal,
    parse::{Name, Program},
    process::{Captures, Command, Expression, Process},
    types::Type,
};

use super::net::{Net, Tree};

type Prog<Name> = Program<Name, Arc<Expression<Loc, Name, Type<Loc, Name>>>>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum VariableKind {
    // Can only be used once.
    Linear,
    // Replicable, but needs no dereliction
    Replicable,
    // Replicable, and needs dereliction
    Boxed,
}

#[derive(Debug, Clone)]
pub struct TypedTree<Name: Clone> {
    tree: Tree,
    ty: Type<Loc, Name>,
}

pub struct Compiler<'program, Name: Debug + Clone + Eq + Hash + Ord> {
    net: Net,
    vars: IndexMap<Name, (TypedTree<Name>, VariableKind)>,
    program: &'program Prog<Name>,
    global_name_to_id: IndexMap<Name, usize>,
    id_to_ty: Vec<Type<Loc, Name>>,
    id_to_package: Vec<Tree>,
}

impl Tree {
    fn with_type<Name: Clone>(self, ty: Type<Loc, Name>) -> TypedTree<Name> {
        TypedTree { tree: self, ty }
    }
}

fn multiplex_trees(mut trees: Vec<Tree>) -> Tree {
    if trees.len() == 0 {
        Tree::e()
    } else if trees.len() == 1 {
        trees.pop().unwrap()
    } else {
        let new_trees = trees.split_off(trees.len() / 2);
        Tree::c(multiplex_trees(trees), multiplex_trees(new_trees))
    }
}

impl<'program, Name: Debug + Clone + Eq + Hash + Display + Ord> Compiler<'program, Name> {
    fn compile_global(&mut self, name: &Name) -> Option<TypedTree<Name>> {
        if let Some(id) = self.global_name_to_id.get(name) {
            let ty = self.id_to_ty.get(*id).unwrap().clone();
            return Some(TypedTree {
                tree: Tree::Package(*id),
                ty,
            });
        };
        let global = self.program.definitions.get(name)?;
        let old_net = core::mem::take(&mut self.net);

        let id = self.id_to_package.len();
        self.global_name_to_id.insert(name.clone(), id);
        let tree = self.with_captures(&Captures::default(), |this| {
            let mut s = String::new();
            global.pretty(&mut s, 0).unwrap();
            println!("{}", s);
            this.compile_expression(global.as_ref())
        });
        self.net.ports.push_back(tree.tree);
        self.net.packages = Arc::new(self.id_to_package.clone().into_iter().enumerate().collect());
        println!("{}", self.net.show());
        self.net.normal();
        let package_contents = self.net.ports.pop_back().unwrap();
        self.id_to_ty.push(tree.ty.clone());
        self.id_to_package.push(package_contents);
        self.net = old_net;

        Some(Tree::Package(id).with_type(tree.ty))
    }
    fn with_captures<T>(
        &mut self,
        captures: &Captures<Loc, Name>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        println!("{:?}", captures);
        let mut vars = IndexMap::new();
        for (name, _) in captures.names.iter() {
            let (tree, kind) = self.use_variable(name);
            vars.insert(name.clone(), (tree, kind));
        }
        core::mem::swap(&mut vars, &mut self.vars);
        let t = f(self);
        core::mem::swap(&mut vars, &mut self.vars);
        // drop all replicables
        for (name, (value, kind)) in vars.into_iter() {
            if kind == VariableKind::Linear {
                panic!("some variables were not closed: {}", name);
            } else {
                self.net.link(value.tree, Tree::e());
            }
        }
        t
    }
    fn bind_variable(&mut self, name: &Name, tree: TypedTree<Name>) {
        assert!(self
            .vars
            .insert(name.clone(), (tree, VariableKind::Linear))
            .is_none())
    }
    fn instantiate_variable(&mut self, name: &Name) -> TypedTree<Name> {
        let (value, kind) = self.use_variable(name);
        if kind == VariableKind::Boxed {
            todo!()
        }
        value
    }
    fn use_variable(&mut self, name: &Name) -> (TypedTree<Name>, VariableKind) {
        self.show_state();
        if let Some((tree, kind)) = self.vars.swap_remove(name) {
            match kind {
                VariableKind::Linear => (tree, kind),
                kind => {
                    let (w0, w1) = self.net.create_wire();
                    let (v0, v1) = self.net.create_wire();
                    self.net.link(Tree::d(v0, w0), tree.tree);
                    self.vars.insert(
                        name.clone(),
                        (
                            TypedTree {
                                tree: w1,
                                ty: tree.ty.clone(),
                            },
                            kind,
                        ),
                    );
                    (
                        TypedTree {
                            tree: v1,
                            ty: tree.ty.clone(),
                        },
                        kind,
                    )
                }
            }
        } else if self.compile_global(name).is_some() {
            let value = self.compile_global(name).unwrap();
            let v = (value.clone(), VariableKind::Replicable);
            self.vars.insert(name.clone(), v.clone());
            v
        } else {
            panic!("unknown variable {}", name)
        }
    }
    fn create_typed_wire(&mut self, t: Type<Loc, Name>) -> (TypedTree<Name>, TypedTree<Name>) {
        let (v0, v1) = self.net.create_wire();
        (
            TypedTree {
                tree: v0,
                ty: t.clone(),
            },
            TypedTree {
                tree: v1,
                ty: t.dual(),
            },
        )
    }
    // cast an expression into another type.
    fn cast(&mut self, from: TypedTree<Name>, to: Type<Loc, Name>) -> TypedTree<Name> {
        match (&from.ty, &to) {
            (Type::Send(_, from_fst, from_snd), Type::Send(_, to_fst, to_snd)) => {
                let (fst0, fst1) = self.create_typed_wire(from_fst.as_ref().clone());
                let (snd0, snd1) = self.create_typed_wire(from_snd.as_ref().clone());
                self.net.link(from.tree, Tree::c(fst1.tree, snd1.tree));
                Tree::c(
                    self.cast(fst0, to_fst.as_ref().clone()).tree,
                    self.cast(snd0, to_snd.as_ref().clone()).tree,
                )
                .with_type(to)
            }
            (Type::Receive(_, from_fst, from_snd), Type::Receive(_, to_fst, to_snd)) => {
                let (fst0, fst1) = self.create_typed_wire(from_fst.dual());
                let (snd0, snd1) = self.create_typed_wire(from_snd.as_ref().clone());
                self.net.link(from.tree, Tree::c(fst1.tree, snd1.tree));
                Tree::c(
                    self.cast(fst0, to_fst.dual()).tree,
                    self.cast(snd0, to_snd.as_ref().clone()).tree,
                )
                .with_type(to)
            }
            (Type::Choice(_, from_choices), Type::Choice(_, to_choices)) => {
                // to_choices <= from_choices
                // Construct a choice object that, on each case,
                // picks the correct choice from from_choicse
                // the context is simply the from_choices object
                let mut from_choices = from_choices.clone();
                let mut to_choices = to_choices.clone();
                from_choices.sort_keys();
                to_choices.sort_keys();
                let mut cases = vec![];
                let out_of = from_choices.len();
                for (k, ty) in to_choices {
                    let (x0, x1) = self.create_typed_wire(ty.clone());

                    let index = from_choices
                        .get_index_of(&k)
                        .expect("Can't cast between these choices");
                    cases.push((self.either_instance(x1.tree, index, out_of), x0.tree));
                }
                self.choice_instance(from.tree, cases).with_type(to)
            }

            (Type::Either(_, from_either), Type::Either(_, to_either)) => {
                // from_either <= to_either
                let mut from_either = from_either.clone();
                let mut to_either = to_either.clone();
                from_either.sort_keys();
                to_either.sort_keys();

                let (v0, v1) = self.create_typed_wire(to.clone());
                let mut cases = vec![];
                let out_of = to_either.len();
                for (k, ty) in from_either {
                    let (x0, x1) = self.create_typed_wire(ty.clone());

                    let index = to_either
                        .get_index_of(&k)
                        .expect("Can't cast between these choices");
                    cases.push((self.either_instance(x1.tree, index, out_of), x0.tree));
                }
                let either = self.choice_instance(v1.tree, cases);
                self.net.link(from.tree, either);
                v0
            }

            (Type::Break(_), Type::Break(_)) => from,
            (Type::Continue(_), Type::Continue(_)) => from,
            (Type::Name(_, name, args), to) => {
                let ty = self.dereference_type_def(name, args);
                self.cast(from.tree.with_type(ty), to.clone())
            }
            (Type::DualName(_, name, args), to) => {
                let ty = self.dereference_type_def(name, args).dual();
                self.cast(from.tree.with_type(ty), to.clone())
            }
            (_, Type::Name(_, name, args)) => {
                let ty = self.dereference_type_def(name, args);
                self.cast(from, ty)
            }
            (_, Type::DualName(_, name, args)) => {
                let ty = self.dereference_type_def(name, args).dual();
                self.cast(from, ty)
            }
            (a, b) => todo!("Can't cast from {:?} to {:?}", a, b),
        }
    }
    fn dereference_type_def(&mut self, name: &Name, args: &[Type<Loc, Name>]) -> Type<Loc, Name> {
        let def = self.program.type_defs.get(name).unwrap();
        let mut typ = def.1.clone();
        for (src, tgt) in def.0.iter().zip(args) {
            typ = typ.substitute(&src, tgt).unwrap();
        }
        typ
    }
    fn compile_expression(
        &mut self,
        expr: &Expression<Loc, Name, Type<Loc, Name>>,
    ) -> TypedTree<Name> {
        match expr {
            Expression::Reference(_, name, ty) => {
                let inner = self.instantiate_variable(name);
                self.cast(inner, ty.clone())
            }
            Expression::Fork(_, captures, name, _, typ, proc) => {
                self.with_captures(captures, |this| {
                    let (v0, v1) = this.create_typed_wire(typ.clone());
                    this.bind_variable(name, v0);
                    println!("Pre-fork: name = {}", this.net.show_tree(&v1.tree));
                    this.compile_process(proc);
                    println!("Post-fork: name = {}", this.net.show_tree(&v1.tree));
                    v1
                })
            }
        }
    }
    fn compile_process(&mut self, proc: &Process<Loc, Name, Type<Loc, Name>>) {
        {
            let mut s = String::new();
            proc.pretty(&mut s, 1);
            println!("Compile process: {}", s);
            self.show_state();
        }
        match proc {
            Process::Let(_, key, _, _, value, rest) => {
                let value = self.compile_expression(value);
                println!("Var inserted: {}", key);
                self.vars.insert(key.clone(), (value, VariableKind::Linear));
                self.compile_process(rest);
            }

            Process::Do(_, name, target_ty, command) => {
                self.compile_command(name.clone(), target_ty.clone(), command)
            }
            _ => todo!(),
        }
    }
    fn show_state(&mut self) {
        println!("Variables:");
        for (name, (value, kind)) in &self.vars {
            println!(
                "    {}: {:?} = {}",
                name,
                value.ty,
                self.net.show_tree(&value.tree)
            )
        }
        println!("Net:");
        println!("{}", self.net.show_indent(1));
    }
    fn link_typed(&mut self, a: TypedTree<Name>, b: TypedTree<Name>) {
        self.net.link(a.tree, b.tree);
    }
    fn either_instance(&mut self, tree: Tree, index: usize, out_of: usize) -> Tree {
        let (w0, w1) = self.net.create_wire();
        let mut trees: Vec<_> = std::iter::repeat(Tree::e()).take(out_of).collect();
        *trees.get_mut(index).unwrap() = Tree::c(w1, tree);
        Tree::c(w0, multiplex_trees(trees))
    }
    fn choice_instance(&mut self, ctx_out: Tree, cases: Vec<(Tree, Tree)>) -> Tree {
        Tree::c(
            ctx_out,
            multiplex_trees(cases.into_iter().map(|(a, b)| Tree::c(a, b)).collect()),
        )
    }
    fn normalize_type(&mut self, ty: Type<Loc, Name>) -> Type<Loc, Name> {
        match ty {
            Type::Name(loc, name, args) => self.dereference_type_def(&name, &args),
            Type::DualName(loc, name, args) => self.dereference_type_def(&name, &args).dual(),
            Type::Either(a, mut index_map) => {
                index_map.sort_keys();
                Type::Either(a, index_map)
            }
            Type::Choice(a, mut index_map) => {
                index_map.sort_keys();
                Type::Choice(a, index_map)
            }
            a => a,
        }
    }
    fn compile_command(
        &mut self,
        name: Name,
        ty: Type<Loc, Name>,
        cmd: &Command<Loc, Name, Type<Loc, Name>>,
    ) {
        match cmd {
            Command::Link(a) => {
                let a = self.compile_expression(a);
                let b = self.instantiate_variable(&name);
                self.link_typed(a, b);
            }
            // types get erased.
            Command::SendType(_, process) => self.compile_process(process),
            Command::ReceiveType(_, process) => self.compile_process(process),
            Command::Send(expr, process) => {
                // < name(expr) process >
                // ==
                // name = free
                // free = (name < expr >)
                // < process >
                let a = self.instantiate_variable(&name);
                let a = self.cast(a, ty);
                let Type::Receive(_, arg_type, ret_type) = a.ty else {
                    unreachable!()
                };
                let expr = self.compile_expression(expr);
                let expr = self.cast(expr, *arg_type);

                let (v0, v1) = self.create_typed_wire(*ret_type);
                self.bind_variable(&name, v0);
                //
                self.net.link(Tree::c(v1.tree, expr.tree), a.tree);
                self.compile_process(process);
            }
            Command::Receive(target, _, process) => {
                // < name[target] process >
                // ==
                // name = free
                // free = (name target)
                // < process >
                let a = self.instantiate_variable(&name);
                let a = self.cast(a, ty);
                let Type::Send(_, arg_type, ret_type) = a.ty else {
                    unreachable!()
                };
                let (v0, v1) = self.create_typed_wire(*arg_type);
                let (w0, w1) = self.create_typed_wire(*ret_type);
                self.bind_variable(&name, w0);
                self.bind_variable(&target, v0);
                //
                self.net.link(Tree::c(w1.tree, v1.tree), a.tree);
                self.compile_process(process);
            }
            Command::Choose(chosen, process) => {
                let a = self.instantiate_variable(&name);
                let a = self.cast(a, ty);

                let Type::Choice(_, mut branches) = self.normalize_type(a.ty) else {
                    unreachable!()
                };
                let Some(branch_type) = branches.get(chosen) else {
                    unreachable!()
                };
                let branch_index = branches.get_index_of(chosen).unwrap();
                let (v0, v1) = self.create_typed_wire(branch_type.clone());
                let choosing_tree = self.either_instance(v1.tree, branch_index, branches.len());
                self.net.link(choosing_tree, a.tree);
                self.bind_variable(&name, v0);
                self.compile_process(process);
            }
            Command::Match(names, processes) => {
                let old_tree = self.instantiate_variable(&name);
                // Multiplex all other variables in the context.
                let mut m_trees = vec![];
                let mut m_tys = vec![];
                let mut m_vars = vec![];
                let mut m_kind = vec![];
                for (k, (v, kind)) in core::mem::take(&mut self.vars) {
                    m_vars.push(k);
                    m_trees.push(v.tree);
                    m_tys.push(v.ty);
                    m_kind.push(kind);
                }
                let context_in = multiplex_trees(m_trees);

                let mut branches = vec![];
                let Type::Either(_, mut required_branches) = self.normalize_type(ty) else {
                    unreachable!()
                };
                let mut choice_and_process: Vec<_> = names.iter().zip(processes.iter()).collect();
                choice_and_process.sort_by_key(|k| k.0);

                for ((choice_here, process), branch) in choice_and_process
                    .into_iter()
                    .zip(required_branches.values())
                {
                    let (w0, w1) = self.create_typed_wire(branch.clone());
                    self.bind_variable(&name, w0);
                    println!("Vars: {:?}", self.vars);

                    // multiplex the conetxt frmo the inside now
                    let mut m_trees = vec![];
                    for (name, (ty, kind)) in m_vars.iter().zip(m_tys.iter().zip(m_kind.iter())) {
                        let (v0, v1) = self.net.create_wire();
                        self.vars
                            .insert(name.clone(), (v0.with_type(ty.clone()), kind.clone()));

                        m_trees.push(v1);
                    }
                    let context_out = multiplex_trees(m_trees);

                    self.compile_process(process);
                    branches.push((context_out, w1.tree))
                }
                let t = self.choice_instance(context_in, branches);
                self.net.link(old_tree.tree, t);
            }
            Command::Break => {
                // < name ! >
                // ==
                // name = *
                let a = self.instantiate_variable(&name).tree;
                self.net.link(a, Tree::e());
            }
            Command::Continue(process) => {
                // < name ? process >
                // ==
                // name = *
                // < process >
                let a = self.instantiate_variable(&name).tree;
                self.net.link(a, Tree::e());
                self.compile_process(process);
            }
            Command::Begin(name, rest) => {
                unreachable!()
            }
            Command::Loop(name) => {
                unreachable!()
            }
        }
    }
}

pub fn compile_file(program: &Prog<Internal<Name>>) -> IcCompiled {
    let mut compiler = Compiler {
        net: Net::default(),
        vars: IndexMap::default(),
        global_name_to_id: Default::default(),
        id_to_package: Default::default(),
        id_to_ty: Default::default(),
        program,
    };
    for k in compiler.program.definitions.clone().keys() {
        compiler.compile_global(k);
    }
    IcCompiled {
        id_to_package: Arc::new(compiler.id_to_package.into_iter().enumerate().collect()),
        name_to_id: compiler.global_name_to_id,
    }
}

#[derive(Clone, Default)]
pub struct IcCompiled {
    id_to_package: Arc<IndexMap<usize, Tree>>,
    name_to_id: IndexMap<Internal<Name>, usize>,
}

impl Display for IcCompiled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, v) in self.id_to_package.iter() {
            // check if it has a name
            for (name, id) in self.name_to_id.iter() {
                if id == k {
                    f.write_fmt(format_args!("// {} \n", name))?;
                }
            }
            f.write_fmt(format_args!("@{} = {}\n", k, v.show()))?;
        }
        Ok(())
    }
}
