//! This module contains a simple implementation for Interaction Combinators.
//! It is not performant; it's mainly here to act as a storage and interchange format

use std::any::Any;
use std::collections::{BTreeMap, VecDeque};
use std::sync::Arc;

use indexmap::IndexMap;

pub type VarId = usize;

pub fn number_to_string(mut number: usize) -> String {
    let mut result = String::new();
    number += 1;
    while number > 0 {
        let remainder = (number - 1) % 26;
        let character = (b'a' + remainder as u8) as char;
        result.insert(0, character);
        number = (number - 1) / 26;
    }
    result
}

/// A `Tree` corresponds to a port that is the root of a tree of interaction combinators.
/// The `Tree` enum itself contains the whole tree, although it some parts of it might be inside
/// half-linked `Tree::Var`s
pub enum Tree {
    Con(Box<Tree>, Box<Tree>),
    Dup(Box<Tree>, Box<Tree>),
    Era,
    Var(usize),
    Package(usize),
    Ext(
        Box<
            dyn FnOnce(
                    &mut Net,
                    Result<Tree, Box<dyn Any + Send + Sync>>,
                    Box<dyn Any + Send + Sync>,
                ) + Send
                + Sync,
        >,
        Box<dyn Any + Send + Sync>,
    ),
}

impl Tree {
    /// Construct a CON node
    pub fn c(a: Tree, b: Tree) -> Tree {
        Tree::Con(Box::new(a), Box::new(b))
    }

    /// Construct a DUP node
    pub fn d(a: Tree, b: Tree) -> Tree {
        Tree::Dup(Box::new(a), Box::new(b))
    }

    /// Construct an ERA node
    pub fn e() -> Tree {
        Tree::Era
    }

    /// Construct an external node, given its closure and data.
    pub fn ext(
        f: impl FnOnce(&mut Net, Result<Tree, Box<dyn Any + Send + Sync>>, Box<dyn Any + Send + Sync>)
            + 'static
            + Send
            + Sync,
        a: impl Any + Send + Sync,
    ) -> Tree {
        Tree::Ext(Box::new(f), Box::new(a))
    }

    pub fn map_vars(&mut self, m: &mut impl FnMut(VarId) -> VarId) {
        use Tree::*;
        match self {
            Var(x) => *x = m(*x),
            Con(a, b) => {
                a.map_vars(m);
                b.map_vars(m);
            }
            Dup(a, b) => {
                a.map_vars(m);
                b.map_vars(m);
            }
            _ => {}
        }
    }

    fn map_vars_tree(&mut self, m: &mut impl FnMut(VarId) -> Tree) {
        use Tree::*;
        match self {
            t @ Var(..) => {
                let Var(id) = &t else { unreachable!() };
                *t = m(id.clone())
            }
            Con(a, b) => {
                a.map_vars_tree(m);
                b.map_vars_tree(m);
            }
            Dup(a, b) => {
                a.map_vars_tree(m);
                b.map_vars_tree(m);
            }
            _ => {}
        }
    }

    pub fn show(&self) -> String {
        self.show_with_context(&mut (BTreeMap::new(), 0))
    }

    pub fn show_with_context(&self, ctx: &mut (BTreeMap<usize, String>, usize)) -> String {
        use Tree::*;
        match self {
            Var(id) => {
                if let Some(name) = ctx.0.get(id) {
                    name.clone()
                } else {
                    ctx.1 += 1;
                    let free_var = ctx.1 - 1;
                    ctx.0.insert(*id, number_to_string(free_var));
                    number_to_string(free_var)
                }
            }
            Con(a, b) => format!(
                "({} {})",
                a.show_with_context(ctx),
                b.show_with_context(ctx)
            ),
            Dup(a, b) => format!(
                "[{} {}]",
                a.show_with_context(ctx),
                b.show_with_context(ctx)
            ),
            Era => format!("*"),
            Package(id) => format!("@{}", id),
            Ext(_, _) => format!("<ext>"),
        }
    }
}

impl core::fmt::Debug for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tree::Con(a, b) => f.debug_tuple("Con").field(a).field(b).finish(),
            Tree::Dup(a, b) => f.debug_tuple("Dup").field(a).field(b).finish(),
            Tree::Era => f.debug_tuple("Era").finish(),
            Tree::Var(id) => f.debug_tuple("Var").field(id).finish(),
            Tree::Package(id) => f.debug_tuple("Package").field(id).finish(),
            Tree::Ext(_, _) => f.debug_tuple("Ext").finish_non_exhaustive(),
        }
    }
}

impl Clone for Tree {
    fn clone(&self) -> Self {
        match self {
            Tree::Con(a, b) => Tree::Con(a.clone(), b.clone()),
            Tree::Dup(a, b) => Tree::Dup(a.clone(), b.clone()),
            Tree::Era => Tree::Era,
            Tree::Var(id) => Tree::Var(id.clone()),
            Tree::Package(id) => Tree::Package(id.clone()),
            Tree::Ext(_, _) => panic!("Can't clone `Ext` tree!"),
        }
    }
}

#[derive(Debug, Default, Clone)]
/// A Net represents the current state of the runtime
/// It contains a list of active pairs, as well as a list of free ports.
/// It also stores a map of variables, which records whether variables were linked by either of their sides
pub struct Net {
    pub ports: VecDeque<Tree>,
    pub redexes: VecDeque<(Tree, Tree)>,
    pub vars: BTreeMap<VarId, Option<Tree>>,
    pub free_vars: Vec<VarId>,
    pub packages: Arc<IndexMap<usize, Net>>,
}

impl Net {
    fn interact(&mut self, a: Tree, b: Tree) {
        use Tree::*;
        /*println!(
            "Interacting {} and {}",
            self.show_tree(&a),
            self.show_tree(&b)
        );*/
        match (a, b) {
            (a @ Var(..), b @ _) | (a @ _, b @ Var(..)) => {
                // link anyway
                self.link(a, b);
            }
            (Era, Era) => (),
            (Con(a0, a1), Era) | (Dup(a0, a1), Era) | (Era, Con(a0, a1)) | (Era, Dup(a0, a1)) => {
                self.link(*a0, Era);
                self.link(*a1, Era);
            }
            (Con(a0, a1), Con(b0, b1)) | (Dup(a0, a1), Dup(b0, b1)) => {
                self.link(*a0, *b0);
                self.link(*a1, *b1);
            }
            (Con(a0, a1), Dup(b0, b1)) | (Dup(b0, b1), Con(a0, a1)) => {
                let (a00, b00) = self.create_wire();
                let (a01, b01) = self.create_wire();
                let (a10, b10) = self.create_wire();
                let (a11, b11) = self.create_wire();
                self.link(*a0, Tree::Dup(Box::new(a00), Box::new(a01)));
                self.link(*a1, Tree::Dup(Box::new(a10), Box::new(a11)));
                self.link(*b0, Tree::Con(Box::new(b00), Box::new(b10)));
                self.link(*b1, Tree::Con(Box::new(b01), Box::new(b11)));
            }
            (Ext(f, a), b) | (b, Ext(f, a)) => {
                f(self, Ok(b), a);
            }
            (Package(_), Era) | (Era, Package(_)) => {}
            (Package(id), Dup(a, b)) | (Dup(a, b), Package(id)) => {
                self.link(*a, Package(id));
                self.link(*b, Package(id));
            }
            (Package(_), Package(_)) => {
                unreachable!("Packages should not interact with packages");
            }
            (Package(id), a) | (a, Package(id)) => {
                let b = self.dereference_package(id);
                self.interact(a, b);
            }
        }
    }

    pub fn freshen_variables(&mut self, net: &mut Net) {
        let mut package_to_net: BTreeMap<usize, usize> = BTreeMap::new();
        net.map_vars(&mut |var_id| {
            if let Some(id) = package_to_net.get(&var_id) {
                id.clone()
            } else {
                let id = self.alloc_var(None);
                package_to_net.insert(var_id, id);
                id
            }
        });
    }

    pub fn dereference_package(&mut self, package: usize) -> Tree {
        let net = self
            .packages
            .get(&package)
            .unwrap_or_else(|| panic!("Unknown package with ID {}", package))
            .clone();
        self.inject_net(net)
    }

    pub fn inject_net(&mut self, mut net: Net) -> Tree {
        // Now, we have to freshen all variables in the tree
        self.freshen_variables(&mut net);
        self.redexes.append(&mut net.redexes);
        self.vars.append(&mut net.vars);
        net.ports.pop_back().unwrap()
    }

    /// Returns whether a reduction was carried out
    pub fn reduce_one(&mut self) -> bool {
        //eprintln!("{}\n----------", self.show());
        if let Some((a, b)) = self.redexes.pop_front() {
            self.interact(a, b);
            true
        } else {
            false
        }
    }

    pub fn substitute_tree(&mut self, tree: &mut Tree) {
        match tree {
            Tree::Con(a, b) | Tree::Dup(a, b) => {
                self.substitute_tree(a);
                self.substitute_tree(b);
            }
            t @ Tree::Var(_) => {
                let Tree::Var(id) = t else { unreachable!() };
                if let Some(Some(mut a)) = self.vars.remove(id) {
                    self.dealloc_var(*id);
                    self.substitute_tree(&mut a);
                    *t = a;
                } else {
                    self.vars.insert(*id, None);
                }
            }
            _ => {}
        }
    }

    pub fn substitute_ref(&self, tree: &Tree) -> Tree {
        match tree {
            Tree::Con(a, b) => Tree::c(self.substitute_ref(a), self.substitute_ref(b)),
            Tree::Dup(a, b) => Tree::d(self.substitute_ref(a), self.substitute_ref(b)),
            Tree::Var(id) => {
                if let Some(Some(a)) = self.vars.get(id) {
                    self.substitute_ref(&a)
                } else {
                    tree.clone()
                }
            }
            _ => tree.clone(),
        }
    }

    pub fn normal(&mut self) {
        while self.reduce_one() {}
        // dereference all variables
        let mut ports = core::mem::take(&mut self.ports);
        ports.iter_mut().for_each(|x| self.substitute_tree(x));
        self.ports = ports;
    }

    pub fn link(&mut self, a: Tree, b: Tree) {
        if let Tree::Var(id) = a {
            match self.vars.remove(&id).unwrap_or_else(||{
                panic!("A variable, with id {id}, was linked to more than twice\n\
                    This is an internal Par bug, which can be caused by many things, such as incorrectly cloning `Tree`s\n\
                    This happened while linking\n\
                    \t{a:?}\nand\n\t{b:?}",
                )

            }) {
                Some(a) => {
                    self.dealloc_var(id);
                    self.link(a, b);
                }
                None => {
                    self.vars.insert(id, Some(b));
                }
            }
        } else if let Tree::Var(id) = b {
            self.link(Tree::Var(id), a)
        } else {
            self.redexes.push_back((a, b))
        }
    }

    pub fn alloc_var(&mut self, value: Option<Tree>) -> VarId {
        let id = match self.free_vars.pop() {
            Some(i) => i,
            None => self.vars.len(),
        };
        assert!(self.vars.insert(id, value).is_none());
        id
    }

    pub fn dealloc_var(&mut self, id: VarId) -> Option<Option<Tree>> {
        self.free_vars.push(id);
        self.vars.remove(&id)
    }

    pub fn create_wire(&mut self) -> (Tree, Tree) {
        let id = self.alloc_var(None);
        (Tree::Var(id), Tree::Var(id))
    }

    pub fn map_vars(&mut self, m: &mut impl FnMut(VarId) -> VarId) {
        self.ports.iter_mut().for_each(|x| x.map_vars(m));
        self.redexes.iter_mut().for_each(|(a, b)| {
            a.map_vars(m);
            b.map_vars(m)
        });
        let vars = core::mem::take(&mut self.vars);
        self.vars = vars
            .into_iter()
            .map(|(mut k, mut v)| {
                k = m(k);
                v.as_mut().map(|x| x.map_vars(m));
                (k, v)
            })
            .collect();
    }

    pub fn map_vars_tree(&mut self, m: &mut impl FnMut(VarId) -> Tree) {
        self.ports.iter_mut().for_each(|x| x.map_vars_tree(m));
        self.redexes.iter_mut().for_each(|(a, b)| {
            a.map_vars_tree(m);
            b.map_vars_tree(m)
        });
    }

    pub fn show_tree(&self, t: &Tree) -> String {
        use Tree::*;
        match t {
            Var(id) => {
                if let Some(Some(b)) = self.vars.get(id) {
                    self.show_tree(b)
                } else {
                    number_to_string(*id)
                }
            }
            Con(a, b) => format!("({} {})", self.show_tree(a), self.show_tree(b)),
            Dup(a, b) => format!("[{} {}]", self.show_tree(a), self.show_tree(b)),
            Era => format!("*"),
            Package(id) => format!("@{}", id),
            Ext(..) => format!("<ext>"),
        }
    }

    pub fn show(&self) -> String {
        self.show_indent(0)
    }

    pub fn show_indent(&self, indent: usize) -> String {
        use core::fmt::Write;
        let indent_string = "    ".repeat(indent);
        let mut s = String::new();
        for i in &self.ports {
            write!(&mut s, "{}{}\n", indent_string, self.show_tree(i)).unwrap();
        }
        for (a, b) in &self.redexes {
            write!(
                &mut s,
                "{}{} ~ {}\n",
                indent_string,
                self.show_tree(a),
                self.show_tree(b)
            )
            .unwrap();
        }
        s
    }

    pub fn is_active(&self, tree: &Tree) -> bool {
        if let Tree::Var(_) = self.substitute_ref(tree) {
            false
        } else {
            true
        }
    }

    fn assert_no_vicious(&self) {
        for (var, tree) in self.vars.iter() {
            if let Some(tree) = tree {
                self.assert_tree_not_contains(tree, var);
            }
        }
    }

    fn assert_tree_not_contains(&self, tree: &Tree, idx: &usize) {
        match tree {
            Tree::Con(a, b) | Tree::Dup(a, b) => {
                self.assert_tree_not_contains(a, idx);
                self.assert_tree_not_contains(b, idx);
            }
            Tree::Var(id) => {
                if id == idx {
                    panic!("Vicious circle detected");
                }
                if let Some(Some(tree)) = self.vars.get(id) {
                    self.assert_tree_not_contains(tree, idx);
                }
            }
            _ => {}
        }
    }

    pub fn assert_valid_with<'a>(&self, iter: impl Iterator<Item = &'a Tree>) {
        self.assert_no_vicious();

        let mut vars = vec![];
        for (a, b) in &self.redexes {
            vars.append(&mut self.assert_tree_valid(a));
            vars.append(&mut self.assert_tree_valid(b));
        }
        for tree in &self.ports {
            vars.append(&mut self.assert_tree_valid(tree));
        }
        for tree in iter {
            vars.append(&mut self.assert_tree_valid(tree));
        }

        let vars_counter = vars.into_iter().fold(BTreeMap::new(), |mut acc, x| {
            *acc.entry(x).or_insert(0) += 1;
            acc
        });
        for (var, count) in vars_counter {
            if count != 2 {
                let name = number_to_string(var.clone());
                println!("net = {}", self.show());
                println!("num ports {}", self.ports.len());
                panic!("Variable {name} was used {count} times");
            }
        }

        // Perhaps we should check that all packages are valid too
        // Right now this creates nonsensical error messages
        // And in any case, each package is checked when it is created
    }

    pub fn assert_valid(&self) {
        self.assert_valid_with(std::iter::empty());
    }

    fn assert_tree_valid(&self, tree: &Tree) -> Vec<usize> {
        match tree {
            Tree::Con(a, b) => {
                let mut a = self.assert_tree_valid(a.as_ref());
                let mut b = self.assert_tree_valid(b.as_ref());
                a.append(&mut b);
                a
            }
            Tree::Dup(a, b) => {
                // do the same thing as Con
                let mut a = self.assert_tree_valid(a.as_ref());
                let mut b = self.assert_tree_valid(b.as_ref());
                a.append(&mut b);
                a
            }
            Tree::Era => {
                vec![]
            }
            Tree::Var(idx) => {
                if let Some(Some(tree)) = self.vars.get(idx) {
                    self.assert_tree_valid(tree)
                } else {
                    vec![idx.clone()]
                }
            }
            Tree::Package(idx) => {
                if self.packages.get(idx).is_some() {
                    vec![]
                } else {
                    panic!("Package with id {idx} is not found")
                }
            }
            Tree::Ext(_, _) => {
                vec![]
            }
        }
    }
}
