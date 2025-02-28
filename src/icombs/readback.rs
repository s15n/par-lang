use futures::{
    channel::{
        mpsc::{channel as mpsc_channel, Receiver as MpscReceiver, Sender as MpscSender},
        oneshot::{channel, Receiver, Sender},
    },
    executor::LocalPool,
};
use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
};

use super::Tree;

pub struct CoroState {
    executor: Mutex<futures::executor::LocalPool>,
    redexes: Mutex<VecDeque<(Tree, Tree)>>,
}

impl Default for CoroState {
    fn default() -> Self {
        Self {
            executor: Mutex::new(LocalPool::new()),
            redexes: Mutex::new(vec![].into()),
        }
    }
}

pub struct SharedState {
    shared: Arc<CoroState>,
}

enum ReadbackResult {
    // A connection to somewhere else that has already been read back.
    Var(usize),
    // A package; which isn't expanded to prevent infinite readback.
    Package(usize),
    // A tree, which should be readback further, if necessary
    Tree(Tree),
}

impl SharedState {
    pub fn make_oneshot_ext(&self) -> (Tree, Receiver<ReadbackResult>) {
        let (tx, rx) = channel();
        let shared = self.shared.clone();
        let tree = Tree::ext(move |net, tree| {
            let res = match tree {
                Err(thing) => *thing.downcast().unwrap(),
                Ok(Tree::Package(id)) => ReadbackResult::Package(id)
                Ok(tree) => {}
            };
            shared
                .executor
                .lock()
                .unwrap()
                .run_until(async { tx.send(res).unwrap() });
            net.redexes.append(&mut *shared.redexes.lock().unwrap())
        });
        (tree, rx)
    }
    pub fn add_redex(&self, a: Tree, b: Tree) {
        self.shared.redexes.lock().unwrap().push_back((a, b))
    }
    pub async fn read_port(&self, tree: Tree) -> Tree {
        let (ext, rx) = self.make_oneshot_ext();
        self.add_redex(ext, tree);
        rx.await.unwrap()
    }
    pub async fn read_con(&self, tree: Tree) -> Result<(Tree, Tree), Tree> {
        let tree = self.read_port(tree).await;
        match tree {
            Tree::Con(a, b) => Ok((*a, *b)),
            other => Err(other),
        }
    }
    pub async fn read_par(&self, tree: Tree) -> Result<(Tree, Tree), Tree> {
        self.read_con(tree).await
    }
    pub async fn read_times(&self, tree: Tree) -> Result<(Tree, Tree), Tree> {
        self.read_con(tree).await
    }
    pub async fn read_era(&self, tree: Tree) -> Result<(), Tree> {
        let tree = self.read_port(tree).await;
        match tree {
            Tree::Era => Ok(()),
            other => Err(other),
        }
    }
}
