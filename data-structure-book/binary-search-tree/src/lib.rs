use std::mem;

/// Binary Search Tree
#[derive(Debug)]
pub struct Bst {
    root: Tree,
    pub length: usize,
}

impl Bst {
    pub fn new() -> Self {
        Bst {
            root: None,
            length: 0,
        }
    }

    pub fn add(&mut self, value: usize) {
        self.length += 1;
        let root = mem::replace(&mut self.root, None);
        self.root = self.add_rec(root, value);
    }

    fn add_rec(&mut self, node: Tree, value: usize) -> Tree {
        match node {
            Some(mut n) => {
                if value <= n.value {
                    n.left = self.add_rec(n.left, value);
                    return Some(n);
                } else {
                    n.right = self.add_rec(n.right, value);
                    return Some(n);
                }
            }
            _ => Node::new(value),
        }
    }

    pub fn find(&self, value: usize) -> bool {
        self.find_rec(&self.root, value)
    }

    fn find_rec(&self, node: &Tree, value: usize) -> bool {
        match node {
            Some(n) => {
                if n.value == value {
                    true
                } else if value < n.value {
                    self.find_rec(&n.left, value)
                } else {
                    self.find_rec(&n.right, value)
                }
            }
            _ => false,
        }
    }

    pub fn walk(&self, callback: &impl Fn(usize) -> ()) {
        walk_in_order(&self.root, callback);
    }
}

fn walk_in_order(node: &Tree, callback: &impl Fn(usize) -> ()) {
    if let Some(n) = node {
        walk_in_order(&n.left, callback);
        callback(n.value);
        walk_in_order(&n.right, callback);
    }
}

type Tree = Option<Box<Node>>;

#[derive(Debug)]
struct Node {
    pub value: usize,
    left: Tree,
    right: Tree,
}

impl Node {
    pub fn new(value: usize) -> Tree {
        Some(Box::new(Node {
            value,
            left: None,
            right: None,
        }))
    }
}
