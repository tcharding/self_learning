use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

/// A red-black tree is a binary search tree that satisfies the following rules:
///
/// - The root node is always black.
/// - Each node is either red or black.
/// - All leaves (often null values) are considered black.
/// - A red node can only have black children.
/// - Any path from the root to its leaves has the same number of block nodes.

/// Operations within the tree, `LeftNode` traverses the tree to
/// the left and `RightNode` traverses down the tree to the right.
#[derive(Debug, PartialEq)]
pub enum RbOperation {
    LeftNode,
    RightNode,
}

/// Nodes are either `Red` or `Black`.
#[derive(Clone, Debug, PartialEq)]
pub enum Colour {
    Red,
    Black,
}

type BareTree = Rc<RefCell<Node>>;
type Tree = Option<BareTree>;

struct Node {
    pub colour: Colour,
    pub value: usize, // The data the tree stores.
    pub parent: Tree,
    left: Tree,
    right: Tree,
}

impl Node {
    /// Returns a Tree so we don't have to do the Rc/RefCell stuff manually.
    /// By definition, nodes start off as red.
    pub fn new(value: usize) -> Tree {
        Some(Rc::new(RefCell::new(Node {
            colour: Colour::Red,
            value,
            parent: None,
            left: None,
            right: None,
        })))
    }
}

#[allow(dead_code)]
fn is_black(node: Tree) -> bool {
    if let Some(n) = node {
        return bare_tree_node_is_black(n);
    }
    return true; // NULL leaf nodes are defined as black.
}

fn bare_tree_node_is_black(node: BareTree) -> bool {
    let colour = &node.borrow().colour;
    return *colour == Colour::Black;
}

/// Red Black Tree (see rules at top of file).
pub struct Rbt {
    root: Tree,
    length: usize,
}

impl Rbt {
    /// Creates a new empty red-black tree.
    pub fn new() -> Self {
        Rbt {
            root: None,
            length: 0,
        }
    }

    /// Returns true if the tree is a valid red-black tree.
    pub fn is_valid(&self) -> bool {
        let result = validate(&self.root, Colour::Red, 0);
        let red_red = result.0;
        let black_height_min = result.1;
        let black_height_max = result.2;
        red_red == 0 && black_height_min == black_height_max
    }

    /// Adds 'value' to the tree.
    pub fn add(&mut self, value: usize) {
        self.length += 1;
        let root = mem::replace(&mut self.root, None);
        let new_tree = self.add_r(root, value);
        self.root = self.fix_tree(new_tree.1);
    }

    // Recursively adds 'add_value' to the tree starting at 'node'.
    // Returns node and the newly added node.
    fn add_r(&self, mut node: Tree, add_value: usize) -> (Tree, BareTree) {
        if let Some(n) = node.take() {
            let new: BareTree; // Used to reference the new root node.
            let node_value = n.borrow().value;

            // TODO: Refactor this once tree is functional and tested.
            match self.check(add_value, node_value) {
                RbOperation::LeftNode => {
                    let left = n.borrow().left.clone();
                    let new_tree = self.add_r(left, add_value);
                    new = new_tree.1;
                    let new_tree = new_tree.0.unwrap();
                    new_tree.borrow_mut().parent = Some(n.clone());
                    n.borrow_mut().left = Some(new_tree);
                }
                RbOperation::RightNode => {
                    let right = n.borrow().right.clone();
                    let new_tree = self.add_r(right, add_value);
                    new = new_tree.1;
                    let new_tree = new_tree.0.unwrap();
                    new_tree.borrow_mut().parent = Some(n.clone());
                    n.borrow_mut().right = Some(new_tree);
                }
            }
            (Some(n), new)
        } else {
            let new = Node::new(add_value);
            (new.clone(), new.unwrap())
        }
    }

    /// All nodes to the left of a node will have less than or equal value.
    /// Checks whether add_value should be to the left or right of node_value.
    fn check(&self, add_value: usize, node_value: usize) -> RbOperation {
        if add_value <= node_value {
            return RbOperation::LeftNode;
        }
        RbOperation::RightNode
    }

    fn fix_tree(&mut self, inserted: BareTree) -> Tree {
        let mut not_root = inserted.borrow().parent.is_some();

        let root = if not_root {
            let mut parent_is_red = self.parent_colour(&inserted) == Colour::Red;
            let mut n = inserted.clone();
            while parent_is_red && not_root {
                if let Some(uncle) = self.uncle(n.clone()) {
                    let which = uncle.1;
                    let uncle = uncle.0;

                    match which {
                        RbOperation::LeftNode => {
                            let mut parent = n.borrow().parent.as_ref().unwrap().clone();
                            if uncle.is_some()
                                && uncle.as_ref().unwrap().borrow().colour == Colour::Red
                            {
                                let uncle = uncle.unwrap();
                                parent.borrow_mut().colour = Colour::Black;
                                uncle.borrow_mut().colour = Colour::Black;
                                parent.borrow().parent.as_ref().unwrap().borrow_mut().colour =
                                    Colour::Red;

                                n = parent.borrow().parent.as_ref().unwrap().clone();
                            } else {
                                // do only if its the right child
                                if parent.borrow().value > n.borrow().value {
                                    let tmp = n.borrow().parent.as_ref().unwrap().clone();
                                    n = tmp;
                                    self.rotate_right(n.clone());
                                    parent = n.borrow().parent.as_ref().unwrap().clone();
                                }
                                // until here then for all black uncles.
                                parent.borrow_mut().colour = Colour::Black;
                                parent.borrow().parent.as_ref().unwrap().borrow_mut().colour =
                                    Colour::Red;
                                let grandparent = n
                                    .borrow()
                                    .parent
                                    .as_ref()
                                    .unwrap()
                                    .borrow()
                                    .parent
                                    .as_ref()
                                    .unwrap()
                                    .clone();
                                self.rotate_left(grandparent)
                            }
                        }
                        RbOperation::RightNode => {
                            // What goes here?
                        }
                    }
                }
                not_root = n.borrow().parent.is_some();
                if not_root {
                    parent_is_red = self.parent_colour(&n) == Colour::Red;
                }
            }
            while n.borrow().parent.is_some() {
                let t = n.borrow().parent.as_ref().unwrap().clone();
                n = t;
            }
            Some(n)
        } else {
            Some(inserted)
        };
        root.map(|r| {
            r.borrow_mut().colour = Colour::Black;
            r
        })
    }

    #[allow(dead_code)]
    fn uncle(&self, node: BareTree) -> Option<(Tree, RbOperation)> {
        // FIXME: These could be as_ref() instead of clone().
        if let Some(p) = node.borrow().parent.clone() {
            if let Some(gp) = p.borrow().parent.clone() {
                let pv = p.borrow().value;
                let gpv = gp.borrow().value;

                match self.check(pv, gpv) {
                    RbOperation::LeftNode => {
                        return Some((gp.borrow().right.clone(), RbOperation::RightNode));
                    }
                    _ => {
                        return Some((gp.borrow().left.clone(), RbOperation::LeftNode));
                    }
                }
            }
        }
        None
    }

    /// Returns parent colour of 'node', 'node; must not be the root node.
    fn parent_colour(&self, node: &BareTree) -> Colour {
        node.borrow()
            .parent
            .as_ref()
            .unwrap()
            .borrow()
            .colour
            .clone()
    }

    fn rotate_right(&self, node: BareTree) {}

    fn rotate_left(&self, node: BareTree) {}
}

//     /// Returns true if value exists in the tree.
//     pub fn find(&self, value: usize) -> bool {
//         self.find_r(&self.root, value)
//     }

//     fn find_r(&self, node: &Tree, vaule: usize) -> bool {
//         if let Some(n) => node { let n = n.borrow(); if n.value == value {
//             return true; }

//             if n.value <= value { return self.find_r(&n.left, value); }

//             return self.find_r(&n.right, value); };

//         return false; // Value not found.  }

//     pub fn walk(&self, callback: impl Fn(value) -> ()) {
//         self.walk_in_order(&self, &callback); }

//     fn walk_in_order(&self, node: &Tree, callback: impl Fn(vaule) -> ()) {
//         if let Some(n) = node {
//             let n = n.borrow();

//             self.walk_in_order(&n.left, callback);
//             callback(&n.value);
//             self.walk_in_order(&n.right, callback);
//         }                                                                       }
//     }
// }

/// Returns (red red violations, min black height, max black height).
fn validate(node: &Tree, parent_colour: Colour, black_height: usize) -> (usize, usize, usize) {
    if let Some(n) = node {
        let n = n.borrow();
        let red_red = if parent_colour == Colour::Red && n.colour == Colour::Red {
            1
        } else {
            0
        };
        let black_height = black_height
            + match n.colour {
                Colour::Black => 1,
                Colour::Red => 0,
            };
        let left = validate(&n.left, n.colour.clone(), black_height);
        let right = validate(&n.right, n.colour.clone(), black_height);
        (
            red_red + left.0 + right.0,
            min(left.1, right.1),
            max(left.2, right.2),
        )
    } else {
        (0, black_height, black_height)
    }
}

fn min(this: usize, that: usize) -> usize {
    if this < that {
        return this;
    }
    that
}

fn max(this: usize, that: usize) -> usize {
    if this > that {
        return this;
    }
    that
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_tree_is_valid() {
        let tree = Rbt::new();
        assert!(tree.is_valid());
    }

    #[test]
    fn bare_tree_node_is_black_returns_true_for_black_node() {
        let node = Node {
            colour: Colour::Black,
            value: 0,
            parent: None,
            left: None,
            right: None,
        };
        let bare_tree = Rc::new(RefCell::new(node));
        assert!(bare_tree_node_is_black(bare_tree));
    }

    #[test]
    fn bare_tree_node_is_black_returns_false_for_red_node() {
        let node = Node {
            colour: Colour::Red,
            value: 0,
            parent: None,
            left: None,
            right: None,
        };
        let bare_tree = Rc::new(RefCell::new(node));
        assert!(!bare_tree_node_is_black(bare_tree));
    }

    #[test]
    fn new_tree_is_valid() {
        let tree = Rbt::new();
        assert!(tree.is_valid());
    }

    #[test]
    fn single_node_tree_is_valid() {
        let mut tree = Rbt::new();
        tree.add(2);
        assert!(tree.is_valid());
    }

    #[test]
    fn new_root_node_is_black() {
        let rbt = Rbt::new();
        assert!(is_black(rbt.root));
    }

    // #[test]
    // fn check_works() {
    //     let rbt = Rbt::new();

    //     if let RbOperation::RightNode = rbt.check(5, 6) {
    //         panic!("Expected LeftNode");
    //     }

    //     if let RbOperation::RightNode = rbt.check(6, 6) {
    //         panic!("Expected LeftNode");
    //     }

    //     if let RbOperation::LeftNode = rbt.check(7, 6) {
    //         panic!("Expected RightNode");
    //     }
    // }

    // #[test]
    // fn can_add_value_to_tree() {
    //     let mut rbt = Rbt::new();
    //     rbt.add(5);
    //     assert_eq!(rbt.length, 1);
    // }

    #[test]
    fn returns_correct_uncle() {
        let rbt = Rbt::new(); // Tree does not store data, just used to call uncle().

        let zero = Node::new(0).unwrap();
        let one = Node::new(1).unwrap();
        let two = Node::new(2).unwrap(); // root node
        let three = Node::new(3).unwrap();
        let four = Node::new(4).unwrap();

        // Connect children of root node.
        two.borrow_mut().left = Some(one.clone());
        two.borrow_mut().right = Some(three.clone());
        one.borrow_mut().parent = Some(two.clone());
        three.borrow_mut().parent = Some(two.clone());

        // Connect grandchildren.
        three.borrow_mut().right = Some(four.clone());
        four.borrow_mut().parent = Some(three.clone());
        one.borrow_mut().left = Some(zero.clone());
        zero.borrow_mut().parent = Some(one.clone());

        let uncle = rbt.uncle(four).unwrap();
        let (uncle, which) = uncle;
        assert_eq!(which, RbOperation::LeftNode);
        let uncle = uncle.unwrap();
        assert_eq!(uncle.borrow().value, 1);

        let uncle = rbt.uncle(zero).unwrap();
        let (uncle, which) = uncle;
        assert_eq!(which, RbOperation::RightNode);
        let uncle = uncle.unwrap();
        assert_eq!(uncle.borrow().value, 3);
    }

    #[test]
    fn nil_uncle_is_returned() {
        let rbt = Rbt::new(); // Tree does not store data, just used to call uncle().

        let zero = Node::new(0).unwrap();
        //  'one' is a null leaf node.
        let two = Node::new(2).unwrap(); // root node
        let three = Node::new(3).unwrap();
        let four = Node::new(4).unwrap();

        // Connect children of root node.
        two.borrow_mut().right = Some(three.clone());
        three.borrow_mut().parent = Some(two.clone());

        // Connect grandchildren.
        three.borrow_mut().right = Some(four.clone());
        four.borrow_mut().parent = Some(three.clone());

        let uncle = rbt.uncle(four).unwrap();
        let (uncle, which) = uncle;
        assert!(uncle.is_none());
    }

    #[test]
    fn can_flatten_empty_tree() {
        let rbt = Rbt::new();
        let v: Vec = rbt.flatten();
        assert!(v.is_empty());
    }
}
