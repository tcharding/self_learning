use std::cell::RefCell;
use std::rc::Rc;

type Link = Option<Rc<RefCell<Node>>>;

#[derive(Clone, Debug)]
pub struct Node {
    value: String,
    next: Link,
}

impl Node {
    fn new(value: String) -> Rc<RefCell<Node>> {
        Rc::new(RefCell::new(Node {
            value: value,
            next: None,
        }))
    }
}

#[derive(Debug)]
pub struct TransactionLog {
    head: Link,
    tail: Link,
    pub length: usize,
}

impl TransactionLog {
    pub fn new() -> TransactionLog {
        TransactionLog {
            head: None,
            tail: None,
            length: 0,
        }
    }

    pub fn append(&mut self, value: String) {
        let new = Node::new(value);
        // Why `self.tail.take()`, why not `&self.tail`
        match self.tail.take() {
            Some(old) => old.borrow_mut().next = Some(new.clone()),
            None => self.head = Some(new.clone()),
        }
        self.length += 1;
        self.tail = Some(new);
    }

    pub fn pop(&mut self) -> Option<String> {
        self.head.take().map(|head| {
            if let Some(next) = head.borrow_mut().next.take() {
                self.head = Some(next);
            } else {
                self.tail.take(); // Does this set self.tail to None?
            }
            self.length -= 1;
            Rc::try_unwrap(head)
                .ok()
                .expect("Something is terribly wrong")
                .into_inner()
                .value
        })
    }

    pub fn iter(&self) -> ListIterator {
        ListIterator::new(&self)
    }
}

pub struct ListIterator {
    current: Link,
}

impl ListIterator {
    pub fn new(log: &TransactionLog) -> ListIterator {
        ListIterator {
            current: log.head.clone(),
        }
    }
}

impl Iterator for ListIterator {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        let current = &self.current;
        let mut result = None;
        self.current = match current {
            Some(ref current) => {
                let current = current.borrow();
                result = Some(current.value.clone());
                current.next.clone()
            }
            None => None,
        };
        result
    }
}

impl Drop for TransactionLog {
    fn drop(&mut self) {
        // TODO: implement Drop trait to drop nodes from the list iteratively.
    }
}
