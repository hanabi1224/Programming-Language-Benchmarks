use hashbrown::HashMap;
use std::{
    cell::RefCell,
    hash::Hash,
    rc::{Rc, Weak},
};

type NodePtr<T> = Rc<RefCell<LinkedListNode<T>>>;
type NodeWeakPtr<T> = Weak<RefCell<LinkedListNode<T>>>;

#[derive(Debug)]
struct LinkedListNode<T> {
    prev: Option<NodeWeakPtr<T>>,
    next: Option<NodePtr<T>>,
    data: T,
}

impl<T> LinkedListNode<T> {
    pub fn new(data: T) -> Self {
        Self {
            prev: None,
            next: None,
            data,
        }
    }
}

struct LinkedList<T> {
    head: Option<NodePtr<T>>,
    tail: Option<NodeWeakPtr<T>>,
    len: usize,
}

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        Self {
            head: None,
            tail: None,
            len: 0,
        }
    }

    pub fn add(&mut self, data: T) -> NodeWeakPtr<T> {
        let node = Rc::new(RefCell::new(LinkedListNode::new(data)));
        let weak = Rc::downgrade(&node);
        self._add_node(node);
        self.len += 1;
        weak
    }

    fn _add_node(&mut self, node: NodePtr<T>) {
        let node_weak = Rc::downgrade(&node);
        {
            node.borrow_mut().next = None;
            if self.head.is_none() {
                node.borrow_mut().prev = None;
                self.head = Some(node);
            } else if let Some(tail) = &self.tail {
                node.borrow_mut().prev = Some(tail.clone());
                let tail_strong = tail.upgrade().unwrap();
                let mut tail_mut = tail_strong.borrow_mut();
                tail_mut.next = Some(node);
            }
        }
        self.tail = Some(node_weak);
    }

    fn _remove(&mut self, node: &NodePtr<T>) {
        let node_ptr = node.as_ptr();
        let node_weak_ptr = Rc::downgrade(&node).as_ptr();
        let node_im = node.borrow();
        if let Some(head) = &self.head {
            if head.as_ptr() == node_ptr {
                self.head = node_im.next.clone();
            }
        }
        if let Some(tail) = &self.tail {
            if tail.as_ptr() == node_weak_ptr {
                self.tail = node_im.prev.clone();
            }
        }
        if let Some(prev) = &node_im.prev {
            if let Some(prev) = prev.upgrade() {
                prev.borrow_mut().next = node_im.next.clone();
            }
        }
        if let Some(next) = &node_im.next {
            next.borrow_mut().prev = node_im.prev.clone();
        }
    }

    pub fn move_to_end(&mut self, node: NodePtr<T>) {
        self._remove(&node);
        self._add_node(node);
    }

    pub fn head_mut(&mut self) -> Option<&mut NodePtr<T>> {
        match &mut self.head {
            Some(head) => Some(head),
            _ => None,
        }
    }
}

struct LCG {
    seed: u32,
}

impl LCG {
    pub fn new(seed: u32) -> Self {
        Self { seed }
    }

    pub fn next(&mut self) -> u32 {
        self.lcg();
        self.seed
    }

    fn lcg(&mut self) {
        const A: u32 = 1103515245;
        const C: u32 = 12345;
        const M: u32 = 1 << 31;
        let (v, _) = A.overflowing_mul(self.seed);
        let (v, _) = v.overflowing_add(C);
        self.seed = v % M
    }
}

struct LRU<K, V> {
    size: usize,
    key_lookup: HashMap<K, NodeWeakPtr<(K, V)>>,
    entries: LinkedList<(K, V)>,
}

impl<K, V> LRU<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    pub fn new(size: usize) -> Self {
        Self {
            size,
            key_lookup: HashMap::with_capacity(size),
            entries: LinkedList::new(),
        }
    }

    pub fn get(&mut self, key: &K) -> Option<V> {
        if let Some(node) = self.key_lookup.get(key) {
            if let Some(node) = node.upgrade() {
                let v = node.borrow().data.1.clone();
                self.entries.move_to_end(node);
                return Some(v);
            }
        }
        None
    }

    pub fn put(&mut self, key: K, value: V) {
        if let Some(node) = self.key_lookup.get_mut(&key) {
            if let Some(node) = node.upgrade() {
                {
                    let mut node_mut = node.borrow_mut();
                    node_mut.data.1 = value;
                }
                self.entries.move_to_end(node);
            }
        } else if self.entries.len == self.size {
            if let Some(head) = self.entries.head_mut() {
                let head_clone = head.clone();
                self.key_lookup.remove(&head.borrow().data.0);
                {
                    head.borrow_mut().data = (key.clone(), value);
                }
                self.key_lookup.insert(key, Rc::downgrade(head));
                self.entries.move_to_end(head_clone);
            }
        } else {
            let node = self.entries.add((key.clone(), value));
            self.key_lookup.insert(key, node);
        }
    }
}

fn main() {
    let size = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(100);
    let n = std::env::args_os()
        .nth(2)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(10000);
    let modular = size as u32 * 10;
    let mut rng0 = LCG::new(0);
    let mut rng1 = LCG::new(1);
    let mut lru = LRU::new(size);
    let mut hit = 0;
    let mut missed = 0;
    for _i in 0..n {
        let n0 = rng0.next() % modular;
        lru.put(n0, n0);
        let n1 = rng1.next() % modular;
        if let Some(_) = lru.get(&n1) {
            hit += 1;
        } else {
            missed += 1;
        }
    }
    println!("{hit}\n{missed}");
}
