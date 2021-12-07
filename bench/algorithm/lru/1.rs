use hashbrown::HashMap;
use std::{collections::VecDeque, hash::Hash};

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
    key_lookup: HashMap<K, usize>,
    entries: VecDeque<(K, V)>,
    idx_offset: usize,
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
            entries: VecDeque::with_capacity(size),
            idx_offset: 0,
        }
    }

    pub fn get(&mut self, key: &K) -> Option<V> {
        if let Some(&i) = self.key_lookup.get(key) {
            let i_fixed = i - self.idx_offset;
            let (_, v) = &self.entries[i_fixed];
            let vc = v.clone();
            self.move_to_back(i_fixed);
            Some(vc)
        } else {
            None
        }
    }

    pub fn put(&mut self, key: K, value: V) {
        if let Some(&i) = self.key_lookup.get(&key) {
            self.move_to_back(i - self.idx_offset);
            if let Some(back_mut) = self.entries.back_mut() {
                (*back_mut).1 = value;
            }
        } else {
            if self.entries.len() == self.size {
                self.pop_front();
            }
            self.key_lookup
                .insert(key.clone(), self.entries.len() + self.idx_offset);
            self.entries.push_back((key, value));
        }
    }

    fn pop_front(&mut self) {
        if let Some((key, _)) = self.entries.pop_front() {
            self.key_lookup.remove(&key);
            if self.idx_offset < 10000 {
                self.idx_offset += 1;
            } else {
                self.idx_offset = 0;
                let end = self.entries.len() - 1;
                let key_lookup = &mut self.key_lookup;
                for i in 0..=end {
                    let (k, _) = &self.entries[i];
                    *key_lookup.entry(k.clone()).or_default() = i;
                }
            }
        }
    }

    fn move_to_back(&mut self, i: usize) {
        let end = self.entries.len() - 1;
        if i != end {
            let key_lookup = &mut self.key_lookup;
            if let Some(pair) = self.entries.remove(i) {
                self.entries.push_back(pair);
            }
            for j in i..=end {
                let (k, _) = &self.entries[j];
                *key_lookup.entry(k.clone()).or_default() = j + self.idx_offset;
            }
        }
    }
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(1000);
    let mut rng0 = LCG::new(0);
    let mut rng1 = LCG::new(1);
    let mut lru = LRU::new(10);
    let mut hit = 0;
    let mut missed = 0;
    for _i in 0..n {
        let n0 = rng0.next() % 100;
        lru.put(n0, n0);
        let n1 = rng1.next() % 100;
        if let Some(_) = lru.get(&n1) {
            hit += 1;
        } else {
            missed += 1;
        }
    }
    println!("{}\n{}", hit, missed);
}
