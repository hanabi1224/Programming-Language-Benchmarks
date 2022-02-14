use indexmap::IndexMap;
use std::hash::Hash;

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
    m: IndexMap<K, V>,
}

impl<K, V> LRU<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    pub fn new(size: usize) -> Self {
        Self {
            size,
            m: IndexMap::with_capacity(size),
        }
    }

    pub fn get(&mut self, key: K) -> Option<V> {
        if let Some(v) = self.m.shift_remove(&key) {
            self.m.insert(key, v.clone());
            Some(v)
        } else {
            None
        }
    }

    pub fn put(&mut self, key: K, value: V) {
        let not_found = self.m.shift_remove(&key).is_none();
        if not_found && self.m.len() == self.size {
            self.m.shift_remove_index(0);
        }
        self.m.insert(key, value);
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
        .unwrap_or(1000);
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
        if let Some(_) = lru.get(n1) {
            hit += 1;
        } else {
            missed += 1;
        }
    }
    println!("{}\n{}", hit, missed);
}
