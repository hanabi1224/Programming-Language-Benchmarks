// The cache's entries are 16-byte nodes in one pool, on a circular list
// through a sentinel, least recently used first; the links, and the
// dictionary's values, are 4-byte offsets from the pool's base. A full
// cache evicts by reusing its least recently used node for the new key.

import std;
import os;
import dictionary;

struct Node { key: u32, val: u32, prev: Node&<u32 in pool>, next: Node&<u32 in pool> }

reusable var pool: Node[>..] = [];
let head = pool.alloc_index(Node { key: 0, val: 0, prev: self, next: self });

struct Lcg { seed: u32 }

fn next(r: Lcg&) -> u32 {
    r.seed = (1103515245 * r.seed + 12345) % 2147483648;
    return r.seed;
}

inline fn unlink(n: Node&) {
    n.prev.next .= n.next;
    n.next.prev .= n.prev;
}

// n becomes the most recently used.
inline fn link_last(n: Node&) {
    let h .= pool[head];
    n.prev .= h.prev;
    n.next .= h;
    h.prev.next .= n;
    h.prev .= n;
}

fn main() {
    let a = args();
    var size = 100;
    var n = 1000;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { size = v; } }
    if a.len > 2 { let v, ok = parse_int(a[2]); if ok { n = v; } }
    let modular = (size * 10) as u32;
    var map = dictionary<u32, Node&<u32 in pool>?> {};
    map.reserve(size);
    var count = 0;

    fn cache_put(key: u32, val: u32) {
        let f = map.get(key);
        if f {
            f.val = val;
            unlink(f);
            link_last(f);
            return;
        }
        if count == size {
            let lru .= pool[head].next;
            map.remove(lru.key);
            lru.key = key;
            lru.val = val;
            unlink(lru);
            link_last(lru);
            map.insert(key, lru);
            return;
        }
        let e .= pool.alloc_ref(Node { key: key, val: val, prev: pool[head], next: pool[head] });
        link_last(e);
        map.insert(key, e);
        count++;
    }

    fn cache_get(key: u32) -> bool {
        let f = map.get(key);
        guard f else { return false; }
        unlink(f);
        link_last(f);
        return true;
    }

    var rng0 = Lcg { 0 };
    var rng1 = Lcg { 1 };
    var hit = 0;
    var missed = 0;
    for i in n {
        let n0 = rng0.next() % modular;
        cache_put(n0, n0);
        let n1 = rng1.next() % modular;
        if cache_get(n1) { hit++; } else { missed++; }
    }
    print(hit);
    print(missed);
}
