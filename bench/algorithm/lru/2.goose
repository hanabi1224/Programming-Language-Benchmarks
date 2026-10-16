// 1.goose with the dictionary replaced by an open-addressing table made for
// the cache: 8-byte slots holding a key and a 4-byte link into the node
// pool (null for an empty slot), a multiplicative hash, linear probing and
// backward-shift deletion. Nine lookups in ten miss, and a miss probes to
// the end of its run, so the table is kept at most a quarter full: 32 KB
// for 1000 entries. At half full the program takes twice as long.

import std;
import os;

struct Node { key: u32, val: u32, prev: Node&<u32 in pool>, next: Node&<u32 in pool> }
struct Slot { key: u32, node: Node&<u32 in pool>? }

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

    var bits: u32 = 3;
    while (1 << bits) < 4 * size { bits++; }
    let shift = 32 - bits;
    var slots: Slot[>..] = [];
    push_n(slots, Slot { key: 0, node: null }, 1 << bits);

    // Slot indices run on past the end of the table and wrap here.
    fn slot(i: i64) -> Slot& { slots[i & (slots.len - 1)] }
    fn home(key: u32) -> i64 { (key * 2654435769) >> shift }

    // The slot holding key, or the empty slot where it goes.
    fn find(key: u32) -> i64 {
        var i = home(key);
        loop {
            let s = slot(i);
            guard s.node && s.key != key else { return i; }
            i++;
        }
    }

    // Empties slot `at`, moving the entries after it in its run that may
    // move back into the hole.
    fn remove_at(at: i64) {
        var i = at;
        var j = at;
        loop {
            j++;
            let s = slot(j);
            guard s.node else { break; }
            if ((j - home(s.key)) & (slots.len - 1)) >= j - i {
                let hole .= slot(i);
                hole = s;
                i = j;
            }
        }
        slot(i).node .= null;
    }

    var count = 0;

    fn cache_put(key: u32, val: u32) {
        let at = find(key);
        let f = slot(at).node;
        if f {
            f.val = val;
            unlink(f);
            link_last(f);
            return;
        }
        if count == size {
            // The new key goes into the empty slot found for it before the
            // old one's slot is emptied, which may move it.
            let lru .= pool[head].next;
            let old = find(lru.key);
            lru.key = key;
            lru.val = val;
            unlink(lru);
            link_last(lru);
            let s .= slot(at);
            s = Slot { key: key, node: lru };
            remove_at(old);
            return;
        }
        let e .= pool.alloc_ref(Node { key: key, val: val, prev: pool[head], next: pool[head] });
        link_last(e);
        let s .= slot(at);
        s = Slot { key: key, node: e };
        count++;
    }

    fn cache_get(key: u32) -> bool {
        let f = slot(find(key)).node;
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
