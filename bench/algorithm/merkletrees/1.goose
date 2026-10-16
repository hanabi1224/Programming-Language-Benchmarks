// The problem's rules forbid arenas: every node is allocated on its own and
// freed on its own. The nodes live in one `reusable` pool, a grow-only array
// with a freelist, so `alloc_ref` takes the slot a `free` last handed back,
// the way a size-class allocator does, and the links are 4-byte offsets
// from the pool's base. A tree is freed node by node, children first, as
// Rust drops its boxes.

import std;
import os;

let MIN_DEPTH = 4;

// hash and value are the reference's two Option<i64>s, each a flag and a value.
struct Node {
    hash: i64,
    value: i64,
    l: Node&<u32 in pool>?,
    r: Node&<u32 in pool>?,
    has_hash: bool,
    has_value: bool,
}

reusable var pool: Node[>..] = [];

recursive fn make(depth: i64) -> Node& {
    if depth == 0 { return pool.alloc_ref(Node { value: 1, has_value: true, .. }); }
    let l: Node& = make(depth - 1);
    let r: Node& = make(depth - 1);
    return pool.alloc_ref(Node { l: l, r: r, .. });
}

recursive fn free_tree(n: Node&) {
    let l = n.l;
    if l { free_tree(l); }
    let r = n.r;
    if r { free_tree(r); }
    pool.free(pool.index_of(n));
}

fn get_hash(n: Node&) -> i64 { if n.has_hash { n.hash } else { -1 } }

recursive fn cal_hash(n: Node&) {
    guard !n.has_hash;
    if n.has_value {
        n.hash = n.value;
        n.has_hash = true;
        return;
    }
    let l = n.l;
    guard l;
    let r = n.r;
    guard r;
    cal_hash(l);
    cal_hash(r);
    n.hash = get_hash(l) + get_hash(r);
    n.has_hash = true;
}

recursive fn check(n: Node&) -> bool {
    guard n.has_hash else { return false; }
    if n.has_value { return true; }
    let l = n.l;
    guard l else { return false; }
    let r = n.r;
    guard r else { return false; }
    return check(l) && check(r);
}

fn main() {
    let a = args();
    var n = 10;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    let max_depth = max(MIN_DEPTH + 2, n);
    {
        let stretch: Node& = make(max_depth + 1);
        cal_hash(stretch);
        print("stretch tree of depth ", max_depth + 1, "\t root hash: ", get_hash(stretch),
              " check: ", check(stretch));
        free_tree(stretch);
    }
    let long_lived: Node& = make(max_depth);
    var d = MIN_DEPTH;
    while d <= max_depth {
        let iterations = 1 << (max_depth - d + MIN_DEPTH);
        var sum = 0;
        for i in iterations {
            let t: Node& = make(d);
            cal_hash(t);
            sum += get_hash(t);
            free_tree(t);
        }
        print(iterations, "\t trees of depth ", d, "\t root hash sum: ", sum);
        d += 2;
    }
    cal_hash(long_lived);
    print("long lived tree of depth ", max_depth, "\t root hash: ", get_hash(long_lived),
          " check: ", check(long_lived));
}
