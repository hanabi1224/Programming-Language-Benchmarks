// Every tree is a pool of 8-byte nodes local to the function that builds it,
// linked by 4-byte self-relative references; the tree is freed when that
// function returns, by resetting one pointer.

import std;
import os;

let MIN_DEPTH = 4;

struct Node { l: Node&<u32>?, r: Node&<u32>? }

recursive fn make(pool: Node[>..]&, depth: i64) -> Node& {
    if depth == 0 { return pool.push(Node {}); }
    let l: Node& = make(pool, depth - 1);
    let r: Node& = make(pool, depth - 1);
    return pool.push(Node { l: l, r: r });
}

recursive fn check(n: Node&) -> i64 {
    let l = n.l;
    guard l else { return 1; }
    let r = n.r;
    guard r else { return 1; }
    return 1 + check(l) + check(r);
}

fn tree_check(depth: i64) -> i64 {
    var pool: Node[>..] = [];
    return check(make(pool, depth));
}

fn main() {
    let a = args();
    var n = 10;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    let max_depth = max(MIN_DEPTH + 2, n);
    print("stretch tree of depth ", max_depth + 1, "\t check: ", tree_check(max_depth + 1));
    var long: Node[>..] = [];
    let long_root: Node& = make(long, max_depth);
    var d = MIN_DEPTH;
    while d <= max_depth {
        let iterations = 1 << (max_depth - d + MIN_DEPTH);
        var chk = 0;
        for i in iterations { chk += tree_check(d); }
        print(iterations, "\t trees of depth ", d, "\t check: ", chk);
        d += 2;
    }
    print("long lived tree of depth ", max_depth, "\t check: ", check(long_root));
}
