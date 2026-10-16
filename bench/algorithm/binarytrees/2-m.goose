// 1.goose's trees, built on a pool of worker threads, one per hardware
// thread. The stretch tree is one job and each depth's iterations are split
// into 16 jobs of equal work; a job's trees are built one after another in
// the worker's own pool, and only the check sums come back. The main thread
// builds the long-lived tree while the workers run.

import std;
import os;

let MIN_DEPTH = 4;
let SPLIT = 16;

struct Node { l: Node&<u32>?, r: Node&<u32>? }

// count trees of the given depth; slot -1 is the stretch tree, slot i the
// depth MIN_DEPTH + 2i, and depth -1 tells a worker to stop.
struct Job { depth: i64, count: i64, slot: i64 }
struct Done { slot: i64, check: i64 }

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

thread_fn worker() {
    loop {
        let job = qget<Job>();
        guard job.depth >= 0 else { break; }
        var chk = 0;
        for i in job.count { chk += tree_check(job.depth); }
        qput(Done { slot: job.slot, check: chk });
    }
}

fn main() {
    let a = args();
    var n = 10;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    let max_depth = max(MIN_DEPTH + 2, n);
    let depths = (max_depth - MIN_DEPTH) / 2 + 1;

    var jobs = 1;
    qput(Job { depth: max_depth + 1, count: 1, slot: -1 });
    for i in depths {
        let iterations = 1 << (max_depth - 2 * i);
        let split = min(SPLIT, iterations);
        for j in split {
            qput(Job { depth: MIN_DEPTH + 2 * i, count: iterations / split, slot: i });
        }
        jobs += split;
    }
    let workers = max(1, hardware_threads());
    for i in workers { qput(Job { depth: -1, count: 0, slot: 0 }); }
    var ids: i64[>..] = [];
    for i in workers { ids.push(thread_spawn(worker)); }

    var long: Node[>..] = [];
    let long_root: Node& = make(long, max_depth);

    var stretch = 0;
    var sums: i64[>..] = [];
    sums.resize(depths, 0);
    for k in jobs {
        let d = qget<Done>();
        if d.slot < 0 { stretch = d.check; } else { sums[d.slot] += d.check; }
    }
    for id in ids { thread_wait(id); }

    print("stretch tree of depth ", max_depth + 1, "\t check: ", stretch);
    for i in depths {
        print(1 << (max_depth - 2 * i), "\t trees of depth ", MIN_DEPTH + 2 * i, "\t check: ", sums[i]);
    }
    print("long lived tree of depth ", max_depth, "\t check: ", check(long_root));
}
