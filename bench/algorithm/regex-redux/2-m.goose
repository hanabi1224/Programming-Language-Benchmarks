// 1.goose with the variant counts on worker threads while the main thread
// does the substitutions, which take about as long as three of the counts.
// Threads share no memory, so each worker is handed its own copy of the
// sequence when it is spawned, and takes variants off a queue.

import std;
import os;
import regex;

let VARIANTS = [
    "agggtaaa|tttaccct",
    "[cgt]gggtaaa|tttaccc[acg]",
    "a[act]ggtaaa|tttacc[agt]t",
    "ag[act]gtaaa|tttac[agt]ct",
    "agg[act]taaa|ttta[agt]cct",
    "aggg[acg]aaa|ttt[cgt]ccct",
    "agggt[cgt]aa|tt[acg]accct",
    "agggta[cgt]a|t[acg]taccct",
    "agggtaa[cgt]|[acg]ttaccct",
];

let SUBST_PATTERNS = ["tHa[Nt]", "aND|caN|Ha[DS]|WaS", "a[NSt]|BY", "<[^>]*>", "\\|[^|][^|]*\\|"];
let SUBST_WITH = ["<4>", "<3>", "<2>", "|", "-"];

let MAX_WORKERS = 3;

struct Job { k: i32 }           // a variant to count; -1: no more
struct Found { k: i32, n: i64 }

// text with every match of re replaced by `with`, appended to out.
fn replace_all(out: u8[>..]&, re: regex::Regex&, text: u8[:], with: u8[:]) {
    var last = 0;
    regex::each_match(re, text) { s, e =>
        out.append(text[last..s]);
        out.append(with);
        last = e;
    };
    out.append(text[last..]);
}

thread_fn counter(seq: u8[]) {
    loop {
        let job = qget<Job>();
        guard job.k >= 0 else { break; }
        let re = regex::compile(VARIANTS[job.k]);
        qput(Found { k: job.k, n: regex::count(re, seq) });
    }
}

fn main() {
    let argv = args();
    var input: u8[>..] = [];
    if !read_file(if argv.len > 1 { argv[1] } else { "25000_in" }, input) { abort("cannot read the input"); }

    var seq: u8[>..] = [];
    let strip = regex::compile(">[^\n]*\n|\n");
    replace_all(seq, strip, input, "");

    let workers = clamp(hardware_threads() - 1, 1, MAX_WORKERS);
    for k in VARIANTS.len { qput(Job { k: k as i32 }); }
    for w in workers { qput(Job { k: -1 }); }
    var ids: i64[>..] = [];
    for w in workers { ids.push(thread_spawn(counter, copy(seq))); }

    // The substitutions go back and forth between two buffers, ending in a.
    var a: u8[>..] = [];
    var b: u8[>..] = [];
    for p, i in SUBST_PATTERNS {
        let re = regex::compile(p);
        if i == 0 {
            replace_all(a, re, seq, SUBST_WITH[i]);
        } else if i % 2 == 1 {
            b.clear();
            replace_all(b, re, a, SUBST_WITH[i]);
        } else {
            a.clear();
            replace_all(a, re, b, SUBST_WITH[i]);
        }
    }

    var counts: i64[>..] = [];
    counts.resize(VARIANTS.len, 0);
    for k in VARIANTS.len {
        let f = qget<Found>();
        counts[f.k] = f.n;
    }
    for id in ids { thread_wait(id); }
    for v, k in VARIANTS { print(v, " ", counts[k]); }
    print("\n", input.len, "\n", seq.len, "\n", a.len);
}
