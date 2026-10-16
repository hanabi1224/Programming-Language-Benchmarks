// Goose has no coroutines, and its threads cannot form this chain: there is
// one queue per element type, so n filters cannot each have a channel of
// their own, and n is an input of thousands where a worker is an OS thread
// with data stacks of its own. So the chain is the one the generator
// programs (2.cs, 2.d, 2.py) build, a generator followed by one lazy filter
// per prime found, each pulling the next value from the stage before it.
// A stage's state lives in an array instead of a heap-allocated frame:
// the generator's counter, and a filter's prime. Asking stage k for a value
// is a call and handing it back a return, as resume and suspend are in the
// Zig program; every value visits the filters in the order the channels
// would hand it on, and a new filter joins at the end of the chain. The
// values are u32, as in the Zig program.

import std;
import os;

// The next value of stage k, k == 0 being the generator.
recursive fn pull(primes: u32[:], k: i64, gen: u32&) -> u32 {
    if k == 0 {
        let v = gen;
        gen += 1;
        return v;
    }
    // The prime is read inside the loop: hoisted into a local, the compiler
    // precomputes a reciprocal for the `%` with a 128-bit division per call,
    // where a call here divides about once.
    loop {
        let v = pull(primes, k - 1, gen);
        if v % primes[k - 1] != 0 { return v; }
    }
}

fn main() {
    let a = args();
    var n = 100;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    var primes: u32[>..] = [];
    var gen: u32 = 2;
    // Goose's stdout is unbuffered, so the lines are collected and written
    // at once, as the references' buffered writers do.
    var out: u8[>..] = [];
    for i in n {
        let prime = pull(primes, primes.len, gen);
        format(out, prime, "\n");
        primes.push(prime);
    }
    write_stdout(out);
}
