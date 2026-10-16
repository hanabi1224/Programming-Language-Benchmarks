// A permutation of up to 16 elements is one u64, element i in nibble i (as
// the SIMD programs keep it in one 128-bit register, a byte an element), each
// element stored complemented, as 15 - v. A flip of the first f + 1 elements
// moves bits between the word and a nibble-reversed copy of it, kept up to
// date alongside, with shifts by s = 4 * (15 - f): the low nibble of the
// complemented word times four. That makes s == 60 mean that 0 is first, and
// a "flip" with s == 60 moves nothing. Two permutations are flipped at once,
// until both are done, for the instruction-level parallelism and so that the
// unpredictable end of the flipping loop is paid once per pair.

import std;
import os;

let ONES: u64 = 0xFFFFFFFFFFFFFFFF;

// HI[c]: the top 4c bits, which a flip with s = 4c leaves in place.
let HI: u64[16] = [0x0000000000000000, 0xF000000000000000, 0xFF00000000000000, 0xFFF0000000000000,
                   0xFFFF000000000000, 0xFFFFF00000000000, 0xFFFFFF0000000000, 0xFFFFFFF000000000,
                   0xFFFFFFFF00000000, 0xFFFFFFFFF0000000, 0xFFFFFFFFFF000000, 0xFFFFFFFFFFF00000,
                   0xFFFFFFFFFFFF0000, 0xFFFFFFFFFFFFF000, 0xFFFFFFFFFFFFFF00, 0xFFFFFFFFFFFFFFF0];

fn nibble_reverse(x: u64) -> u64 {
    var y = ((x >> 4) & 0x0F0F0F0F0F0F0F0F) | ((x & 0x0F0F0F0F0F0F0F0F) << 4);
    y = ((y >> 8) & 0x00FF00FF00FF00FF) | ((y & 0x00FF00FF00FF00FF) << 8);
    y = ((y >> 16) & 0x0000FFFF0000FFFF) | ((y & 0x0000FFFF0000FFFF) << 16);
    return (y >> 32) | (y << 32);
}

// The flip counts of the permutations a and b.
fn flips2(a: u64, b: u64) -> i64, i64 {
    var xa = a;
    var ra = nibble_reverse(a);
    var sa = (a & 15) << 2;
    var ka = 0;
    var xb = b;
    var rb = nibble_reverse(b);
    var sb = (b & 15) << 2;
    var kb = 0;
    while (sa & sb) != 60 {
        ka += if sa != 60 { 1 } else { 0 };
        kb += if sb != 60 { 1 } else { 0 };
        let ha = ra >> sa;
        let ya = (xa & HI[sa >> 2]) | ha;
        ra = (ra & ~(ONES << sa)) | (xa << sa);
        xa = ya;
        sa = (ha & 15) << 2;
        let hb = rb >> sb;
        let yb = (xb & HI[sb >> 2]) | hb;
        rb = (rb & ~(ONES << sb)) | (xb << sb);
        xb = yb;
        sb = (hb & 15) << 2;
    }
    return ka, kb;
}

// Rotates the first k elements left by one.
fn rotate(x: u64, k: i64) -> u64 {
    let low = ONES >> (64 - (k << 2));
    return (x & ~low) | ((x >> 4) & (low >> 4)) | ((x & 15) << ((k - 1) << 2));
}

// The next permutation in the counting order: the first two elements swap,
// and each counter that wraps rotates one more element in.
fn next_perm(perm: u64, count: i64[16]&) -> u64 {
    var x = (perm & ~0xFF) | ((perm & 15) << 4) | ((perm >> 4) & 15);
    var i = 1;
    while i < 15 {
        count[i] += 1;
        if count[i] <= i { break; }
        count[i] = 0;
        i++;
        x = rotate(x, i + 1);
    }
    return x;
}

fn factorial(n: i64) -> i64 {
    var f = 1;
    for i in 2..n + 1 { f *= i; }
    return f;
}

// The permutation with index `start`, and its counters.
fn start_perm(n: i64, start: i64, count: i64[16]&) -> u64 {
    var p: u8[16] = [0; 16];
    for i in 16 { p[i] = i as u8; }
    var idx = start;
    var i = n - 1;
    while i > 0 {
        let f = factorial(i);
        let d = idx / f;
        idx %= f;
        count[i] = d;
        let t = p;
        for j in i + 1 { p[j] = t[(j + d) % (i + 1)]; }
        i--;
    }
    var perm: u64 = 0;
    for j in 16 { perm |= (p[j] as u64) << (j << 2); }
    return ~perm;
}

// The checksum and the largest flip count of the permutations with indices
// start .. start + len, start and len even.
fn run_block(n: i64, start: i64, len: i64) -> i64, i64 {
    var count: i64[16] = [0; 16];
    var perm = start_perm(n, start, count);
    var checksum = 0;
    var max_flips = 0;
    for pair in len >> 1 {
        let a = perm;
        perm = next_perm(perm, count);
        let b = perm;
        perm = next_perm(perm, count);
        let ka, kb = flips2(a, b);
        checksum += ka - kb;
        max_flips = max(max_flips, max(ka, kb));
    }
    return checksum, max_flips;
}

fn main() {
    let a = args();
    var n = 7;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    assert(n >= 2 && n <= 16);
    let checksum, max_flips = run_block(n, 0, factorial(n));
    print(checksum, "\nPfannkuchen(", n, ") = ", max_flips);
}
