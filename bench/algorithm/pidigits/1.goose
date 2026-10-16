// The spigot of the Rust programs: n1 / d and n2 / d bound the digits still
// to come, a term of the series narrows them, and a digit is emitted once
// both quotients agree. The integers are 64-bit limbs, least significant
// first, with the shortcuts of GMP's functions: the products by small
// factors and the sums of a step in one pass over the limbs, and a quotient
// known to be small taken from the leading limbs, with a pass over all of
// them only where those cannot decide it.

import std;
import os;

// The high half of a 64x64-bit product. clang compiles the pattern to one
// mulx, but only while it sees it on its own: inlined first, it rewrites the
// halves of an operand it knows something about (2k + 1, 10) and the pattern
// is gone. `recursive` is what keeps a Goose function out of line.
recursive fn mulhi(a: u64, b: u64) -> u64 {
    let al = a & 0xffffffff;
    let ah = a >> 32;
    let bl = b & 0xffffffff;
    let bh = b >> 32;
    let ll = al * bl;
    let lh = al * bh;
    let hl = ah * bl;
    let hh = ah * bh;
    let mid = (ll >> 32) + (lh & 0xffffffff) + (hl & 0xffffffff);
    return hh + (lh >> 32) + (hl >> 32) + (mid >> 32);
}

fn bits(a: u64[:]) -> i64 {
    var i = a.len - 1;
    while i >= 0 {
        if a[i] != 0 { return i * 64 + 64 - clz(a[i]); }
        i--;
    }
    return 0;
}

// The 64 bits of a from bit s up.
fn bits_at(a: u64[:], s: i64) -> u64 {
    let w = s >> 6;
    let b = s & 63;
    var x = a[w] >> b;
    if b > 0 && w + 1 < a.len { x |= a[w + 1] << (64 - b); }
    return x;
}

// Whether q * d <= n.
fn fits(n: u64[:], d: u64[:], q: u64) -> bool {
    var c: u64 = 0;
    var borrow = false;
    for i in max(n.len, d.len + 1) {
        var lo: u64 = 0;
        var hi: u64 = 0;
        if i < d.len { lo = d[i] * q; hi = mulhi(d[i], q); }
        lo += c;
        if lo < c { hi++; }
        c = hi;
        let x: u64 = if i < n.len { n[i] } else { 0 };
        let y = x - lo;
        let b1 = x < lo;
        borrow = b1 || (borrow && y == 0);
    }
    return !borrow;
}

// n / d, for a quotient below 2^32. The leading 64 bits of n, and the bits
// of d at the same place, bound it to [lo, hi]; only where those differ
// does it take a pass over the limbs.
fn quotient(n: u64[:], d: u64[:]) -> u64 {
    let nb = bits(n);
    if nb < bits(d) { return 0; }
    if nb <= 64 { return bits_at(n, 0) / bits_at(d, 0); }
    let s = nb - 64;
    let nt = bits_at(n, s);
    let dt = bits_at(d, s);
    assert(dt >> 32 != 0);
    let lo = if dt == 0xffffffffffffffff { 0 } else { nt / (dt + 1) };
    var hi = nt / dt;
    if nt % dt == dt - 1 { hi++; }
    var q = hi;
    while q > lo && !fits(n, d, q) { q--; }
    return q;
}

// n1, n2, d = n1 (2k - 1) + 2 n2, n1 (k - 1) + n2 (k + 2), d (2k + 1), all of
// the same length, in one pass over their limbs.
fn next_term(n1: u64[>..]&, n2: u64[>..]&, d: u64[>..]&, k: u64) {
    let len = n1.len;
    assert(n2.len == len && d.len == len);
    let a1 = 2 * k - 1;
    let a2 = k - 1;
    let b2 = k + 2;
    let m = 2 * k + 1;
    var c1: u64 = 0;
    var c2: u64 = 0;
    var c3: u64 = 0;
    for i in len {
        let x = n1[i];
        let y = n2[i];
        var lo1 = x * a1;
        var hi1 = mulhi(x, a1);
        let y2 = y << 1;
        lo1 += y2;
        if lo1 < y2 { hi1++; }
        hi1 += y >> 63;
        lo1 += c1;
        if lo1 < c1 { hi1++; }
        var lo2 = x * a2;
        var hi2 = mulhi(x, a2);
        let ylo = y * b2;
        let yhi = mulhi(y, b2);
        lo2 += ylo;
        if lo2 < ylo { hi2++; }
        hi2 += yhi;
        lo2 += c2;
        if lo2 < c2 { hi2++; }
        let z = d[i];
        var lo3 = z * m;
        var hi3 = mulhi(z, m);
        lo3 += c3;
        if lo3 < c3 { hi3++; }
        n1[i] = lo1;
        n2[i] = lo2;
        d[i] = lo3;
        c1 = hi1;
        c2 = hi2;
        c3 = hi3;
    }
    if c1 != 0 || c2 != 0 || c3 != 0 {
        n1.push(c1);
        n2.push(c2);
        d.push(c3);
    }
}

// n1, n2 = (n1 - q d) 10, (n2 - q d) 10, for q d at most either, in one pass.
fn eliminate_digit(n1: u64[>..]&, n2: u64[>..]&, d: u64[>..]&, q: u64) {
    let len = n1.len;
    assert(n2.len == len && d.len == len);
    var c: u64 = 0;
    var borrow1 = false;
    var borrow2 = false;
    var c1: u64 = 0;
    var c2: u64 = 0;
    for i in len {
        var lo = d[i] * q;
        var hi = mulhi(d[i], q);
        lo += c;
        if lo < c { hi++; }
        c = hi;
        let x1 = n1[i];
        let y1 = x1 - lo;
        let z1 = if borrow1 { y1 - 1 } else { y1 };
        borrow1 = x1 < lo || (borrow1 && y1 == 0);
        var p1 = z1 * 10;
        let h1 = mulhi(z1, 10);
        p1 += c1;
        c1 = if p1 < c1 { h1 + 1 } else { h1 };
        n1[i] = p1;
        let x2 = n2[i];
        let y2 = x2 - lo;
        let z2 = if borrow2 { y2 - 1 } else { y2 };
        borrow2 = x2 < lo || (borrow2 && y2 == 0);
        var p2 = z2 * 10;
        let h2 = mulhi(z2, 10);
        p2 += c2;
        c2 = if p2 < c2 { h2 + 1 } else { h2 };
        n2[i] = p2;
    }
    if c1 != 0 || c2 != 0 {
        n1.push(c1);
        n2.push(c2);
        d.push(0);
    }
    while n1.len > 1 && n1[n1.len - 1] == 0 && n2[n2.len - 1] == 0 && d[d.len - 1] == 0 {
        n1.pop();
        n2.pop();
        d.pop();
    }
}

fn main() {
    let a = args();
    var digits_to_print = 27;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { digits_to_print = v; } }
    var out: u8[>..] = [];
    var digits_printed = 0;
    var k: u64 = 1;
    var n1: u64[>..] = [4];
    var n2: u64[>..] = [3];
    var d: u64[>..] = [1];
    loop {
        let u = quotient(n1, d);
        let v = quotient(n2, d);
        if u == v {
            out.push(('0' + u) as! u8);
            digits_printed++;
            let m = digits_printed % 10;
            if m == 0 { format(out, "\t:", digits_printed, "\n"); }
            if digits_printed >= digits_to_print {
                if m > 0 {
                    for i in 10 - m { out.push(' '); }
                    format(out, "\t:", digits_printed, "\n");
                }
                break;
            }
            eliminate_digit(n1, n2, d, u);
        } else {
            next_term(n1, n2, d, k);
            k++;
        }
    }
    write_stdout(out);
}
