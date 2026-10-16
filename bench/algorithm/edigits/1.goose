// Digits of e by binary splitting, as the Rust programs compute them:
// sum_terms gives p / q = 1/1! + ... + 1/(k-1)!, and the digits are
// (p + q) * 10^(n-1) / q in decimal. The integers are 64-bit limbs, least
// significant first, in slices of one preallocated arena, with the
// algorithms of the Rust programs' libraries: multiplication by Toom-3 and
// Karatsuba down to the schoolbook, division by halves of the quotient down
// to the schoolbook (as GMP's dcpi1_div_qr), and the decimal conversion by
// dividing by 10^(19*2^i) recursively.

import std;
import os;
import math;

// Limbs from which Karatsuba and then Toom-3 beat the method below them.
let KARATSUBA = 24;
let KARATSUBA_SQR = 40;
let TOOM3 = 96;
let TOOM3_SQR = 96;
let LN_10 = 2.302585092994046;

// The high half of a 64x64-bit product. clang compiles the pattern to one
// mulx, but only while it sees it on its own: inlined first, it rewrites the
// halves of an operand it knows something about (a constant, a square) and
// the pattern is gone. `recursive` is what keeps a Goose function out of line.
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

// --- limb vectors ------------------------------------------------------------

fn top(a: u64[:]) -> i64 {
    var n = a.len;
    while n > 0 && a[n - 1] == 0 { n--; }
    return n;
}

fn norm(a: u64[:]) -> u64[:] { a[..top(a)] }

fn zero(r: u64[:]) { for &x in r { x = 0; } }

// r = a + b over r.len limbs; the carry out
fn add_n(r: u64[:], a: u64[:], b: u64[:]) -> u64 {
    let n = r.len;
    assert(a.len == n && b.len == n);
    var c: u64 = 0;
    for i in n {
        let x = a[i] + b[i];
        let y = x + c;
        c = if x < a[i] || y < x { 1 } else { 0 };
        r[i] = y;
    }
    return c;
}

// r = a - b over r.len limbs; the borrow out
fn sub_n(r: u64[:], a: u64[:], b: u64[:]) -> u64 {
    let n = r.len;
    assert(a.len == n && b.len == n);
    var c: u64 = 0;
    for i in n {
        let x = a[i] - b[i];
        let y = x - c;
        c = if a[i] < b[i] || x < c { 1 } else { 0 };
        r[i] = y;
    }
    return c;
}

fn add_1(r: u64[:], c: u64) -> u64 {
    var k = c;
    for &x in r {
        if k == 0 { return 0; }
        x += k;
        k = if x < k { 1 } else { 0 };
    }
    return k;
}

fn sub_1(r: u64[:], c: u64) -> u64 {
    var k = c;
    for &x in r {
        if k == 0 { return 0; }
        let y = x - k;
        k = if x < k { 1 } else { 0 };
        x = y;
    }
    return k;
}

// r = a + b for a.len >= b.len and r.len == a.len; the carry out
fn add(r: u64[:], a: u64[:], b: u64[:]) -> u64 {
    let n = b.len;
    assert(r.len == a.len && a.len >= n);
    let c = add_n(r[..n], a[..n], b);
    if r.len == n { return c; }
    copy_into(r[n..], a[n..]);
    return add_1(r[n..], c);
}

// r = a - b for a.len >= b.len and r.len == a.len; the borrow out
fn sub(r: u64[:], a: u64[:], b: u64[:]) -> u64 {
    let n = b.len;
    assert(r.len == a.len && a.len >= n);
    let c = sub_n(r[..n], a[..n], b);
    if r.len == n { return c; }
    copy_into(r[n..], a[n..]);
    return sub_1(r[n..], c);
}

// r += b for r.len >= b.len; the carry out
fn add_in(r: u64[:], b: u64[:]) -> u64 {
    let n = b.len;
    let c = add_n(r[..n], r[..n], b);
    return add_1(r[n..], c);
}

// r -= b for r.len >= b.len; the borrow out
fn sub_in(r: u64[:], b: u64[:]) -> u64 {
    let n = b.len;
    let c = sub_n(r[..n], r[..n], b);
    return sub_1(r[n..], c);
}

fn cmp(a: u64[:], b: u64[:]) -> i64 {
    var n = max(a.len, b.len);
    while n > 0 {
        n--;
        let x: u64 = if n < a.len { a[n] } else { 0 };
        let y: u64 = if n < b.len { b[n] } else { 0 };
        if x != y { return if x < y { -1 } else { 1 }; }
    }
    return 0;
}

// d = |x - y| for x.len == d.len >= y.len; whether x < y
fn absdiff(d: u64[:], x: u64[:], y: u64[:]) -> bool {
    if cmp(x, y) >= 0 {
        sub(d, x, y);
        return false;
    }
    let n = y.len;
    sub_n(d[..n], y, x[..n]);
    zero(d[n..]);
    return true;
}

// r = a * m; the carry out
fn mul_1(r: u64[:], a: u64[:], m: u64) -> u64 {
    assert(r.len == a.len);
    var c: u64 = 0;
    for i in a.len {
        var lo = a[i] * m;
        var hi = mulhi(a[i], m);
        lo += c;
        if lo < c { hi++; }
        r[i] = lo;
        c = hi;
    }
    return c;
}

// r += a * m; the carry out
fn addmul_1(r: u64[:], a: u64[:], m: u64) -> u64 {
    assert(r.len == a.len);
    var c: u64 = 0;
    for i in a.len {
        var lo = a[i] * m;
        var hi = mulhi(a[i], m);
        lo += c;
        if lo < c { hi++; }
        let x = r[i] + lo;
        if x < lo { hi++; }
        r[i] = x;
        c = hi;
    }
    return c;
}

fn mul_basecase(r: u64[:], a: u64[:], b: u64[:]) {
    let n = a.len;
    assert(r.len == n + b.len && b.len >= 1);
    r[n] = mul_1(r[..n], a, b[0]);
    for j in 1..b.len { r[j + n] = addmul_1(r[j..j + n], a, b[j]); }
}

// r = a^2: the products of distinct limbs once, doubled, and the squares of
// the limbs added
fn sqr_basecase(r: u64[:], a: u64[:]) {
    let n = a.len;
    assert(r.len == 2 * n && n >= 1);
    r[0] = 0;
    r[2 * n - 1] = 0;
    if n > 1 {
        r[n] = mul_1(r[1..n], a[1..], a[0]);
        for i in 1..n - 1 { r[n + i] = addmul_1(r[2 * i + 1..n + i], a[i + 1..], a[i]); }
        var c: u64 = 0;
        for &x in r[1..2 * n] {
            let y = (x << 1) | c;
            c = x >> 63;
            x = y;
        }
    }
    var c: u64 = 0;
    for i in n {
        let lo = a[i] * a[i];
        let hi = mulhi(a[i], a[i]);
        let x = r[2 * i] + lo;
        let y = x + c;
        let k: u64 = if x < lo || y < x { 1 } else { 0 };
        r[2 * i] = y;
        let z = r[2 * i + 1] + hi;
        let w = z + k;
        c = if z < hi || w < z { 1 } else { 0 };
        r[2 * i + 1] = w;
    }
}

// x / 3 for x a multiple of 3, two's complement over x.len limbs
fn divexact3(x: u64[:]) {
    var c: u64 = 0;
    for &w in x {
        let b: u64 = if w < c { 1 } else { 0 };
        let q = (w - c) * 0xAAAAAAAAAAAAAAAB;      // the inverse of 3 modulo 2^64
        w = q;
        c = b;
        if q >= 0x5555555555555556 { c++; }
        if q >= 0xAAAAAAAAAAAAAAAB { c++; }
    }
}

// x / 2 for x even, two's complement over x.len limbs
fn halve(x: u64[:]) {
    let n = x.len;
    for i in n - 1 { x[i] = (x[i] >> 1) | (x[i + 1] << 63); }
    x[n - 1] = ((x[n - 1] as! i64) >> 1) as! u64;
}

fn negate(x: u64[:]) {
    for &w in x { w = ~w; }
    add_1(x, 1);
}

// x(1) = x0 + x1 + x2, |x(-1)| = |x0 - x1 + x2| and |x(-2)| = |x0 - 2 x1 + 4 x2|
// into k + 1 limbs each, for x0 and x1 of k limbs and x2 of at most k; the
// signs of x(-1) and x(-2), and t is scratch of k + 1 limbs.
fn toom3_eval(e1: u64[:], em1: u64[:], em2: u64[:], x0: u64[:], x1: u64[:], x2: u64[:], t: u64[:]) -> bool, bool {
    let k = x0.len;
    em1[k] = add(em1[..k], x0, x2);
    e1[k] = em1[k] + add(e1[..k], em1[..k], x1);
    let neg1 = cmp(em1, x1) < 0;
    if neg1 { sub_n(em1[..k], x1, em1[..k]); } else { sub_in(em1, x1); }
    zero(em2);
    em2[x2.len] = shl_bits(em2[..x2.len], x2, 2);
    em2[k] += add(em2[..k], em2[..k], x0);
    t[k] = shl_bits(t[..k], x1, 1);
    let neg2 = cmp(em2, t) < 0;
    if neg2 { sub_n(em2, t, em2); } else { sub_n(em2, em2, t); }
    return neg1, neg2;
}

// The Toom-3 recombination of w0 = r[..2k], winf = r[4k..] and the values
// at 1, -1 and -2, the last two signed, each of 2k + 2 limbs, into r.
fn toom3_interpolate(r: u64[:], k: i64, w1: u64[:], wm1: u64[:], wm2: u64[:]) {
    let w0 = r[..2 * k];
    let winf = r[4 * k..];
    // r3 = (w(-2) - w(1)) / 3, r1 = (w(1) - w(-1)) / 2, r2 = w(-1) - w(0)
    sub_n(wm2, wm2, w1);
    divexact3(wm2);
    sub_n(w1, w1, wm1);
    halve(w1);
    sub_in(wm1, w0);
    // r3 = (r2 - r3) / 2 + 2 w(inf), r2 = r2 + r1 - w(inf), r1 = r1 - r3
    sub_n(wm2, wm1, wm2);
    halve(wm2);
    add_in(wm2, winf);
    add_in(wm2, winf);
    add_n(wm1, wm1, w1);
    sub_in(wm1, winf);
    sub_n(w1, w1, wm2);
    zero(r[2 * k..4 * k]);
    for c, i in [w1, wm1, wm2] {
        let rd = r[(i + 1) * k..];
        add_in(rd, c[..min(c.len, rd.len)]);     // beyond r's end the limbs are zero
    }
}

// --- multiplication -----------------------------------------------------------

// r = a^2 by mul's methods with the operand shared, Karatsuba's step being
// a^2 = a1^2 B^2h + (a0^2 + a1^2 - d^2) B^h + a0^2 for d = |a0 - a1|.
recursive fn sqr(r: u64[:], a: u64[:], s: u64[:]) {
    let n = a.len;
    assert(r.len == 2 * n);
    if n < KARATSUBA_SQR { sqr_basecase(r, a); return; }
    if n >= TOOM3_SQR {
        let k = (n + 2) / 3;
        let l = 2 * k + 2;
        let e1 = s[..k + 1];
        let em1 = s[k + 1..2 * k + 2];
        let em2 = s[2 * k + 2..3 * k + 3];
        let w1 = s[3 * k + 3..3 * k + 3 + l];
        let wm1 = s[3 * k + 3 + l..3 * k + 3 + 2 * l];
        let wm2 = s[3 * k + 3 + 2 * l..3 * k + 3 + 3 * l];
        let rest = s[3 * k + 3 + 3 * l..];
        toom3_eval(e1, em1, em2, a[..k], a[k..2 * k], a[2 * k..], rest[..k + 1]);
        sqr(w1, e1, rest);
        sqr(wm1, em1, rest);
        sqr(wm2, em2, rest);
        sqr(r[..2 * k], a[..k], rest);
        sqr(r[4 * k..], a[2 * k..], rest);
        toom3_interpolate(r, k, w1, wm1, wm2);
        return;
    }
    let h = (n + 1) >> 1;
    let a0 = a[..h];
    let a1 = a[h..];
    sqr(r[..2 * h], a0, s);
    sqr(r[2 * h..], a1, s);
    let d = s[..h];
    let v = s[h..3 * h];
    let t = s[3 * h..5 * h + 1];
    absdiff(d, a0, a1);
    sqr(v, d, s[5 * h + 1..]);
    t[2 * h] = add(t[..2 * h], r[..2 * h], r[2 * h..]);
    sub_in(t, v);
    let rh = r[h..];
    add_in(rh, t[..min(t.len, rh.len)]);
}

// r = a * b, all a.len + b.len limbs of it, with s as scratch.
recursive fn mul(r: u64[:], a: u64[:], b: u64[:], s: u64[:]) {
    if a.len < b.len { mul(r, b, a, s); return; }
    let n = a.len;
    let m = b.len;
    assert(r.len == n + m && m >= 1);
    if m < KARATSUBA { mul_basecase(r, a, b); return; }
    if m >= TOOM3 && m > 2 * ((n + 2) / 3) { toom3(r, a, b, s); return; }
    let h = (n + 1) >> 1;
    if m > h { karatsuba(r, a, b, s); return; }
    // b is at most half of a: a's pieces of b's length one at a time
    mul(r[..2 * m], a[..m], b, s);
    var off = m;
    while off < n {
        let len = min(m, n - off);
        let t = s[..len + m];
        mul(t, a[off..off + len], b, s[len + m..]);
        let c = add_n(r[off..off + m], r[off..off + m], t[..m]);
        copy_into(r[off + m..off + m + len], t[m..]);
        add_1(r[off + m..off + m + len], c);
        off += len;
    }
}

// a = a1 B^h + a0 and b = b1 B^h + b0 with h = ceil(a.len / 2) < b.len:
// a b = a1 b1 B^2h + (a0 b0 + a1 b1 - (a0 - a1)(b0 - b1)) B^h + a0 b0
fn karatsuba(r: u64[:], a: u64[:], b: u64[:], s: u64[:]) {
    let h = (a.len + 1) >> 1;
    let a0 = a[..h];
    let a1 = a[h..];
    let b0 = b[..h];
    let b1 = b[h..];
    mul(r[..2 * h], a0, b0, s);
    mul(r[2 * h..], a1, b1, s);
    let da = s[..h];
    let db = s[h..2 * h];
    let v = s[2 * h..4 * h];
    let t = s[4 * h..6 * h + 1];
    let na = absdiff(da, a0, a1);
    let nb = absdiff(db, b0, b1);
    mul(v, da, db, s[6 * h + 1..]);
    t[2 * h] = add(t[..2 * h], r[..2 * h], r[2 * h..]);
    if na == nb { sub_in(t, v); } else { add_in(t, v); }
    let rh = r[h..];
    add_in(rh, t[..min(t.len, rh.len)]);   // what lies beyond r is zero
}

// a = a2 B^2k + a1 B^k + a0, likewise b, with k = ceil(a.len / 3) and
// 2k < b.len: the five products at 0, 1, -1, -2 and infinity, and the
// interpolation of Bodrato's sequence.
fn toom3(r: u64[:], a: u64[:], b: u64[:], s: u64[:]) {
    let n = a.len;
    let k = (n + 2) / 3;
    let l = 2 * k + 2;
    let e1 = s[..k + 1];
    let em1 = s[k + 1..2 * k + 2];
    let em2 = s[2 * k + 2..3 * k + 3];
    let f1 = s[3 * k + 3..4 * k + 4];
    let fm1 = s[4 * k + 4..5 * k + 5];
    let fm2 = s[5 * k + 5..6 * k + 6];
    let w1 = s[6 * k + 6..6 * k + 6 + l];
    let wm1 = s[6 * k + 6 + l..6 * k + 6 + 2 * l];
    let wm2 = s[6 * k + 6 + 2 * l..6 * k + 6 + 3 * l];
    let rest = s[6 * k + 6 + 3 * l..];
    let na1, na2 = toom3_eval(e1, em1, em2, a[..k], a[k..2 * k], a[2 * k..], rest[..k + 1]);
    let nb1, nb2 = toom3_eval(f1, fm1, fm2, b[..k], b[k..2 * k], b[2 * k..], rest[..k + 1]);
    mul(w1, e1, f1, rest);
    mul(wm1, em1, fm1, rest);
    if na1 != nb1 { negate(wm1); }
    mul(wm2, em2, fm2, rest);
    if na2 != nb2 { negate(wm2); }
    mul(r[..2 * k], a[..k], b[..k], rest);
    mul(r[4 * k..], a[2 * k..], b[2 * k..], rest);
    toom3_interpolate(r, k, w1, wm1, wm2);
}

// --- division -----------------------------------------------------------------

let DC_DIV = 48;               // limbs from which dividing in halves beats the schoolbook

fn shl_bits(r: u64[:], a: u64[:], k: i64) -> u64 {
    assert(r.len == a.len);
    if k == 0 { copy_into(r, a); return 0; }
    var c: u64 = 0;
    for i in a.len {
        let x = a[i];
        r[i] = (x << k) | c;
        c = x >> (64 - k);
    }
    return c;
}

// r -= a * m; the borrow out
fn submul_1(r: u64[:], a: u64[:], m: u64) -> u64 {
    assert(r.len == a.len);
    var c: u64 = 0;
    for i in a.len {
        var lo = a[i] * m;
        var hi = mulhi(a[i], m);
        lo += c;
        if lo < c { hi++; }
        let x = r[i];
        if x < lo { hi++; }
        r[i] = x - lo;
        c = hi;
    }
    return c;
}

// (u1 B + u0) / v for u1 < v and v's top bit set, by 32-bit digits
// (Hacker's Delight, divlu).
fn divlu(u1: u64, u0: u64, v: u64) -> u64 {
    let b: u64 = 0x100000000;
    let vn1 = v >> 32;
    let vn0 = v & 0xffffffff;
    let un1 = u0 >> 32;
    let un0 = u0 & 0xffffffff;
    var q1 = u1 / vn1;
    var rhat = u1 - q1 * vn1;
    while q1 >= b || q1 * vn0 > (rhat << 32) + un1 {
        q1--;
        rhat += vn1;
        if rhat >= b { break; }
    }
    let un21 = (u1 << 32) + un1 - q1 * v;
    var q0 = un21 / vn1;
    rhat = un21 - q0 * vn1;
    while q0 >= b || q0 * vn0 > (rhat << 32) + un0 {
        q0--;
        rhat += vn1;
        if rhat >= b { break; }
    }
    return (q1 << 32) + q0;
}

// floor((B^3 - 1) / (d1 B + d0)) - B, the reciprocal the 3-by-2 limb
// division multiplies by (Moller and Granlund; GMP's invert_pi1).
fn invert_pi1(d1: u64, d0: u64) -> u64 {
    var v = divlu(~d1, 0xffffffffffffffff, d1);
    var p = d1 * v;
    p += d0;
    if p < d0 {
        v--;
        if p >= d1 { v--; p -= d1; }
        p -= d1;
    }
    let t0 = d0 * v;
    let t1 = mulhi(d0, v);
    p += t1;
    if p < t1 {
        v--;
        if p >= d1 && (p > d1 || t0 >= d0) { v--; }
    }
    return v;
}

// (n2 B^2 + n1 B + n0) / (d1 B + d0) for (n2, n1) < (d1, d0), with v from
// invert_pi1: the quotient and the two limbs of the remainder (GMP's
// udiv_qr_3by2).
fn div_3by2(n2: u64, n1: u64, n0: u64, d1: u64, d0: u64, v: u64) -> u64, u64, u64 {
    var q = mulhi(n2, v);
    var q0 = n2 * v;
    let s0 = q0 + n1;
    q += n2;
    if s0 < q0 { q++; }
    q0 = s0;
    var r1 = n1 - d1 * q;
    var r0 = n0 - d0;
    r1 -= d1;
    if n0 < d0 { r1--; }
    let t0 = d0 * q;
    let t1 = mulhi(d0, q);
    if r0 < t0 { r1--; }
    r0 -= t0;
    r1 -= t1;
    q++;
    if r1 >= q0 {
        q--;
        let s = r0 + d0;
        r1 += d1;
        if s < r0 { r1++; }
        r0 = s;
    }
    if r1 >= d1 && (r1 > d1 || r0 >= d0) {
        q++;
        if r0 < d0 { r1--; }
        r0 -= d0;
        r1 -= d1;
    }
    return q, r1, r0;
}

fn cmp_n(a: u64[:], b: u64[:]) -> i64 {
    assert(a.len == b.len);
    var i = a.len;
    while i > 0 {
        i--;
        if a[i] != b[i] { return if a[i] < b[i] { -1 } else { 1 }; }
    }
    return 0;
}

// The schoolbook division of np by d, d.len >= 2 and normalized: the
// quotient into q (np.len - d.len limbs) and the remainder into np[..d.len];
// returns the quotient's top limb, 0 or 1 (GMP's sbpi1_div_qr).
fn sb_div_qr(q: u64[:], np: u64[:], d: u64[:], v: u64) -> u64 {
    let nn = np.len;
    let dn = d.len;
    let dm = dn - 2;
    assert(q.len == nn - dn && dn >= 2);
    var qh: u64 = 0;
    let head = np[nn - dn..];
    if cmp_n(head, d) >= 0 {
        sub_n(head, head, d);
        qh = 1;
    }
    let d1 = d[dn - 1];
    let d0 = d[dn - 2];
    var idx = nn - 2;
    var n1 = np[idx + 1];
    var i = nn - dn;
    while i > 0 {
        idx--;
        var qi: u64 = 0;
        if n1 == d1 && np[idx + 1] == d0 {
            qi = 0xffffffffffffffff;
            submul_1(np[idx - dm..idx + 2], d, qi);
            n1 = np[idx + 1];
        } else {
            let qq, r1, r0 = div_3by2(n1, np[idx + 1], np[idx], d1, d0, v);
            qi = qq;
            var n0 = r0;
            n1 = r1;
            let cy = submul_1(np[idx - dm..idx], d[..dm], qi);
            let cy1: u64 = if n0 < cy { 1 } else { 0 };
            n0 -= cy;
            let cy2 = n1 < cy1;
            n1 -= cy1;
            np[idx] = n0;
            if cy2 {
                n1 += d1 + add_n(np[idx - dm..idx + 1], np[idx - dm..idx + 1], d[..dm + 1]);
                qi--;
            }
        }
        i--;
        q[i] = qi;
    }
    np[idx + 1] = n1;
    return qh;
}

// np (2n limbs) by d (n limbs, normalized) in two halves of the quotient,
// each the division of a top part by d's top half and a product to correct
// by: the quotient into q (n limbs), the remainder into np[..n]; returns the
// quotient's top limb (GMP's dcpi1_div_qr_n).
recursive fn dc_div_qr(q: u64[:], np: u64[:], d: u64[:], v: u64, tp: u64[:]) -> u64 {
    let n = d.len;
    if n < DC_DIV { return sb_div_qr(q, np, d, v); }
    let lo = n >> 1;
    let hi = n - lo;
    var qh = dc_div_qr(q[lo..], np[2 * lo..], d[lo..], v, tp);
    mul(tp[..n], q[lo..], d[..lo], tp[n..]);
    var cy = sub_n(np[lo..lo + n], np[lo..lo + n], tp[..n]);
    if qh != 0 { cy += sub_n(np[n..n + lo], np[n..n + lo], d[..lo]); }
    while cy != 0 {
        qh -= sub_1(q[lo..], 1);
        cy -= add_n(np[lo..lo + n], np[lo..lo + n], d);
    }
    let ql = dc_div_qr(q[..lo], np[hi..hi + 2 * lo], d[hi..], v, tp);
    mul(tp[..n], d[..hi], q[..lo], tp[n..]);
    cy = sub_n(np[..n], np[..n], tp[..n]);
    if ql != 0 { cy += sub_n(np[lo..lo + hi], np[lo..lo + hi], d[..hi]); }
    while cy != 0 {
        sub_1(q[..lo], 1);
        cy -= add_n(np[..n], np[..n], d);
    }
    return qh;
}

// A divisor shifted until its top bit is set, and the reciprocal of its
// top two limbs.
struct Divisor { shift: i64, d: u64[:], v: u64 }

fn divisor(dst: u64[:], src: u64[:]) -> Divisor {
    let m = src.len;
    assert(dst.len == m);
    let shift = clz(src[m - 1]);
    shl_bits(dst, src, shift);
    let v = if m >= 2 { invert_pi1(dst[m - 1], dst[m - 2]) } else { 0 };
    return Divisor { shift: shift, d: dst, v: v };
}

// q = num / div and rem = num % div, for num below div * B^div.len; q gets
// num.len - div.len + 1 limbs, rem div.len.
fn divrem(q: u64[:], rem: u64[:], num: u64[:], dv: Divisor, s: u64[:]) {
    let m = dv.d.len;
    assert(num.len <= 2 * m && rem.len == m);
    let a = s[..2 * m];
    zero(a);
    let c = shl_bits(a[..num.len], num, dv.shift);
    if num.len < 2 * m { a[num.len] = c; }
    let qq = s[2 * m..3 * m + 1];
    qq[m] = dc_div_qr(qq[..m], a, dv.d, dv.v, s[3 * m + 1..]);
    let sh = dv.shift;
    for i in m {
        var w = a[i] >> sh;
        if sh > 0 && i + 1 < m { w |= a[i + 1] << (64 - sh); }
        rem[i] = w;
    }
    zero(q);
    let qn = min(q.len, qq.len);
    copy_into(q[..qn], qq[..qn]);
}

// --- decimal conversion ----------------------------------------------------------

let CHUNK = 19;                                  // 10^19 < 2^64
let TEN19: u64 = 10000000000000000000;

// The low digits of a (of at most four limbs) right-aligned in out, with
// leading zeros, by division by 10^9 on 32-bit halves.
fn base_digits(out: u8[:], a: u64[:]) {
    var w: u32[8] = [0; 8];
    var n = 0;
    for i in a.len {
        w[2 * i] = a[i] as! u32;
        w[2 * i + 1] = (a[i] >> 32) as! u32;
        n = 2 * i + 2;
    }
    var pos = out.len;
    while pos > 0 {
        var rem: u64 = 0;
        var j = n;
        while j > 0 {
            j--;
            let cur = (rem << 32) | w[j];
            w[j] = (cur / 1000000000) as! u32;
            rem = cur % 1000000000;
        }
        while n > 0 && w[n - 1] == 0 { n--; }
        for d in 9 {
            if pos == 0 { break; }
            pos--;
            out[pos] = ('0' + rem % 10) as! u8;
            rem /= 10;
        }
    }
}

// The digits of a, of at most CHUNK * 2^(i+1) of them, right-aligned in out,
// with leading zeros; pows[i] is 10^(CHUNK * 2^i).
recursive fn to_digits(out: u8[:], a: u64[:], pows: Divisor[:], i: i64, s: u64[:]) {
    let an = norm(a);
    if an.len <= 2 || i < 0 {
        assert(an.len <= 4);
        base_digits(out, an);
        return;
    }
    let dv = pows[i];
    let m = dv.d.len;
    if an.len < m {
        to_digits(out, an, pows, i - 1, s);
        return;
    }
    let ql = an.len - m + 1;
    let q = s[..ql];
    let r = s[ql..ql + m];
    divrem(q, r, an, dv, s[ql + m..]);
    let low = CHUNK << i;
    to_digits(out[..out.len - low], q, pows, i - 1, s[ql + m..]);
    to_digits(out[out.len - low..], r, pows, i - 1, s[ql + m..]);
}

// --- the series ------------------------------------------------------------------

fn bitlen(v: i64) -> i64 { 64 - clz(v as u64) }

// Limbs enough for p and q of sum_terms(a, b) and its intermediate results.
fn space(a: i64, b: i64) -> i64 { 2 * (((b - a) * bitlen(b) + 63) / 64 + 2) + 8 }

// a! (1/(a+1)! + ... + 1/b!) as p / q, with q = (a+1) ... b: p and then q
// into the front of out, returning their lengths.
recursive fn sum_terms(a: i64, b: i64, out: u64[:], s: u64[:]) -> i64, i64 {
    if b == a + 1 {
        out[0] = 1;
        out[1] = b as u64;
        return 1, 1;
    }
    let mid = (a + b) / 2;
    let ls = space(a, mid);
    let rs = space(mid, b);
    let left = s[..ls];
    let right = s[ls..ls + rs];
    let rest = s[ls + rs..];
    let pln, qln = sum_terms(a, mid, left, rest);
    let prn, qrn = sum_terms(mid, b, right, rest);
    let pl = left[..pln];
    let ql = left[pln..pln + qln];
    let pr = right[..prn];
    let qr = right[prn..prn + qrn];
    // p = pl qr + pr, q = ql qr
    var pn = pln + qrn;
    mul(out[..pn], pl, qr, rest);
    let c = add_in(out[..pn], pr);
    if c != 0 { out[pn] = c; pn++; }
    pn = top(out[..pn]);
    let qn = qln + qrn;
    mul(out[pn..pn + qn], ql, qr, rest);
    return pn, top(out[pn..pn + qn]);
}

// The smallest k with log10(k!) >= n + 50, by Stirling's approximation.
fn terms_needed(n: i64) -> i64 {
    fn ok(k: i64) -> bool {
        if k <= 0 { return false; }
        let kf = k as f64;
        let ln_k_factorial = kf * log(kf) - kf + 0.5 * log(TAU * kf);
        return ln_k_factorial / LN_10 >= n + 50;
    }
    var a = 0;
    var b = 1;
    while !ok(b) { a = b; b *= 2; }
    while b - a > 1 {
        let m = a + (b - a) / 2;
        if ok(m) { b = m; } else { a = m; }
    }
    return b;
}

fn main() {
    let a = args();
    var n = 27;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    let k = terms_needed(n);

    let size = space(0, k - 1);                   // limbs of p and q
    var mem: u64[>..] = [];
    mem.resize(12 * size + 4096, 0);
    let pq = mem[..size];
    let s = mem[size..];

    let pn, qn = sum_terms(0, k - 1, pq, s);
    // p += q
    let qv = pq[pn..pn + qn];
    let pv = s[..max(pn, qn) + 1];
    zero(pv);
    copy_into(pv[..pn], pq[..pn]);
    add_in(pv, qv);
    let p = norm(pv);
    let q = norm(qv);
    let work = s[pv.len..];

    // 10^(n-1), by squaring from the top bit of the exponent
    let e = n - 1;
    let pw_cap = e * 3322 / 1000 / 64 + 4;        // log2(10) < 3.322
    let pw = work[..pw_cap];
    let tmp = work[pw_cap..3 * pw_cap];
    let work2 = work[3 * pw_cap..];
    zero(pw);
    pw[0] = 1;
    var pl = 1;
    var bit = bitlen(e) - 1;
    while bit >= 0 {
        sqr(tmp[..2 * pl], pw[..pl], work2);
        pl = top(tmp[..2 * pl]);
        copy_into(pw[..pl], tmp[..pl]);
        if ((e >> bit) & 1) != 0 {
            let c = mul_1(pw[..pl], pw[..pl], 10);
            if c != 0 { pw[pl] = c; pl++; }
        }
        bit--;
    }
    // answer = p 10^(n-1) / q
    let num = work2[..p.len + pl];
    mul(num, p, pw[..pl], work2[p.len + pl..]);
    let numn = norm(num);
    let w3 = work2[p.len + pl..];
    let qdiv = divisor(w3[..q.len], q);
    let w4 = w3[q.len..];
    let ansl = numn.len - q.len + 1;
    let ans = w4[..ansl];
    let remd = w4[ansl..ansl + q.len];
    let w5 = w4[ansl + q.len..];
    divrem(ans, remd, numn, qdiv, w5);
    let answer = norm(ans);

    // the powers 10^(19 2^i) the conversion divides by
    var pows: Divisor[>..] = [];
    var plen = 1;
    var cur = w5[..1];
    cur[0] = TEN19;
    var free = w5[1..];
    var chunk = CHUNK;
    while chunk < n {
        pows.push(divisor(free[..plen], cur));
        free = free[plen..];
        chunk *= 2;
        if chunk >= n { break; }
        let sq = free[..2 * plen];
        sqr(sq, cur, free[2 * plen..]);
        plen = top(sq);
        cur = sq[..plen];
        free = free[2 * plen..];
    }
    var text: u8[>..] = [];
    text.resize(chunk, '0');
    to_digits(text, answer, pows, pows.len - 1, free);
    var start = 0;
    while start < text.len - 1 && text[start] == '0' { start++; }
    let digits = text[start..];

    var out: u8[>..] = [];
    var i = 0;
    while i < n {
        if i + 10 <= n {
            out.append(digits[i..i + 10]);
            format(out, "\t:", i + 10, "\n");
        } else {
            out.append(digits[i..n]);
            for j in 10 - n % 10 { out.push(' '); }
            format(out, "\t:", n, "\n");
        }
        i += 10;
    }
    write_stdout(out);
}
