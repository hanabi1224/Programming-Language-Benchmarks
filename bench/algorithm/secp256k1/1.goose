// secp256k1 point multiplication with the algorithm of the hand-written
// programs (1.rs, after 1.ts): Jacobian coordinates, the scalar split by the
// GLV endomorphism into two halves of about 128 bits that share one chain of
// doublings, and an extended-Euclid inversion back to affine coordinates.
// Field elements are four 64-bit limbs, kept below p = 2^256 - 2^32 - 977;
// the scalar split and the inversion use 384-bit two's complement integers.
// The constants are the Rust program's, written out as limbs.

import std;
import os;

type Fe = u64[4];
type Big = u64[6];

struct Jp { x: Fe, y: Fe, z: Fe }

let PC: u64 = 0x1000003D1;      // 2^256 - p

let P: Fe = [0xFFFFFFFEFFFFFC2F, 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF];
let N: Big = [0xBFD25E8CD0364141, 0xBAAEDCE6AF48A03B, 0xFFFFFFFFFFFFFFFE, 0xFFFFFFFFFFFFFFFF, 0, 0];
let BETA: Fe = [0xc1396c28719501ee, 0x9cf0497512f58995, 0x6e64479eac3434e9, 0x7ae96a2b657c0710];
let A1: Big = [0xe86c90e49284eb15, 0x3086d221a7d46bcd, 0, 0, 0, 0];
let MINUS_B1: Big = [0x6f547fa90abfe4c3, 0xe4437ed6010e8828, 0, 0, 0, 0];
let A2: Big = [0x57c1108d9d44cfd8, 0x14ca50f7a8e2f3f6, 1, 0, 0, 0];
let PRIVATE_KEY: Big = [0xD9132D70B5BC7693, 0xB64592E3FE0AB0DA, 0x4FCA3EF970FF4D38, 0x2DEE927079283C3C, 0, 0];
let GX: Fe = [0x59F2815B16F81798, 0x029BFCDB2DCE28D9, 0x55A06295CE870B07, 0x79BE667EF9DCBBAC];
let GY: Fe = [0x9C47D08FFB10D4B8, 0xFD17B448A6855419, 0x5DA4FBFC0E1108A8, 0x483ADA7726A3C465];

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

// --- the field ---------------------------------------------------------------

// t (eight limbs) modulo p, using 2^256 = PC (mod p).
inline fn reduce(t: u64[8]) -> Fe {
    var r: Fe = [0; 4];
    var c: u64 = 0;
    for i in 4 {
        let h = t[i + 4];
        var lo = h * PC;
        var hi = mulhi(h, PC);
        lo += c;
        if lo < c { hi++; }
        let x = t[i] + lo;
        if x < lo { hi++; }
        r[i] = x;
        c = hi;
    }
    // c < 2^34, and c * PC < 2^68
    var lo = c * PC;
    var hi = mulhi(c, PC);
    r[0] += lo;
    if r[0] < lo { hi++; }
    r[1] += hi;
    var carry = r[1] < hi;
    for i in 2..4 {
        if carry { r[i]++; carry = r[i] == 0; }
    }
    if carry { r[0] += PC; }          // what wraps past 2^256 is tiny, so this cannot carry
    return canon(r);
}

// r modulo p, for r < 2^256.
inline fn canon(r: Fe) -> Fe {
    var s: Fe = r;
    s[0] += PC;
    var carry = s[0] < PC;
    for i in 1..4 {
        if carry { s[i]++; carry = s[i] == 0; }
    }
    if carry { return s; }
    return r;
}

inline fn fe_mul(a: Fe, b: Fe) -> Fe {
    var t: u64[8] = [0; 8];
    for i in 4 {
        var c: u64 = 0;
        let ai = a[i];
        for j in 4 {
            var lo = ai * b[j];
            var hi = mulhi(ai, b[j]);
            lo += c;
            if lo < c { hi++; }
            let x = t[i + j] + lo;
            if x < lo { hi++; }
            t[i + j] = x;
            c = hi;
        }
        t[i + 4] = c;
    }
    return reduce(t);
}

// The products of distinct limbs once, doubled, and the squares of the limbs.
inline fn fe_sqr(a: Fe) -> Fe {
    var t: u64[8] = [0; 8];
    for i in 3 {
        var c: u64 = 0;
        let ai = a[i];
        for j in i + 1..4 {
            var lo = ai * a[j];
            var hi = mulhi(ai, a[j]);
            lo += c;
            if lo < c { hi++; }
            let x = t[i + j] + lo;
            if x < lo { hi++; }
            t[i + j] = x;
            c = hi;
        }
        t[i + 4] = c;
    }
    var top: u64 = 0;
    for k in 1..8 {
        let x = t[k];
        t[k] = (x << 1) | top;
        top = x >> 63;
    }
    var c: u64 = 0;
    for i in 4 {
        var lo = a[i] * a[i];
        var hi = mulhi(a[i], a[i]);
        lo += c;
        if lo < c { hi++; }
        let x = t[2 * i] + lo;
        if x < lo { hi++; }
        t[2 * i] = x;
        let y = t[2 * i + 1] + hi;
        c = if y < hi { 1 } else { 0 };
        t[2 * i + 1] = y;
    }
    return reduce(t);
}

inline fn fe_add(a: Fe, b: Fe) -> Fe {
    var s: Fe = [0; 4];
    var c = false;
    for i in 4 {
        let x = a[i] + b[i];
        let c1 = x < a[i];
        let y = if c { x + 1 } else { x };
        s[i] = y;
        c = c1 || (c && y == 0);
    }
    if c {                            // a + b - 2^256 + PC is below p
        s[0] += PC;
        var k = s[0] < PC;
        for i in 1..4 { if k { s[i]++; k = s[i] == 0; } }
        return s;
    }
    return canon(s);
}

inline fn fe_sub(a: Fe, b: Fe) -> Fe {
    var s: Fe = [0; 4];
    var borrow = false;
    for i in 4 {
        let x = a[i] - b[i];
        let b1 = a[i] < b[i];
        let y = if borrow { x - 1 } else { x };
        s[i] = y;
        borrow = b1 || (borrow && x == 0);
    }
    if borrow {                       // a - b + 2^256 - PC = a - b + p
        let old = s[0];
        s[0] -= PC;
        var k = old < PC;
        for i in 1..4 { if k { k = s[i] == 0; s[i]--; } }
    }
    return s;
}

fn fe_is_zero(a: Fe) -> bool { (a[0] | a[1] | a[2] | a[3]) == 0 }

fn fe_neg(a: Fe) -> Fe { fe_sub([0; 4], a) }

// --- 384-bit integers ---------------------------------------------------------

fn big(a: Fe) -> Big { [a[0], a[1], a[2], a[3], 0, 0] }

fn fe(a: Big) -> Fe { [a[0], a[1], a[2], a[3]] }

fn big_add(a: Big, b: Big) -> Big {
    var s: Big = [0; 6];
    var c: u64 = 0;
    for i in 6 {
        let x = a[i] + b[i];
        let y = x + c;
        c = if x < a[i] || y < x { 1 } else { 0 };
        s[i] = y;
    }
    return s;
}

fn big_neg(a: Big) -> Big {
    var s: Big = [0; 6];
    var c: u64 = 1;
    for i in 6 {
        let y = ~a[i] + c;
        c = if y < c { 1 } else { 0 };
        s[i] = y;
    }
    return s;
}

fn big_sub(a: Big, b: Big) -> Big { big_add(a, big_neg(b)) }

// The product modulo 2^384, which is the product itself for every operand
// pair the program forms.
fn big_mul(a: Big, b: Big) -> Big {
    var t: Big = [0; 6];
    for i in 6 {
        var c: u64 = 0;
        let ai = a[i];
        if ai == 0 { continue; }
        for j in 6 - i {
            var lo = ai * b[j];
            var hi = mulhi(ai, b[j]);
            lo += c;
            if lo < c { hi++; }
            let x = t[i + j] + lo;
            if x < lo { hi++; }
            t[i + j] = x;
            c = hi;
        }
    }
    return t;
}

fn big_is_neg(a: Big) -> bool { (a[5] >> 63) != 0 }

fn big_is_zero(a: Big) -> bool { (a[0] | a[1] | a[2] | a[3] | a[4] | a[5]) == 0 }

// Unsigned comparison: -1, 0 or 1.
fn big_cmp(a: Big, b: Big) -> i64 {
    var i = 5;
    while i >= 0 {
        if a[i] != b[i] { return if a[i] < b[i] { -1 } else { 1 }; }
        i--;
    }
    return 0;
}

fn big_bits(a: Big) -> i64 {
    var i = 5;
    while i >= 0 {
        if a[i] != 0 { return i * 64 + 64 - clz(a[i]); }
        i--;
    }
    return 0;
}

fn big_shl(a: Big, k: i64) -> Big {
    var s: Big = [0; 6];
    let w = k >> 6;
    let b = k & 63;
    for i in w..6 {
        var x = a[i - w] << b;
        if b > 0 && i - w > 0 { x |= a[i - w - 1] >> (64 - b); }
        s[i] = x;
    }
    return s;
}

fn big_shr1(a: Big) -> Big {
    var s: Big = [0; 6];
    for i in 6 {
        var x = a[i] >> 1;
        if i < 5 { x |= a[i + 1] << 63; }
        s[i] = x;
    }
    return s;
}

// Quotient and remainder of non-negative a and positive d, by shifting and
// subtracting.
fn big_divmod(a: Big, d: Big) -> Big, Big {
    var q: Big = [0; 6];
    var r = a;
    let shift = big_bits(a) - big_bits(d);
    if shift < 0 { return q, r; }
    var m = big_shl(d, shift);
    var i = shift;
    while i >= 0 {
        if big_cmp(r, m) >= 0 {
            r = big_sub(r, m);
            q[i >> 6] |= (1 as u64) << (i & 63);
        }
        m = big_shr1(m);
        i--;
    }
    return q, r;
}

// The Euclidean remainder of a signed a modulo a positive m.
fn big_rem(a: Big, m: Big) -> Big {
    if !big_is_neg(a) {
        let q, r = big_divmod(a, m);
        return r;
    }
    let q, r = big_divmod(big_neg(a), m);
    if big_is_zero(r) { return r; }
    return big_sub(m, r);
}

fn fe_bits(a: Fe) -> i64 {
    var i = 3;
    while i >= 0 {
        if a[i] != 0 { return i * 64 + 64 - clz(a[i]); }
        i--;
    }
    return 0;
}

// The 64 bits of a from bit s up.
fn fe_bits_at(a: Fe, s: i64) -> u64 {
    let w = s >> 6;
    let b = s & 63;
    var x = a[w] >> b;
    if b > 0 && w < 3 { x |= a[w + 1] << (64 - b); }
    return x;
}

fn big_mul_u64(a: Big, q: u64) -> Big {
    var t: Big = [0; 6];
    var c: u64 = 0;
    for i in 6 {
        var lo = a[i] * q;
        var hi = mulhi(a[i], q);
        lo += c;
        if lo < c { hi++; }
        t[i] = lo;
        c = hi;
    }
    return t;
}

// b / a and b % a where the quotient is below 2^32, from the leading 64 bits
// of both and a correction of at most a few steps; false where the quotient
// may be larger.
fn divmod_short(b: Fe, a: Fe) -> u64, Fe, bool {
    let nb = fe_bits(b);
    if nb - fe_bits(a) > 31 { return 0, b, false; }
    let s = max(nb - 64, 0);
    var q = fe_bits_at(b, s) / fe_bits_at(a, s);
    // r = b - q * a, five limbs wide
    var r: u64[5] = [b[0], b[1], b[2], b[3], 0];
    var c: u64 = 0;
    var borrow = false;
    for i in 5 {
        var lo: u64 = 0;
        var hi: u64 = 0;
        if i < 4 { lo = a[i] * q; hi = mulhi(a[i], q); }
        lo += c;
        if lo < c { hi++; }
        let x = r[i] - lo;
        let b1 = r[i] < lo;
        let y = if borrow { x - 1 } else { x };
        borrow = b1 || (borrow && x == 0);
        r[i] = y;
        c = hi;
    }
    while (r[4] >> 63) != 0 {          // q was too large
        q--;
        var k: u64 = 0;
        for i in 5 {
            let ai = if i < 4 { a[i] } else { 0 };
            let x = r[i] + ai;
            let y = x + k;
            k = if x < ai || y < x { 1 } else { 0 };
            r[i] = y;
        }
    }
    var rem: Fe = [r[0], r[1], r[2], r[3]];
    while fe_bits(rem) >= fe_bits(a) && big_cmp(big(rem), big(a)) >= 0 {
        q++;
        rem = fe(big_sub(big(rem), big(a)));
    }
    return q, rem, true;
}

fn invert(number: Fe) -> Fe {
    var a = number;
    var b = P;
    var x: Big = [0; 6];
    var y: Big = [1, 0, 0, 0, 0, 0];
    var u: Big = [1, 0, 0, 0, 0, 0];
    var v: Big = [0; 6];
    while !fe_is_zero(a) {
        var m: Big = [0; 6];
        var n: Big = [0; 6];
        let qs, rs, short = divmod_short(b, a);
        var r = rs;
        if short {
            m = big_sub(x, big_mul_u64(u, qs));
            n = big_sub(y, big_mul_u64(v, qs));
        } else {
            let q, rb = big_divmod(big(b), big(a));
            r = fe(rb);
            m = big_sub(x, big_mul(u, q));
            n = big_sub(y, big_mul(v, q));
        }
        b = a;
        a = r;
        x = u;
        y = v;
        u = m;
        v = n;
    }
    return fe(big_rem(x, big(P)));
}

fn div_nearest(a: Big, b: Big) -> Big {
    let q, r = big_divmod(big_add(a, big_shr1(b)), b);
    return q;
}

// k = k1 + k2 * lambda (mod n), with k1 and k2 of about 128 bits, each with
// the sign it is to be applied with.
fn split_scalar_endo(k: Big) -> bool, Big, bool, Big {
    let b2 = A1;
    let c1 = div_nearest(big_mul(b2, k), N);
    let c2 = div_nearest(big_mul(MINUS_B1, k), N);
    var k1 = big_rem(big_sub(big_sub(k, big_mul(c1, A1)), big_mul(c2, A2)), N);
    var k2 = big_rem(big_sub(big_mul(c1, MINUS_B1), big_mul(c2, b2)), N);
    let pow_2_128: Big = [0, 0, 1, 0, 0, 0];
    let k1neg = big_cmp(k1, pow_2_128) > 0;
    let k2neg = big_cmp(k2, pow_2_128) > 0;
    if k1neg { k1 = big_sub(N, k1); }
    if k2neg { k2 = big_sub(N, k2); }
    return k1neg, k1, k2neg, k2;
}

// --- the curve -----------------------------------------------------------------

fn jp_zero() -> Jp { Jp { x: [0; 4], y: [1, 0, 0, 0], z: [0; 4] } }

fn jp_double(p: Jp) -> Jp {
    let a = fe_sqr(p.x);
    let b = fe_sqr(p.y);
    let c = fe_sqr(b);
    let t = fe_sub(fe_sub(fe_sqr(fe_add(p.x, b)), a), c);
    let d = fe_add(t, t);
    let e = fe_add(fe_add(a, a), a);
    let f = fe_sqr(e);
    let x3 = fe_sub(f, fe_add(d, d));
    let c2 = fe_add(c, c);
    let c4 = fe_add(c2, c2);
    let y3 = fe_sub(fe_mul(e, fe_sub(d, x3)), fe_add(c4, c4));
    let yz = fe_mul(p.y, p.z);
    return Jp { x: x3, y: y3, z: fe_add(yz, yz) };
}

fn jp_add(p: Jp, q: Jp) -> Jp {
    if fe_is_zero(q.x) || fe_is_zero(q.y) { return p; }
    if fe_is_zero(p.x) || fe_is_zero(p.y) { return q; }
    let z1z1 = fe_sqr(p.z);
    let z2z2 = fe_sqr(q.z);
    let u1 = fe_mul(p.x, z2z2);
    let u2 = fe_mul(q.x, z1z1);
    let s1 = fe_mul(fe_mul(p.y, q.z), z2z2);
    let s2 = fe_mul(fe_mul(q.y, p.z), z1z1);
    let h = fe_sub(u2, u1);
    let r = fe_sub(s2, s1);
    if fe_is_zero(h) {
        if fe_is_zero(r) { return jp_double(p); }
        return jp_zero();
    }
    let hh = fe_sqr(h);
    let hhh = fe_mul(h, hh);
    let v = fe_mul(u1, hh);
    let x3 = fe_sub(fe_sub(fe_sqr(r), hhh), fe_add(v, v));
    let y3 = fe_sub(fe_mul(r, fe_sub(v, x3)), fe_mul(s1, hhh));
    let z3 = fe_mul(fe_mul(p.z, q.z), h);
    return Jp { x: x3, y: y3, z: z3 };
}

fn bit(k: Big, i: i64) -> bool { ((k[i >> 6] >> (i & 63)) & 1) != 0 }

fn jp_mul(p: Jp, scalar: Big) -> Jp {
    let k1neg, k1, k2neg, k2 = split_scalar_endo(scalar);
    var k1p = jp_zero();
    var k2p = jp_zero();
    var d = p;
    let bits = max(big_bits(k1), big_bits(k2));
    for i in bits {
        if bit(k1, i) { k1p = jp_add(k1p, d); }
        if bit(k2, i) { k2p = jp_add(k2p, d); }
        d = jp_double(d);
    }
    if k1neg { k1p.y = fe_neg(k1p.y); }
    if k2neg { k2p.y = fe_neg(k2p.y); }
    k2p.x = fe_mul(k2p.x, BETA);
    return jp_add(k1p, k2p);
}

fn point_mul(x: Fe, y: Fe, scalar: Big) -> Fe, Fe {
    let r = jp_mul(Jp { x: x, y: y, z: [1, 0, 0, 0] }, scalar);
    let inv_z = invert(r.z);
    let inv_z_pow = fe_sqr(inv_z);
    return fe_mul(r.x, inv_z_pow), fe_mul(fe_mul(r.y, inv_z), inv_z_pow);
}

fn format_hex(out: u8[>..]&, a: Fe) {
    var i = 3;
    while i > 0 && a[i] == 0 { i--; }
    format_uint(out, a[i], 16, 0, '0');
    while i > 0 {
        i--;
        format_uint(out, a[i], 16, 16, '0');
    }
}

fn main() {
    let a = args();
    var n = 1;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    var x = GX;
    var y = GY;
    for i in n { x, y = point_mul(x, y, PRIVATE_KEY); }
    var line: u8[>..] = [];
    format_hex(line, x);
    line.push(',');
    format_hex(line, y);
    print(line);
}
