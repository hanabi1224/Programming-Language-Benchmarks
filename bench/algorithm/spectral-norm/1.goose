// A product works on eight rows at once, one row a lane, so that each column
// is one vector division. Within a row the entries of A are made by addition
// as the column advances, as in the C++ program by Bela Pecsek: going from
// column j - 1 to j adds r + j to A(r, j) and r + j + 1 to A(j, r), all of
// them integers that doubles hold exactly. Each row's sum runs over the
// columns in order, so every product is the same to the bit as a plain
// row-by-row loop computes it.

import std;
import os;
import math;

// dst[k] = row lo + k of A src, or with transposed, of A' src.
simd fn times(dst: f64[:], src: f64[:], lo: i64, transposed: bool) {
    let n = src.len;
    var i = 0;
    while i < dst.len {
        var a: f64[8] = [0.0; 8];
        var step: f64[8] = [0.0; 8];
        var sum: f64[8] = [0.0; 8];
        for k in 8 {
            let r = (lo + i + k) as f64;
            // A(r, 0) = r (r + 1) / 2 + r + 1, and A(0, r) = r (r + 1) / 2 + 1.
            a[k] = r * (r + 1.0) * 0.5 + 1.0;
            step[k] = r;
            if transposed { step[k] += 1.0; } else { a[k] += r; }
            sum[k] = src[0] / a[k];
        }
        for j in 1..n {
            let s = src[j];
            for k in 8 {
                step[k] += 1.0;
                a[k] += step[k];
                sum[k] += s / a[k];
            }
        }
        for k in 8 {
            if i + k < dst.len { dst[i + k] = sum[k]; }
        }
        i += 8;
    }
}

fn times_ata(dst: f64[:], src: f64[:], tmp: f64[:]) {
    times(tmp, src, 0, false);
    times(dst, tmp, 0, true);
}

fn main() {
    let a = args();
    var n = 100;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    var u: f64[>..] = [];
    var v: f64[>..] = [];
    var tmp: f64[>..] = [];
    u.resize(n, 1.0);
    v.resize(n, 0.0);
    tmp.resize(n, 0.0);
    for i in 10 {
        times_ata(v, u, tmp);
        times_ata(u, v, tmp);
    }
    var vbv = 0.0;
    var vv = 0.0;
    for i in n {
        vbv += u[i] * v[i];
        vv += v[i] * v[i];
    }
    var line: u8[>..] = [];
    format_flt(line, sqrt(vbv / vv), 9);
    print(line);
}
