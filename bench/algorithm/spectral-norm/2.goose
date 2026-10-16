// A product works on eight rows at once, one row a lane, and on four columns
// at once: as in the C program with intrinsics by Miles, the four terms
// u[j] / A(r, j) of a row are summed over the product of their four
// denominators, so that the four columns take one vector division. Within
// a row the entries of A are made by addition as the column advances, as in
// the C++ program by Bela Pecsek: going from column j - 1 to j adds r + j to
// A(r, j) and r + j + 1 to A(j, r), all of them integers that doubles hold
// exactly.

import std;
import os;
import math;

// dst[k] = row lo + k of A src, or with transposed, of A' src.
simd fn times(dst: f64[:], src: f64[:], lo: i64, transposed: bool) {
    let cols = src[1..];
    var i = 0;
    while i < dst.len {
        // a holds the entry of the last column done, and d0 to d3 what the
        // next four columns' entries add to it.
        var a: f64[8] = [0.0; 8];
        var d0: f64[8] = [0.0; 8];
        var d1: f64[8] = [0.0; 8];
        var d2: f64[8] = [0.0; 8];
        var d3: f64[8] = [0.0; 8];
        var sum: f64[8] = [0.0; 8];
        for k in 8 {
            let r = (lo + i + k) as f64;
            // A(r, 0) = r (r + 1) / 2 + r + 1, and A(0, r) = r (r + 1) / 2 + 1.
            a[k] = r * (r + 1.0) * 0.5 + 1.0;
            var step = r + 1.0;
            if transposed { step += 1.0; } else { a[k] += r; }
            sum[k] = src[0] / a[k];
            d0[k] = step;
            d1[k] = 2.0 * step + 1.0;
            d2[k] = 3.0 * step + 3.0;
            d3[k] = 4.0 * step + 6.0;
        }
        var j = 0;
        while j + 4 <= cols.len {
            let s0 = cols[j];
            let s1 = cols[j + 1];
            let s2 = cols[j + 2];
            let s3 = cols[j + 3];
            for k in 8 {
                let a0 = a[k] + d0[k];
                let a1 = a[k] + d1[k];
                let a2 = a[k] + d2[k];
                let a3 = a[k] + d3[k];
                a[k] = a3;
                d0[k] += 4.0;
                d1[k] += 8.0;
                d2[k] += 12.0;
                d3[k] += 16.0;
                // s0 / a0 + s1 / a1 + s2 / a2 + s3 / a3, over a0 a1 a2 a3.
                let p01 = a0 * a1;
                let p012 = p01 * a2;
                let n01 = s0 * a1 + s1 * a0;
                let n012 = n01 * a2 + s2 * p01;
                sum[k] += (n012 * a3 + s3 * p012) / (p012 * a3);
            }
            j += 4;
        }
        while j < cols.len {
            let s = cols[j];
            for k in 8 {
                a[k] += d0[k];
                d0[k] += 1.0;
                sum[k] += s / a[k];
            }
            j++;
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
