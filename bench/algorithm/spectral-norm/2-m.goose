// The products of 2.goose, with each product's rows dealt out to worker
// threads in as many slices as there are workers. A worker shares no memory
// with the main program, so a slice's job carries the vector it multiplies,
// and its result comes back as the slice's part of the product.

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

struct Job { lo: i64, hi: i64, transposed: bool, src: f64[] }   // hi < 0: no more work
struct Part { lo: i64, vals: f64[] }

fn rows(job: Job&) -> f64[>..] {
    var out: f64[>..] = [];
    out.resize(job.hi - job.lo, 0.0);
    times(out, job.src, job.lo, job.transposed);
    return out;
}

thread_fn worker() {
    loop {
        let job = qget<Job>();
        guard job.hi >= 0 else { break; }
        qput(Part { lo: job.lo, vals: rows(job) });
    }
}

fn product(dst: f64[:], src: f64[:], transposed: bool, slice: i64) {
    let n = src.len;
    var parts = 0;
    var lo = 0;
    while lo < n {
        let hi = min(lo + slice, n);
        qput(Job { lo: lo, hi: hi, transposed: transposed, src: src });
        parts++;
        lo = hi;
    }
    for k in parts {
        let p = qget<Part>();
        copy_into(dst[p.lo..p.lo + p.vals.len], p.vals);
    }
}

fn times_ata(dst: f64[:], src: f64[:], tmp: f64[:], slice: i64) {
    product(tmp, src, false, slice);
    product(dst, tmp, true, slice);
}

fn main() {
    let a = args();
    var n = 100;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    let workers = max(1, hardware_threads());
    // A multiple of the eight rows times works on at once.
    let slice = ((n + workers - 1) / workers + 7) / 8 * 8;
    var ids: i64[>..] = [];
    for w in workers { ids.push(thread_spawn(worker)); }
    var u: f64[>..] = [];
    var v: f64[>..] = [];
    var tmp: f64[>..] = [];
    u.resize(n, 1.0);
    v.resize(n, 0.0);
    tmp.resize(n, 0.0);
    for i in 10 {
        times_ata(v, u, tmp, slice);
        times_ata(u, v, tmp, slice);
    }
    for w in workers { qput(Job { lo: 0, hi: -1, transposed: false, src: [] }); }
    for id in ids { thread_wait(id); }
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
