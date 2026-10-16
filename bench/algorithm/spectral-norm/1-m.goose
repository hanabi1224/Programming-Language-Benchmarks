// The products of 1.goose, with each product's rows dealt out to worker
// threads in as many slices as there are workers. A worker shares no memory
// with the main program, so a slice's job carries the vector it multiplies,
// and its result comes back as the slice's part of the product.

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
