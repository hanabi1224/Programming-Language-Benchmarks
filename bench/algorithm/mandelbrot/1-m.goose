// The rows of 1.goose, rendered by worker threads a few rows to a job. The
// main program puts the rows into the image as they come back and hashes
// the image as far as it is complete while the rest is rendered.

import std;
import os;

let MAX_ITER = 50;
let BLOCK = 32;

// The bits of the pixels at cr in the row at ci, eight to a byte with the
// leftmost pixel in the high bit.
inline fn mbrot(cr: f64[BLOCK], ci: f64) -> u8[BLOCK / 8] {
    var bytes: u8[BLOCK / 8] = [0; BLOCK / 8];
    var zr: f64[BLOCK] = [0.0; BLOCK];
    var zi: f64[BLOCK] = [0.0; BLOCK];
    var tr: f64[BLOCK] = [0.0; BLOCK];
    var ti: f64[BLOCK] = [0.0; BLOCK];
    for i in MAX_ITER / 5 {
        for j in 5 {
            for k in BLOCK {
                zi[k] = (zr[k] + zr[k]) * zi[k] + ci;
                zr[k] = tr[k] - ti[k] + cr[k];
                tr[k] = zr[k] * zr[k];
                ti[k] = zi[k] * zi[k];
            }
        }
        var inside = false;
        for k in BLOCK {
            if tr[k] + ti[k] <= 4.0 { inside = true; }
        }
        if !inside { return bytes; }
    }
    for b in BLOCK / 8 {
        var bits: u8 = 0;
        for k in 8 {
            if tr[b * 8 + k] + ti[b * 8 + k] <= 4.0 { bits |= 0x80 >> k; }
        }
        bytes[b] = bits;
    }
    return bytes;
}

// One row of the image, BLOCK / 8 bytes to a block of xs.
simd fn render_row(out: u8[:], xs: f64[BLOCK][:], ci: f64) {
    assert(out.len == xs.len * (BLOCK / 8));
    for cr, i in xs {
        copy_into(out[i * (BLOCK / 8)..(i + 1) * (BLOCK / 8)], mbrot(cr, ci));
    }
}

// The real parts of a row's pixels, in blocks; the last block runs past size.
fn x_coords(size: i64) -> f64[BLOCK][>..] {
    let inv = 2.0 / size;
    var xs: f64[BLOCK][>..] = [];
    for x in (size + BLOCK - 1) / BLOCK {
        var cr: f64[BLOCK] = [0.0; BLOCK];
        for k in BLOCK { cr[k] = (x * BLOCK + k) * inv - 1.5; }
        xs.push(cr);
    }
    return xs;
}

// MD5 (RFC 1321).
const MD5_S: u32[64] = [
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
];
const MD5_K: u32[64] = [
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
];

fn md5_block(h: u32[4]&, blk: u8[:]) {
    assert(blk.len == 64);
    var w: u32[16] = [0; 16];
    for i in 16 {
        w[i] = blk[4 * i] | (blk[4 * i + 1] as u32 << 8) | (blk[4 * i + 2] as u32 << 16) |
               (blk[4 * i + 3] as u32 << 24);
    }
    var a = h[0];
    var b = h[1];
    var c = h[2];
    var d = h[3];
    for i in 16 {
        let t = a + ((b & c) | (~b & d)) + MD5_K[i] + w[i];
        a = d;
        d = c;
        c = b;
        b = b + ((t << MD5_S[i]) | (t >> (32 - MD5_S[i])));
    }
    for i in 16..32 {
        let t = a + ((d & b) | (~d & c)) + MD5_K[i] + w[(5 * i + 1) % 16];
        a = d;
        d = c;
        c = b;
        b = b + ((t << MD5_S[i]) | (t >> (32 - MD5_S[i])));
    }
    for i in 32..48 {
        let t = a + (b ^ c ^ d) + MD5_K[i] + w[(3 * i + 5) % 16];
        a = d;
        d = c;
        c = b;
        b = b + ((t << MD5_S[i]) | (t >> (32 - MD5_S[i])));
    }
    for i in 48..64 {
        let t = a + (c ^ (b | ~d)) + MD5_K[i] + w[(7 * i) % 16];
        a = d;
        d = c;
        c = b;
        b = b + ((t << MD5_S[i]) | (t >> (32 - MD5_S[i])));
    }
    h[0] += a;
    h[1] += b;
    h[2] += c;
    h[3] += d;
}

struct Md5 { h: u32[4], len: i64 }

fn md5() -> Md5 { Md5 { h: [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476], len: 0 } }

// Hashes data, a whole number of blocks.
fn md5_blocks(m: Md5&, data: u8[:]) {
    assert(data.len % 64 == 0);
    var i = 0;
    while i < data.len {
        md5_block(m.h, data[i..i + 64]);
        i += 64;
    }
    m.len += data.len;
}

// Hashes rest, less than a block, and the padding, and appends the digest
// in hex to out.
fn md5_finish(m: Md5&, rest: u8[:], out: u8[>..]&) {
    assert(rest.len < 64);
    var tail: u8[128] = [0; 128];
    for k in rest.len { tail[k] = rest[k]; }
    tail[rest.len] = 0x80;
    let end = if rest.len < 56 { 64 } else { 128 };
    let bitlen = ((m.len + rest.len) as u64) << 3;
    for k in 8 { tail[end - 8 + k] = (bitlen >> (8 * k)) as! u8; }
    md5_block(m.h, tail[0..64]);
    if end == 128 { md5_block(m.h, tail[64..128]); }
    for word in m.h {
        for k in 4 { format_uint(out, (word >> (8 * k)) & 0xff, 16, 2, '0'); }
    }
}

let ROWS_PER_JOB = 8;

struct Job { y0: i64, y1: i64 }          // y0 < 0: no more work
struct Rows { y0: i64, bits: u8[] }

fn render_rows(xs: f64[BLOCK][:], size: i64, y0: i64, y1: i64) -> u8[>..] {
    let inv = 2.0 / size;
    let row_bytes = size / 8;
    var out: u8[>..] = [];
    out.resize((y1 - y0) * row_bytes, 0);
    var row: u8[>..] = [];
    row.resize(xs.len * (BLOCK / 8), 0);
    for y in y0..y1 {
        render_row(row, xs, y * inv - 1.0);
        copy_into(out[(y - y0) * row_bytes..(y - y0 + 1) * row_bytes], row[..row_bytes]);
    }
    return out;
}

thread_fn worker(size: i64) {
    let xs = x_coords(size);
    loop {
        let job = qget<Job>();
        guard job.y0 >= 0 else { break; }
        qput(Rows { y0: job.y0, bits: render_rows(xs, size, job.y0, job.y1) });
    }
}

fn main() {
    let a = args();
    var n = 200;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    let size = (n + 7) / 8 * 8;
    let row_bytes = size / 8;
    let jobs = (size + ROWS_PER_JOB - 1) / ROWS_PER_JOB;
    let workers = max(1, hardware_threads());
    for j in jobs { qput(Job { y0: j * ROWS_PER_JOB, y1: min(size, (j + 1) * ROWS_PER_JOB) }); }
    for w in workers { qput(Job { y0: -1, y1: -1 }); }
    var ids: i64[>..] = [];
    for w in workers { ids.push(thread_spawn(worker, size)); }
    var image: u8[>..] = [];
    image.resize(size * row_bytes, 0);
    var ready: bool[>..] = [];
    ready.resize(jobs, false);
    var next = 0;
    var hashed = 0;
    var m = md5();
    for k in jobs {
        let r = qget<Rows>();
        let at = r.y0 * row_bytes;
        copy_into(image[at..at + r.bits.len], r.bits);
        ready[r.y0 / ROWS_PER_JOB] = true;
        while next < jobs && ready[next] { next++; }
        let upto = min(size, next * ROWS_PER_JOB) * row_bytes / 64 * 64;
        if upto > hashed {
            md5_blocks(m, image[hashed..upto]);
            hashed = upto;
        }
    }
    for id in ids { thread_wait(id); }
    var out: u8[>..] = [];
    format(out, "P4\n", size, " ", size, "\n");
    md5_finish(m, image[hashed..], out);
    print(out);
}
