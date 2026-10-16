// The single-threaded program's reading and counting (see 1.goose), with
// each k counted by a worker thread of its own: the workers take jobs from a
// queue, the largest first, and send back the text of their part of the
// output. Each worker gets a copy of the coded sequence as it starts, packed
// four codes a byte to keep the copies small.

import std;
import os;

// A slot holds 0 when it is unused, else the key, a set bit and the count in
// the low COUNT_BITS bits: one compare of the bits above the count tests the
// key, and the count increments in place.
let COUNT_BITS = 27;
let COUNT_MASK: u64 = (1 << COUNT_BITS) - 1;
let CHUNK = 1 << 14;

fn home(key: u64) -> i64 { (key ^ (key >> 7)) as! i64 }

fn code_of(c: u8) -> u8 { (c >> 1) & 3 }

// The codes of the sequence after the line starting with ">THREE", written
// over the start of text.
fn read_three(text: u8[:]) -> u8[:] {
    let header = byte_set(">");
    var at = 0;
    loop {
        let k = find_any(text[at..], header);
        assert(k >= 0);
        at += k;
        if starts_with(text[at..], ">THREE") { break; }
        at += 1;
    }
    at += find(text[at..], "\n") + 1;
    var end = find_any(text[at..], header);
    end = if end < 0 { text.len } else { at + end };
    // Every byte is stored, and the next one overwrites a newline's.
    var n = 0;
    for c in text[at..end] {
        text[n] = code_of(c);
        n += if c != '\n' { 1 } else { 0 };
    }
    return text[..n];
}

// Doubles the table, putting every entry in again.
fn grow(slots: u64[>..]&) {
    var old: u64[>..] = [];
    old.append(slots);
    slots.clear();
    slots.resize(old.len * 2, 0);
    let mask = slots.len - 1;
    for v in old {
        guard v != 0;
        var i = home(v >> (COUNT_BITS + 1)) & mask;
        while slots[i] != 0 { i = (i + 1) & mask; }
        slots[i] = v;
    }
}

// Four codes a byte, the first in the top bits.
fn pack(seq: u8[:]) -> u8[>..] {
    var packed: u8[>..] = [];
    packed.resize((seq.len + 3) / 4, 0);
    for c, i in seq { packed[i >> 2] |= c << (6 - 2 * (i & 3)); }
    return packed;
}

// The table of the k-nucleotides of the n codes packed, which are unpacked
// a chunk at a time into a buffer that stays in the cache.
fn count(packed: u8[:], n: i64, k: i64) -> u64[>..] {
    var slots: u64[>..] = [];
    slots.resize(16, 0);
    var used = 0;
    var chunk: u8[>..] = [];
    chunk.resize(CHUNK, 0);
    let key_mask: u64 = (1 << (2 * k)) - 1;
    // The codes so far, the last k of them the key (see 1.goose).
    var codes: u64 = 0;
    var pos = 0;
    while pos < n {
        let m = min(CHUNK, n - pos);
        for b, i in packed[pos >> 2..(pos + m + 3) >> 2] {
            let d = chunk[4 * i..4 * i + 4];
            d[0] = b >> 6;
            d[1] = (b >> 4) & 3;
            d[2] = (b >> 2) & 3;
            d[3] = b & 3;
        }
        var from = 0;
        if pos == 0 {
            for c in chunk[..k - 1] { codes = (codes << 2) | c; }
            from = k - 1;
        }
        for c in chunk[from..m] {
            codes = (codes << 2) | c;
            let key = codes & key_mask;
            let tag = (key << 1) | 1;
            let mask = slots.len - 1;
            var i = home(key);
            loop {
                let s = i & mask;
                let v = slots[s];
                if v >> COUNT_BITS == tag { slots[s] = v + 1; break; }
                if v == 0 {
                    slots[s] = (tag << COUNT_BITS) | 1;
                    used++;
                    if used * 2 > slots.len { grow(slots); }
                    break;
                }
                i++;
            }
        }
        pos += m;
    }
    return slots;
}

fn decode(out: u8[>..]&, key: u64, k: i64) {
    for i in k {
        out.push("ACTG"[((key >> (2 * (k - 1 - i))) & 3) as! i64]);
    }
}

struct Entry { key: u64, count: i64 }

fn frequencies(out: u8[>..]&, packed: u8[:], n: i64, k: i64) {
    let slots = count(packed, n, k);
    var entries: Entry[>..] = [];
    var total = 0;
    for v in slots {
        guard v != 0;
        let n = (v & COUNT_MASK) as! i64;
        entries.push(Entry { v >> (COUNT_BITS + 1), n });
        total += n;
    }
    sort(entries) { a, b => a.count > b.count || (a.count == b.count && a.key > b.key) };
    for e in entries {
        decode(out, e.key, k);
        out.push(' ');
        format_flt(out, (e.count as f32 * 100.0) / total, 3);
        out.push('\n');
    }
    out.push('\n');
}

fn occurrences(out: u8[>..]&, packed: u8[:], n: i64, name: u8[:]) {
    let slots = count(packed, n, name.len);
    var key: u64 = 0;
    for c in name { key = (key << 2) | code_of(c); }
    let tag = (key << 1) | 1;
    var n: u64 = 0;
    let mask = slots.len - 1;
    var i = home(key) & mask;
    while slots[i] != 0 {
        if slots[i] >> COUNT_BITS == tag { n = slots[i] & COUNT_MASK; break; }
        i = (i + 1) & mask;
    }
    format(out, n, "\t", name, "\n");
}

// A part of the output: the frequencies of the k-nucleotides where name is
// empty, else the count of name; k == 0 tells a worker to stop.
struct Job { part: i64, k: i64, name: u8[..18] }
struct Report { part: i64, text: u8[] }

thread_fn worker(packed: u8[], n: i64) {
    loop {
        let job = qget<Job>();
        guard job.k > 0 else { break; }
        var out: u8[>..] = [];
        if job.name.len == 0 {
            frequencies(out, packed, n, job.k);
        } else {
            occurrences(out, packed, n, job.name);
        }
        qput(Report { part: job.part, text: copy(out) });
    }
}

fn main() {
    let a = args();
    assert(a.len > 1);
    var text: u8[>..] = [];
    if !read_file(a[1], text) { abort("cannot read the input"); }
    let seq = read_three(text);
    let packed = pack(seq);
    let names = ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"];
    let parts = 2 + names.len;
    for i in names.len {
        let name = names[names.len - 1 - i];
        qput(Job { part: parts - 1 - i, k: name.len, name: name });
    }
    qput(Job { part: 1, k: 2, name: "" });
    qput(Job { part: 0, k: 1, name: "" });
    let workers = clamp(hardware_threads(), 1, parts);
    for i in workers { qput(Job { part: 0, k: 0, name: "" }); }
    var ids: i64[>..] = [];
    for i in workers { ids.push(thread_spawn(worker, packed[..], seq.len)); }
    // The parts arrive in any order; at[p] is where part p's text went.
    var reports: u8[>..] = [];
    var at: i64[>..] = [];
    at.resize(parts, 0);
    var len: i64[>..] = [];
    len.resize(parts, 0);
    for i in parts {
        let r = qget<Report>();
        at[r.part] = reports.len;
        len[r.part] = r.text.len;
        reports.append(r.text);
    }
    for id in ids { thread_wait(id); }
    var out: u8[>..] = [];
    for p in parts { out.append(reports[at[p]..at[p] + len[p]]); }
    write_stdout(out);
}
