// The third sequence is read into memory once and coded two bits a
// nucleotide (A 0, C 1, T 2, G 3: bits 1 and 2 of the letter, either case),
// in place over the file's own bytes. Each k-nucleotide is then the last k
// codes packed into an integer, counted in a hash table of this program's
// own, as C's program has: open addressing and linear probing over a
// power-of-two array of u64 slots, each holding a key and its count, which
// starts at 16 slots and doubles at half load. Half rather than the usual
// three quarters, as most lookups then end at the first slot they try,
// which spares a mispredicted branch on most of the others. (2.goose counts
// with the library's dictionary instead.)

import std;
import os;

// A slot holds 0 when it is unused, else the key, a set bit and the count in
// the low COUNT_BITS bits: one compare of the bits above the count tests the
// key, and the count increments in place.
let COUNT_BITS = 27;
let COUNT_MASK: u64 = (1 << COUNT_BITS) - 1;

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

// The table of the k-nucleotides of seq.
fn count(seq: u8[:], k: i64) -> u64[>..] {
    var slots: u64[>..] = [];
    slots.resize(16, 0);
    var used = 0;
    let key_mask: u64 = (1 << (2 * k)) - 1;
    // The codes so far, the last k of them the key: shifting the earlier
    // ones out of the word rather than masking them off keeps the masking
    // out of the chain from one position to the next.
    var codes: u64 = 0;
    for c in seq[..k - 1] { codes = (codes << 2) | c; }
    for c in seq[k - 1..] {
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
    return slots;
}

fn decode(out: u8[>..]&, key: u64, k: i64) {
    for i in k {
        out.push("ACTG"[((key >> (2 * (k - 1 - i))) & 3) as! i64]);
    }
}

struct Entry { key: u64, count: i64 }

fn frequencies(out: u8[>..]&, seq: u8[:], k: i64) {
    let slots = count(seq, k);
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

fn occurrences(out: u8[>..]&, seq: u8[:], name: u8[:]) {
    let slots = count(seq, name.len);
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

fn main() {
    let a = args();
    assert(a.len > 1);
    var text: u8[>..] = [];
    if !read_file(a[1], text) { abort("cannot read the input"); }
    let seq = read_three(text);
    var out: u8[>..] = [];
    frequencies(out, seq, 1);
    frequencies(out, seq, 2);
    for name in ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"] {
        occurrences(out, seq, name);
    }
    write_stdout(out);
}
