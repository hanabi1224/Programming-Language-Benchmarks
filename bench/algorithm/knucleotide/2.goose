// The third sequence is read into memory once and coded two bits a
// nucleotide (A 0, C 1, T 2, G 3: bits 1 and 2 of the letter, either case),
// in place over the file's own bytes. Each k-nucleotide is then the last k
// codes packed into a u64, the key of the library's dictionary counting
// them. (1.goose counts with a hash table of its own.)

import std;
import os;
import dictionary;

// The key, with a hash of its own: std's hash of a u64 scatters the small
// dense keys of short k-nucleotides as it would any, into the collisions of
// a random hash, where this one keeps them apart and is cheaper.
struct Code { v: u64 }
fn hash(c: Code) -> u64 { c.v ^ (c.v >> 7) }

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

fn count(seq: u8[:], k: i64) -> dictionary<Code, i32> {
    var counts = dictionary<Code, i32> {};
    let mask: u64 = (1 << (2 * k)) - 1;
    var codes: u64 = 0;
    for c in seq[..k - 1] { codes = (codes << 2) | c; }
    for c in seq[k - 1..] {
        codes = (codes << 2) | c;
        let key = codes & mask;
        counts.update(Code { key }, 0) { it += 1; };
    }
    return counts;
}

fn decode(out: u8[>..]&, key: u64, k: i64) {
    for i in k {
        out.push("ACTG"[((key >> (2 * (k - 1 - i))) & 3) as! i64]);
    }
}

struct Entry { key: u64, count: i32 }

fn frequencies(out: u8[>..]&, seq: u8[:], k: i64) {
    var counts = count(seq, k);
    var entries: Entry[>..] = [];
    var total = 0;
    counts.each() { key, n => entries.push(Entry { key.v, n }); total += n; };
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
    var counts = count(seq, name.len);
    var key: u64 = 0;
    for c in name { key = (key << 2) | code_of(c); }
    let n = counts.get(Code { key });
    format(out, if n { n } else { 0 }, "\t", name, "\n");
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
