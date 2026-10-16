// The output is built in one buffer, written out and reused every 1 MB:
// stdout is unbuffered, so each write is a system call. The cumulative
// probabilities become integer thresholds, and each random number's
// nucleotide is the number of thresholds at or below it: a linear search
// without branches, which the C compiler does in vector registers.

import std;
import os;
import math;

let LINE = 60;
let FLUSH = 1 << 20;
let IM = 139968;

let ALU = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

// The benchmark's naive linear congruential generator.
inline fn next_random(s: u64) -> u64 { (s * 3877 + 29573) % IM }

fn flush_if_full(out: u8[>..]&) {
    if out.len >= FLUSH {
        write_stdout(out);
        out.clear();
    }
}

fn repeat_fasta(out: u8[>..]&, seq: u8[:], count: i64) {
    // The sequence followed by a line's worth of its start, so that every
    // line is one slice of it.
    var padded: u8[>..] = [];
    padded.append(seq);
    padded.append(seq[..LINE]);
    var off = 0;
    var left = count;
    while left > 0 {
        let len = min(LINE, left);
        out.append(padded[off..off + len]);
        out.push('\n');
        off += len;
        if off >= seq.len { off -= seq.len; }
        left -= len;
        flush_if_full(out);
    }
}

// Appends count nucleotides of the distribution, returning the generator's
// state to go on from.
fn random_fasta(out: u8[>..]&, seed: u64, probs: f64[:], chars: u8[:], count: i64) -> u64 {
    // Padded to 16 with thresholds no random number reaches.
    var thresholds: i32[16] = [0x7FFFFFFF; 16];
    var table: u8[16] = [0; 16];
    var cum = 0.0;
    for p, i in probs {
        cum += p;
        thresholds[i] = ceil(cum * IM) as i32;
        table[i] = chars[i];
    }
    var s = seed;
    var left = count;
    while left > 0 {
        let len = min(LINE, left);
        let start = out.len;
        out.resize(start + len + 1, '\n');
        for &c in out[start..start + len] {
            s = next_random(s);
            let r = s as! i32;
            var k = 0;
            for t in thresholds { k += if t <= r { 1 } else { 0 }; }
            c = table[k & 15];
        }
        left -= len;
        flush_if_full(out);
    }
    return s;
}

fn main() {
    let a = args();
    var n = 1000;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    var out: u8[>..] = [];
    out.append(">ONE Homo sapiens alu\n");
    repeat_fasta(out, ALU, 2 * n);
    var seed: u64 = 42;
    out.append(">TWO IUB ambiguity codes\n");
    seed = random_fasta(out, seed, [0.27, 0.12, 0.12, 0.27, 0.02, 0.02, 0.02, 0.02,
                                    0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02],
                        "acgtBDHKMNRSVWY", 3 * n);
    out.append(">THREE Homo sapiens frequency\n");
    seed = random_fasta(out, seed, [0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008],
                        "acgt", 5 * n);
    write_stdout(out);
}
