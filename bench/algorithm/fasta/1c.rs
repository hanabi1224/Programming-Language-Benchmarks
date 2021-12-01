// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// add compile-time calculation by hanabi1224

#![feature(const_fn_floating_point_arithmetic)]

use std::cmp::min;
use std::io::{self, BufWriter, Write};

const LINE_LENGTH: usize = 60;
const IM: u32 = 139968;

const ALU: &[u8] = b"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

const fn normalize_rand(p: f32) -> u32 {
    (p * IM as f32) as u32
}

const fn make_random<const N: usize>(data: [(u8, f32); N]) -> [(u32, u8); N] {
    let mut acc = 0.;
    let mut i = 0;
    let mut buf = [(0, 0); N];
    while i < N {
        let (ch, p) = data[i];
        acc += p;
        buf[i].0 = normalize_rand(acc);
        buf[i].1 = ch;
        i += 1;
    }
    buf
}

const IUB: [(u8, f32); 15] = [
    (b'a', 0.27),
    (b'c', 0.12),
    (b'g', 0.12),
    (b't', 0.27),
    (b'B', 0.02),
    (b'D', 0.02),
    (b'H', 0.02),
    (b'K', 0.02),
    (b'M', 0.02),
    (b'N', 0.02),
    (b'R', 0.02),
    (b'S', 0.02),
    (b'V', 0.02),
    (b'W', 0.02),
    (b'Y', 0.02),
];
const IUB_RAND: [(u32, u8); 15] = make_random(IUB);
const HOMOSAPIENS: [(u8, f32); 4] = [
    (b'a', 0.3029549426680),
    (b'c', 0.1979883004921),
    (b'g', 0.1975473066391),
    (b't', 0.3015094502008),
];
const HOMOSAPIENS_RAND: [(u32, u8); 4] = make_random(HOMOSAPIENS);

struct MyRandom {
    last: u32,
}
impl MyRandom {
    fn new() -> MyRandom {
        MyRandom { last: 42 }
    }
    fn gen(&mut self) -> u32 {
        self.last = (self.last * 3877 + 29573) % IM;
        self.last
    }
}

struct AAGen<'a, const N: usize> {
    rng: &'a mut MyRandom,
    data: &'static [(u32, u8); N],
}
impl<'a, const N: usize> AAGen<'a, N> {
    fn new<'b>(rng: &'b mut MyRandom, aa: &'static [(u32, u8); N]) -> AAGen<'b, N> {
        AAGen { rng: rng, data: aa }
    }
}
impl<'a, const N: usize> Iterator for AAGen<'a, N> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        let r = self.rng.gen();
        self.data
            .iter()
            .skip_while(|pc| pc.0 < r)
            .map(|&(_, c)| c)
            .next()
    }
}

fn make_fasta<I: Iterator<Item = u8>>(header: &str, mut it: I, mut n: usize) -> anyhow::Result<()> {
    let mut stdout = BufWriter::new(io::stdout());
    stdout.write_all(header.as_bytes())?;
    let mut line = [0u8; LINE_LENGTH + 1];
    while n > 0 {
        let nb = min(LINE_LENGTH, n);
        for i in 0..nb {
            line[i] = it.next().unwrap();
        }
        n -= nb;
        line[nb] = '\n' as u8;
        stdout.write_all(&line[..(nb + 1)])?;
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(1000);

    let rng = &mut MyRandom::new();

    make_fasta(
        ">ONE Homo sapiens alu\n",
        ALU.iter().cycle().map(|&c| c),
        n * 2,
    )?;
    make_fasta(
        ">TWO IUB ambiguity codes\n",
        AAGen::new(rng, &IUB_RAND),
        n * 3,
    )?;
    make_fasta(
        ">THREE Homo sapiens frequency\n",
        AAGen::new(rng, &HOMOSAPIENS_RAND),
        n * 5,
    )?;
    Ok(())
}
