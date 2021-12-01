// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// use BufWriter by hanabi1224

use std::cmp::min;
use std::io::{self, BufWriter, Write};

const LINE_LENGTH: usize = 60;
const IM: u32 = 139968;

struct MyRandom {
    last: u32,
}
impl MyRandom {
    fn new() -> MyRandom {
        MyRandom { last: 42 }
    }
    fn normalize(p: f32) -> u32 {
        (p * IM as f32).floor() as u32
    }
    fn gen(&mut self) -> u32 {
        self.last = (self.last * 3877 + 29573) % IM;
        self.last
    }
}

struct AAGen<'a> {
    rng: &'a mut MyRandom,
    data: Vec<(u32, u8)>,
}
impl<'a> AAGen<'a> {
    fn new<'b>(rng: &'b mut MyRandom, aa: &[(char, f32)]) -> AAGen<'b> {
        let mut cum = 0.;
        let data = aa
            .iter()
            .map(|&(ch, p)| {
                cum += p;
                (MyRandom::normalize(cum), ch as u8)
            })
            .collect();
        AAGen {
            rng: rng,
            data: data,
        }
    }
}
impl<'a> Iterator for AAGen<'a> {
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
    let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
        GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
        CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
        ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
        GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
        AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
        AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
    let iub = &[
        ('a', 0.27),
        ('c', 0.12),
        ('g', 0.12),
        ('t', 0.27),
        ('B', 0.02),
        ('D', 0.02),
        ('H', 0.02),
        ('K', 0.02),
        ('M', 0.02),
        ('N', 0.02),
        ('R', 0.02),
        ('S', 0.02),
        ('V', 0.02),
        ('W', 0.02),
        ('Y', 0.02),
    ];
    let homosapiens = &[
        ('a', 0.3029549426680),
        ('c', 0.1979883004921),
        ('g', 0.1975473066391),
        ('t', 0.3015094502008),
    ];

    make_fasta(
        ">ONE Homo sapiens alu\n",
        alu.as_bytes().iter().cycle().map(|c| *c),
        n * 2,
    )?;
    make_fasta(">TWO IUB ambiguity codes\n", AAGen::new(rng, iub), n * 3)?;
    make_fasta(
        ">THREE Homo sapiens frequency\n",
        AAGen::new(rng, homosapiens),
        n * 5,
    )?;

    Ok(())
}
