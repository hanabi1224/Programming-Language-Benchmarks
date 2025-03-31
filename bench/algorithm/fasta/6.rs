// Ported from 1.zig

use std::io::{self, BufWriter, StdoutLock, Write};

const MAX_LINE_LENGTH: usize = 60;
const IM: u32 = 139968;
const IA: u32 = 3877;
const IC: u32 = 29573;

struct AminoAcid {
    l: u8,
    p: f64,
}

struct Random {
    seed: u32,
}

impl Random {
    fn new() -> Self {
        Self { seed: 42 }
    }

    fn next(&mut self) -> f64 {
        self.seed = (self.seed * IA + IC) % IM;
        (IM as f64 * self.seed as f64) / IM as f64
    }
}

impl Iterator for Random {
    type Item = f64;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next())
    }
}

fn repeat_and_wrap(
    out: &mut BufWriter<StdoutLock<'static>>,
    sequence: &[u8],
    count: usize,
) -> io::Result<()> {
    let n = sequence.len();
    let padded_sequence = (0..(n + MAX_LINE_LENGTH))
        .map(|i| sequence[i % n])
        .collect::<Vec<_>>();

    let mut off = 0;
    let mut idx = 0;
    while idx < count {
        let rem = count - idx;
        let line_length = MAX_LINE_LENGTH.min(rem);
        out.write_all(&padded_sequence[off..off + line_length])?;
        out.write_all(&[b'\n'])?;

        off += line_length;
        if off > n {
            off -= n;
        }
        idx += line_length;
    }
    Ok(())
}

fn generate_and_wrap(
    random: &mut Random,
    out: &mut BufWriter<StdoutLock<'static>>,
    nucleotides: &[AminoAcid],
    count: usize,
) -> io::Result<()> {
    let mut cum_prob = 0.0;
    let cum_prob_total = nucleotides
        .iter()
        .map(|&AminoAcid { l: _, p }| {
            cum_prob += p;
            cum_prob * IM as f64
        })
        .collect::<Vec<_>>();

    let mut line = [0u8; MAX_LINE_LENGTH + 1];
    line[MAX_LINE_LENGTH] = b'\n';

    let mut idx = 0;
    while idx < count {
        let rem = count - idx;
        let line_length = MAX_LINE_LENGTH.min(rem);

        line[..line_length]
            .iter_mut()
            .zip(&mut *random)
            .for_each(|(col, r)| {
                let c = cum_prob_total
                    .iter()
                    .fold(0, |acc, &n| acc + (n <= r) as usize);

                *col = nucleotides[c].l;
            });

        line[line_length] = b'\n';
        out.write_all(&line[..line_length + 1])?;
        idx += line_length;
    }
    Ok(())
}

pub fn main() -> io::Result<()> {
    let stdout = io::stdout();
    let mut stdout = io::BufWriter::new(stdout.lock());
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.as_os_str().to_str().and_then(|s| s.parse().ok()))
        .unwrap_or(1000);

    let homo_sapiens_alu =
        b"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTC\
AGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCG\
TGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGG\
AGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

    stdout.write_all(b">ONE Homo sapiens alu\n".as_slice())?;
    repeat_and_wrap(&mut stdout, &homo_sapiens_alu[..], 2 * n)?;
    let mut random = Random::new();

    let iub_nucleotide_info = &[
        AminoAcid { l: b'a', p: 0.27 },
        AminoAcid { l: b'c', p: 0.12 },
        AminoAcid { l: b'g', p: 0.12 },
        AminoAcid { l: b't', p: 0.27 },
        AminoAcid { l: b'B', p: 0.02 },
        AminoAcid { l: b'D', p: 0.02 },
        AminoAcid { l: b'H', p: 0.02 },
        AminoAcid { l: b'K', p: 0.02 },
        AminoAcid { l: b'M', p: 0.02 },
        AminoAcid { l: b'N', p: 0.02 },
        AminoAcid { l: b'R', p: 0.02 },
        AminoAcid { l: b'S', p: 0.02 },
        AminoAcid { l: b'V', p: 0.02 },
        AminoAcid { l: b'W', p: 0.02 },
        AminoAcid { l: b'Y', p: 0.02 },
    ];

    stdout.write_all(b">TWO IUB ambiguity codes\n".as_slice())?;
    generate_and_wrap(&mut random, &mut stdout, iub_nucleotide_info, 3 * n)?;

    let homo_sapien_nucleotide_info = &[
        AminoAcid {
            l: b'a',
            p: 0.3029549426680,
        },
        AminoAcid {
            l: b'c',
            p: 0.1979883004921,
        },
        AminoAcid {
            l: b'g',
            p: 0.1975473066391,
        },
        AminoAcid {
            l: b't',
            p: 0.3015094502008,
        },
    ];

    stdout.write_all(b">THREE Homo sapiens frequency\n".as_slice())?;
    generate_and_wrap(&mut random, &mut stdout, homo_sapien_nucleotide_info, 5 * n)?;
    Ok(())
}
