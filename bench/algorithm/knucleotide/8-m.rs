// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Tom Kaitchuck

// Based on k-nucleotide Rust #7
// Switched to used Hashbrown and removed custom hash code.
// Removed rayon and use threads directly
// Copied the read_input function from k-nucleotide Rust #4

use hashbrown::HashMap;
use std::io::{BufRead, BufReader};
use std::sync::Arc;
use std::thread;

type Map = HashMap<Code, u32>;

#[derive(Hash, PartialEq, PartialOrd, Ord, Eq, Clone, Copy)]
struct Code(u64);
impl Code {
    fn push(&mut self, c: u8, mask: u64) {
        self.0 <<= 2;
        self.0 |= c as u64;
        self.0 &= mask;
    }
    fn from_str(s: &str) -> Code {
        let mask = Code::make_mask(s.len());
        let mut res = Code(0);
        for c in s.as_bytes() {
            res.push(Code::encode_byte(c), mask);
        }
        res
    }
    fn to_string(&self, frame: usize) -> String {
        let mut res = vec![];
        let mut code = self.0;
        for _ in 0..frame {
            let c = match code as u8 & 0b11 {
                c if c == Code::encode_byte(&b'A') => b'A',
                c if c == Code::encode_byte(&b'T') => b'T',
                c if c == Code::encode_byte(&b'G') => b'G',
                c if c == Code::encode_byte(&b'C') => b'C',
                _ => unreachable!(),
            };
            res.push(c);
            code >>= 2;
        }
        res.reverse();
        String::from_utf8(res).unwrap()
    }
    fn make_mask(frame: usize) -> u64 {
        (1u64 << (2 * frame)) - 1
    }
    #[inline(always)]
    fn encode_byte(c: &u8) -> u8 {
        (c >> 1) & 0b11
    }
}

struct Iter<'a> {
    iter: std::slice::Iter<'a, u8>,
    code: Code,
    mask: u64,
}
impl<'a> Iter<'a> {
    fn new(input: &[u8], frame: usize) -> Iter {
        let mut iter = input.iter();
        let mut code = Code(0);
        let mask = Code::make_mask(frame);
        for c in iter.by_ref().take(frame - 1) {
            code.push(*c, mask);
        }
        Iter {
            iter: iter,
            code: code,
            mask: mask,
        }
    }
}
impl<'a> Iterator for Iter<'a> {
    type Item = Code;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|&c| {
            self.code.push(c, self.mask);
            self.code
        })
    }
}

fn gen_freq(input: &[u8], frame: usize) -> Map {
    let mut freq = Map::default();
    for code in Iter::new(input, frame) {
        *freq.entry(code).or_insert(0) += 1;
    }
    freq
}

#[derive(Clone, Copy)]
struct Freq(usize);
#[derive(Clone, Copy)]
struct Occ(&'static str);

impl Freq {
    fn print(&self, freq: &Map) {
        let mut v: Vec<_> = freq.iter().map(|(&code, &count)| (count, code)).collect();
        v.sort();
        let total = v.iter().map(|&(count, _)| count).sum::<u32>() as f32;
        for &(count, key) in v.iter().rev() {
            println!(
                "{} {:.3}",
                key.to_string(self.0),
                (count as f32 * 100.) / total
            );
        }
        println!("");
    }
}
impl Occ {
    fn print(&self, freq: &Map) {
        let count = if freq.contains_key(&Code::from_str(self.0)) {
            freq[&Code::from_str(self.0)]
        } else {
            0
        };
        println!("{}\t{}", count, self.0);
    }
}

fn read_input() -> Vec<u8> {
    let file_name = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .unwrap_or("25000_in".into());

    let file = std::fs::File::open(file_name).unwrap();
    let mut r = BufReader::new(file);
    let key = ">THREE";
    let mut res = Vec::with_capacity(65536);
    let mut line = String::with_capacity(64);

    loop {
        match r.read_line(&mut line) {
            Ok(b) if b > 0 => {
                if line.starts_with(key) {
                    break;
                }
            }
            _ => break,
        }
        line.clear();
    }

    loop {
        line.clear();
        match r.read_line(&mut line) {
            Ok(b) if b > 0 => {
                let bytes = line.as_bytes();
                res.extend(bytes[..bytes.len() - 1].into_iter().map(Code::encode_byte))
            }
            _ => break,
        }
    }
    res
}

fn main() {
    let occs = vec![
        Occ("GGTATTTTAATTTATAGT"),
        Occ("GGTATTTTAATT"),
        Occ("GGTATT"),
        Occ("GGTA"),
        Occ("GGT"),
    ];
    let input = Arc::new(read_input());

    // In reverse to spawn big tasks first
    let results: Vec<_> = occs
        .into_iter()
        .map(|item| {
            let input = input.clone();
            thread::spawn(move || (item, gen_freq(&input, item.0.len())))
        })
        .collect();

    Freq(1).print(&gen_freq(&input, 1));
    Freq(2).print(&gen_freq(&input, 2));

    for t in results.into_iter().rev() {
        let (item, freq) = t.join().unwrap();
        item.print(&freq);
    }
}
