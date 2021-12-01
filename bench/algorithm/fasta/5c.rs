// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// multi-threaded version contributed by Alisdair Owens
// improvements made to Rust #2 by pmarcelll
//     * deleted floor() from normalize() since p is always nonnegative
//     * switched from AoS to SoA (idea borrowed from the C++ solution)
//     * also preallocate the whole buffer for the data arrays
//     * cleaned up code a bit (reordering, renaming, formatting, etc.)
// add compile-time calculation by hanabi1224

#![feature(const_fn_floating_point_arithmetic)]

use std::cmp::min;
use std::io;
use std::io::{BufWriter, ErrorKind, Write};
use std::sync::{Arc, Mutex};
use std::thread;

const LINE_LENGTH: usize = 60;
const IM: u32 = 139968;
const LINES: usize = 1024;
const BLKLEN: usize = LINE_LENGTH * LINES;

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
    count: usize,
    thread_count: u16,
    next_thread_num: u16,
}

impl MyRandom {
    fn new(count: usize, thread_count: u16) -> MyRandom {
        MyRandom {
            last: 42,
            count: count,
            thread_count: thread_count,
            next_thread_num: 0,
        }
    }

    fn reset(&mut self, count: usize) {
        self.next_thread_num = 0;
        self.count = count;
    }

    fn gen(&mut self, buf: &mut [u32], cur_thread: u16) -> Result<usize, ()> {
        if self.next_thread_num != cur_thread {
            return Err(());
        }

        self.next_thread_num += 1;
        if self.next_thread_num == self.thread_count {
            self.next_thread_num = 0;
        }

        let to_gen = min(buf.len(), self.count);
        for i in 0..to_gen {
            self.last = (self.last * 3877 + 29573) % IM;
            buf[i] = self.last;
        }
        self.count -= to_gen;
        Ok(to_gen)
    }
}

struct MyStdOut {
    thread_count: u16,
    next_thread_num: u16,
    stdout: io::Stdout,
}

impl MyStdOut {
    fn new(thread_count: u16) -> MyStdOut {
        MyStdOut {
            thread_count: thread_count,
            next_thread_num: 0,
            stdout: io::stdout(),
        }
    }

    fn write(&mut self, data: &[u8], cur_thread: u16) -> io::Result<()> {
        if self.next_thread_num != cur_thread {
            return Err(io::Error::new(ErrorKind::Other, ""));
        }

        self.next_thread_num += 1;
        if self.next_thread_num == self.thread_count {
            self.next_thread_num = 0;
        }

        self.stdout.write_all(data)
    }
}

fn make_fasta_single<I: Iterator<Item = u8>>(
    header: impl AsRef<[u8]>,
    mut it: I,
    mut n: usize,
) -> io::Result<()> {
    let mut stdout = BufWriter::new(io::stdout());
    stdout.write_all(header.as_ref())?;
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

fn do_fasta<const N: usize>(
    thread_num: u16,
    rng: Arc<Mutex<MyRandom>>,
    wr: Arc<Mutex<MyStdOut>>,
    data: &[(u32, u8); N],
) {
    let mut rng_buf = [0u32; BLKLEN];
    let mut out_buf = [0u8; BLKLEN + LINES];
    let mut count;
    loop {
        loop {
            if let Ok(x) = rng.lock().unwrap().gen(&mut rng_buf, thread_num) {
                count = x;
                break;
            }
        }

        if count == 0 {
            break;
        }
        let mut line_count = 0;
        for i in 0..count {
            if i % LINE_LENGTH == 0 && i > 0 {
                out_buf[i + line_count] = b'\n';
                line_count += 1;
            }
            let rn = rng_buf[i];
            for (p, ch) in data {
                if p >= &rn {
                    out_buf[i + line_count] = *ch;
                    break;
                }
            }
        }
        out_buf[count + line_count] = b'\n';

        while let Err(_) = wr
            .lock()
            .unwrap()
            .write(&out_buf[..(count + line_count + 1)], thread_num)
        {}
    }
}

fn make_fasta<const N: usize>(
    header: impl AsRef<[u8]>,
    rng: Arc<Mutex<MyRandom>>,
    data: &'static [(u32, u8); N],
    num_threads: u16,
) -> io::Result<()> {
    let stdout = Arc::new(Mutex::new(MyStdOut::new(num_threads)));
    io::stdout().write_all(header.as_ref())?;
    let mut threads = Vec::with_capacity(num_threads as usize);
    for thread in 0..num_threads {
        let rng = rng.clone();
        let stdout = stdout.clone();
        threads.push(thread::spawn(move || {
            do_fasta(thread, rng, stdout, data);
        }));
    }
    for thread_guard in threads {
        thread_guard.join().unwrap();
    }
    Ok(())
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(1000);

    let num_threads: u16 = num_cpus::get() as u16;

    let rng = Arc::new(Mutex::new(MyRandom::new(n * 3, num_threads)));

    make_fasta_single(
        b">ONE Homo sapiens alu\n",
        ALU.iter().cycle().map(|c| *c),
        n * 2,
    )
    .unwrap();

    make_fasta(
        b">TWO IUB ambiguity codes\n",
        rng.clone(),
        &IUB_RAND,
        num_threads,
    )
    .unwrap();

    rng.lock().unwrap().reset(n * 5);

    make_fasta(
        b">THREE Homo sapiens frequency\n",
        rng,
        &HOMOSAPIENS_RAND,
        num_threads,
    )
    .unwrap();

    io::stdout().flush().unwrap();
}
