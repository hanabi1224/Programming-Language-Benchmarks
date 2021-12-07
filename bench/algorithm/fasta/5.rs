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
// small tweaks by hanabi1224

use std::cmp::min;
use std::io;
use std::io::{BufWriter, ErrorKind, Write};
use std::sync::{Arc, Mutex};
use std::thread;

const LINE_LENGTH: usize = 60;
const IM: u32 = 139968;
const LINES: usize = 1024;
const BLKLEN: usize = LINE_LENGTH * LINES;

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

    fn normalize(p: f32) -> u32 {
        (p * IM as f32) as u32
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

fn make_random(data: &[(u8, f32)]) -> (Box<[u32]>, Box<[u8]>) {
    let mut acc = 0.;
    let mut buf_p = Vec::with_capacity(data.len());
    let mut buf_ch = Vec::with_capacity(data.len());

    for &(ch, p) in data {
        acc += p;
        buf_p.push(MyRandom::normalize(acc));
        buf_ch.push(ch);
    }

    (buf_p.into(), buf_ch.into())
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
        line[nb] = b'\n';
        stdout.write_all(&line[..(nb + 1)])?;
    }
    Ok(())
}

fn do_fasta(
    thread_num: u16,
    rng: Arc<Mutex<MyRandom>>,
    wr: Arc<Mutex<MyStdOut>>,
    data: (Box<[u32]>, Box<[u8]>),
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
            for j in data.0.iter().zip(data.1.iter()) {
                if *j.0 >= rn {
                    out_buf[i + line_count] = *j.1;
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

fn make_fasta(
    header: impl AsRef<[u8]>,
    rng: Arc<Mutex<MyRandom>>,
    data: (Box<[u32]>, Box<[u8]>),
    num_threads: u16,
) -> io::Result<()> {
    let stdout = Arc::new(Mutex::new(MyStdOut::new(num_threads)));
    io::stdout().write_all(header.as_ref())?;
    let mut threads = Vec::with_capacity(num_threads as usize);
    for thread in 0..num_threads {
        let data = data.clone();
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
    let alu: &[u8] = b"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTT\
                       GGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTC\
                       GAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACT\
                       AAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTG\
                       TAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCT\
                       TGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCG\
                       CCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCT\
                       CAAAAA";

    let iub = &[
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

    let homosapiens = &[
        (b'a', 0.3029549426680),
        (b'c', 0.1979883004921),
        (b'g', 0.1975473066391),
        (b't', 0.3015094502008),
    ];

    make_fasta_single(
        b">ONE Homo sapiens alu\n",
        alu.iter().cycle().map(|c| *c),
        n * 2,
    )
    .unwrap();

    make_fasta(
        b">TWO IUB ambiguity codes\n",
        rng.clone(),
        make_random(iub),
        num_threads,
    )
    .unwrap();

    rng.lock().unwrap().reset(n * 5);

    make_fasta(
        b">THREE Homo sapiens frequency\n",
        rng,
        make_random(homosapiens),
        num_threads,
    )
    .unwrap();

    io::stdout().flush().unwrap();
}
