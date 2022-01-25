use bitvec::prelude::*;

fn nsieve(n: usize) {
    let mut count = 0;
    let mut flags = bitvec![u32, LocalBits; 0; n];
    for i in 2..n {
        // if flags[i] {
        if unsafe { !flags.get_unchecked(i) } {
            count += 1;
            for j in ((i << 1)..n).step_by(i) {
                // flags.set(j, false);
                unsafe { flags.set_unchecked(j, true) };
            }
        }
    }
    println!("Primes up to {:8} {:8}", n, count);
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(4);

    for i in 0..3 {
        nsieve(10000 << (n - i));
    }
}
