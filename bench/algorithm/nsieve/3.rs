// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// Wheel 8-of-30 bit-sieve Rust implementation by ndt0208

const STEPS: [usize; 8] = [3, 2, 1, 2, 1, 2, 3, 1];

fn nsieve(max: usize, bits: &mut [u64]) {
    if max <= 2 {
        println!("Primes up to {:8} {:8}", max, 0);
        return;
    }

    let maxintsh = max >> 1;
    let num_words = (maxintsh + 63) >> 6;
    bits[..num_words].fill(0);
    let ptr = bits.as_mut_ptr();

    let qh = ((max as f64).sqrt() as usize) >> 1;

    let mut step = 1;
    let mut factorh = 3; // 7 >> 1

    unsafe {
        while factorh <= qh {
            let w_idx = factorh >> 6;
            let b_idx = factorh & 63;

            if (*ptr.add(w_idx) & (1u64 << b_idx)) == 0 {
                let factor = (factorh << 1) + 1;
                let mut istep = step;
                let mut i = (factor * factor) >> 1;

                let s0 = factor * *STEPS.get_unchecked(istep);
                let s1 = factor * *STEPS.get_unchecked((istep + 1) & 7);
                let s2 = factor * *STEPS.get_unchecked((istep + 2) & 7);
                let s3 = factor * *STEPS.get_unchecked((istep + 3) & 7);
                let s4 = factor * *STEPS.get_unchecked((istep + 4) & 7);
                let s5 = factor * *STEPS.get_unchecked((istep + 5) & 7);
                let s6 = factor * *STEPS.get_unchecked((istep + 6) & 7);
                let s7 = factor * *STEPS.get_unchecked((istep + 7) & 7);
                let cycle = factor * 15;

                while i + cycle < maxintsh {
                    *ptr.add(i >> 6) |= 1u64 << (i & 63); i += s0;
                    *ptr.add(i >> 6) |= 1u64 << (i & 63); i += s1;
                    *ptr.add(i >> 6) |= 1u64 << (i & 63); i += s2;
                    *ptr.add(i >> 6) |= 1u64 << (i & 63); i += s3;
                    *ptr.add(i >> 6) |= 1u64 << (i & 63); i += s4;
                    *ptr.add(i >> 6) |= 1u64 << (i & 63); i += s5;
                    *ptr.add(i >> 6) |= 1u64 << (i & 63); i += s6;
                    *ptr.add(i >> 6) |= 1u64 << (i & 63); i += s7;
                }
                while i < maxintsh {
                    *ptr.add(i >> 6) |= 1u64 << (i & 63);
                    i += factor * *STEPS.get_unchecked(istep);
                    istep = (istep + 1) & 7;
                }
            }

            factorh += *STEPS.get_unchecked(step);
            step = (step + 1) & 7;
        }

        let mut count = (max > 2) as usize + (max > 3) as usize + (max > 5) as usize;
        let mut factor = 7;
        let cycle = 30;

        while factor + cycle < max {
            let h0 = factor >> 1; factor += 4;
            let h1 = factor >> 1; factor += 2;
            let h2 = factor >> 1; factor += 4;
            let h3 = factor >> 1; factor += 2;
            let h4 = factor >> 1; factor += 4;
            let h5 = factor >> 1; factor += 6;
            let h6 = factor >> 1; factor += 2;
            let h7 = factor >> 1; factor += 6;

            count += (((*ptr.add(h0 >> 6) >> (h0 & 63)) ^ 1) & 1) as usize;
            count += (((*ptr.add(h1 >> 6) >> (h1 & 63)) ^ 1) & 1) as usize;
            count += (((*ptr.add(h2 >> 6) >> (h2 & 63)) ^ 1) & 1) as usize;
            count += (((*ptr.add(h3 >> 6) >> (h3 & 63)) ^ 1) & 1) as usize;
            count += (((*ptr.add(h4 >> 6) >> (h4 & 63)) ^ 1) & 1) as usize;
            count += (((*ptr.add(h5 >> 6) >> (h5 & 63)) ^ 1) & 1) as usize;
            count += (((*ptr.add(h6 >> 6) >> (h6 & 63)) ^ 1) & 1) as usize;
            count += (((*ptr.add(h7 >> 6) >> (h7 & 63)) ^ 1) & 1) as usize;
        }

        let mut cstep = 1;
        while factor < max {
            let half = factor >> 1;
            count += (((*ptr.add(half >> 6) >> (half & 63)) ^ 1) & 1) as usize;
            factor += *STEPS.get_unchecked(cstep) << 1;
            cstep = (cstep + 1) & 7;
        }

        println!("Primes up to {:8} {:8}", max, count);
    }
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(4);

    let max_n = 10000 << n;
    let max_words = ((max_n >> 1) + 63) >> 6;
    let mut buffer = vec![0u64; max_words];

    for i in 0..3 {
        let m = 10000 << (n - i);
        nsieve(m, &mut buffer);
    }
}
