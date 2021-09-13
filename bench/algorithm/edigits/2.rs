// From https://github.com/tczajka/bigint-benchmark-rs/blob/main/src/digits_of_e.rs

use num_bigint::BigInt;
use std::f64;

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(27);
    let s = calculate(n);
    for i in (0..n).step_by(10) {
        let (line, count) = if i + 10 <= n {
            (s[i..i + 10].to_owned(), i + 10)
        } else {
            let n_space = 10 - n % 10;
            let mut l = s[i..n].to_owned();
            for _ in 0..n_space {
                l += " ";
            }
            (l, n)
        };
        println!("{}\t:{}", line, count)
    }
}

/// n digits of the number e.
pub(crate) fn calculate(n: usize) -> String {
    assert!(n > 0);
    // Find k such that log_10 k! is approximately n + 50.
    // This makes 1 / k! and subsequent terms small enough.
    // Use Stirling's approximation: ln k! ~= k ln k - k + 0.5 * ln(2*pi*k).
    let k = binary_search(|k| {
        k > 0 && {
            let k = k as f64;
            let ln_k_factorial = k * k.ln() - k + 0.5 * (f64::consts::TAU * k).ln();
            let log_10_k_factorial = ln_k_factorial / f64::consts::LN_10;
            log_10_k_factorial >= (n + 50) as f64
        }
    });

    // 1/1! + ... + 1/(k-1)!
    let (p, q) = sum_terms(0, k - 1);
    // Add 1/0! = 1.
    let p = p + &q;
    // e ~= p/q.
    // Calculate p/q * 10^(n-1) to get the answer as an integer.
    let answer_int = p * BigInt::from(10u32).pow((n - 1) as u32) / q;
    answer_int.to_string()
}

/// a! * (1/(a+1)! + 1/(a+2)! + ... + 1/b!) as a fraction p / q.
/// q = (a+1) * (a+2) * ... * (b-1) * b
/// p = (a+2)...b + (a+3)...b + ... + 1
fn sum_terms(a: usize, b: usize) -> (BigInt, BigInt) {
    if b == a + 1 {
        (1u32.into(), b.into())
    } else {
        let mid = (a + b) / 2;
        let (p_left, q_left) = sum_terms(a, mid);
        let (p_right, q_right) = sum_terms(mid, b);
        // p / q = p_left / q_left + a!/mid! * p_right / q_right
        // a! / mid! = 1 / q_left
        // p / q = (p_left * q_right + p_right) / (q_left * q_right)
        (p_left * &q_right + p_right, q_left * q_right)
    }
}

// Find k such that f(k) is true.
fn binary_search<F: Fn(usize) -> bool>(f: F) -> usize {
    let mut a = 0;
    let mut b = 1;
    while !f(b) {
        a = b;
        b *= 2;
    }
    while b - a > 1 {
        let m = a + (b - a) / 2;
        if f(m) {
            b = m;
        } else {
            a = m;
        }
    }
    b
}
