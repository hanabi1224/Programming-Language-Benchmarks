// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by the Rust Project Developers
// contributed by Matt Brubeck
// contributed by TeXitoi
// modified by Tung Duong
// contributed by Cristi Cobzarenco (@cristicbz)
// contributed by Andre Bogus
// contributed by Ryohei Machida

#[macro_use]
extern crate generic_array;
#[macro_use]
extern crate numeric_array;
extern crate rayon;

use generic_array::typenum::consts::U4;
use numeric_array::NumericArray;
use rayon::prelude::*;
use std::ops::*;

type F64x4 = NumericArray<f64, U4>;
type I32x4 = NumericArray<i32, U4>;

fn main() {
    let n = std::env::args().nth(1).and_then(|n| n.parse().ok()).unwrap_or(100);
    let answer = spectralnorm(n);
    println!("{:.9}", answer);
}

fn spectralnorm(mut n: usize) -> f64 {
    // round up to multiple of 4
    n = n.wrapping_add(3) & !3usize;

    // This program overflows when n > 23170
    assert!(n <= 23170 as usize);

    let mut u = vec![F64x4::splat(1.0); n / 4];
    let mut v = vec![F64x4::default(); n / 4];
    let mut tmp = vec![F64x4::default(); n / 4];

    for _ in 0..10 {
        mult_at_av(&u, &mut v, &mut tmp);
        mult_at_av(&v, &mut u, &mut tmp);
    }

    (inner_product(&u, &v) / inner_product(&v, &v)).sqrt()
}

fn mult_at_av(v: &[F64x4], out: &mut [F64x4], tmp: &mut [F64x4]) {
    dot_par(v, tmp, |i, j| inv_a(i, j));
    dot_par(tmp, out, |i, j| inv_a(j, i));
}

fn dot_par<F>(v: &[F64x4], out: &mut [F64x4], inv_a: F)
where
    F: Fn(I32x4, I32x4) -> F64x4 + Sync,
{
    // Parallelize along the output vector, with each pair of slots as a
    // parallelism unit.
    out.par_iter_mut().enumerate().for_each(|(i, slot)| {
        *slot = dot(i as i32, v, &inv_a);
    });
}

#[inline(never)]
fn dot<F>(i: i32, v: &[F64x4], inv_a: F) -> F64x4
where
    F: Fn(I32x4, I32x4) -> F64x4,
{
    let mut result = F64x4::default();

    for k in 0..4 {
        // We're computing everything in chunks of four so the indces of output
        // are 4*i, 4*i+1, 4*i+2 and 4*i+3.
        let ix4 = I32x4::splat(4 * i + k);

        // column indices of A (equivarent to indices of v)
        let mut jx4 = narr![i32; 0, 1, 2, 3];
        let mut sum = F64x4::default();

        // Each slot in the pair gets its own sum, which is further computed in
        // four f64 lanes (which are summed at the end).
        for j in 0..v.len() {
            sum += v[j] / inv_a(ix4, jx4);
            jx4 += nconstant!(4);
        }

        // Sum the four lanes for each slot.
        result[k as usize] = sum[0] + sum[1] + sum[2] + sum[3];
    }

    result
}

/// Calculate 1 / A[i, j] for each element of i, j
#[inline]
fn inv_a(i: I32x4, j: I32x4) -> F64x4 {
    let one = nconstant!(1);
    let two = nconstant!(2);
    let a_ij = (i + j) * (i + j + one) / two + i + one;
    narr![f64; a_ij[0] as f64, a_ij[1] as f64, a_ij[2] as f64, a_ij[3] as f64]
}

/// Vectorised form of inner product
fn inner_product(v: &[F64x4], u: &[F64x4]) -> f64 {
    // (1) compute inner product across four lanes.
    let r = u
        .iter()
        .zip(v)
        .map(|(&x, &y)| x * y)
        .fold(F64x4::default(), |s, x| s + x);

    // (2) sum the four lanes.
    r[0] + r[1] + r[2] + r[3]
}
