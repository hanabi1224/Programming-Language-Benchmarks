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
// modified by hanabi1224, use portable_simd on nightly rust

#![feature(portable_simd)]

use rayon::prelude::*;
use std::ops::*;
use std::simd::Simd;

const LANES: usize = 4;
const LANES_I64: i64 = LANES as i64;
type F64Vec = Simd<f64, LANES>;
type I64Vec = Simd<i64, LANES>;

lazy_static::lazy_static! {
    static ref ONE: I64Vec = I64Vec::splat(1);
    static ref TWO: I64Vec = I64Vec::splat(2);
    static ref LANES_VEC: I64Vec = I64Vec::splat(LANES_I64);
    static ref J_ARRAY: I64Vec = I64Vec::from_array([0, 1, 2, 3]);
    // static ref J_ARRAY: I64Vec = I64Vec::from_array([0, 1, 2, 3, 4, 5, 6, 7]);
}

fn main() {
    let n = std::env::args()
        .nth(1)
        .and_then(|n| n.parse().ok())
        .unwrap_or(100);
    let answer = spectralnorm(n);
    println!("{:.9}", answer);
}

#[inline]
fn spectralnorm(n: usize) -> f64 {
    // round up to multiple of 4
    let n = (n + LANES - 1) / LANES * LANES;

    // This program overflows when n > 23170
    assert!(n <= 23170 as usize);

    let array_size = n / LANES;

    let mut u = vec![F64Vec::splat(1.0); array_size];
    let mut v = vec![F64Vec::default(); array_size];

    for _ in 0..10 {
        mult_at_av(&u, &mut v, array_size);
        mult_at_av(&v, &mut u, array_size);
    }

    (inner_product(&u, &v) / inner_product(&v, &v)).sqrt()
}

#[inline]
fn mult_at_av(v: &[F64Vec], out: &mut [F64Vec], array_size: usize) {
    let mut tmp = vec![F64Vec::default(); array_size];
    dot_par(v, &mut tmp, |i, j| inv_a(i, j));
    dot_par(&tmp, out, |i, j| inv_a(j, i));
}

#[inline]
fn dot_par<F>(v: &[F64Vec], out: &mut [F64Vec], inv_a: F)
where
    F: Fn(I64Vec, I64Vec) -> F64Vec + Sync,
{
    // Parallelize along the output vector, with each pair of slots as a
    // parallelism unit.
    out.par_iter_mut().enumerate().for_each(|(i, slot)| {
        *slot = dot(i as i64, v, &inv_a);
    });
}

fn dot<F>(i: i64, v: &[F64Vec], inv_a: F) -> F64Vec
where
    F: Fn(I64Vec, I64Vec) -> F64Vec,
{
    let mut result = F64Vec::default();

    for k in 0..LANES_I64 {
        // We're computing everything in chunks of four so the indces of output
        // are 4*i, 4*i+1, 4*i+2 and 4*i+3.
        let i_vec = I64Vec::splat(LANES_I64 * i + k);

        // column indices of A (equivarent to indices of v)
        let mut j_vec = *J_ARRAY;
        let mut sum = F64Vec::default();

        // Each slot in the pair gets its own sum, which is further computed in
        // four f64 lanes (which are summed at the end).
        for j in 0..v.len() {
            sum += v[j] / inv_a(i_vec, j_vec);
            j_vec += *LANES_VEC;
        }

        // Sum the four lanes for each slot.
        result[k as usize] = sum.horizontal_sum();
    }

    result
}

/// Calculate 1 / A[i, j] for each element of i, j
#[inline]
fn inv_a(i: I64Vec, j: I64Vec) -> F64Vec {
    let sum_i_j = i + j;
    let a_ij = sum_i_j * (sum_i_j + *ONE) / *TWO + i + *ONE;
    F64Vec::round_from_int(a_ij)
}

/// Vectorised form of inner product
#[inline]
fn inner_product(v: &[F64Vec], u: &[F64Vec]) -> f64 {
    // (1) compute inner product across four lanes.
    let r = u
        .iter()
        .zip(v)
        .map(|(&x, &y)| x * y)
        .fold(F64Vec::default(), |s, x| s + x);

    // (2) sum the four lanes.
    r.horizontal_sum()
}
