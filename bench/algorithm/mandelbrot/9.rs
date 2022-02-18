// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Matt Watson
// contributed by TeXitoi
// contributed by Volodymyr M. Lisivka
// contributed by Michael Cicotti
// modified by hanabi1224, use portable_simd on nightly rust

#![feature(portable_simd)]

use std::simd::{f64x8, StdFloat};

const MAX_ITER: usize = 50;
const VLEN: usize = 8;

#[inline(always)]
pub fn mbrot8(out: &mut u8, cr: f64x8, ci: f64) {
    let ci = f64x8::splat(ci);
    let mut zr = f64x8::default();
    let mut zi = f64x8::default();
    let mut tr = f64x8::default();
    let mut ti = f64x8::default();
    let mut absz = f64x8::default();

    for _ in 0..MAX_ITER / 5 {
        for _ in 0..5 {
            zi = (zr + zr).mul_add(zi, ci);
            zr = tr - ti + cr;
            tr = zr * zr;
            ti = zi * zi;
        }
        absz = tr + ti;
        if absz.horizontal_min() > 4. {
            return;
        }
    }

    *out = absz.as_array().iter().enumerate().fold(0, |accu, (i, &t)| {
        accu | if t <= 4. { 0x80 >> i } else { 0 }
    });
}

fn main() {
    let size = std::env::args()
        .nth(1)
        .and_then(|n| n.parse().ok())
        .unwrap_or(200);
    // Round size to multiple of 8
    let size = (size + VLEN - 1) / VLEN * VLEN;
    let inv = 2. / size as f64;
    let mut xloc = vec![f64x8::splat(0.0); size / VLEN];
    for i in 0..size {
        xloc[i / VLEN][i % VLEN] = i as f64 * inv - 1.5;
    }

    println!("P4\n{} {}", size, size);

    let mut rows = vec![0; size * size / VLEN];
    rows.chunks_mut(size / VLEN)
        .enumerate()
        .for_each(|(y, out)| {
            let ci = y as f64 * inv - 1.;
            out.iter_mut()
                .enumerate()
                .for_each(|(i, inner_out)| mbrot8(inner_out, xloc[i], ci));
        });

    let digest = md5::compute(&rows);
    println!("{:x}", digest);
}
