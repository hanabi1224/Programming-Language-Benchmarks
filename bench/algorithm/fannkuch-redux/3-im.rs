// Contributed by Henry Jayakusuma
// Inspired by C++ #6 SIMD implementation by Andrei Simion
// on https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
// Requires SSE3 and SSE4 instruction set

extern crate rayon;

use std::arch::x86_64::{__m128i, _mm_extract_epi8, _mm_shuffle_epi8};
use std::cmp::max;
use std::mem::transmute;
use rayon::prelude::*;

#[inline(always)]
fn reverse_array(array: u128, num_to_reverse: usize) -> u128 {
    const MASK: [u128; 16] = [
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01_00,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_00_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_00_01_02,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_00_01_02_03,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_00_01_02_03_04,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_00_01_02_03_04_05,
        0x0F_0E_0D_0C_0B_0A_09_08_07_00_01_02_03_04_05_06,
        0x0F_0E_0D_0C_0B_0A_09_08_00_01_02_03_04_05_06_07,
        0x0F_0E_0D_0C_0B_0A_09_00_01_02_03_04_05_06_07_08,
        0x0F_0E_0D_0C_0B_0A_00_01_02_03_04_05_06_07_08_09,
        0x0F_0E_0D_0C_0B_00_01_02_03_04_05_06_07_08_09_0A,
        0x0F_0E_0D_0C_00_01_02_03_04_05_06_07_08_09_0A_0B,
        0x0F_0E_0D_00_01_02_03_04_05_06_07_08_09_0A_0B_0C,
        0x0F_0E_00_01_02_03_04_05_06_07_08_09_0A_0B_0C_0D,
        0x0F_00_01_02_03_04_05_06_07_08_09_0A_0B_0C_0D_0E,
        0x00_01_02_03_04_05_06_07_08_09_0A_0B_0C_0D_0E_0F,
    ];
    unsafe {
        transmute::<__m128i, u128>(
            _mm_shuffle_epi8(
                transmute::<u128, __m128i>(array),
                transmute::<u128, __m128i>(*MASK.get_unchecked(num_to_reverse)),
            )
        )
    }
}

#[inline(always)]
fn rotate_array(array: u128, num_to_rotate: usize) -> u128 {
    const MASK: [u128; 16] = [
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01_00,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_00_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_00_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_00_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_00_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_00_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_00_06_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_00_07_06_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_00_08_07_06_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_00_09_08_07_06_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_00_0A_09_08_07_06_05_04_03_02_01,
        0x0F_0E_0D_0C_00_0B_0A_09_08_07_06_05_04_03_02_01,
        0x0F_0E_0D_00_0C_0B_0A_09_08_07_06_05_04_03_02_01,
        0x0F_0E_00_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01,
        0x0F_00_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01,
        0x00_0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01,
    ];
    unsafe {
        transmute::<__m128i, u128>(
            _mm_shuffle_epi8(
                transmute::<u128, __m128i>(array),
                transmute::<u128, __m128i>(*MASK.get_unchecked(num_to_rotate)),
            )
        )
    }
}

#[inline(always)]
fn get<const IMM8: i32>(array: u128) -> i32 {
    unsafe { _mm_extract_epi8::<IMM8>(transmute::<u128, __m128i>(array)) }
}

#[inline(always)]
fn forced_indexing(array: u128, idx: u8) -> u8 {
    unsafe { *transmute::<u128, [u8; 16]>(array).get_unchecked(idx as usize) }
}

#[inline(always)]
fn advance_array(mut array: u128, count: &mut [u8; 16]) -> u128 {
    for layer in 1..16 {
        array = rotate_array(array, layer);
        count[layer] += 1;
        if count[layer] <= layer as u8 { break; }
        count[layer] = 0;
    }
    array
}

fn fannkuchredux(n: usize) -> (i32, i32) {
    assert!(n <= 16);
    let mut current = 0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01_00;
    let count = [0u8; 16];

    // Trivial cases, not implemented
    if n == 0 { return (0, 0); }
    if n == 1 { return (0, 0); }
    if n == 2 { return (-1, 1); }

    // Divide work blocks for each n element rotation
    let mut arrays = vec![current];
    for _ in 1..n {
        current = rotate_array(current, n - 1);
        arrays.push(current);
    }
    (0..n).into_par_iter().map(|rotate_count| {
        let mut current = arrays[rotate_count];
        let mut count = count.clone();
        count[n - 1] = rotate_count as u8;

        // Divide work blocks for each n - 1 element rotation
        let mut arrays = vec![current];
        for _ in 1..n - 1 {
            current = rotate_array(current, n - 2);
            arrays.push(current);
        }
        (0..n - 1).into_par_iter().map(|rotate_count| {
            let mut current = arrays[rotate_count];
            let mut count = count.clone();
            count[n - 2] = rotate_count as u8;

            // Calculating checksum and max_rev
            let mut checksum = 0;
            let mut max_rev = 0;
            while count[n - 2] == rotate_count as u8 {
                let mut tmp = current;
                let mut rev_count = 0;
                let mut first = get::<0>(tmp) as u8;
                if first > 0 {
                    while first > 0 {
                        let next = forced_indexing(tmp, first);
                        tmp = reverse_array(tmp, first as usize);
                        first = next;
                        rev_count += 1;
                    }
                    // Bit hack: conditional negation, oddly impactful on performance
                    checksum += (rev_count ^ -(count[1] as i32)) + count[1] as i32;
                    max_rev = max(max_rev, rev_count);
                }
                current = advance_array(current, &mut count);
            }
            (checksum, max_rev)
        }).reduce(|| (0, 0), |(cs1, mr1), (cs2, mr2)| (cs1 + cs2, max(mr1, mr2)))
    }).reduce(|| (0, 0), |(cs1, mr1), (cs2, mr2)| (cs1 + cs2, max(mr1, mr2)))
}

pub fn main() {
    let n = std::env::args().nth(1)
        .and_then(|n| n.parse().ok())
        .unwrap_or(7);
    let (checksum, max_rev) = fannkuchredux(n);
    println!("{}\nPfannkuchen({}) = {}", checksum, n, max_rev);
}
