// Ported from 2-m.zig
// Use simd

use core::arch::x86_64::*;
use rayon::prelude::*;

const V_SIZE: usize = 16;
type VItem = i8;
type V = [VItem; V_SIZE];

const NEXT_PERM_MASKS: [V; V_SIZE] = next_perm_masks();
const REVERSE_MASKS: [V; V_SIZE] = reverse_masks();

fn simd_shuffle(a: __m128i, mask: __m128i) -> __m128i {
    unsafe { _mm_shuffle_epi8(a, mask) }
}

const fn shuffle(a: &V, mask: &V) -> V {
    let mut r = [0; V_SIZE];
    let mut i = 0;
    while i < V_SIZE {
        r[i] = a[mask[i] as usize];
        i += 1;
    }
    r
}

const fn reverse_mask(n: VItem) -> V {
    let mut v: V = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    let mut i = 0;
    while i < n {
        v[i as usize] = n - i - 1;
        i += 1;
    }
    v
}

const fn reverse_masks() -> [V; V_SIZE] {
    let mut v = [[0; V_SIZE]; V_SIZE];
    let mut i = 0;
    while i < V_SIZE {
        v[i] = reverse_mask(i as VItem);
        i += 1;
    }
    v
}

const fn rotate_mask(n: VItem) -> V {
    let mut v = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    let mut i = 0;
    while i < n {
        v[i as usize] = (i + 1) % n;
        i += 1;
    }
    v
}

const fn next_perm_mask(n: VItem) -> V {
    let mut v = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    let mut i = 2;
    while i <= n {
        v = shuffle(&v, &rotate_mask(i));
        i += 1;
    }
    v
}

const fn next_perm_masks() -> [V; V_SIZE] {
    let mut v = [[0; V_SIZE]; V_SIZE];
    let mut i = 0;
    while i < V_SIZE {
        v[i] = next_perm_mask(i as VItem);
        i += 1;
    }
    v
}

fn pfannkuchen(perm: &__m128i) -> u32 {
    let mut flip_count = 0;
    let mut a = *perm;
    loop {
        let k = unsafe { _mm_extract_epi8::<0>(a) };
        if k == 0 {
            return flip_count;
        }
        let mask = REVERSE_MASKS[k as usize + 1];
        // let mask = reverse_mask(k as i8 + 1);
        a = simd_shuffle(a, to_mask(mask));
        flip_count += 1;
    }
}

const fn factorial(n: i8) -> usize {
    let mut res: usize = 1;
    let mut i = 2;
    while i <= n as usize {
        res *= i;
        i += 1;
    }
    return res;
}

const fn count_at_pos(n: i8, start: usize) -> [i8; 16] {
    let mut count = [0; 16];
    let mut r = start;
    let mut i = n;
    while i > 0 {
        i -= 1;
        let total_perms = factorial(i);
        count[i as usize] = i + 1 - (r / total_perms) as i8;
        r %= total_perms;
    }
    count
}

fn perm_with_count(n: i8, count: &[i8; 16]) -> __m128i {
    let mut perm = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    let mut i = n as usize;
    while i > 0 {
        perm[0..i].rotate_left(i - count[i - 1] as usize);
        i -= 1;
    }
    to_mask(perm)
}

fn next_permutation(perm: __m128i, count: &mut [i8], size: usize) -> Option<__m128i> {
    let mut r = 0;
    let mut none = true;
    for i in 0..size {
        if count[i] != 1 {
            r = i;
            none = false;
            break;
        }
    }
    if none {
        return None;
    }
    let next_perm = simd_shuffle(perm, to_mask(NEXT_PERM_MASKS[r + 1]));
    // let next_perm = simd_shuffle(perm, to_mask(next_perm_mask(r as i8 + 1)));
    count[r] -= 1;
    for i in 0..r {
        count[i] = (i + 1) as i8;
    }
    Some(next_perm)
}

fn to_mask(v: [VItem; V_SIZE]) -> __m128i {
    unsafe {
        _mm_setr_epi8(
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13],
            v[14], v[15],
        )
    }
}

fn calculate_part(first: usize, last: usize, n: i8) -> (i32, u32) {
    let mut max_flip_count: u32 = 0;
    let mut checksum: i32 = 0;
    let mut count = count_at_pos(n, first);
    let mut perm = perm_with_count(n, &count);
    for i in first..last {
        let flip_count = pfannkuchen(&perm);
        if flip_count > max_flip_count {
            max_flip_count = flip_count;
        }
        if i % 2 == 0 {
            checksum += flip_count as i32;
        } else {
            checksum -= flip_count as i32;
        }
        if let Some(perm_next) = next_permutation(perm, &mut count, n as usize) {
            perm = perm_next;
        } else {
            break;
        }
    }
    (checksum, max_flip_count)
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(3);

    let n_cpu = num_cpus::get();
    let perms_count = factorial(n);
    let len_per_task = perms_count / n_cpu;
    let mut task_params = Vec::with_capacity(n_cpu);
    for first in (0..perms_count).into_iter().step_by(len_per_task) {
        let last = (first + len_per_task).min(perms_count);
        task_params.push((first, last));
    }
    let (checksum, max_flip_count) = task_params
        .into_par_iter()
        .map(|(first, last)| calculate_part(first, last, n))
        .reduce(|| (0, 0), |a, b| (a.0 + b.0, a.1.max(b.1)));
    println!("{}\nPfannkuchen({}) = {}", checksum, n, max_flip_count);
}
