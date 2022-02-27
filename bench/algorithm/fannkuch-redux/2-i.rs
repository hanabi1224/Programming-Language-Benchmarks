// Ported from 2.zig
// Use simd

use core::arch::x86_64::*;

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
        a = simd_shuffle(a, to_mask(mask));
        flip_count += 1;
    }
}

fn to_mask(v: [VItem; V_SIZE]) -> __m128i {
    unsafe {
        _mm_setr_epi8(
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13],
            v[14], v[15],
        )
    }
}

fn calculate(n: usize) -> (i32, u32) {
    let mut max_flip_count: u32 = 0;
    let mut checksum: i32 = 0;
    let mut perm = unsafe { _mm_setr_epi8(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) };
    let mut count = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
    let mut parity = false;
    loop {
        let flip_count = pfannkuchen(&perm);
        if flip_count > max_flip_count {
            max_flip_count = flip_count;
        }
        if parity {
            checksum -= flip_count as i32;
        } else {
            checksum += flip_count as i32;
        }
        let mut r = 0;
        let mut end = true;
        let mut i = 0;
        while i < n {
            if count[i] != 1 {
                r = i;
                end = false;
                break;
            }
            i += 1;
        }
        if end {
            break;
        }
        let mask = NEXT_PERM_MASKS[r + 1];
        perm = simd_shuffle(perm, to_mask(mask));
        count[r] -= 1;
        let mut i = 1;
        while i < r {
            count[i] = (i + 1) as u8;
            i += 1;
        }
        parity = !parity;
    }

    (checksum, max_flip_count)
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(6);

    let (checksum, max_flip_count) = calculate(n);
    println!("{}\nPfannkuchen({}) = {}", checksum, n, max_flip_count);
}
