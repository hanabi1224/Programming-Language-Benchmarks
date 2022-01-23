// Ported from 2.zig

const V_SIZE: usize = 16;
type VItem = usize;
type V = [VItem; V_SIZE];

const fn suffle(a: &V, mask: &V) -> V {
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
        v = suffle(&v, &rotate_mask(i));
        i += 1;
    }
    v
}

const fn pfannkuchen(perm: &V) -> u32 {
    let mut flip_count = 0;
    let mut a = *perm;
    loop {
        let k = a[0];
        if k == 0 {
            return flip_count;
        }
        a = suffle(&a, &reverse_mask(k + 1));
        flip_count += 1;
    }
}

const fn calculate(n: usize) -> (i32, u32) {
    let mut max_flip_count: u32 = 0;
    let mut checksum: i32 = 0;
    let mut perm = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
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
        perm = suffle(&perm, &next_perm_mask((r + 1) as VItem));
        count[r] -= 1;
        let mut i = 1;
        while i < r {
            count[i] = (i + 1) as VItem;
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
