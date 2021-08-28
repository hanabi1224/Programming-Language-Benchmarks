// ported from v version by hanabi1224

fn main() {
    let n = std::env::args()
        .nth(1)
        .and_then(|n| n.parse().ok())
        .unwrap_or(100);
    let ret = spectralnorm(n);
    println!("{:.9}", ret);
}

#[inline]
fn spectralnorm(n: usize) -> f64 {
    let mut u = vec![1.0; n];
    let mut v = vec![1.0; n];
    for _ in 0..10 {
        a_times_transp(&mut v, &u, n);
        a_times_transp(&mut u, &v, n);
    }
    let mut vbv = 0.0;
    let mut vv = 0.0;
    for i in 0..n {
        vbv += u[i] * v[i];
        vv += v[i].powi(2);
    }
    (vbv / vv).sqrt()
}

#[inline]
fn a_times_transp(v: &mut [f64], u: &[f64], len: usize) {
    let mut x = vec![0.0; len];
    times(&mut x, u, len);
    times_trans(v, &x, len);
}

#[inline]
fn times(v: &mut [f64], u: &[f64], len: usize) {
    for i in 0..len {
        let mut a = 0.0;
        for j in 0..len {
            a += u[j] / evala(i, j);
        }
        v[i] = a;
    }
}

#[inline]
fn times_trans(v: &mut [f64], u: &[f64], len: usize) {
    for i in 0..len {
        let mut a = 0.0;
        for j in 0..len {
            a += u[j] / evala(j, i);
        }
        v[i] = a;
    }
}

#[inline]
fn evala(i: usize, j: usize) -> f64 {
    let sum = i + j;
    (sum * (sum + 1) / 2 + i + 1) as f64
}
