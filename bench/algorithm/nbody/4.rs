// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Contributed by Henry Jayakusuma
// Improved from Rust #9
// Which was ported from gcc #9
//
// Requires CPU feature SSE, AVX, implementing SIMD operations.

use std::arch::x86_64::*;
use std::f64::consts::PI;
use std::mem::{transmute, MaybeUninit};

const SOLAR_MASS: f64 = 4.0 * PI * PI;
const DAYS_PER_YEAR: f64 = 365.24;

const MASS: [f64; 5] = [
    SOLAR_MASS,                           // sol
    9.54791938424326609e-04 * SOLAR_MASS, // jupiter
    2.85885980666130812e-04 * SOLAR_MASS, // saturn
    4.36624404335156298e-05 * SOLAR_MASS, // ouranos
    5.15138902046611451e-05 * SOLAR_MASS, // neptunus
];

// Approximation of inverse square root of [f64;4]
#[inline(always)]
unsafe fn _mm256_rsqrt_pd(s: __m256d) -> __m256d {
    let x = _mm256_cvtps_pd(_mm_rsqrt_ps(_mm256_cvtpd_ps(s)));
    let y = _mm256_mul_pd(_mm256_mul_pd(s, x), x);
    let a = _mm256_sub_pd(y, _mm256_set1_pd(1.0 / 3.0));
    let b = _mm256_sub_pd(y, _mm256_set1_pd(3.0));
    let y = _mm256_add_pd(_mm256_mul_pd(a, b), _mm256_set1_pd(4.0));
    _mm256_mul_pd(_mm256_mul_pd(_mm256_set1_pd(0.375), x), y)
}

#[inline(always)]
unsafe fn calc_delta_pos(position: &[__m256d; 5]) -> [__m256d; 12] {
    let mut delta_pos: [__m256d; 12] = MaybeUninit::uninit().assume_init();
    let mut k = 0;
    for i in 1..5 {
        for j in 0..i {
            delta_pos[k] = _mm256_sub_pd(position[i], position[j]);
            k += 1;
        }
    }
    delta_pos
}

#[inline(always)]
unsafe fn calc_inv_distance(delta_pos: &[__m256d; 12]) -> [__m256d; 3] {
    let mut inv_distance: [__m256d; 3] = MaybeUninit::uninit().assume_init();
    for k in 0..3 {
        let dpos2_0 = _mm256_mul_pd(delta_pos[4 * k + 0], delta_pos[4 * k + 0]);
        let dpos2_1 = _mm256_mul_pd(delta_pos[4 * k + 1], delta_pos[4 * k + 1]);
        let dpos2_2 = _mm256_mul_pd(delta_pos[4 * k + 2], delta_pos[4 * k + 2]);
        let dpos2_3 = _mm256_mul_pd(delta_pos[4 * k + 3], delta_pos[4 * k + 3]);

        let tmp0 = _mm256_hadd_pd(dpos2_0, dpos2_1);
        let tmp1 = _mm256_hadd_pd(dpos2_2, dpos2_3);
        let y0 = _mm256_permute2f128_pd::<0x21>(tmp0, tmp1);
        let y1 = _mm256_blend_pd::<0b1100>(tmp0, tmp1);

        let z = _mm256_add_pd(y0, y1);
        inv_distance[k] = _mm256_rsqrt_pd(z);
    }
    inv_distance
}

unsafe fn energy(position: &[__m256d; 5], velocity: &[__m256d; 5]) -> f64 {
    let mut total_energy: f64 = 0.0;

    for k in 0..5 {
        let velocity2: [f64; 4] = transmute(_mm256_mul_pd(velocity[k], velocity[k]));
        let speed2: f64 = velocity2.iter().sum();
        total_energy += 0.5 * MASS[k] * speed2;
    }

    let delta_pos = calc_delta_pos(&position);
    let inv_distance: [f64; 12] = transmute(calc_inv_distance(&delta_pos));

    let mut k = 0;
    for i in 1..5 {
        for j in 0..i {
            total_energy -= MASS[i] * MASS[j] * inv_distance[k];
            k += 1;
        }
    }

    total_energy
}

unsafe fn advance(n: i32, position: &mut [__m256d; 5], velocity: &mut [__m256d; 5]) {
    let dt = _mm256_set1_pd(0.01);
    let mut rm: [__m256d; 5] = MaybeUninit::uninit().assume_init();
    for i in 0..5 {
        rm[i] = _mm256_set1_pd(MASS[i]);
    }
    for _ in 0..n {
        let delta_pos = calc_delta_pos(&position);
        let inv_distance = calc_inv_distance(&delta_pos);
        let mut mag: [__m256d; 3] = MaybeUninit::uninit().assume_init();

        for i in 0..3 {
            let inv_dist = inv_distance[i];
            let inv_dist2 = _mm256_mul_pd(inv_dist, inv_dist);
            let dt_inv_dist = _mm256_mul_pd(inv_dist, dt);
            mag[i] = _mm256_mul_pd(inv_dist2, dt_inv_dist);
        }

        let mag: [f64; 12] = transmute(mag);
        let mut k: usize = 0;
        for i in 1..5 {
            for j in 0..i {
                let mag = _mm256_set1_pd(mag[k]);
                let dpos_mag = _mm256_mul_pd(delta_pos[k], mag);
                let mi_dpos_mag = _mm256_mul_pd(dpos_mag, rm[i]);
                let mj_dpos_mag = _mm256_mul_pd(dpos_mag, rm[j]);
                velocity[i] = _mm256_sub_pd(velocity[i], mj_dpos_mag);
                velocity[j] = _mm256_add_pd(velocity[j], mi_dpos_mag);
                k += 1;
            }
        }

        for i in 0..5 {
            let movement = _mm256_mul_pd(velocity[i], dt);
            position[i] = _mm256_add_pd(position[i], movement);
       }
    }
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(1000);
    unsafe {
        let mut position = [
            // sol
            _mm256_set1_pd(0.0),
            // jupiter
            _mm256_setr_pd(
                0.0,
                4.84143144246472090e+00,
                -1.16032004402742839e+00,
                -1.03622044471123109e-01,
            ),
            // saturn
            _mm256_setr_pd(
                0.0,
                8.34336671824457987e+00,
                4.12479856412430479e+00,
                -4.03523417114321381e-01,
            ),
            // ouranos
            _mm256_setr_pd(
                0.0,
                1.28943695621391310e+01,
                -1.51111514016986312e+01,
                -2.23307578892655734e-01,
            ),
            // neptunus
            _mm256_setr_pd(
                0.0,
                1.53796971148509165e+01,
                -2.59193146099879641e+01,
                1.79258772950371181e-01,
            ),
        ];

        let mut velocity = [
            // sol
            _mm256_set1_pd(0.0),
            // jupiter
            _mm256_setr_pd(
                0.0,
                1.66007664274403694e-03 * DAYS_PER_YEAR,
                7.69901118419740425e-03 * DAYS_PER_YEAR,
                -6.90460016972063023e-05 * DAYS_PER_YEAR,
            ),
            // saturn
            _mm256_setr_pd(
                0.0,
                -2.76742510726862411e-03 * DAYS_PER_YEAR,
                4.99852801234917238e-03 * DAYS_PER_YEAR,
                2.30417297573763929e-05 * DAYS_PER_YEAR,
            ),
            // ouranos
            _mm256_setr_pd(
                0.0,
                2.96460137564761618e-03 * DAYS_PER_YEAR,
                2.37847173959480950e-03 * DAYS_PER_YEAR,
                -2.96589568540237556e-05 * DAYS_PER_YEAR,
            ),
            // neptunus
            _mm256_setr_pd(
                0.0,
                2.68067772490389322e-03 * DAYS_PER_YEAR,
                1.62824170038242295e-03 * DAYS_PER_YEAR,
                -9.51592254519715870e-05 * DAYS_PER_YEAR,
            ),
        ];

        // offset momentum
        let mut offset = _mm256_set1_pd(0.0);
        for i in 0..5 {
            let t = _mm256_mul_pd(_mm256_set1_pd(MASS[i]), velocity[i]);
            offset = _mm256_add_pd(offset, t);
        }
        velocity[0] = _mm256_mul_pd(offset, _mm256_set1_pd(-1.0 / SOLAR_MASS));

        println!("{:.9}", energy(&position, &velocity));
        advance(n, &mut position, &mut velocity);
        println!("{:.9}", energy(&position, &velocity));
    }
}
