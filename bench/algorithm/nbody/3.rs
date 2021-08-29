// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// modified by hanabi1224 to use core_simd, remove borrow checker hack, do not derive copy, more compile time calculation

#![feature(portable_simd)]

use core_simd::f64x4;
use std::f64::consts::PI;

const SOLAR_MASS: f64 = 4.0 * PI * PI;
const YEAR: f64 = 365.24;
const N_BODIES: usize = 5;
const ADVANCE_DT: f64 = 0.01;

macro_rules! planet {
    ($x:expr, $y:expr, $z:expr, $vx:expr, $vy:expr, $vz:expr, $mass_ratio:expr) => {{
        Planet {
            position: f64x4::from_array([$x, $y, $z, 0.0]),
            velocity: f64x4::from_array([$vx, $vy, $vz, 0.0]),
            mass_ratio: $mass_ratio,
            mass: $mass_ratio * SOLAR_MASS,
            mass_half: $mass_ratio * SOLAR_MASS * 0.5,
        }
    }};
}

const BODIES: [Planet; N_BODIES] = [
    // Sun
    planet!(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
    // Jupiter
    planet!(
        4.84143144246472090e+00,
        -1.16032004402742839e+00,
        -1.03622044471123109e-01,
        1.66007664274403694e-03 * YEAR,
        7.69901118419740425e-03 * YEAR,
        -6.90460016972063023e-05 * YEAR,
        9.54791938424326609e-04
    ),
    // Saturn
    planet!(
        8.34336671824457987e+00,
        4.12479856412430479e+00,
        -4.03523417114321381e-01,
        -2.76742510726862411e-03 * YEAR,
        4.99852801234917238e-03 * YEAR,
        2.30417297573763929e-05 * YEAR,
        2.85885980666130812e-04
    ),
    // Uranus
    planet!(
        1.28943695621391310e+01,
        -1.51111514016986312e+01,
        -2.23307578892655734e-01,
        2.96460137564761618e-03 * YEAR,
        2.37847173959480950e-03 * YEAR,
        -2.96589568540237556e-05 * YEAR,
        4.36624404335156298e-05
    ),
    // Neptune
    planet!(
        1.53796971148509165e+01,
        -2.59193146099879641e+01,
        1.79258772950371181e-01,
        2.68067772490389322e-03 * YEAR,
        1.62824170038242295e-03 * YEAR,
        -9.51592254519715870e-05 * YEAR,
        5.15138902046611451e-05
    ),
];

#[derive(Clone)]
struct Planet {
    position: f64x4,
    velocity: f64x4,
    mass_ratio: f64,
    mass: f64,
    mass_half: f64,
}

#[inline]
fn advance(bodies: &mut [Planet; N_BODIES], dt: f64, steps: i32) {
    for _ in 0..steps {
        for i in 0..(N_BODIES - 1) {
            let (bi_position, mut bi_velocity, bi_mass) =
                (|p: &Planet| (p.position, p.velocity, p.mass))(&bodies[i]);
            for j in (i + 1)..N_BODIES {
                let bj = &mut bodies[j];
                let mut d = bi_position - bj.position;
                let d2 = dot(d);
                let mag = dt / (d2 * d2.sqrt());
                d *= mag;
                bj.velocity += d * bi_mass;
                bi_velocity -= d * bj.mass;
            }
            let bi = &mut bodies[i];
            bi.velocity = bi_velocity;
            bi.position += bi_velocity * dt;
        }
        // advance the last one
        let last = &mut bodies[N_BODIES - 1];
        last.position += last.velocity * dt;
    }
}

#[inline]
fn energy(bodies: &[Planet; N_BODIES]) -> f64 {
    let mut e = 0.0;
    for i in 0..N_BODIES {
        let bi = &bodies[i];
        e += dot(bi.velocity) * bi.mass_half;
        for j in (i + 1)..N_BODIES {
            let bj = &bodies[j];
            let distance = dot(bi.position - bj.position).sqrt();
            e -= bi.mass * bj.mass / distance;
        }
    }
    e
}

#[inline]
fn offset_momentum(bodies: &mut [Planet; N_BODIES]) {
    let mut p = f64x4::default();
    for bi in bodies.iter() {
        p -= bi.velocity * bi.mass_ratio;
    }
    let sun = &mut bodies[0];
    sun.velocity = p;
}

#[inline]
fn dot(v: f64x4) -> f64 {
    (v * v).horizontal_sum()
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(1000);

    let mut bodies = BODIES.clone();
    offset_momentum(&mut bodies);
    println!("{:.9}", energy(&bodies));

    advance(&mut bodies, ADVANCE_DT, n);
    println!("{:.9}", energy(&bodies));
}
