// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// modified by hanabi1224 to use numeric_array, remove borrow checker hack, do not derive copy, more compile time calculation

use generic_array::{arr, typenum::consts::U4};
use numeric_array::{geometry::Geometric, narr, NumericArray, NumericConstant};
use std::f64::consts::PI;

type F64x4 = NumericArray<f64, U4>;
type F64c = NumericConstant<f64>;

const SOLAR_MASS: f64 = 4.0 * PI * PI;
const YEAR: f64 = 365.24;
const N_BODIES: usize = 5;
const ADVANCE_DT: F64c = NumericConstant(0.01);

macro_rules! planet {
    ($x:expr, $y:expr, $z:expr, $vx:expr, $vy:expr, $vz:expr, $mass_ratio:expr) => {
        {
            let mass = $mass_ratio * SOLAR_MASS;
            Planet {
                position: narr![f64; $x, $y, $z, 0.0],
                velocity: narr![f64; $vx, $vy, $vz, 0.0],
                mass_ratio: NumericConstant($mass_ratio),
                mass,
                massc: NumericConstant(mass),
                mass_half: mass * 0.5,
            }
        }
    };
}

lazy_static::lazy_static! {
    static ref BODIES: [Planet; N_BODIES] = [
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
}

#[derive(Debug, Clone)]
struct Planet {
    position: F64x4,
    velocity: F64x4,
    mass_ratio: F64c,
    mass: f64,
    massc: F64c,
    mass_half: f64,
}

#[inline]
fn advance(bodies: &mut [Planet; N_BODIES], dt: F64c, steps: i32) {
    for _ in 0..steps {
        for i in 0..(N_BODIES - 1) {
            let (bi_position, mut bi_velocity, bi_massc) =
                (|p: &Planet| (p.position, p.velocity, p.massc))(&bodies[i]);
            for j in (i + 1)..N_BODIES {
                let bj = &mut bodies[j];
                let mut d = bi_position - bj.position;
                let d2 = d.norm_squared();
                let mag = NumericConstant(dt.0 / (d2 * d2.sqrt()));
                d *= mag;
                bj.velocity += d * bi_massc;
                bi_velocity -= d * bj.massc;
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
        e += bi.velocity.norm_squared() * bi.mass_half;
        for j in (i + 1)..N_BODIES {
            let bj = &bodies[j];
            let distance = (bi.position - bj.position).norm_squared().sqrt();
            e -= bi.mass * bj.mass / distance;
        }
    }
    e
}

#[inline]
fn offset_momentum(bodies: &mut [Planet; N_BODIES]) {
    let mut p = narr!(f64; 0, 0, 0, 0);
    for bi in bodies.iter() {
        p -= bi.velocity * bi.mass_ratio;
    }
    let sun = &mut bodies[0];
    sun.velocity = p;
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
