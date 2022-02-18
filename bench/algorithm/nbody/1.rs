// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// Remove borrow checker hacks

use std::f64::consts::PI;

const SOLAR_MASS: f64 = 4.0 * PI * PI;
const YEAR: f64 = 365.24;
const N_BODIES: usize = 5;
const ADVANCE_DT: f64 = 0.01;

macro_rules! planet {
    ($x:expr, $y:expr, $z:expr, $vx:expr, $vy:expr, $vz:expr, $mass_ratio:expr) => {{
        Planet {
            x: $x,
            y: $y,
            z: $z,
            vx: $vx,
            vy: $vy,
            vz: $vz,
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
    x: f64,
    y: f64,
    z: f64,
    vx: f64,
    vy: f64,
    vz: f64,
    mass_ratio: f64,
    mass: f64,
    mass_half: f64,
}

fn advance(bodies: &mut [Planet; N_BODIES], dt: f64) {
    for i in 0..(N_BODIES - 1) {
        let (bi_x, bi_y, bi_z, mut bi_vx, mut bi_vy, mut bi_vz, bi_mass) =
            (|p: &Planet| (p.x, p.y, p.z, p.vx, p.vy, p.vz, p.mass))(&bodies[i]);
        for j in (i + 1)..N_BODIES {
            let bj = &mut bodies[j];
            let dx = bi_x - bj.x;
            let dy = bi_y - bj.y;
            let dz = bi_z - bj.z;
            let distance_square = dx * dx + dy * dy + dz * dz;
            let mag = dt / (distance_square * distance_square.sqrt());

            let bj_mass_mag = bj.mass * mag;
            bi_vx -= dx * bj_mass_mag;
            bi_vy -= dy * bj_mass_mag;
            bi_vz -= dz * bj_mass_mag;

            let bi_mass_mag = bi_mass * mag;
            bj.vx += dx * bi_mass_mag;
            bj.vy += dy * bi_mass_mag;
            bj.vz += dz * bi_mass_mag;
        }
        let bi = &mut bodies[i];
        bi.vx = bi_vx;
        bi.vy = bi_vy;
        bi.vz = bi_vz;

        bi.x += bi_vx * dt;
        bi.y += bi_vy * dt;
        bi.z += bi_vz * dt;
    }
    // advance the last one
    let last = &mut bodies[N_BODIES - 1];
    last.x += last.vx * dt;
    last.y += last.vy * dt;
    last.z += last.vz * dt;
}

fn energy(bodies: &[Planet; N_BODIES]) -> f64 {
    let mut e = 0.0;
    let mut bodies = bodies.iter();
    loop {
        let bi = match bodies.next() {
            Some(bi) => bi,
            None => break,
        };
        e += (bi.vx * bi.vx + bi.vy * bi.vy + bi.vz * bi.vz) * bi.mass_half;
        for bj in bodies.clone() {
            let dx = bi.x - bj.x;
            let dy = bi.y - bj.y;
            let dz = bi.z - bj.z;
            let dist = (dx * dx + dy * dy + dz * dz).sqrt();
            e -= bi.mass * bj.mass / dist;
        }
    }
    e
}

fn offset_momentum(bodies: &mut [Planet; N_BODIES]) {
    let mut px = 0.0;
    let mut py = 0.0;
    let mut pz = 0.0;
    for bi in bodies.iter() {
        px -= bi.vx * bi.mass_ratio;
        py -= bi.vy * bi.mass_ratio;
        pz -= bi.vz * bi.mass_ratio;
    }
    let sun = &mut bodies[0];
    sun.vx = px;
    sun.vy = py;
    sun.vz = pz;
}

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(1000);
    let mut bodies = BODIES;

    offset_momentum(&mut bodies);
    println!("{:.9}", energy(&bodies));

    for _ in 0..n {
        advance(&mut bodies, ADVANCE_DT);
    }

    println!("{:.9}", energy(&bodies));
}
