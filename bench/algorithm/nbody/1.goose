// The bodies are a fixed array the run keeps in a local, and the pair loops
// have constant bounds, so the C compiler unrolls them completely and keeps
// every body in registers, as the C++ program by Jeffrey Beu gets its
// templates to do. (2.goose lays the bodies out for vector registers.)

import std;
import os;
import math;

let SOLAR_MASS = 4.0 * PI * PI;
let DAYS_PER_YEAR = 365.24;
let DT = 0.01;

struct Body { x: f64, y: f64, z: f64, vx: f64, vy: f64, vz: f64, mass: f64 }

fn body(x: f64, y: f64, z: f64, vx: f64, vy: f64, vz: f64, mass: f64) -> Body {
    Body { x, y, z, vx * DAYS_PER_YEAR, vy * DAYS_PER_YEAR, vz * DAYS_PER_YEAR, mass * SOLAR_MASS }
}

fn solar_system() -> Body[5] {
    [
        body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
        body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
             1.66007664274403694e-03, 7.69901118419740425e-03, -6.90460016972063023e-05,
             9.54791938424326609e-04),
        body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
             -2.76742510726862411e-03, 4.99852801234917238e-03, 2.30417297573763929e-05,
             2.85885980666130812e-04),
        body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
             2.96460137564761618e-03, 2.37847173959480950e-03, -2.96589568540237556e-05,
             4.36624404335156298e-05),
        body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
             2.68067772490389322e-03, 1.62824170038242295e-03, -9.51592254519715870e-05,
             5.15138902046611451e-05),
    ]
}

fn offset_momentum(b: Body[5]&) {
    var px = 0.0;
    var py = 0.0;
    var pz = 0.0;
    for q in b {
        px += q.vx * q.mass;
        py += q.vy * q.mass;
        pz += q.vz * q.mass;
    }
    b[0].vx = -px / SOLAR_MASS;
    b[0].vy = -py / SOLAR_MASS;
    b[0].vz = -pz / SOLAR_MASS;
}

fn energy(b: Body[5]&) -> f64 {
    var e = 0.0;
    for i in 5 {
        e += 0.5 * b[i].mass * (b[i].vx * b[i].vx + b[i].vy * b[i].vy + b[i].vz * b[i].vz);
        for j in i + 1..5 {
            let dx = b[i].x - b[j].x;
            let dy = b[i].y - b[j].y;
            let dz = b[i].z - b[j].z;
            e -= b[i].mass * b[j].mass / sqrt(dx * dx + dy * dy + dz * dz);
        }
    }
    return e;
}

fn advance(b: Body[5]&, steps: i64) {
    var p = b;
    for s in steps {
        for i in 5 {
            for j in i + 1..5 {
                let dx = p[i].x - p[j].x;
                let dy = p[i].y - p[j].y;
                let dz = p[i].z - p[j].z;
                let d2 = dx * dx + dy * dy + dz * dz;
                let mag = DT / (d2 * sqrt(d2));
                let mi = p[i].mass * mag;
                let mj = p[j].mass * mag;
                p[i].vx -= dx * mj;
                p[i].vy -= dy * mj;
                p[i].vz -= dz * mj;
                p[j].vx += dx * mi;
                p[j].vy += dy * mi;
                p[j].vz += dz * mi;
            }
        }
        for &q in p {
            q.x += DT * q.vx;
            q.y += DT * q.vy;
            q.z += DT * q.vz;
        }
    }
    b = p;
}

fn print_energy(b: Body[5]&) {
    var line: u8[>..] = [];
    format_flt(line, energy(b), 9);
    print(line);
}

fn main() {
    let a = args();
    var n = 1000;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    var b = solar_system();
    offset_momentum(b);
    print_energy(b);
    advance(b, n);
    print_energy(b);
}
