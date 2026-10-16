// The five bodies sit in the lanes of 8-wide arrays, one array per
// coordinate. Since five is odd, every pair is (i, i + 1) or (i, i + 2) mod 5
// for exactly one i, so a step is two rounds of lane-wise arithmetic on
// whole arrays, each between the bodies and a rotated copy of them: one
// vector square root and one vector division a round, and nothing but
// rotations moving data between lanes. Lanes 5 to 7 are padding, computed
// along and never read into lanes 0 to 4.
//
// As in the C program by Christoph Bauer, the velocities are kept multiplied
// by the time step and the masses by its square while advancing, which takes
// a multiplication off every step.

import std;
import os;
import math;

let SOLAR_MASS = 4.0 * PI * PI;
let DAYS_PER_YEAR = 365.24;
let DT = 0.01;

struct Bodies {
    x: f64[8], y: f64[8], z: f64[8],
    vx: f64[8], vy: f64[8], vz: f64[8],
    mass: f64[8],
}

fn solar_system() -> Bodies {
    var b = Bodies { .. };
    let p: f64[7][5] = [
        [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0],
        [4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
         1.66007664274403694e-03, 7.69901118419740425e-03, -6.90460016972063023e-05,
         9.54791938424326609e-04],
        [8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
         -2.76742510726862411e-03, 4.99852801234917238e-03, 2.30417297573763929e-05,
         2.85885980666130812e-04],
        [1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
         2.96460137564761618e-03, 2.37847173959480950e-03, -2.96589568540237556e-05,
         4.36624404335156298e-05],
        [1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
         2.68067772490389322e-03, 1.62824170038242295e-03, -9.51592254519715870e-05,
         5.15138902046611451e-05],
    ];
    for i in 5 {
        b.x[i] = p[i][0];
        b.y[i] = p[i][1];
        b.z[i] = p[i][2];
        b.vx[i] = p[i][3] * DAYS_PER_YEAR;
        b.vy[i] = p[i][4] * DAYS_PER_YEAR;
        b.vz[i] = p[i][5] * DAYS_PER_YEAR;
        b.mass[i] = p[i][6] * SOLAR_MASS;
    }
    return b;
}

fn offset_momentum(b: Bodies&) {
    var px = 0.0;
    var py = 0.0;
    var pz = 0.0;
    for i in 5 {
        px += b.vx[i] * b.mass[i];
        py += b.vy[i] * b.mass[i];
        pz += b.vz[i] * b.mass[i];
    }
    b.vx[0] = -px / SOLAR_MASS;
    b.vy[0] = -py / SOLAR_MASS;
    b.vz[0] = -pz / SOLAR_MASS;
}

fn energy(b: Bodies&) -> f64 {
    var e = 0.0;
    for i in 5 {
        e += 0.5 * b.mass[i] * (b.vx[i] * b.vx[i] + b.vy[i] * b.vy[i] + b.vz[i] * b.vz[i]);
        for j in i + 1..5 {
            let dx = b.x[i] - b.x[j];
            let dy = b.y[i] - b.y[j];
            let dz = b.z[i] - b.z[j];
            e -= b.mass[i] * b.mass[j] / sqrt(dx * dx + dy * dy + dz * dz);
        }
    }
    return e;
}

// Lane l of up1(a) holds a[l + 1 mod 5], of down1(a) a[l - 1 mod 5], and so on.
inline fn up1(a: f64[8]) -> f64[8] { [a[1], a[2], a[3], a[4], a[0], a[5], a[6], a[7]] }
inline fn up2(a: f64[8]) -> f64[8] { [a[2], a[3], a[4], a[0], a[1], a[5], a[6], a[7]] }
inline fn down1(a: f64[8]) -> f64[8] { [a[4], a[0], a[1], a[2], a[3], a[5], a[6], a[7]] }
inline fn down2(a: f64[8]) -> f64[8] { [a[3], a[4], a[0], a[1], a[2], a[5], a[6], a[7]] }

simd fn advance(b: Bodies&, steps: i64) {
    var x = b.x;
    var y = b.y;
    var z = b.z;
    var vx = b.vx;
    var vy = b.vy;
    var vz = b.vz;
    var m = b.mass;
    for l in 8 {
        vx[l] *= DT;
        vy[l] *= DT;
        vz[l] *= DT;
        m[l] *= DT * DT;
    }
    let m1 = up1(m);
    let m2 = up2(m);
    for s in steps {
        let x1 = up1(x);
        let y1 = up1(y);
        let z1 = up1(z);
        let x2 = up2(x);
        let y2 = up2(y);
        let z2 = up2(z);
        // Pair (l, l + 1) gives body l + 1 g1 and takes f1 from body l;
        // pair (l, l + 2) gives body l + 2 g2 and takes f2 from body l.
        var f1x: f64[8] = [0.0; 8];
        var f1y: f64[8] = [0.0; 8];
        var f1z: f64[8] = [0.0; 8];
        var g1x: f64[8] = [0.0; 8];
        var g1y: f64[8] = [0.0; 8];
        var g1z: f64[8] = [0.0; 8];
        var f2x: f64[8] = [0.0; 8];
        var f2y: f64[8] = [0.0; 8];
        var f2z: f64[8] = [0.0; 8];
        var g2x: f64[8] = [0.0; 8];
        var g2y: f64[8] = [0.0; 8];
        var g2z: f64[8] = [0.0; 8];
        for l in 8 {
            let dx = x[l] - x1[l];
            let dy = y[l] - y1[l];
            let dz = z[l] - z1[l];
            let d2 = dx * dx + dy * dy + dz * dz;
            let mag = 1.0 / (d2 * sqrt(d2));
            let ex = x[l] - x2[l];
            let ey = y[l] - y2[l];
            let ez = z[l] - z2[l];
            let e2 = ex * ex + ey * ey + ez * ez;
            let mag2 = 1.0 / (e2 * sqrt(e2));
            f1x[l] = dx * m1[l] * mag;
            f1y[l] = dy * m1[l] * mag;
            f1z[l] = dz * m1[l] * mag;
            g1x[l] = dx * m[l] * mag;
            g1y[l] = dy * m[l] * mag;
            g1z[l] = dz * m[l] * mag;
            f2x[l] = ex * m2[l] * mag2;
            f2y[l] = ey * m2[l] * mag2;
            f2z[l] = ez * m2[l] * mag2;
            g2x[l] = ex * m[l] * mag2;
            g2y[l] = ey * m[l] * mag2;
            g2z[l] = ez * m[l] * mag2;
        }
        let h1x = down1(g1x);
        let h1y = down1(g1y);
        let h1z = down1(g1z);
        let h2x = down2(g2x);
        let h2y = down2(g2y);
        let h2z = down2(g2z);
        for l in 8 {
            vx[l] += (h1x[l] - f1x[l]) + (h2x[l] - f2x[l]);
            vy[l] += (h1y[l] - f1y[l]) + (h2y[l] - f2y[l]);
            vz[l] += (h1z[l] - f1z[l]) + (h2z[l] - f2z[l]);
            x[l] += vx[l];
            y[l] += vy[l];
            z[l] += vz[l];
        }
    }
    for l in 8 {
        vx[l] /= DT;
        vy[l] /= DT;
        vz[l] /= DT;
    }
    b.x = x;
    b.y = y;
    b.z = z;
    b.vx = vx;
    b.vy = vy;
    b.vz = vz;
}

fn print_energy(b: Bodies&) {
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
