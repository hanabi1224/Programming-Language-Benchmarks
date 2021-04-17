/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Jos Hirth,
   modified by Srdjan Mitrovic (typed-data),
   transliterated from Isaac Gouy and Robert F. Tobler's C# program
   migrated
*/

import 'dart:math' as Math;
import 'dart:typed_data';

void main(args) {
  int n = args.length > 0 ? int.parse(args[0]) : 10000;

  NBodySystem system = NBodySystem();
  print(system.energy().toStringAsFixed(9));
  for (int i = 0; i < n; i++) {
    system.advance(0.01);
  }
  print(system.energy().toStringAsFixed(9));
}

class Body {
  final Float64List _data = Float64List(7);
  get x => _data[0];
  get y => _data[1];
  get z => _data[2];
  get vx => _data[3];
  get vy => _data[4];
  get vz => _data[5];
  get mass => _data[6];

  set x(v) {
    _data[0] = v;
  }

  set y(v) {
    _data[1] = v;
  }

  set z(v) {
    _data[2] = v;
  }

  set vx(v) {
    _data[3] = v;
  }

  set vy(v) {
    _data[4] = v;
  }

  set vz(v) {
    _data[5] = v;
  }

  set mass(v) {
    _data[6] = v;
  }

  Body({x, y, z, vx, vy, vz, mass}) {
    this.x = x;
    this.y = y;
    this.z = z;
    this.vx = vx;
    this.vy = vy;
    this.vz = vz;
    this.mass = mass;
  }
}

class NBodySystem {
  var bodies;

  static double solarmass = 4 * Math.pi * Math.pi;
  static double daysPeryear = 365.24;

  NBodySystem() {
    bodies = <Body>[];
    bodies.addAll([
      // Sun

      Body(x: 0.0, y: 0.0, z: 0.0, vx: 0.0, vy: 0.0, vz: 0.0, mass: solarmass),
      // Jupiter

      Body(
          x: 4.84143144246472090e+00,
          y: -1.16032004402742839e+00,
          z: -1.03622044471123109e-01,
          vx: 1.66007664274403694e-03 * daysPeryear,
          vy: 7.69901118419740425e-03 * daysPeryear,
          vz: -6.90460016972063023e-05 * daysPeryear,
          mass: 9.54791938424326609e-04 * solarmass),
      // Saturn

      Body(
          x: 8.34336671824457987e+00,
          y: 4.12479856412430479e+00,
          z: -4.03523417114321381e-01,
          vx: -2.76742510726862411e-03 * daysPeryear,
          vy: 4.99852801234917238e-03 * daysPeryear,
          vz: 2.30417297573763929e-05 * daysPeryear,
          mass: 2.85885980666130812e-04 * solarmass),
      // Uranus

      Body(
          x: 1.28943695621391310e+01,
          y: -1.51111514016986312e+01,
          z: -2.23307578892655734e-01,
          vx: 2.96460137564761618e-03 * daysPeryear,
          vy: 2.37847173959480950e-03 * daysPeryear,
          vz: -2.96589568540237556e-05 * daysPeryear,
          mass: 4.36624404335156298e-05 * solarmass),
      // Neptune

      Body(
          x: 1.53796971148509165e+01,
          y: -2.59193146099879641e+01,
          z: 1.79258772950371181e-01,
          vx: 2.68067772490389322e-03 * daysPeryear,
          vy: 1.62824170038242295e-03 * daysPeryear,
          vz: -9.51592254519715870e-05 * daysPeryear,
          mass: 5.15138902046611451e-05 * solarmass)
    ]);

    double px = 0.0, py = 0.0, pz = 0.0;
    for (var b in bodies) {
      px += b.vx * b.mass;
      py += b.vy * b.mass;
      pz += b.vz * b.mass;
    }
    ;

    var sol = bodies[0];
    sol.vx = -px / solarmass;
    sol.vy = -py / solarmass;
    sol.vz = -pz / solarmass;
  }

  void advance(double dt) {
    for (int na = 0; na < bodies.length; na++) {
      Body a = bodies[na];
      for (int nb = na + 1; nb < bodies.length; nb++) {
        Body b = bodies[nb];

        double dx = a.x - b.x, dy = a.y - b.y, dz = a.z - b.z;
        double d2 = dx * dx + dy * dy + dz * dz;
        double mag = dt / (d2 * Math.sqrt(d2));

        a.vx -= dx * b.mass * mag;
        b.vx += dx * a.mass * mag;

        a.vy -= dy * b.mass * mag;
        b.vy += dy * a.mass * mag;

        a.vz -= dz * b.mass * mag;
        b.vz += dz * a.mass * mag;
      }
    }
    for (var b in bodies) {
      b.x += dt * b.vx;
      b.y += dt * b.vy;
      b.z += dt * b.vz;
    }
  }

  double energy() {
    double e = 0.0;
    for (int i = 0; i < bodies.length; i++) {
      var bi = bodies[i];
      e += 0.5 * bi.mass * (bi.vx * bi.vx + bi.vy * bi.vy + bi.vz * bi.vz);
      for (int j = i + 1; j < bodies.length; j++) {
        var bj = bodies[j];
        double dx = bi.x - bj.x, dy = bi.y - bj.y, dz = bi.z - bj.z;
        e -= (bi.mass * bj.mass) / Math.sqrt(dx * dx + dy * dy + dz * dz);
      }
    }
    return e;
  }
}
