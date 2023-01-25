/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Jos Hirth,
   modified by Srdjan Mitrovic (typed-data),
   transliterated from Isaac Gouy and Robert F. Tobler's C# program
   migrated
*/

import 'dart:math' as math;

void main(List<String> args) {
  final n = args.isNotEmpty ? int.parse(args[0]) : 1000;

  final system = NBodySystem();
  print(system.energy().toStringAsFixed(9));
  for (var i = 0; i < n; i++) {
    system.advance(0.01);
  }
  print(system.energy().toStringAsFixed(9));
}

class Body {
  double x;
  double y;
  double z;
  double vx;
  double vy;
  double vz;
  double mass;

  Body(
      {required this.x,
      required this.y,
      required this.z,
      required this.vx,
      required this.vy,
      required this.vz,
      required this.mass});
}

class NBodySystem {
  final bodies = <Body>[];

  static const solarmass = 4 * math.pi * math.pi;
  static const daysPeryear = 365.24;
  static const N = 5;

  NBodySystem() {
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

    var px = 0.0, py = 0.0, pz = 0.0;
    for (var b in bodies) {
      px += b.vx * b.mass;
      py += b.vy * b.mass;
      pz += b.vz * b.mass;
    }

    final sol = bodies[0];
    sol.vx = -px / solarmass;
    sol.vy = -py / solarmass;
    sol.vz = -pz / solarmass;
  }

  void advance(double dt) {
    for (var na = 0; na < N; na++) {
      final a = bodies[na];
      for (var nb = na + 1; nb < N; nb++) {
        final b = bodies[nb];

        final dx = a.x - b.x, dy = a.y - b.y, dz = a.z - b.z;
        final d2 = dx * dx + dy * dy + dz * dz;
        final mag = dt / (d2 * math.sqrt(d2));

        final bmMag = b.mass * mag;
        a.vx -= dx * bmMag;
        a.vy -= dy * bmMag;
        a.vz -= dz * bmMag;

        final amMag = a.mass * mag;
        b.vx += dx * amMag;
        b.vy += dy * amMag;
        b.vz += dz * amMag;
      }
      a.x += dt * a.vx;
      a.y += dt * a.vy;
      a.z += dt * a.vz;
    }
  }

  double energy() {
    var e = 0.0;
    for (var i = 0; i < bodies.length; i++) {
      final bi = bodies[i];
      e += 0.5 * bi.mass * (bi.vx * bi.vx + bi.vy * bi.vy + bi.vz * bi.vz);
      for (var j = i + 1; j < bodies.length; j++) {
        final bj = bodies[j];
        final dx = bi.x - bj.x, dy = bi.y - bj.y, dz = bi.z - bj.z;
        e -= (bi.mass * bj.mass) / math.sqrt(dx * dx + dy * dy + dz * dz);
      }
    }
    return e;
  }
}
