/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Mark C. Lewis
   use simd powered Vector
*/

import java.text.*;
import jdk.incubator.vector.*;

// nbody
final class app {
  public static void main(String[] args) {
    NumberFormat nf = NumberFormat.getInstance();
    nf.setMaximumFractionDigits(9);
    nf.setMinimumFractionDigits(9);
    nf.setGroupingUsed(false);

    int n = args.length > 0 ? Integer.parseInt(args[0]) : 1000;

    NBodySystem bodies = new NBodySystem();

    System.out.println(nf.format(bodies.energy()));
    for (int i = 0; i < n; ++i) {
      bodies.advance(0.01);
    }
    System.out.println(nf.format(bodies.energy()));
  }
}

final class NBodySystem {
  static final int N = 5;
  private Body[] bodies;

  public NBodySystem() {
    bodies = new Body[] {Body.sun(), Body.jupiter(), Body.saturn(), Body.uranus(), Body.neptune()};

    var pos = DoubleVector.fromArray(Body.SPECIES, new double[] {0.0, 0.0, 0.0, 0.0}, 0);
    for (int i = 0; i < bodies.length; ++i) {
      var bi = bodies[i];
      pos = pos.sub(bi.velocity.mul(bi.mass));
    }
    final var sol = bodies[0];
    sol.velocity = pos.div(Body.SOLAR_MASS);
  }

  public void advance(double dt) {
    for (int i = 0; i < N; ++i) {
      final var bi = bodies[i];
      var vi = bi.velocity;
      for (int j = i + 1; j < N; ++j) {
        final var bj = bodies[j];
        var dpos = bi.pos.sub(bj.pos);
        final var distance2 = dpos.mul(dpos).reduceLanes(VectorOperators.ADD);
        final var mag = dt / (distance2 * Math.sqrt(distance2));
        dpos = dpos.mul(mag);
        vi = vi.sub(dpos.mul(bj.mass));
        bj.velocity = bj.velocity.add(dpos.mul(bi.mass));
      }
      bi.velocity = vi;
      bi.pos = bi.pos.add(vi.mul(dt));
    }
  }

  public double energy() {
    var e = 0.0;
    for (int i = 0; i < bodies.length; ++i) {
      final var bi = bodies[i];
      e += 0.5 * bi.mass * (bi.velocity.mul(bi.velocity).reduceLanes(VectorOperators.ADD));
      for (int j = i + 1; j < bodies.length; ++j) {
        final var bj = bodies[j];
        final var dpos = bi.pos.sub(bj.pos);
        final var distance = Math.sqrt(dpos.mul(dpos).reduceLanes(VectorOperators.ADD));
        e -= (bi.mass * bj.mass) / distance;
      }
    }
    return e;
  }
}

final class Body {
  static final VectorSpecies<Double> SPECIES = DoubleVector.SPECIES_256;
  static final double PI = Math.PI;
  static final double SOLAR_MASS = 4 * PI * PI;
  static final double DAYS_PER_YEAR = 365.24;

  public DoubleVector pos, velocity;
  public final double mass;

  public Body(double x, double y, double z, double vx, double vy, double vz, double m) {
    pos = DoubleVector.fromArray(SPECIES, new double[] {x, y, z, 0.0}, 0);
    velocity = DoubleVector.fromArray(SPECIES, new double[] {vx, vy, vz, 0.0}, 0);
    mass = m;
  }

  static Body jupiter() {
    return new Body(
        4.84143144246472090e+00,
        -1.16032004402742839e+00,
        -1.03622044471123109e-01,
        1.66007664274403694e-03 * DAYS_PER_YEAR,
        7.69901118419740425e-03 * DAYS_PER_YEAR,
        -6.90460016972063023e-05 * DAYS_PER_YEAR,
        9.54791938424326609e-04 * SOLAR_MASS);
  }

  static Body saturn() {
    return new Body(
        8.34336671824457987e+00,
        4.12479856412430479e+00,
        -4.03523417114321381e-01,
        -2.76742510726862411e-03 * DAYS_PER_YEAR,
        4.99852801234917238e-03 * DAYS_PER_YEAR,
        2.30417297573763929e-05 * DAYS_PER_YEAR,
        2.85885980666130812e-04 * SOLAR_MASS);
  }

  static Body uranus() {
    return new Body(
        1.28943695621391310e+01,
        -1.51111514016986312e+01,
        -2.23307578892655734e-01,
        2.96460137564761618e-03 * DAYS_PER_YEAR,
        2.37847173959480950e-03 * DAYS_PER_YEAR,
        -2.96589568540237556e-05 * DAYS_PER_YEAR,
        4.36624404335156298e-05 * SOLAR_MASS);
  }

  static Body neptune() {
    return new Body(
        1.53796971148509165e+01,
        -2.59193146099879641e+01,
        1.79258772950371181e-01,
        2.68067772490389322e-03 * DAYS_PER_YEAR,
        1.62824170038242295e-03 * DAYS_PER_YEAR,
        -9.51592254519715870e-05 * DAYS_PER_YEAR,
        5.15138902046611451e-05 * SOLAR_MASS);
  }

  static Body sun() {
    return new Body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SOLAR_MASS);
  }
}
