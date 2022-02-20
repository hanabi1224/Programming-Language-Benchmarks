import java.io.*;
import java.security.*;
import java.util.stream.*;
import jdk.incubator.vector.*;

final class F64x8 {
  // avx512 is not allowed
  static final VectorSpecies<Double> SPECIES = DoubleVector.SPECIES_256;

  final DoubleVector v0;
  final DoubleVector v1;

  public F64x8(DoubleVector v0, DoubleVector v1) {
    this.v0 = v0;
    this.v1 = v1;
  }

  public F64x8(double[] array) {
    v0 = DoubleVector.fromArray(SPECIES, array, 0);
    v1 = DoubleVector.fromArray(SPECIES, array, 4);
  }

  public F64x8(double v) {
    v0 = DoubleVector.fromArray(SPECIES, new double[] {v, v, v, v}, 0);
    v1 = DoubleVector.fromArray(SPECIES, new double[] {v, v, v, v}, 0);
  }

  public final F64x8 add(F64x8 b) {
    return new F64x8(v0.add(b.v0), v1.add(b.v1));
  }

  public final F64x8 sub(F64x8 b) {
    return new F64x8(v0.sub(b.v0), v1.sub(b.v1));
  }

  public final F64x8 mul(F64x8 b) {
    return new F64x8(v0.mul(b.v0), v1.mul(b.v1));
  }

  public final F64x8 fma(F64x8 b, F64x8 c) {
    return new F64x8(v0.fma(b.v0, c.v0), v1.fma(b.v1, c.v1));
  }

  public final double get(int i) {
    if (i < 4) {
      return v0.lane(i);
    } else {
      return v1.lane(i - 4);
    }
  }

  public final double min() {
    return Math.min(v0.reduceLanes(VectorOperators.MIN), v1.reduceLanes(VectorOperators.MIN));
  }
}

public final class app {
  // avx512 is not allowed
  static final VectorSpecies<Double> SPECIES = DoubleVector.SPECIES_256;

  public static void main(String[] args) throws Exception {
    int N = 200;
    if (args.length >= 1) {
      N = Integer.parseInt(args[0]);
    }

    N = (N + 7) / 8 * 8;
    final int chunkSize = N / 8;
    final double inv = 2.0 / N;

    final var xloc = new F64x8[chunkSize];
    for (int i = 0; i < chunkSize; i++) {
      final var offset = (double) (i * 8);
      xloc[i] =
          new F64x8(
              new double[] {
                offset * inv - 1.5,
                (offset + 1.0) * inv - 1.5,
                (offset + 2.0) * inv - 1.5,
                (offset + 3.0) * inv - 1.5,
                (offset + 4.0) * inv - 1.5,
                (offset + 5.0) * inv - 1.5,
                (offset + 6.0) * inv - 1.5,
                (offset + 7.0) * inv - 1.5,
              });
    }

    byte[] data = new byte[N * chunkSize];
    IntStream.range(0, N)
        .forEach(
            y -> {
              var ci = y * inv - 1.0;
              IntStream.range(0, chunkSize)
                  .forEach(
                      x -> {
                        data[y * chunkSize + x] = mbrot8(xloc[x], ci);
                      });
            });
    System.out.println("P4\n" + N + " " + N);
    // System.out.println(toHexString(data));
    MessageDigest hasher = MessageDigest.getInstance("md5");
    hasher.update(data);
    String hash = toHexString(hasher.digest());
    System.out.println(hash);
  }

  static final byte mbrot8(final F64x8 cr, final double civ) {
    var ci = new F64x8(civ);
    var zr = new F64x8(0.0);
    var zi = new F64x8(0.0);
    var tr = new F64x8(0.0);
    var ti = new F64x8(0.0);
    F64x8 absz = null;
    for (var _i = 0; _i < 10; _i++) {
      for (var _j = 0; _j < 5; _j++) {
        var tmp = zr.add(zr);
        zi = tmp.fma(zi, ci);

        tmp = tr.sub(ti);
        zr = tmp.add(cr);

        tr = zr.mul(zr);
        ti = zi.mul(zi);
      }
      absz = tr.add(ti);
      if (absz.min() > 4.0) {
        return 0;
      }
    }

    var accu = (byte) 0;
    for (var i = 0; i < 8; i++) {
      if (absz.get(i) <= 4.0) {
        accu |= 0x80 >> i;
      }
    }
    return accu;
  }

  static final String toHexString(byte[] a) {
    StringBuilder sb = new StringBuilder(a.length * 2);
    for (byte b : a) sb.append(String.format("%02x", b));
    return sb.toString();
  }
}
