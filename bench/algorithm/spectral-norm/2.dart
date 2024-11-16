// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Jos Hirth
// based on the JavaScript version by Ian Osgood with modifications by Isaac
// Gouy.

import 'dart:math';
import 'dart:typed_data';

void main(List<String> args) {
  final n = args.isNotEmpty ? int.parse(args[0]) : 100;
  if (n % 2 != 0) {
    throw ArgumentError('n must be even, but is $n.');
  }

  print(SpectralNorm(n).spectralNorm().toStringAsFixed(9));
}

final Float64x2 one = Float64x2(1, 1);
final Float64x2 two = Float64x2(2, 2);

Float64x2 A(Float64x2 i, Float64x2 j) =>
    (i + j) * (j + i + one) / two + i + one;

class SpectralNorm {
  int n;
  SpectralNorm(int n) : n = n ~/ 2;

  void au(Float64x2List u, Float64x2List w) {
    for (var i = 0; i < n; ++i) {
      final i2 = i * 2.0;
      final i2x2 = Float64x2(i2, i2);
      final i2x2plusOne = i2x2 + one;
      var t = Float64x2.zero();
      var t2 = Float64x2.zero();
      for (var j = 0; j < n; ++j) {
        final u2 = u[j];
        final j2 = j * 2.0;
        final j2x2 = Float64x2(j2, j2 + 1);
        t += u2 / A(i2x2, j2x2);
        t2 += u2 / A(i2x2plusOne, j2x2);
      }
      w[i] = Float64x2(t.x + t.y, t2.x + t2.y);
    }
  }

  void atu(Float64x2List w, Float64x2List v) {
    for (var i = 0; i < n; ++i) {
      final i2 = i * 2.0;
      final i2x2 = Float64x2(i2, i2);
      final i2x2plusOne = i2x2 + one;
      var t = Float64x2.zero();
      var t2 = Float64x2.zero();
      for (var j = 0; j < n; ++j) {
        final w2 = w[j];
        final j2 = j * 2.0;
        final j2x2 = Float64x2(j2, j2 + 1);
        t += w2 / A(j2x2, i2x2);
        t2 += w2 / A(j2x2, i2x2plusOne);
      }
      v[i] = Float64x2(t.x + t.y, t2.x + t2.y);
    }
  }

  void atAu(Float64x2List u, Float64x2List v, Float64x2List w) {
    au(u, w);
    atu(w, v);
  }

  double spectralNorm() {
    var u = Float64x2List(n)..fillRange(0, n, Float64x2.splat(1.0)),
        v = Float64x2List(n),
        w = Float64x2List(n),
        vv = Float64x2.zero(),
        vBv = Float64x2.zero();

    for (var i = 0; i < 10; ++i) {
      atAu(u, v, w);
      atAu(v, u, w);
    }
    for (var i = 0; i < n; ++i) {
      vBv += u[i] * v[i];
      vv += v[i] * v[i];
    }
    return sqrt((vBv.x + vBv.y) / (vv.x + vv.y));
  }
}
