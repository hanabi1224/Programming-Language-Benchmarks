// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Jos Hirth
// based on the JavaScript version by Ian Osgood with modifications by Isaac Gouy

import 'dart:math' as math;
import 'dart:typed_data';

double A(int i, int j) {
  final div = ((i + j) * (i + j + 1) >> 1) + i + 1;
  return 1.0 / div;
}

void Au(Float64List u, Float64List w) {
  final len = u.length;
  for (var i = 0; i < len; ++i) {
    var t = 0.0;
    for (var j = 0; j < len; ++j) {
      t += A(i, j) * u[j];
    }
    w[i] = t;
  }
}

void atu(Float64List w, Float64List v) {
  final len = w.length;
  for (var i = 0; i < len; ++i) {
    var t = 0.0;
    for (var j = 0; j < len; ++j) {
      t += A(j, i) * w[j];
    }
    v[i] = t;
  }
}

void AtAu(Float64List u, Float64List v, Float64List w) {
  Au(u, w);
  atu(w, v);
}

double spectralNorm(int n) {
  var u = Float64List(n)..fillRange(0, n, 1.0),
      v = Float64List(n),
      w = Float64List(n),
      vv = 0.0,
      vBv = 0.0;

  for (var i = 0; i < 10; ++i) {
    AtAu(u, v, w);
    AtAu(v, u, w);
  }
  for (var i = 0; i < n; ++i) {
    vBv += u[i] * v[i];
    vv += v[i] * v[i];
  }
  return math.sqrt(vBv / vv);
}

void main(List<String> args) {
  final n = args.isNotEmpty ? int.parse(args[0]) : 100;
  print(spectralNorm(n).toStringAsFixed(9));
}
