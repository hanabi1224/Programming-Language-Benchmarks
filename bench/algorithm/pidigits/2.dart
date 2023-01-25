/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Alexander Fyodorov
*/

String pad(int i, bool last) {
  var res = i.toString();
  var count = 10 - res.length;
  while (count > 0) {
    last ? res += ' ' : res = '0$res';
    count--;
  }
  return res;
}

void calculatePi(int N) {
  var i = 0, ns = 0;

  final bigintOne = BigInt.from(1);
  final bigintTwo = BigInt.from(2);
  final bigintThree = BigInt.from(3);
  final bigintTen = BigInt.from(10);

  var k = BigInt.from(0);
  var k1 = BigInt.from(1);
  var a = BigInt.from(0);
  var d = BigInt.from(1);
  var m = BigInt.from(0);
  var n = BigInt.from(1);
  var t = BigInt.from(0);
  var u = BigInt.from(1);

  while (true) {
    k += bigintOne;
    k1 += bigintTwo;
    t = n << 1;
    n *= k;
    a += t;
    a *= k1;
    d *= k1;

    if (a.compareTo(n) >= 0) {
      m = n * bigintThree + a;
      t = m ~/ d;
      u = m % d + n;

      if (d.compareTo(u) > 0) {
        ns = ns * 10 + t.toInt();
        i += 1;

        final last = i >= N;
        if (i % 10 == 0 || last) {
          print('${pad(ns, last)}\t:$i');
          ns = 0;
        }

        if (last) break;

        a = (a - d * t) * bigintTen;
        n = n * bigintTen;
      }
    }
  }
}

void main(List<String> arguments) {
  final N = arguments.isNotEmpty ? int.parse(arguments[0]) : 27;
  calculatePi(N);
}
