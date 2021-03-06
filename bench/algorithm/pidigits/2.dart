/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Alexander Fyodorov
*/

String pad(i, last) {
  var res = i.toString(), count;
  count = 10 - res.length;
  while (count > 0) {
    last ? res += ' ' : res = '0' + res;
    count--;
  }
  return res;
}

void calculatePi(N) {
  var i = 0, ns = 0;

  BigInt bigint_one = new BigInt.from(1);
  BigInt bigint_two = new BigInt.from(2);
  BigInt bigint_three = new BigInt.from(3);
  BigInt bigint_ten = new BigInt.from(10);

  BigInt k = new BigInt.from(0);
  BigInt k1 = new BigInt.from(1);
  BigInt a = new BigInt.from(0);
  BigInt d = new BigInt.from(1);
  BigInt m = new BigInt.from(0);
  BigInt n = new BigInt.from(1);
  BigInt t = new BigInt.from(0);
  BigInt u = new BigInt.from(1);

  while (true) {
    k += bigint_one;
    k1 += bigint_two;
    t = n << 1;
    n *= k;
    a += t;
    a *= k1;
    d *= k1;

    if (a.compareTo(n) >= 0) {
      m = n * bigint_three + a;
      t = m ~/ d;
      u = m % d + n;

      if (d.compareTo(u) > 0) {
        ns = ns * 10 + t.toInt();
        i += 1;

        var last = i >= N;
        if (i % 10 == 0 || last) {
          print(pad(ns, last) + '\t:$i');
          ns = 0;
        }

        if (last) break;

        a = (a - d * t) * bigint_ten;
        n = n * bigint_ten;
      }
    }
  }
}

void main(List<String> arguments) {
  int N = arguments.length > 0 ? int.parse(arguments[0]) : 27;
  calculatePi(N);
}
