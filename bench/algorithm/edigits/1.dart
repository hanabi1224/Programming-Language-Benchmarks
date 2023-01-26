import 'dart:math' as math;

void main(List<String> arguments) {
  final n = arguments.isNotEmpty ? int.parse(arguments[0]) : 27;
  final k = binarySearch(n);
  final l = sumTerms(0, k - 1);
  var p = l[0];
  final q = l[1];
  p += q;
  final a = BigInt.from(10).pow(n - 1);
  final answer = p * a ~/ q;
  final answerStr = answer.toString();
  for (var i = 0; i < n; i += 10) {
    var count = i + 10;
    if (count > n) {
      count = n;
    }
    final sb = [32, 32, 32, 32, 32, 32, 32, 32, 32, 32];
    for (var j = i; j < n && j < i + 10; j++) {
      sb[j - i] = answerStr.codeUnitAt(j);
    }
    print('${String.fromCharCodes(sb)}\t:$count');
  }
}

List<BigInt> sumTerms(int a, int b) {
  if (b == a + 1) {
    return [BigInt.from(1), BigInt.from(b)];
  }
  final mid = (a + b) ~/ 2;
  final pairLeft = sumTerms(a, mid);
  final pairRight = sumTerms(mid, b);
  return [
    pairLeft[0] * pairRight[1] + pairRight[0],
    pairLeft[1] * pairRight[1]
  ];
}

int binarySearch(int n) {
  var a = 0;
  var b = 1;
  while (!testK(n, b)) {
    a = b;
    b *= 2;
  }
  while (b - a > 1) {
    final m = (a + b) ~/ 2;
    if (testK(n, m)) {
      b = m;
    } else {
      a = m;
    }
  }
  return b;
}

bool testK(int n, int k) {
  if (k < 0) {
    return false;
  }
  final lnKFactorial = k * (math.log(k) - 1) + 0.5 * math.log(math.pi * 2);
  final log10KFactorial = lnKFactorial / math.ln10;
  return log10KFactorial >= n + 50;
}
