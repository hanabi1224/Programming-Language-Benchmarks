import 'dart:math' as Math;

void main(List<String> arguments) {
  var n = arguments.length > 0 ? int.parse(arguments[0]) : 27;
  var k = binarySearch(n);
  var l = sumTerms(0, k - 1);
  var p = l[0];
  var q = l[1];
  p += q;
  var a = BigInt.from(10).pow(n - 1);
  var answer = p * a ~/ q;
  var answerStr = answer.toString();
  for (var i = 0; i < n; i += 10) {
    var count = i + 10;
    if (count > n) {
      count = n;
    }
    var sb = [32, 32, 32, 32, 32, 32, 32, 32, 32, 32];
    for (var j = i; j < n && j < i + 10; j++) {
      sb[j - i] = answerStr.codeUnitAt(j);
    }
    print("${String.fromCharCodes(sb)}\t:$count");
  }
}

List<BigInt> sumTerms(int a, int b) {
  if (b == a + 1) {
    return [BigInt.from(1), BigInt.from(b)];
  }
  var mid = (a + b) ~/ 2;
  var pairLeft = sumTerms(a, mid);
  var pairRight = sumTerms(mid, b);
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
    var m = (a + b) ~/ 2;
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
  var lnKFactorial = k * (Math.log(k) - 1) + 0.5 * Math.log(Math.pi * 2);
  var log10KFactorial = lnKFactorial / Math.ln10;
  return log10KFactorial >= n + 50;
}
