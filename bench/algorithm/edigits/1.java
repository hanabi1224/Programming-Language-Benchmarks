import java.math.BigInteger;

final class app {
  public static void main(String args[]) {
    int n = 27;
    if (args.length > 0) {
      n = Integer.parseInt(args[0]);
    }
    int k = binarySearch(n);
    BigPair pair = sumTerms(0, k - 1);
    BigInteger p = pair.p;
    BigInteger q = pair.q;
    p = p.add(q);
    BigInteger a = BigInteger.valueOf(10).pow(n - 1);
    BigInteger answer = p.multiply(a).divide(q);
    String answerStr = answer.toString();
    for (int i = 0; i < n; i += 10) {
      char[] sb = new char[10];
      for (int j = i; j < i + 10; j++) {
        if (j < n) {
          sb[j - i] = answerStr.charAt(j);
        } else {
          sb[j - i] = ' ';
        }
      }
      int count = i + 10;
      if (count > n) {
        count = n;
      }
      System.out.println(new String(sb) + "\t:" + String.valueOf(count));
    }
  }

  static BigPair sumTerms(int a, int b) {
    if (b == a + 1) {
      BigPair pair = new BigPair(BigInteger.valueOf(1), BigInteger.valueOf(b));
      return pair;
    }
    int mid = (a + b) / 2;
    BigPair pairLeft = sumTerms(a, mid);
    BigPair pairRight = sumTerms(mid, b);
    return new BigPair(
        pairLeft.p.multiply(pairRight.q).add(pairRight.p), pairLeft.q.multiply(pairRight.q));
  }

  static int binarySearch(int n) {
    int a = 0;
    int b = 1;
    while (!testK(n, b)) {
      a = b;
      b *= 2;
    }
    while (b - a > 1) {
      int m = (a + b) / 2;
      if (testK(n, m)) {
        b = m;
      } else {
        a = m;
      }
    }
    return b;
  }

  static boolean testK(int n, int k) {
    if (k < 0) {
      return false;
    }
    double lnKFactorial = k * (Math.log((double) k) - 1) + 0.5 * Math.log(Math.PI * 2);
    double log10KFactorial = lnKFactorial / Math.log(10);
    return log10KFactorial >= (double) (n + 50);
  }
}

class BigPair {
  public BigInteger p, q;

  public BigPair(BigInteger p, BigInteger q) {
    this.p = p;
    this.q = q;
  }
}
