import java.util.BitSet;

final class app {
  public static void main(String args[]) {
    int n = 4;
    if (args.length > 0) {
      n = Integer.parseInt(args[0]);
    }
    for (int i = 0; i < 3; i++) {
      nsieve(10000 << (n - i));
    }
  }

  static void nsieve(int n) {
    int count = 0;
    BitSet flags = new BitSet(n);
    for (int i = 2; i < n; i++) {
      if (!flags.get(i)) {
        count += 1;
      }
      for (int j = i << 1; j < n; j += i) {
        flags.set(j);
      }
    }
    System.out.printf("Primes up to %8d %8d\n", n, count);
  }
}
