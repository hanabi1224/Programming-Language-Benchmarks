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
    boolean[] flags = new boolean[n];
    for (int i = 2; i < n; i++) {
      if (!flags[i]) {
        count += 1;      
        for (int j = i << 1; j < n; j += i) {
          flags[j] = true;
        }
      }
    }
    System.out.printf("Primes up to %8d %8d\n", n, count);
  }
}
