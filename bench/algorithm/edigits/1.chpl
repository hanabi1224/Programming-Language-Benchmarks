use BigInteger;

config const n = 27;

proc main() {
  param digitsPerLine = 10;
  const k = binary_search(n);
  var (p, q) = sum_terms(0, k - 1);
  p += q;
  const a = new bigint(10) ** (n - 1);
  const answer = p * a  / q;
  const s = answer.get_str();
  var i = 0;
  while(i+10<=n){
    const end = i+10;
    writeln(s[i..<end], "\t:", end);
    i = end;
  }
  const rem_len = n - i;
  if (rem_len > 0) {
    writeln(s[i..<n], " " * (10 - rem_len), "\t:", n);
  }
}

proc sum_terms(a, b) : (bigint, bigint) {
    if (b == a + 1) {
        return (new bigint(1), new bigint(b));
    }
    const mid = (a + b) / 2;
    const t_left = sum_terms(a, mid);
    const t_right = sum_terms(mid, b);
    return (t_left(0) * t_right(1) + t_right(0), t_left(1) * t_right(1));
}

proc binary_search(n) {
    var a = 0;
    var b = 1;
    while(!test_k(n, b)) {
        a = b;
        b *= 2;
    }
    while (b - a > 1) {
        const m = (a + b) / 2;
        if (test_k(n, m)) {
            b = m;
        } else {
            a = m;
        }
    }
    return b;
}

proc test_k(n, k) {
    if (k < 0) {
        return false;
    }
    const ln_k_factorial = k * (log(k) - 1) + 0.5 * log(pi * 2);
    const log_10_k_factorial = ln_k_factorial / ln_10;
    return log_10_k_factorial >= n + 50;
}
