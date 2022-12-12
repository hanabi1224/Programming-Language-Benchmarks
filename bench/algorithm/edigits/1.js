function main() {
  let n = 27;
  if (process.argv.length > 2) {
    const _n = parseInt(process.argv[2]);
    if (!isNaN(_n)) {
      n = _n;
    }
  }
  const k = binary_search(n);
  let [p, q] = sum_terms(0n, BigInt(k - 1));
  p = p + q;
  const a = 10n ** BigInt(n - 1);
  let answer = p;
  answer = answer * a;
  answer = answer / q;
  const s = answer.toString();
  for (let i = 0; i < n; i += 10) {
    if (i + 10 <= n) {
      console.log(`${s.slice(i, i + 10)}\t:${i + 10}`);
    } else {
      console.log(`${s.slice(i, n).padEnd(10)}\t:${n}`);
    }
  }
}

function sum_terms(a, b) {
  if (b === a + 1n) {
    return [1n, b];
  }

  const mid = BigInt((a + b) / 2n);
  const [p_left, q_left] = sum_terms(a, mid);
  const [p_right, q_right] = sum_terms(mid, b);
  let l, r;
  l = p_left * q_right;
  l = l + p_right;
  r = q_left * q_right;
  return [l, r];
}

function binary_search(n) {
  let a = 0;
  let b = 1;
  while (!test_k(n, b)) {
    a = b;
    b *= 2;
  }
  while (b - a > 1) {
    const m = Math.floor((a + b) / 2);
    if (test_k(n, m)) {
      b = m;
    } else {
      a = m;
    }
  }
  return b;
}

function test_k(n, k) {
  if (k < 0) {
    return false;
  }
  const ln_k_factorial = k * (Math.log(k) - 1) + 0.5 * Math.log(Math.PI * 2);
  const log_10_k_factorial = ln_k_factorial / Math.LN10;
  return log_10_k_factorial >= n + 50;
}

main();
