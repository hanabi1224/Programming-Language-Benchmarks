module main

import os

fn main() {
  n := if os.args.len > 1 {
    os.args[1].int()
  } else {
    100
  }

  mut sieve := []bool{len: n+1, init: true}
  for i := 2; i*i <= n; i++ {
    if sieve[i] {
      for j := i*i; j <= n; j += i {
        sieve[j] = false
      }
    }
  }
  for i := 2; i <= n; i++ {
    if sieve[i] {
      println(i)
    }
  }
}
