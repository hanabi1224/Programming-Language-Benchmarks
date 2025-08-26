// Single-thread implementation

actor Main
  new create(env: Env) =>
    let n: USize =
      try
        env.args(1)?.usize()?
      else
        100
      end

    let primes = Array[U64]
    var count: USize = 0
    var i: U64 = 2
    while count < n do
      var is_prime = true
      for p in primes.values() do
        if (p * p) > i then break end
        if (i % p) == 0 then
          is_prime = false
          break
        end
      end
      if is_prime then
        env.out.write(i.string() + "\n")
        primes.push(i)
        count = count + 1
      end
      i = i + 1
    end
