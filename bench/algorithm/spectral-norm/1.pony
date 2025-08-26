// Single-threaded implementation of the spectral norm benchmark.
// Based on OCaml (2.ml) and Zig implementation.
// It timeouts on bigger data 

actor Main
  new create(env: Env) =>
    let n: USize = try env.args(1)?.usize()? else 100 end
    let iters: USize = 10
    let u = Array[F64].init(1.0, n)
    let v = Array[F64].init(0.0, n)
    let x = Array[F64].init(0.0, n) // reusable scratch

    var i: USize = 0
    while i < iters do
      try _ata_times_u(v, u, x)? end
      try _ata_times_u(u, v, x)? end
      i = i + 1
    end

    var vBv: F64 = 0
    var vv: F64 = 0
    var k: USize = 0
    try
      while k < n do
        vBv = vBv + (u(k)? * v(k)?)
        vv = vv + (v(k)? * v(k)?)
        k = k + 1
      end
    end
    let res = (vBv / vv).sqrt()
    // format to 9 decimals
    let s = _fmt9(res)
    env.out.print(consume s)

  fun _a(i: USize, j: USize): F64 =>
    let ij = i + j
    1.0 / (((ij * (ij + 1)) >> 1) + i + 1).f64()

  fun _a_times_u(u: Array[F64] box, v: Array[F64] ref) ? =>
    let n = u.size()
    var i: USize = 0
    while i < n do
      var sum: F64 = 0
      var j: USize = 0
      while j < n do
        sum = sum + (_a(i, j) * u(j)?)
        j = j + 1
      end
      v.update(i, sum)?
      i = i + 1
    end

  fun _at_times_u(u: Array[F64] box, v: Array[F64] ref) ? =>
    let n = u.size()
    var i: USize = 0
    while i < n do
      var sum: F64 = 0
      var j: USize = 0
      while j < n do
        sum = sum + (_a(j, i) * u(j)?)
        j = j + 1
      end
      v.update(i, sum)?
      i = i + 1
    end

  fun _ata_times_u(out: Array[F64] ref, inp: Array[F64] box, scratch: Array[F64] ref) ? =>
    // scratch and out are length n
    _a_times_u(inp, scratch)?
    _at_times_u(scratch, out)?

  fun _fmt9(x: F64): String iso^ =>
    var i: I64 = x.trunc().i64()
    let frac: F64 = (x - i.f64()).abs()
    var f: I64 = (frac * 1_000_000_000.0).round().i64()
    if f == 1_000_000_000 then
      f = 0
      if x >= 0 then
        i = i + 1
      else
        i = i - 1
      end
    end
    let s = recover iso String end
    let istr = i.string()
    s.append(consume istr)
    s.push(46)
    let fs = f.string()
    let pad: USize = if fs.size() < 9 then 9 - fs.size() else 0 end
    var k: USize = 0
    while k < pad do s.push(48); k = k + 1 end
    s.append(consume fs)
    consume s