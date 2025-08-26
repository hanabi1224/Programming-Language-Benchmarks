// Based on the Rust (1.rs) implementation.

use "collections"

class RNG
  var seed: U32
  new create() =>
    seed = 42
  fun ref next(max: F64): F64 =>
    seed = ((seed * 3877) + 29573) % 139968
    (max * seed.f64()) / 139968.0

struct Amino
  let l: U8
  let p: F64
  new create(l': U8, p': F64) =>
    l = l'
    p = p'

actor Main
  new create(env: Env) =>
    let n: USize = try env.args(1)?.usize()? else 10 end
  let stdout = env.out
  let rng = RNG
    let alu: String val = 
      "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTC" +
      "AGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCG" +
      "TGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGG" +
      "AGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

  stdout.print(">ONE Homo sapiens alu")
  try _repeat_and_wrap(stdout, alu, 2 * n)? end

  let iub = recover iso Array[Amino](15)
    .> push(Amino(97, 0.27)) .> push(Amino(99, 0.12)) .> push(Amino(103, 0.12)) .> push(Amino(116, 0.27))
    .> push(Amino(66, 0.02)) .> push(Amino(68, 0.02)) .> push(Amino(72, 0.02)) .> push(Amino(75, 0.02))
    .> push(Amino(77, 0.02)) .> push(Amino(78, 0.02)) .> push(Amino(82, 0.02)) .> push(Amino(83, 0.02))
    .> push(Amino(86, 0.02)) .> push(Amino(87, 0.02)) .> push(Amino(89, 0.02))
  end
    stdout.print(">TWO IUB ambiguity codes")
  try _generate_and_wrap(stdout, recover val consume iub end, 3 * n, rng)? end

  let hs = recover iso Array[Amino](4)
    .> push(Amino(97, 0.3029549426680)) .> push(Amino(99, 0.1979883004921))
    .> push(Amino(103, 0.1975473066391)) .> push(Amino(116, 0.3015094502008))
  end
    stdout.print(">THREE Homo sapiens frequency")
  try _generate_and_wrap(stdout, recover val consume hs end, 5 * n, rng)? end

  fun _repeat_and_wrap(out: OutStream, seq: String val, count: USize) ? =>
    let max_line: USize = 60
    let slen = seq.size()
    let sbytes: Array[U8] val = seq.array()
    let padded = recover iso Array[U8](slen + max_line) end
    var i: USize = 0
    while i < (slen + max_line) do
      padded.push(sbytes(i % slen)?)
      i = i + 1
    end
    var off: USize = 0
    var idx: USize = 0
    while idx < count do
      let rem = count - idx
      let line_len = if rem < max_line then rem else max_line end
      let line_arr = recover iso Array[U8](line_len) end
      var t: USize = 0
      while t < line_len do
        line_arr.push(padded(off + t)?)
        t = t + 1
      end
      out.print(String.from_array(consume line_arr))
      off = off + line_len
      if off >= slen then off = off - slen end
      idx = idx + line_len
    end

  fun _generate_and_wrap(out: OutStream, nts: Array[Amino] val, count: USize, rng: RNG ref) ? =>
    let max_line: USize = 60
    var cum: F64 = 0
    let cum_tot = recover iso Array[F64](nts.size()) end
    var ni: USize = 0
    while ni < nts.size() do
      cum = cum + nts(ni)?.p
      cum_tot.push(cum)
      ni = ni + 1
    end
    var idx: USize = 0
    while idx < count do
      let rem = count - idx
      let line_len = if rem < max_line then rem else max_line end
      let line = recover iso Array[U8](line_len) end
      var j: USize = 0
      while j < line_len do
        let r = rng.next(1.0)
        var c: USize = 0
        var ti: USize = 0
        while ti < cum_tot.size() do
          let t = cum_tot(ti)?
          if r > t then c = c + 1 else break end
          ti = ti + 1
        end
        line.push(nts(c)?.l)
        j = j + 1
      end
      out.print(String.from_array(consume line))
      idx = idx + line_len
    end
