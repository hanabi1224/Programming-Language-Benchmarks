// based on 1.go
use "collections"

actor Main
  new create(env: Env) =>
    let n: U32 = try env.args(1)?.u32()? else 4 end
    var i: U32 = 0
    while i < 3 do
      let base: U32 = 10000
      _nsieve(env, (base << (n - i)).u64())
      i = i + 1
    end

  fun _nsieve(env: Env, n: U64) =>
    if n < 2 then
      var buf = recover iso Array[U8](32) end
      buf = _append_bytes(consume buf, "Primes up to ")
      buf = _append_u64_padded(consume buf, n)
      buf.push(32)
      buf = _append_u64_padded(consume buf, 0)
      buf.push(10)
      env.out.write(String.from_array(consume buf))
      return
    end
    let size = n.usize()
    let flags = Array[Bool].init(false, size)
    var count: U64 = 0
    var i: U64 = 2
    while i < n do
      if (try not flags(i.usize())? else false end) then
        count = count + 1
        var j: U64 = i + i
        while j < n do
          try flags.update(j.usize(), true)? end
          j = j + i
        end
      end
      i = i + 1
    end
    var buf2 = recover iso Array[U8](32) end
    buf2 = _append_bytes(consume buf2, "Primes up to ")
    buf2 = _append_u64_padded(consume buf2, n)
    buf2.push(32)
    buf2 = _append_u64_padded(consume buf2, count)
    buf2.push(10)
    env.out.write(String.from_array(consume buf2))

  fun _append_bytes(buf: Array[U8] iso, s: String): Array[U8] iso^ =>
    let a = s.array()
    for b in a.values() do
      buf.push(b)
    end
    consume buf

  fun _append_u64_padded(buf: Array[U8] iso, x: U64): Array[U8] iso^ =>
    // convert to decimal digits
    var tmp = recover iso Array[U8](20) end
    var v = x
    if v == 0 then
      tmp.push(48)
    else
      while v > 0 do
        let d: U8 = (48 + (v % 10).u32()).u8()
        tmp.push(d)
        v = v / 10
      end
    end
    // left pad to width 8
    var digits: USize = tmp.size()
    var pad: USize = if digits >= 8 then 0 else (8 - digits) end
    var k: USize = 0
    while k < pad do
      buf.push(32)
      k = k + 1
    end
    // write digits in reverse
    while digits > 0 do
      digits = digits - 1
      try
        buf.push(tmp(digits)?)
      end
    end
    consume buf
