// based on 1.zig
use "collections"

actor Main
  new create(env: Env) =>
    let n0: USize = try env.args(1)?.usize()? else 8 end
    let n = ((n0 + 7) / 8) * 8
    let size = n
    let inv: F64 = 2.0 / n.f64()
    let stdout = env.out
    stdout.print("P4\n" + size.string() + " " + size.string())
    let pixels = recover iso Array[U8](size * (size / 8)) end
    var y: USize = 0
    while y < size do
      let ci = (y.f64() * inv) - 1.0
      var x: USize = 0
      var byte: U8 = 0
      var bit: USize = 0
      while x < size do
        let cr = (x.f64() * inv) - 1.5
        var zr: F64 = 0
        var zi: F64 = 0
        var trmti: F64 = 0
        var k: USize = 0
        var inside = true
        while k < 50 do
          zi = (2.0 * zr * zi) + ci
          zr = trmti + cr
          let tr = zr * zr
          let ti = zi * zi
          if (tr + ti) > 4.0 then
            inside = false
            break
          end
          trmti = tr - ti
          k = k + 1
        end
        byte = byte << 1
        if inside then byte = byte or 0x01 end
        bit = bit + 1
        if bit == 8 then
          pixels.push(byte)
          byte = 0
          bit = 0
        end
        x = x + 1
      end
      y = y + 1
    end
  // Compute MD5 digest of pixel bytes and print
  let digest = _md5_hex(consume pixels)
  stdout.print(digest)

  fun _hex(bs: Array[U8] val): String val =>
    let buf = recover iso Array[U8](bs.size() * 2) end
    for b in bs.values() do
      let hi: U8 = (b >> 4) and 0x0F
      let lo: U8 = b and 0x0F
      let dhi = hi.u32()
      let dlo = lo.u32()
      buf.push((if dhi < 10 then 48 + dhi else 87 + dhi end).u8())
      buf.push((if dlo < 10 then 48 + dlo else 87 + dlo end).u8())
    end
    String.from_array(consume buf)

  fun _md5_hex(bytes: Array[U8] iso): String val =>
    var md5 = MD5
    let bval: Array[U8] val = consume bytes
    try
      md5.update(bval)?
    end
  let dig =
      try
        md5.final()?
      else
        recover iso Array[U8](0) end
      end
  _hex(consume dig)
class MD5
  var _a: U32 = 0x67452301
  var _b: U32 = 0xefcdab89
  var _c: U32 = 0x98badcfe
  var _d: U32 = 0x10325476
  var _len: U64 = 0
  var _buf: Array[U8] = Array[U8]

  fun ref update(data: Array[U8] box) ? =>
    _len = _len + (data.size().u64() * 8)
    var i: USize = 0
    while i < data.size() do
      _buf.push(data(i)?)
      if _buf.size() == 64 then
        let block = recover iso Array[U8](64) end
        var m: USize = 0
        while m < 64 do block.push(_buf(m)?); m = m + 1 end
        let bval: Array[U8] val = consume block
  _transform(bval)?
        _buf.clear()
      end
      i = i + 1
    end

  fun ref final(): Array[U8] iso^ ? =>
    _buf.push(0x80)
    while (_buf.size() % 64) != 56 do _buf.push(0) end
    var l: U64 = _len
    var j: USize = 0
    while j < 8 do
      _buf.push((l and 0xff).u8())
      l = l >> 8
      j = j + 1
    end
    var k: USize = 0
    while k < _buf.size() do
      let block = recover iso Array[U8](64) end
      var m: USize = 0
      while m < 64 do block.push(_buf(k + m)?); m = m + 1 end
  let bval: Array[U8] val = consume block
  _transform(bval)?
      k = k + 64
    end
  let out = recover iso Array[U8](16) end
  out.push((_a and 0xff).u8())
  out.push(((_a >> 8) and 0xff).u8())
  out.push(((_a >> 16) and 0xff).u8())
  out.push(((_a >> 24) and 0xff).u8())
  out.push((_b and 0xff).u8())
  out.push(((_b >> 8) and 0xff).u8())
  out.push(((_b >> 16) and 0xff).u8())
  out.push(((_b >> 24) and 0xff).u8())
  out.push((_c and 0xff).u8())
  out.push(((_c >> 8) and 0xff).u8())
  out.push(((_c >> 16) and 0xff).u8())
  out.push(((_c >> 24) and 0xff).u8())
  out.push((_d and 0xff).u8())
  out.push(((_d >> 8) and 0xff).u8())
  out.push(((_d >> 16) and 0xff).u8())
  out.push(((_d >> 24) and 0xff).u8())
  out

  fun _to_u32_le(b: Array[U8] box, o: USize): U32 ? =>
    b(o)?.u32() or (b(o+1)?.u32() << 8) or (b(o+2)?.u32() << 16) or (b(o+3)?.u32() << 24)

  fun ref _transform(block: Array[U8] box) ? =>
    var a = _a
    var b = _b
    var c = _c
    var d = _d
  let s1: Array[U32] iso = recover iso Array[U32](4) end; s1.push(7); s1.push(12); s1.push(17); s1.push(22)
  let s2: Array[U32] iso = recover iso Array[U32](4) end; s2.push(5); s2.push(9); s2.push(14); s2.push(20)
  let s3: Array[U32] iso = recover iso Array[U32](4) end; s3.push(4); s3.push(11); s3.push(16); s3.push(23)
  let s4: Array[U32] iso = recover iso Array[U32](4) end; s4.push(6); s4.push(10); s4.push(15); s4.push(21)
  let k: Array[U32] iso = recover iso Array[U32](64) end
  k.push(0xd76aa478); k.push(0xe8c7b756); k.push(0x242070db); k.push(0xc1bdceee); k.push(0xf57c0faf); k.push(0x4787c62a); k.push(0xa8304613); k.push(0xfd469501)
  k.push(0x698098d8); k.push(0x8b44f7af); k.push(0xffff5bb1); k.push(0x895cd7be); k.push(0x6b901122); k.push(0xfd987193); k.push(0xa679438e); k.push(0x49b40821)
  k.push(0xf61e2562); k.push(0xc040b340); k.push(0x265e5a51); k.push(0xe9b6c7aa); k.push(0xd62f105d); k.push(0x02441453); k.push(0xd8a1e681); k.push(0xe7d3fbc8)
  k.push(0x21e1cde6); k.push(0xc33707d6); k.push(0xf4d50d87); k.push(0x455a14ed); k.push(0xa9e3e905); k.push(0xfcefa3f8); k.push(0x676f02d9); k.push(0x8d2a4c8a)
  k.push(0xfffa3942); k.push(0x8771f681); k.push(0x6d9d6122); k.push(0xfde5380c); k.push(0xa4beea44); k.push(0x4bdecfa9); k.push(0xf6bb4b60); k.push(0xbebfbc70)
  k.push(0x289b7ec6); k.push(0xeaa127fa); k.push(0xd4ef3085); k.push(0x04881d05); k.push(0xd9d4d039); k.push(0xe6db99e5); k.push(0x1fa27cf8); k.push(0xc4ac5665)
  k.push(0xf4292244); k.push(0x432aff97); k.push(0xab9423a7); k.push(0xfc93a039); k.push(0x655b59c3); k.push(0x8f0ccc92); k.push(0xffeff47d); k.push(0x85845dd1)
  k.push(0x6fa87e4f); k.push(0xfe2ce6e0); k.push(0xa3014314); k.push(0x4e0811a1); k.push(0xf7537e82); k.push(0xbd3af235); k.push(0x2ad7d2bb); k.push(0xeb86d391)
    var x: Array[U32] iso = recover iso Array[U32](16) end
    var i: USize = 0
    while i < 64 do
      x.push(_to_u32_le(block, i)?)
      i = i + 4
    end

  // Round 1
  a = _r1(a, b, c, d, x(0)?,  s1(0)?, k(0)?)  ; d = _r1(d, a, b, c, x(1)?,  s1(1)?, k(1)?)
  c = _r1(c, d, a, b, x(2)?,  s1(2)?, k(2)?)  ; b = _r1(b, c, d, a, x(3)?,  s1(3)?, k(3)?)
  a = _r1(a, b, c, d, x(4)?,  s1(0)?, k(4)?)  ; d = _r1(d, a, b, c, x(5)?,  s1(1)?, k(5)?)
  c = _r1(c, d, a, b, x(6)?,  s1(2)?, k(6)?)  ; b = _r1(b, c, d, a, x(7)?,  s1(3)?, k(7)?)
  a = _r1(a, b, c, d, x(8)?,  s1(0)?, k(8)?)  ; d = _r1(d, a, b, c, x(9)?,  s1(1)?, k(9)?)
  c = _r1(c, d, a, b, x(10)?, s1(2)?, k(10)?) ; b = _r1(b, c, d, a, x(11)?, s1(3)?, k(11)?)
  a = _r1(a, b, c, d, x(12)?, s1(0)?, k(12)?) ; d = _r1(d, a, b, c, x(13)?, s1(1)?, k(13)?)
  c = _r1(c, d, a, b, x(14)?, s1(2)?, k(14)?) ; b = _r1(b, c, d, a, x(15)?, s1(3)?, k(15)?)

  // Round 2
  a = _r2(a, b, c, d, x(1)?,  s2(0)?, k(16)?)  ; d = _r2(d, a, b, c, x(6)?,  s2(1)?, k(17)?)
  c = _r2(c, d, a, b, x(11)?, s2(2)?, k(18)?)  ; b = _r2(b, c, d, a, x(0)?,  s2(3)?, k(19)?)
  a = _r2(a, b, c, d, x(5)?,  s2(0)?, k(20)?)  ; d = _r2(d, a, b, c, x(10)?, s2(1)?, k(21)?)
  c = _r2(c, d, a, b, x(15)?, s2(2)?, k(22)?)  ; b = _r2(b, c, d, a, x(4)?,  s2(3)?, k(23)?)
  a = _r2(a, b, c, d, x(9)?,  s2(0)?, k(24)?)  ; d = _r2(d, a, b, c, x(14)?, s2(1)?, k(25)?)
  c = _r2(c, d, a, b, x(3)?,  s2(2)?, k(26)?)  ; b = _r2(b, c, d, a, x(8)?,  s2(3)?, k(27)?)
  a = _r2(a, b, c, d, x(13)?, s2(0)?, k(28)?)  ; d = _r2(d, a, b, c, x(2)?,  s2(1)?, k(29)?)
  c = _r2(c, d, a, b, x(7)?,  s2(2)?, k(30)?)  ; b = _r2(b, c, d, a, x(12)?, s2(3)?, k(31)?)

  // Round 3
  a = _r3(a, b, c, d, x(5)?,  s3(0)?, k(32)?)  ; d = _r3(d, a, b, c, x(8)?,  s3(1)?, k(33)?)
  c = _r3(c, d, a, b, x(11)?, s3(2)?, k(34)?)  ; b = _r3(b, c, d, a, x(14)?, s3(3)?, k(35)?)
  a = _r3(a, b, c, d, x(1)?,  s3(0)?, k(36)?)  ; d = _r3(d, a, b, c, x(4)?,  s3(1)?, k(37)?)
  c = _r3(c, d, a, b, x(7)?,  s3(2)?, k(38)?)  ; b = _r3(b, c, d, a, x(10)?, s3(3)?, k(39)?)
  a = _r3(a, b, c, d, x(13)?, s3(0)?, k(40)?)  ; d = _r3(d, a, b, c, x(0)?,  s3(1)?, k(41)?)
  c = _r3(c, d, a, b, x(3)?,  s3(2)?, k(42)?)  ; b = _r3(b, c, d, a, x(6)?,  s3(3)?, k(43)?)
  a = _r3(a, b, c, d, x(9)?,  s3(0)?, k(44)?)  ; d = _r3(d, a, b, c, x(12)?, s3(1)?, k(45)?)
  c = _r3(c, d, a, b, x(15)?, s3(2)?, k(46)?)  ; b = _r3(b, c, d, a, x(2)?,  s3(3)?, k(47)?)

  // Round 4
  a = _r4(a, b, c, d, x(0)?,  s4(0)?, k(48)?)  ; d = _r4(d, a, b, c, x(7)?,  s4(1)?, k(49)?)
  c = _r4(c, d, a, b, x(14)?, s4(2)?, k(50)?)  ; b = _r4(b, c, d, a, x(5)?,  s4(3)?, k(51)?)
  a = _r4(a, b, c, d, x(12)?, s4(0)?, k(52)?)  ; d = _r4(d, a, b, c, x(3)?,  s4(1)?, k(53)?)
  c = _r4(c, d, a, b, x(10)?, s4(2)?, k(54)?)  ; b = _r4(b, c, d, a, x(1)?,  s4(3)?, k(55)?)
  a = _r4(a, b, c, d, x(8)?,  s4(0)?, k(56)?)  ; d = _r4(d, a, b, c, x(15)?, s4(1)?, k(57)?)
  c = _r4(c, d, a, b, x(6)?,  s4(2)?, k(58)?)  ; b = _r4(b, c, d, a, x(13)?, s4(3)?, k(59)?)
  a = _r4(a, b, c, d, x(4)?,  s4(0)?, k(60)?)  ; d = _r4(d, a, b, c, x(11)?, s4(1)?, k(61)?)
  c = _r4(c, d, a, b, x(2)?,  s4(2)?, k(62)?)  ; b = _r4(b, c, d, a, x(9)?,  s4(3)?, k(63)?)

    _a = (_a + a) and 0xffffffff
    _b = (_b + b) and 0xffffffff
    _c = (_c + c) and 0xffffffff
    _d = (_d + d) and 0xffffffff

  fun _rotl(x: U32, s: U32): U32 => ((x << s) or (x >> (32 - s))) and 0xffffffff

  fun _f(x: U32, y: U32, z: U32): U32 => (x and y) or ((not x) and z)
  fun _g(x: U32, y: U32, z: U32): U32 => (x and z) or (y and (not z))
  fun _h(x: U32, y: U32, z: U32): U32 => x xor y xor z
  fun _ii(x: U32, y: U32, z: U32): U32 => y xor (x or (not z))

  fun _r1(a: U32, b: U32, c: U32, d: U32, x: U32, s: U32, t: U32): U32 =>
    var r = a + _f(b, c, d) + x + t
    r = _rotl(r, s)
    (r + b) and 0xffffffff

  fun _r2(a: U32, b: U32, c: U32, d: U32, x: U32, s: U32, t: U32): U32 =>
    var r = a + _g(b, c, d) + x + t
    r = _rotl(r, s)
    (r + b) and 0xffffffff

  fun _r3(a: U32, b: U32, c: U32, d: U32, x: U32, s: U32, t: U32): U32 =>
    var r = a + _h(b, c, d) + x + t
    r = _rotl(r, s)
    (r + b) and 0xffffffff

  fun _r4(a: U32, b: U32, c: U32, d: U32, x: U32, s: U32, t: U32): U32 =>
    var r = a + _ii(b, c, d) + x + t
    r = _rotl(r, s)
    (r + b) and 0xffffffff
