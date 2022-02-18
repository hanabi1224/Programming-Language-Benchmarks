require "digest"

alias F64x8 = Float64[8]

def add(a : F64x8, b : F64x8, r : Pointer(Float64))
  (0...8).each do |i|
    r[i] = a.unsafe_fetch(i) + b.unsafe_fetch(i)
  end
end

def minus(a : F64x8, b : F64x8, r : Pointer(Float64))
  (0...8).each do |i|
    r[i] = a.unsafe_fetch(i) - b.unsafe_fetch(i)
  end
end

def mul(a : F64x8, b : F64x8, r : Pointer(Float64))
  (0...8).each do |i|
    r[i] = a.unsafe_fetch(i) * b.unsafe_fetch(i)
  end
end

def mbrot8(cr : F64x8, civ : Float64) : UInt8
  ci = F64x8.new civ
  zr = F64x8.new 0
  zi = F64x8.new 0
  tr = F64x8.new 0
  ti = F64x8.new 0
  absz = uninitialized F64x8
  tmp = uninitialized F64x8
  (0...10).each do
    (0...5).each do
      add(zr, zr, tmp.to_unsafe)
      mul(tmp, zi, tmp.to_unsafe)
      add(tmp, ci, zi.to_unsafe)

      minus(tr, ti, tmp.to_unsafe)
      add(tmp, cr, zr.to_unsafe)

      mul(zr, zr, tr.to_unsafe)
      mul(zi, zi, ti.to_unsafe)
    end
    add(tr, ti, absz.to_unsafe)
    terminate = true
    (0...8).each do |i|
      if absz.unsafe_fetch(i) <= 4.0
        terminate = false
        break
      end
    end
    if terminate
      return 0_u8
    end
  end
  accu = 0_u8
  (0...8).each do |i|
    if absz.unsafe_fetch(i) <= 4.0
      accu |= (0x80 >> i)
    end
  end
  return accu
end

size = ARGV.size > 0 ? ARGV[0].to_i : 200
size = (size + 7) // 8 * 8
puts "P4\n#{size} #{size}"

chunk_size = size // 8
inv = 2.0_f64 / size

xloc = Array(F64x8).new chunk_size
(0...chunk_size).each do |i|
  xloc << F64x8.new { |j| (i*8 + j)*inv - 1.5 }
end

pixels = Slice(UInt8).new size*chunk_size, 0

(0...size).each do |y|
  ci = inv * y - 1.0
  (0...chunk_size).each do |x|
    r = mbrot8(xloc.unsafe_fetch(x), ci)
    if r > 0
      pixels.unsafe_put y*chunk_size + x, r
    end
  end
end

hasher = Digest::MD5.new
hasher.update(pixels)
hash = hasher.final
puts hash.map { |b| b.to_s(16, precision: 2, upcase: false) }.join
