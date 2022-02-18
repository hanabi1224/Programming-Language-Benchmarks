require "digest"

alias F64x8 = Float64[8]

def add(a : F64x8, b : F64x8) : F64x8
  return F64x8.new { |i| a.unsafe_fetch(i) + b.unsafe_fetch(i) }
end

def minus(a : F64x8, b : F64x8) : F64x8
  return F64x8.new { |i| a.unsafe_fetch(i) - b.unsafe_fetch(i) }
end

def mul(a : F64x8, b : F64x8) : F64x8
  return F64x8.new { |i| a.unsafe_fetch(i) * b.unsafe_fetch(i) }
end

def mbrot8(cr : F64x8, civ : Float64) : UInt8
  ci = F64x8.new civ
  zr = F64x8.new 0.0
  zi = F64x8.new 0.0
  tr = F64x8.new 0.0
  ti = F64x8.new 0.0
  absz = uninitialized F64x8
  (0...10).each do
    (0...5).each do
      tmp = add(zr, zr)
      tmp = mul(tmp, zi)
      zi = add(tmp, ci)

      tmp = minus(tr, ti)
      zr = add(tmp, cr)

      tr = mul(zr, zr)
      ti = mul(zi, zi)
    end
    absz = add(tr, ti)
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
