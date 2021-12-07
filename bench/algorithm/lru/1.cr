A = 1103515245_u64
C =      12345_u64
M = 1_u64 << 31

class LCG
  def initialize(seed : UInt64)
    @seed = seed
  end

  def next
    _lcg()
    return @seed
  end

  def _lcg
    @seed = (A * @seed + C) % M
  end
end

class LRU
  def initialize(size : Int32)
    @size = size
    @hash = Hash(UInt64, UInt64).new
  end

  def get(key : UInt64)
    v = @hash[key]?
    if v != nil
      @hash.delete(key)
      @hash[key] = v.as UInt64
      return v
    end
    return v
  end

  def put(key : UInt64, value : UInt64)
    v = @hash[key]?
    if v == nil
      if @hash.size == @size
        @hash.delete(@hash.first_key)
      end
    else
      @hash.delete(key)
    end
    @hash[key] = value
  end
end

n = ARGV.size > 0 ? ARGV[0].to_i : 100
hit = 0
missed = 0
rng0 = LCG.new 0
rng1 = LCG.new 1
lru = LRU.new 10
(0...n).each do |i|
  n0 = rng0.next % 100
  lru.put(n0, n0)
  n1 = rng1.next % 100
  if lru.get(n1) == nil
    missed += 1
  else
    hit += 1
  end
end
puts hit
puts missed
