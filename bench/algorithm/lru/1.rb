A = 1103515245
C =      12345
M = 1 << 31

class LCG
  def initialize(seed)
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
    def initialize(size)
        @size = size
        @hash = Hash.new
    end
  
    def get(key)
      v = @hash[key]
      if v != nil
        @hash.delete(key)
        @hash[key] = v
      end
      return v
    end
  
    def put(key, value)
      v = @hash[key]
      if v == nil
        if @hash.size == @size
          @hash.delete(@hash.keys.first)
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
