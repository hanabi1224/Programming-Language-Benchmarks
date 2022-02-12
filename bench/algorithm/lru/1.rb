class LCG
  A = 1103515245
  C =      12345
  M = 1 << 31

  def initialize(seed)
    @seed = seed
  end

  def next
    @seed = (A * @seed + C) % M
  end
end

class LRU
  def initialize(size)
    @size = size
    @hash = Hash.new
  end

  def get(key)
    if v = @hash.delete(key)
      @hash[key] = v
    end
  end

  def put(key, value)
    @hash.shift unless @hash.delete(key) || @hash.size < @size
    @hash[key] = value
  end
end

n = ARGV.size > 0 ? ARGV[0].to_i : 100

hit = 0
missed = 0
rng0 = LCG.new 0
rng1 = LCG.new 1
lru = LRU.new 10
n.times do
  n0 = rng0.next % 100
  lru.put(n0, n0)
  n1 = rng1.next % 100
  if lru.get(n1)
    hit += 1
  else
    missed += 1
  end
end

puts hit
puts missed
