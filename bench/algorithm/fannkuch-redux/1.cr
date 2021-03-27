# From https://github.com/kostya/crystal-benchmarks-game/

def fannkuchredux(n)
  perm1 = StaticArray(Int32, 32).new { |i| i }
  perm = StaticArray(Int32, 32).new(0)
  count = StaticArray(Int32, 32).new(0)
  maxFlipsCount = permCount = checksum = 0
  r = n

  while true
    while r > 1
      count[r - 1] = r
      r -= 1
    end

    n.times { |i| perm[i] = perm1[i] } # dup
    flipsCount = 0

    while !((k = perm[0]) == 0)
      k2 = (k + 1) >> 1
      (0...k2).each do |i|
        j = k - i
        perm[i], perm[j] = perm[j], perm[i] # swap
      end
      flipsCount += 1
    end

    maxFlipsCount = flipsCount if flipsCount > maxFlipsCount
    checksum += (permCount % 2 == 0) ? flipsCount : -flipsCount

    while true
      return {checksum, maxFlipsCount} if r == n

      perm0 = perm1[0]
      (0...r).each do |i|
        j = i + 1
        perm1[i], perm1[j] = perm1[j], perm1[i] # swap
      end

      perm1[r] = perm0
      cntr = count[r] -= 1
      break if cntr > 0
      r += 1
    end
    permCount += 1
  end
end

n = (ARGV[0]? || 10).to_i
sum, flips = fannkuchredux(n)
puts "#{sum}\nPfannkuchen(#{n}) = #{flips}"
