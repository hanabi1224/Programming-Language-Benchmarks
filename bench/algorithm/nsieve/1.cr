def nsieve(n)
  count = 0
  flags = [false] * n
  (2...n).each do |i|
    unless flags[i]
      count += 1
      (i << 1).step(to: n - 1, by: i) do |j|
        flags[j] = true
      end
    end
  end
  puts "Primes up to %8d %8d" % [n, count]
end

n = ARGV.size > 0 ? ARGV[0].to_i : 4
(0...3).each do |i|
  nsieve(10000 << (n - i))
end
