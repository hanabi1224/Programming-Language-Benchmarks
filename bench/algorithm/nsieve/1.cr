def nsieve(n)
  count = 0
  flags = Slice(Bool).new n, false
  (2...n).each do |i|
    if !flags.unsafe_fetch(i)
      count += 1
    end
    (i*2...n).step(i).each do |j|
      flags.unsafe_put j, true
    end
  end
  puts "Primes up to %8d %8d" % [n, count]
end

n = ARGV.size > 0 ? ARGV[0].to_i : 4
(0...3).each do |i|
  nsieve(10000 << (n - i))
end
