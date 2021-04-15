require "math"
require "big"

ZERO = BigInt.new 0
ONE  = BigInt.new 1
TEN  = BigInt.new 10

n = ARGV.size > 0 ? ARGV[0].to_i : 27
k = binary_search(n)
p, q = sum_terms(0, k - 1)
p += q
a = TEN ** (n - 1)
answer = p*a//q
s = answer.to_s
(0...n).step(10).each do |i|
  if i + 10 <= n
    puts "#{s[i...i + 10]}\t:#{i + 10}"
  else
    print s[i...n]
    (0...10 - n % 10).each do |j|
      print ' '
    end
    puts "\t:#{n}"
  end
end

def sum_terms(a : Int, b : Int)
  if b == a + 1
    return ONE, BigInt.new b
  end
  mid = (a + b) // 2
  p_left, q_left = sum_terms(a, mid)
  p_right, q_right = sum_terms(mid, b)
  return p_left * q_right + p_right, q_left * q_right
end

def binary_search(n : Int)
  a = 0
  b = 1
  while !test_k(n, b)
    a = b
    b *= 2
  end
  while b - a > 1
    m = (a + b) // 2
    if test_k(n, m)
      b = m
    else
      a = m
    end
  end
  return b
end

def test_k(n : Int, k : Int)
  if k < 0
    return false
  end
  ln_k_factorial = k * (Math.log(k) - 1) + 0.5 * Math.log(Math::TAU)
  log_10_k_factorial = ln_k_factorial / Math::LOG10
  return log_10_k_factorial >= n + 50
end
