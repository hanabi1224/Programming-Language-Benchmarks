# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Jesse Millikan
# Modified by Wesley Moxam
# *reset*


def item_check(left, right)
  return 1 if left.nil?
  1 + item_check(*left) + item_check(*right)
end

def bottom_up_tree(depth)
  return [nil, nil] unless depth > 0
  depth -= 1
  [bottom_up_tree(depth), bottom_up_tree(depth)]
end

max_depth = ARGV[0].to_i
min_depth = 4

max_depth = min_depth + 2 if min_depth + 2 > max_depth

stretch_depth = max_depth + 1
stretch_tree = bottom_up_tree(stretch_depth)

puts "stretch tree of depth #{stretch_depth}\t check: #{item_check(*stretch_tree)}"
stretch_tree = nil

long_lived_tree = bottom_up_tree(max_depth)

min_depth.step(max_depth, 2) do |depth|
  iterations = 2**(max_depth - depth + min_depth)

  check = 0

  for i in 1..iterations
    temp_tree = bottom_up_tree(depth)
    check += item_check(*temp_tree)
  end

  puts "#{iterations}\t trees of depth #{depth}\t check: #{check}"
end

puts "long lived tree of depth #{max_depth}\t check: #{item_check(*long_lived_tree)}"