class Node
  property left : Node? = nil
  property right : Node? = nil

  def initialize(left, right)
    @left = left
    @right = right
  end

  def check
    if @left == nil || @right == nil
      return 1
    end
    return @left.as(Node).check + @right.as(Node).check + 1
  end
end

def createNode(n : Int32)
  if n == 0
    return Node.new(nil, nil)
  end
  return Node.new(createNode(n - 1), createNode(n - 1))
end

max_depth = ARGV.size > 0 ? ARGV[0].to_i : 10
min_depth = 4

max_depth = min_depth + 2 if min_depth + 2 > max_depth

stretch_depth = max_depth + 1
stretch_tree = createNode(stretch_depth)

puts "stretch tree of depth #{stretch_depth}\t check: #{stretch_tree.check}"
stretch_tree = nil

long_lived_tree = createNode(max_depth)

min_depth.step(to: max_depth, by: 2) do |depth|
  iterations = 2**(max_depth - depth + min_depth)

  check = 0

  (1..iterations).each do |i|
    temp_tree = createNode(depth)
    check += temp_tree.check
  end

  puts "#{iterations}\t trees of depth #{depth}\t check: #{check}"
end

puts "long lived tree of depth #{max_depth}\t check: #{long_lived_tree.check}"
