class Node
  property hash : Int64? = nil
  property value : Int64? = nil
  property left : Node? = nil
  property right : Node? = nil

  def initialize(value, left, right)
    @value = value
    @left = left
    @right = right
  end

  def check
    if @hash == nil
      return false
    end
    if @value != nil
      return true
    end
    if @left != nil && @right != nil
      return @left.as(Node).check && @right.as(Node).check
    end
    return false
  end

  def cal_hash
    if @hash == nil
      if @value != nil
        @hash = @value
      else
        if @left != nil && @right != nil
          @left.as(Node).cal_hash
          @right.as(Node).cal_hash
          @hash = @left.as(Node).get_hash + @right.as(Node).get_hash
        end
      end
    end
  end

  def get_hash
    if @hash == nil
      return -1_i64
    else
      return @hash.as(Int64)
    end
  end
end

def createNode(n : Int32)
  if n == 0
    return Node.new(1_i64, nil, nil)
  end
  d = n - 1
  return Node.new(nil, createNode(d), createNode(d))
end

max_depth = ARGV.size > 0 ? ARGV[0].to_i : 10
min_depth = 4

max_depth = min_depth + 2 if min_depth + 2 > max_depth

stretch_depth = max_depth + 1
stretch_tree = createNode(stretch_depth)
stretch_tree.cal_hash
puts "stretch tree of depth #{stretch_depth}\t root hash: #{stretch_tree.get_hash} check: #{stretch_tree.check}"
stretch_tree = nil

long_lived_tree = createNode(max_depth)

min_depth.step(to: max_depth, by: 2) do |depth|
  iterations = 2**(max_depth - depth + min_depth)

  sum = 0

  (1..iterations).each do |i|
    temp_tree = createNode(depth)
    temp_tree.cal_hash
    sum += temp_tree.get_hash
  end

  puts "#{iterations}\t trees of depth #{depth}\t root hash sum: #{sum}"
end
long_lived_tree.cal_hash
puts "long lived tree of depth #{max_depth}\t root hash: #{long_lived_tree.get_hash} check: #{long_lived_tree.check}"
