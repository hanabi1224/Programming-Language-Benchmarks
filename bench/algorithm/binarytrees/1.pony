primitive EmptyNode 
  fun check() : U32 => 0

class Node
  let left:(Node|EmptyNode)
  let right:(Node|EmptyNode)
  new create(depth:U32) =>
    if depth > 0 then
      let d = depth - 1
      left = Node(d)
      right = Node(d)
    else
      left = EmptyNode
      right = EmptyNode
    end
  fun check() : U32 =>
    1 + left.check() + right.check()
    
actor Main
  // https://tutorial.ponylang.io/gotchas/garbage-collection.html?h=gc#long-running-behaviors-and-memory
  new create(env: Env) =>
    let minDepth : U32 = 4
    let n = try env.args(1)?.u32()? else 4 end
    let maxDepth = if (minDepth + 2) > n then minDepth + 2 else n end
    let stretchDepth = maxDepth + 1
    env.out.print("stretch tree of depth " + stretchDepth.string() + "\t check: " + createAndCheck(stretchDepth).string())
    let longLivedTree = Node(maxDepth)
    var depth = minDepth
    while depth <= maxDepth do
      let iterations = 1.shl((maxDepth - depth) + minDepth)
      var sum:U32 = 0
      var i:U32 = 0
      while i < iterations do
        sum = sum + createAndCheck(depth)        
        i = i + 1
      end
      env.out.print(iterations.string() + "\t trees of depth " + depth.string() + "\t check: " + sum.string())
      depth = depth + 2
    end

    env.out.print("long lived tree of depth " + maxDepth.string() + "\t check: " + longLivedTree.check().string())

  fun createAndCheck(depth:U32):U32=>
    let tree = Node(depth)
    tree.check()
