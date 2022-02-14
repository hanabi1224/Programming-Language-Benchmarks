import os, strutils


type
  Node = ref object
    left: Node
    right: Node

proc make(depth: int): Node =
  if depth == 0:
    Node(left: nil, right: nil)
  else:
    let d = depth - 1
    Node(left: make(d), right: make(d))


proc check(node: Node): int =
  if node.left != nil and node.right != nil:
    1 + node.left.check + node.right.check
  else:
    1

const MIN_DEPTH = 4
var max_depth = if paramCount() > 0: parseInt(paramStr(1)) else: 6
if MIN_DEPTH + 2 > max_depth:
  max_depth = MIN_DEPTH + 2

let stretch_depth = max_depth + 1
let stretch_tree = make(stretch_depth)
echo "stretch tree of depth ", stretch_depth, "\t check: ", stretch_tree.check
let long_lived_tree = make(max_depth)
var d = MIN_DEPTH
while d <= max_depth:
  let iterations = 1 shl (max_depth - d + MIN_DEPTH)
  var check = 0
  for i in 1..iterations:
    let tree = make(d)
    check += tree.check
  echo iterations, "\t trees of depth ", d, "\t check: ", check
  d += 2
echo "long lived tree of depth ", max_depth, "\t check: ", long_lived_tree.check
