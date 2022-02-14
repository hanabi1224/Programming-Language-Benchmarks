import os, strutils, options


type
  Node = ref object
    hash: Option[int64]
    value: Option[int64]
    left: Node
    right: Node

proc make(depth: int): Node =
  if depth == 0:
    Node(value: some(int64(1)), left: nil, right: nil)
  else:
    let d = depth - 1
    Node(left: make(d), right: make(d))


proc check(node: Node): bool =
  if node.hash.isNone:
    return false
  if node.value.isSome:
    return true
  if node.left != nil and node.right != nil:
    return node.left.check and node.right.check
  return false

proc cal_hash(node: Node) =
  if node.hash.isNone:
    if node.value.isSome:
      node.hash = node.value
    elif node.left != nil and node.right != nil:
      node.left.cal_hash
      node.right.cal_hash
      node.hash = some(node.left.hash.get + node.right.hash.get)

const MIN_DEPTH = 4
var max_depth = if paramCount() > 0: parseInt(paramStr(1)) else: 6
if MIN_DEPTH + 2 > max_depth:
  max_depth = MIN_DEPTH + 2

let stretch_depth = max_depth + 1
let stretch_tree = make(stretch_depth)
stretch_tree.cal_hash
echo "stretch tree of depth ", stretch_depth, "\t root hash: ",
    stretch_tree.hash.get, " check: ", stretch_tree.check
let long_lived_tree = make(max_depth)
var d = MIN_DEPTH
while d <= max_depth:
  let iterations = 1 shl (max_depth - d + MIN_DEPTH)
  var check = int64(0)
  for i in 1..iterations:
    let tree = make(d)
    tree.cal_hash
    check += tree.hash.get
  echo iterations, "\t trees of depth ", d, "\t root hash sum: ", check
  d += 2
long_lived_tree.cal_hash
echo "long lived tree of depth ", max_depth, "\t root hash: ",
    long_lived_tree.hash.get, " check: ", long_lived_tree.check
