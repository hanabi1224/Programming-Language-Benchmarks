import std/[os, strutils]

type Node {.acyclic.} = ref object
  le: Node
  ri: Node

func bottomUpTree(depth: int): Node =
  if depth > 0:
    result = Node(le: bottomUpTree(depth-1), ri: bottomUpTree(depth-1))
  else:
    result = Node(le: nil, ri: nil)

func check(n: Node): int =
  result = 1
  if n.le != nil:
    result += n.le.check() + n.ri.check()

proc main() =
  const minDepth = 4
  let maxDepth =
    if paramCount() > 0:
      max(minDepth+2, paramStr(1).parseInt)
    else: 6

  block:
    let depth = maxDepth + 1
    let sTree = bottomUpTree(depth)
    echo "stretch tree of depth ", depth, "\t check: ", sTree.check()
  let longLivedTree = bottomUpTree(maxDepth)

  for d in countup(minDepth, maxDepth, 2):
    let iterations = 1 shl (maxDepth - d + minDepth)
    var checks = 0
    for _ in 1..iterations:
      let tree = bottomUpTree(d)
      checks += tree.check()
    echo iterations, "\t trees of depth ", d, "\t check: ", checks

  echo "long lived tree of depth ", maxDepth, "\t check: ", longLivedTree.check()

when isMainModule:
  main()