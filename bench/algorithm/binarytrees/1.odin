// contributed by Branimir Maksimovic

package main

import "core:fmt"
import "core:strconv"
import "core:os"
import "core:math"
import "core:mem"

MIN_DEPTH :: 4
main :: proc() {
  n := strconv.parse_int(os.args[1]) or_else 10
  max_depth := math.max(MIN_DEPTH+2,n)
  allocbuf := make([dynamic]u8,int(math.pow(f64(2),f64(max_depth+1)))*max_depth*size_of(Node))
  arena : mem.Arena
  mem.arena_init(&arena,allocbuf[:])
  global_allocator := context.allocator
  context.allocator = mem.arena_allocator(&arena)
  {
    stretch_depth := max_depth + 1;
    stretch_tree := makeTree(stretch_depth)
    defer { free_all() }
    fmt.printf("stretch tree of depth %d\t check: %d\n", stretch_depth, check(stretch_tree) );
  }
  long_lived_tree := makeTree(max_depth)
  defer { delete (allocbuf) }
  depth:int= MIN_DEPTH;
  for ;depth <= max_depth; depth += 2 {
    allocbuf := make([dynamic]u8,int(math.pow(f64(2),f64(depth+1)))*depth*size_of(Node),global_allocator)
    defer delete(allocbuf)
    arena : mem.Arena
    mem.arena_init(&arena,allocbuf[:])
    context.allocator = mem.arena_allocator(&arena)
    iterations := 1 << u8(max_depth - depth + MIN_DEPTH)
    sum: u64 = 0
    for _ in 0..<iterations {
      tree := makeTree(depth)
      defer { free_all() }
      sum += u64(check(tree))
    }
    fmt.printf("%d\t trees of depth %d\t check: %d\n", iterations, depth, sum )
  }

  fmt.printf("long lived tree of depth %d\t check: %d\n", max_depth, check(long_lived_tree))
}

Node :: struct {
  left,right:^Node,
}

makeTree::proc(depth:int)->^Node {
  node := new(Node)
  if node == nil {
    fmt.println("alloc error")
    return node
  }
  if depth > 0 {
    node.left = makeTree(depth-1)
    node.right = makeTree(depth-1)
  }
  return node
}
check::proc(tree:^Node)->int {
  sum : int = 1
  if tree == nil { return 0 }
  if tree.left != nil {
    sum += check(tree.left)
  }
  if tree.right != nil {
    sum += check(tree.right)
  }
  return sum
}
