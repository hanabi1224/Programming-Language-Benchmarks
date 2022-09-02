const int minDepth = 4;

class Node {
  final Node? left;
  final Node? right;
  Node._(Node? this.left, Node? this.right);

  factory Node.create(int depth) {
    return depth > 0
        ? Node._(Node.create(depth - 1), Node.create(depth - 1))
        : Node._(null, null);
  }

  int check() {
    var r = 1;

    final left = this.left;
    if (left != null) {
      r += left.check();
    }

    final right = this.right;
    if (right != null) {
      r += right.check();
    }

    return r;
  }
}

void main(List<String> args) {
  final n = args.length > 0 ? int.parse(args[0]) : 6;

  final maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
  final stretchDepth = maxDepth + 1;
  final stretchTree = Node.create(stretchDepth);
  print("stretch tree of depth $stretchDepth\t check: ${stretchTree.check()}");

  final longLivedTree = Node.create(maxDepth);

  for (var depth = minDepth; depth <= maxDepth; depth += 2) {
    final iterations = 1 << (maxDepth - depth + minDepth);
    var sum = 0;
    for (var i = 0; i < iterations; i += 1) {
      final tree = Node.create(depth);
      sum += tree.check();
    }
    print("${iterations}\t trees of depth $depth\t check: $sum");
  }
  print("long lived tree of depth $maxDepth\t check: ${longLivedTree.check()}");
}
