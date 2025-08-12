const int minDepth = 4;

class Node {
  final Node? left;
  final Node? right;
  const Node._(this.left, this.right);

  factory Node.create(int depth) => depth > 0
      ? Node._(Node.create(depth - 1), Node.create(depth - 1))
      : const Node._(null, null);

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
  final n = args.isNotEmpty ? int.parse(args[0]) : 6;

  final maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
  final stretchDepth = maxDepth + 1;
  final stretchTree = Node.create(stretchDepth);
  print('stretch tree of depth $stretchDepth\t check: ${stretchTree.check()}');

  final longLivedTree = Node.create(maxDepth);

  for (var depth = minDepth; depth <= maxDepth; depth += 2) {
    final iterations = 1 << (maxDepth - depth + minDepth);
    var sum = 0;
    for (var i = 0; i < iterations; i += 1) {
      final tree = Node.create(depth);
      sum += tree.check();
    }
    print('$iterations\t trees of depth $depth\t check: $sum');
  }
  print('long lived tree of depth $maxDepth\t check: ${longLivedTree.check()}');
}
