const int minDepth = 4;

class Node {
  int? hash = null;
  final int? value;
  final Node? left;
  final Node? right;
  Node(int? this.value, Node? this.left, Node? this.right);

  static Node create(int depth) {
    return depth > 0
        ? Node(null, create(depth - 1), create(depth - 1))
        : Node(1, null, null);
  }

  int getHash() {
    return hash == null ? -1 : hash!;
  }

  void calHash() {
    if (hash == null) {
      if (value != null) {
        hash = value;
      } else if (left != null && right != null) {
        left!.calHash();
        right!.calHash();
        hash = left!.getHash() + right!.getHash();
      }
    }
  }

  bool check() {
    if (hash != null) {
      if (value != null) {
        return true;
      } else if (left != null && right != null) {
        return left!.check() && right!.check();
      }
    }
    return false;
  }
}

void main(List<String> args) {
  final n = args.length > 0 ? int.parse(args[0]) : 6;

  final maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
  final stretchDepth = maxDepth + 1;
  final stretchTree = Node.create(stretchDepth);
  stretchTree.calHash();
  print(
      "stretch tree of depth $stretchDepth\t root hash: ${stretchTree.getHash()} check: ${stretchTree.check()}");

  final longLivedTree = Node.create(maxDepth);

  for (var depth = minDepth; depth <= maxDepth; depth += 2) {
    final iterations = 1 << (maxDepth - depth + minDepth);
    var sum = 0;
    for (var i = 0; i < iterations; i += 1) {
      final tree = Node.create(depth);
      tree.calHash();
      sum += tree.getHash();
    }
    print("${iterations}\t trees of depth $depth\t root hash sum: $sum");
  }

  longLivedTree.calHash();
  print(
      "long lived tree of depth $maxDepth\t root hash: ${longLivedTree.getHash()} check: ${longLivedTree.check()}");
}
