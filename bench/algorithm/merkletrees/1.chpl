use DynamicIters;

config const n = 10;         // the maximum tree depth

proc main() {
  const minDepth = 4,                      // the shallowest tree
        maxDepth = max(minDepth + 2, n),   // the deepest normal tree
        strDepth = maxDepth + 1,           // the depth of the "stretch" tree
        depths = minDepth..maxDepth by 2;  // the range of depths to create
  {
    const strTree = new Tree(strDepth);
    strTree.calHash();
    writeln("stretch tree of depth ", strDepth, "\t root hash: ", strTree.getHash(), "check: ", strTree.check());
  }

  const llTree = new Tree(maxDepth);

  for depth in dynamic(depths) {
    const iterations = 2**(maxDepth - depth + minDepth);
    var sum = 0;
    for i in 1..iterations {
      const t = new Tree(depth);
      t.calHash();
      sum += t.getHash();
    }
    writeln(iterations, "\t trees of depth ", depth, "\t root hash sum: ", sum);
  }
  llTree.calHash();
  writeln("long lived tree of depth ", maxDepth, "\t root hash: ", llTree.getHash(), "check: ", llTree.check());
}

class Int {
  var value:int;
  proc init(v) {
    value = v;
  }
}

class Tree {
  var value, hash: owned Int?;
  var left, right: owned Tree?;
  proc init(depth) {
    if depth > 0 {
      left  = new owned Tree(depth-1);
      right = new owned Tree(depth-1);
    } else {
      value = new owned Int(1);
    }
  }

  proc check(): bool {
    if (hash) {
      return value != nil || left!.check() && right!.check();
    }
    return false;
  }

  proc getHash(): int {
    if hash {
      return hash!.value;
    }
    return 0;
  }

  proc calHash() {
    if hash {
      return;
    }
    if value {
      hash = value;
    } else if (left != nil && right != nil) {
      left!.calHash();
      right!.calHash();
      hash = new Int(left!.getHash() + right!.getHash());
    }
  }
}
