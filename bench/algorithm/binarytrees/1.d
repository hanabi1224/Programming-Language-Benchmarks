@safe:
import std;

const MIN_DEPTH = 4;

class Node {
    Node left;
    Node right;

    this(Node left, Node right)
    {
        this.left = left;
        this.right = right;
    }

    int check() {
        auto r = 1;
        if (this.left !is null) {
            r += this.left.check();
        }
        if (this.right !is null) {
            r += this.right.check();
        }
        return r;
    }

    static Node create(int depth) {
        if (depth > 0) {
            auto d = depth - 1;
            return new Node(Node.create(d), Node.create(d));
        }
        return new Node(null, null);
    }
}

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int() : 6;
    auto maxDepth = max(MIN_DEPTH + 2, n);
    auto stretchDepth = maxDepth + 1;
    auto stretchTree = Node.create(stretchDepth);
    writeln(format("stretch tree of depth %d\t check: %d", stretchDepth, stretchTree.check()));
    auto longLivedTree = Node.create(maxDepth);

    for (int depth = MIN_DEPTH; depth <= maxDepth; depth += 2) {
        auto iterations = 1 << (maxDepth - depth + MIN_DEPTH);
        auto sum = 0;
        for (auto i = 0; i < iterations; i++) {
            sum += Node.create(depth).check();
        }
        writeln(format("%d\t trees of depth %d\t check: %d", iterations, depth, sum));
    }

    writeln(format("long lived tree of depth %d\t check: %d", maxDepth, longLivedTree.check()));
}
