@safe:
import std;

extern(C) __gshared string[] rt_options = [ "gcopt=minPoolSize:300" ];

const MIN_DEPTH = 4;

class Node {
    Nullable! long hash = Nullable!(long).init;
    Nullable! long value;
    Node left;
    Node right;

    this(Nullable! long value, Node left, Node right)
    {
        this.value = value;
        this.left = left;
        this.right = right;
    }

    long getHash() {
        if (!this.hash.isNull) {
            return this.hash.get;
        }
        return -1L;
    }

    void calHash() {
        if (this.hash.isNull) {
            if (!this.value.isNull) {
                this.hash = this.value;
            } else if (this.left !is null && this.right !is null) {
                this.left.calHash();
                this.right.calHash();
                this.hash = (this.left.getHash() + this.right.getHash()).nullable;
            }
        }
    }

    bool check() {
        if (!this.hash.isNull) {
            if (!this.value.isNull) {
                return true;
            } else if (this.left !is null && this.right !is null) {
                return this.left.check() && this.right.check();
            }
        }
        return false;
    }

    static Node create(int depth) {
        if (depth > 0) {
            auto d = depth - 1;
            return new Node(Nullable!(int64_t).init, Node.create(d), Node.create(d));
        }
        return new Node(1L.nullable, null, null);
    }
}

void main(string[] args)
{
    auto n = args.length > 1 ? args[1].to!int() : 6;
    auto maxDepth = max(MIN_DEPTH + 2, n);
    auto stretchDepth = maxDepth + 1;
    auto stretchTree = Node.create(stretchDepth);
    stretchTree.calHash();
    writeln(format("stretch tree of depth %d\t root hash: %d check: %s", stretchDepth, stretchTree.getHash(), stretchTree.check() ? "true" : "false"));
    auto longLivedTree = Node.create(maxDepth);

    for (int depth = MIN_DEPTH; depth <= maxDepth; depth += 2) {
        auto iterations = 1 << (maxDepth - depth + MIN_DEPTH);
        auto sum = 0;
        for (auto i = 0; i < iterations; i++) {
            auto tree = Node.create(depth);
            tree.calHash();
            sum += tree.getHash();
        }
        writeln(format("%d\t trees of depth %d\t root hash sum: %d", iterations, depth, sum));
    }

    longLivedTree.calHash();
    writeln(format("long lived tree of depth %d\t root hash: %d check: %s", maxDepth, longLivedTree.getHash(), longLivedTree.check() ? "true" : "false"));
}
