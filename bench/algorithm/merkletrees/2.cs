using System;

class MerkleTrees
{
    interface IBaseNode<T>
    {
        bool HasHash();
        T GetHash();
    }

    interface INode<T> : IBaseNode<T>
    {
        void CalHash();
        bool Check();
    }

    abstract class BaseNode : IBaseNode<long>
    {
        protected long? hash;

        public bool HasHash() => hash.HasValue;

        public long GetHash()
        {
            if (hash.Value is long v)
            {
                return v;
            }
            return default;
        }
    }

    class Leaf : BaseNode, INode<long>
    {
        private long value;

        public Leaf(long value)
        {
            this.value = value;
        }

        public bool Check() => HasHash();

        public void CalHash()
        {
            if (!HasHash())
            {
                hash = value;
            }
        }
    }

    class TreeNode : BaseNode, INode<long>
    {
        public INode<long> left;
        public INode<long> right;

        public TreeNode(INode<long> left, INode<long> right)
        {
            this.left = left;
            this.right = right;
        }

        public static INode<long> Create(int d)
        {
            return d == 0 ? new Leaf(1L)
                          : new TreeNode(Create(d - 1), Create(d - 1));
        }

        public bool Check()
        {
            return HasHash() && left.Check() && right.Check();
        }

        public void CalHash()
        {
            if (hash == null)
            {
                left.CalHash();
                right.CalHash();
                hash = left.GetHash() + right.GetHash();
            }
        }
    }

    const int MinDepth = 4;
    public static void Main(string[] args)
    {
        var maxDepth = args.Length == 0 ? 10
            : Math.Max(MinDepth + 2, int.Parse(args[0]));

        var stretchDepth = maxDepth + 1;
        var stretchTree = TreeNode.Create(stretchDepth);
        stretchTree.CalHash();
        Console.WriteLine($"stretch tree of depth {stretchDepth}\t root hash: {stretchTree.GetHash()} check: {stretchTree.Check().ToString().ToLowerInvariant()}");

        var longLivedTree = TreeNode.Create(maxDepth);
        var nResults = (maxDepth - MinDepth) / 2 + 1;
        for (int i = 0; i < nResults; i++)
        {
            var depth = i * 2 + MinDepth;
            var n = (1 << maxDepth - depth + MinDepth);

            var sum = 0L;
            for (int j = 0; j < n; j++)
            {
                var tree = TreeNode.Create(depth);
                tree.CalHash();
                sum += tree.GetHash();
            }
            Console.WriteLine($"{n}\t trees of depth {depth}\t root hash sum: {sum}");
        }

        longLivedTree.CalHash();
        Console.WriteLine($"long lived tree of depth {maxDepth}\t root hash: {longLivedTree.GetHash()} check: {longLivedTree.Check().ToString().ToLowerInvariant()}");
    }
}
