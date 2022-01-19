using System;

class MerkleTrees
{
    class TreeNode
    {
        public long? value;
        public long? hash;
        public TreeNode left;
        public TreeNode right;

        public TreeNode(long? value, TreeNode left = null, TreeNode right = null)
        {
            this.value = value;
            this.left = left;
            this.right = right;
        }

        public static TreeNode Create(int d)
        {
            return d == 0 ? new TreeNode(1L, null, null)
                          : new TreeNode(null, Create(d - 1), Create(d - 1));
        }

        public bool Check()
        {
            if (hash != null)
            {
                if (value != null)
                {
                    return true;
                }
                else if (left != null && right != null)
                {
                    return left.Check() && right.Check();
                }
            }
            return false;
        }

        public long GetHash()
        {
            if (hash.HasValue)
            {
                return hash.Value;
            }
            return -1;
        }

        public void CalHash()
        {
            if (hash == null)
            {
                if (value.HasValue)
                {
                    hash = value;
                }
                else if (left != null && right != null)
                {
                    left.CalHash();
                    right.CalHash();
                    hash = left.GetHash() + right.GetHash();
                }
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
