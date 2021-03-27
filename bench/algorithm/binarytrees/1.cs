// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Marek Safar
// concurrency added by Peperud
// fixed long-lived tree by Anthony Lloyd
// ported from F# version by Anthony Lloyd
// removed the trick of using struct by hanabi1224
// removed multi-threading by hanabi1224

using System;
using System.Threading.Tasks;

class BinaryTrees
{
    class TreeNode
    {
        public TreeNode left;
        public TreeNode right;

        TreeNode(TreeNode left = null, TreeNode right = null)
        {
            this.left = left;
            this.right = right;
        }

        internal static TreeNode Create(int d)
        {
            return d == 0 ? new TreeNode()
                          : new TreeNode(Create(d - 1), Create(d - 1));
        }

        internal int Check()
        {
            int c = 1;
            if (right != null)
            {
                c += right.Check();
            }
            if (left != null)
            {
                c += left.Check();
            }
            return c;
        }

        internal void Hold() { }
    }

    const int MinDepth = 4;
    public static void Main(string[] args)
    {
        var maxDepth = args.Length == 0 ? 10
            : Math.Max(MinDepth + 2, int.Parse(args[0]));

        var stretchDepth = maxDepth + 1;
        Console.WriteLine($"stretch tree of depth {stretchDepth}\t check: {TreeNode.Create(stretchDepth).Check()}");

        var longLivedTree = TreeNode.Create(maxDepth);
        var longLivedText = Task.Run(() =>
        {
            return $"long lived tree of depth {maxDepth}\t check: {longLivedTree.Check()}";
        });

        var nResults = (maxDepth - MinDepth) / 2 + 1;
        for (int i = 0; i < nResults; i++)
        {
            var depth = i * 2 + MinDepth;
            var n = (1 << maxDepth - depth + MinDepth);

            var check = 0;
            for (int j = 0; j < n; j++)
            {
                check += TreeNode.Create(depth).Check();
            }

            Console.WriteLine($"{n}\t trees of depth {depth}\t check: {check}");
        }

        Console.WriteLine($"long lived tree of depth {maxDepth}\t check: {longLivedTree.Check()}");
    }
}
