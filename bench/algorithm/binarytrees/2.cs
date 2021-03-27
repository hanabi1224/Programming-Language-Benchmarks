// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Marek Safar
// concurrency added by Peperud
// fixed long-lived tree by Anthony Lloyd
// ported from F# version by Anthony Lloyd
// removed the trick of using struct by hanabi1224
// fixed long-lived tree reference lifetime by hanabi1224

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

    public static async Task Main(string[] args)
    {
        int maxDepth = args.Length == 0 ? 10
            : Math.Max(MinDepth + 2, int.Parse(args[0]));

        var stretchTreeCheck = Task.Run(() =>
        {
            int stretchDepth = maxDepth + 1;
            return $"stretch tree of depth {stretchDepth}\t check: {TreeNode.Create(stretchDepth).Check()}";
        });

        var longLivedTree = TreeNode.Create(maxDepth);
        var longLivedText = Task.Run(() =>
        {
            return $"long lived tree of depth {maxDepth}\t check: {longLivedTree.Check()}";
        });

        var nResults = (maxDepth - MinDepth) / 2 + 1;
        var results = new string[nResults];
        var resultTasks = new Task[nResults];
        for (int i = 0; i < results.Length; i++)
        {
            var index = i;
            var depth = i * 2 + MinDepth;
            var n = (1 << maxDepth - depth + MinDepth);
            resultTasks[i] = Task.Run(async () =>
            {
                var check = 0;
                for (int j = 0; j < n; j++)
                {
                    check += TreeNode.Create(depth).Check();
                }

                results[index] = $"{n}\t trees of depth {depth}\t check: {check}";
            });
        }

        Console.WriteLine(await stretchTreeCheck.ConfigureAwait(false));

        for (int i = 0; i < results.Length; i++)
        {
            await resultTasks[i].ConfigureAwait(false);
            Console.WriteLine(results[i]);
        }

        Console.WriteLine(await longLivedText.ConfigureAwait(false));
        longLivedTree.Hold();
    }
}
