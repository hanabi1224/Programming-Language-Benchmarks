namespace BenchmarkGameBTrees
{
    /*
          The Computer Language Benchmarks Game
          https://salsa.debian.org/benchmarksgame-team/benchmarksgame/ 

          contributed by Marek Safar  
          optimized by kasthack
          *reset*
    */
    using System;
    using System.Threading;
    using System.Threading.Tasks;

    class BinaryTrees
    {
        const int minDepth = 4;
        public static void Main(String[] args)
        {
            int n = 0;
            if (args.Length > 0) n = Int32.Parse(args[0]);
            int maxDepth = Math.Max(minDepth + 2, n);
            int stretchDepth = maxDepth + 1;
            int check = (TreeNode.bottomUpTree(stretchDepth)).itemCheck();
            Console.WriteLine("stretch tree of depth {0}\t check: {1}", stretchDepth, check);
            TreeNode longLivedTree = TreeNode.bottomUpTree(maxDepth);
            for (int depth = minDepth; depth <= maxDepth; depth += 2)
            {
                int iterations = 1 << (maxDepth - depth + minDepth);
                check = 0;

                Parallel.For(1, iterations + 1,
                    () => 0,
                    (i, loop, localCheck) =>
                    {
                        return localCheck + 
                            (TreeNode.bottomUpTree(depth)).itemCheck();
                    },
                    localCheck => Interlocked.Add(ref check, localCheck)
                );

                Console.WriteLine("{0}\t trees of depth {1}\t check: {2}",
                    iterations, depth, check);
            }
            Console.WriteLine("long lived tree of depth {0}\t check: {1}",
                maxDepth, longLivedTree.itemCheck());
        }

        class TreeNode
        {
            private TreeNode left, right;

            internal static TreeNode bottomUpTree(int depth)
            {
                TreeNode t;
                ChildTreeNodes(out t, depth);
                return t;
            }
            static void ChildTreeNodes(out TreeNode node, int depth)
            {
                node = new TreeNode();
                if (depth > 0)
                {
                    ChildTreeNodes(out node.left, depth - 1);
                    ChildTreeNodes(out node.right, depth - 1);
                }
            }
            internal int itemCheck()
            {
                if (right == null) return 1;
                else return 1 + left.itemCheck() + right.itemCheck();
            }
        }
    }
}