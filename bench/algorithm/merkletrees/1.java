/*
    The merkletrees problem is based on the binarytrees one,
    but with some rule changes:
    - leaf nodes store data(1) and hash, non-leaf nodes only store hash
    - leaf data type should be 64 bit signed integer
    - hash of each node should not be calculated during construction
    - calculated hash should be stored in every tree node
    - make in-place updates on every tree node after hash is calculated
    - use self-referencing binary tree data structure, array is not acceptable
    - no arena usage, every tree node needs to be either separately freed or GCed
    - check function ensures hash value of every tree node is calculated and updated

    ref: http://en.wikipedia.org/wiki/Merkle_tree
*/

class app {

    private static final int minDepth = 4;

    public static void main(String[] args) {
        int n = 0;
        if (args.length > 0)
            n = Integer.parseInt(args[0]);

        int maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
        int stretchDepth = maxDepth + 1;

        TreeNode stretchTree = TreeNode.make(stretchDepth);
        stretchTree.calHash();
        System.out.println("stretch tree of depth " + stretchDepth + "\t root hash: " + stretchTree.getHash()
                + " check: " + stretchTree.check());

        TreeNode longLivedTree = TreeNode.make(maxDepth);

        for (int depth = minDepth; depth <= maxDepth; depth += 2) {
            int iterations = 1 << (maxDepth - depth + minDepth);
            var sum = 0;
            for (int i = 1; i <= iterations; i++) {
                TreeNode tree = TreeNode.make(depth);
                tree.calHash();
                sum += tree.getHash();
            }
            System.out.println(iterations + "\t trees of depth " + depth + "\t root hash sum: " + sum);
        }

        longLivedTree.calHash();
        System.out.println(
                "long lived tree of depth " + maxDepth + "\t root hash: " + longLivedTree.getHash() + " check: "
                        + longLivedTree.check());
    }

    private static class TreeNode {
        private Long value = null, hash = null;
        private TreeNode left = null, right = null;

        public static TreeNode make(int depth) {
            if (depth > 0) {
                return new TreeNode(null, make(depth - 1), make(depth - 1));
            } else {
                return new TreeNode(1L, null, null);
            }
        }

        public TreeNode(Long value, TreeNode left, TreeNode right) {
            this.value = value;
            this.left = left;
            this.right = right;
        }

        public long getHash() {
            if (hash != null) {
                return hash;
            }
            return -1;
        }

        public void calHash() {
            if (hash == null) {
                if (value != null) {
                    hash = value;
                } else if (left != null && right != null) {
                    left.calHash();
                    right.calHash();
                    hash = left.getHash() + right.getHash();
                }
            }
        }

        public boolean check() {
            if (hash == null) {
                return false;
            } else {
                if (value != null) {
                    return true;
                } else if (left != null && right != null) {
                    return left.check() && right.check();
                }
                return false;
            }
        }
    }
}
