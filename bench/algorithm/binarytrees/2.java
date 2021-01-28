/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 
   contributed by Jarkko Miettinen
   *reset*
*/

class binarytrees {

   private final static int minDepth = 4;
   
   public static void main(String[] args){
      int n = 0;
      if (args.length > 0) n = Integer.parseInt(args[0]);
      
      int maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
      int stretchDepth = maxDepth + 1;
      
      int check = (TreeNode.bottomUpTree(stretchDepth)).itemCheck();
      System.out.println("stretch tree of depth "+stretchDepth+"\t check: " + check);
      
      TreeNode longLivedTree = TreeNode.bottomUpTree(maxDepth);
      
      for (int depth=minDepth; depth<=maxDepth; depth+=2){
         int iterations = 1 << (maxDepth - depth + minDepth);
         check = 0;
         
         for (int i=1; i<=iterations; i++){
            check += (TreeNode.bottomUpTree(depth)).itemCheck();
         }
         System.out.println(iterations + "\t trees of depth " + depth + "\t check: " + check);
      }   
      System.out.println("long lived tree of depth " + maxDepth + "\t check: "+ longLivedTree.itemCheck());
   }
   
   
   private static class TreeNode
   {
      private TreeNode left, right;
      
      private static TreeNode bottomUpTree(int depth){
         if (depth>0){
            return new TreeNode(
                  bottomUpTree(depth-1)
                  , bottomUpTree(depth-1)
            );
         }
         else {
            return new TreeNode(null,null);
         }
      }
      
      TreeNode(TreeNode left, TreeNode right){
         this.left = left;
         this.right = right;
      }
      
      private int itemCheck(){
         // if necessary deallocate here
         if (left==null) return 1;
         else return 1 + left.itemCheck() + right.itemCheck();
      }
   }
}
