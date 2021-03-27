import kotlin.math.*;

const val MinDepth = 4;

fun main(args: Array<String>) {
    val maxDepth = if (args.size > 0) max(args[0].toInt(), MinDepth + 2) else 10

    val stretchDepth = maxDepth + 1
    println("stretch tree of depth ${stretchDepth}\t check: ${Node.create(stretchDepth).check()}")

    val longLivedTree = Node.create(maxDepth)

    val nResults = (maxDepth - MinDepth) / 2 + 1
    for (i in 0..nResults-1)
    {
        val depth = i * 2 + MinDepth
        val n = 1.shl(maxDepth - depth + MinDepth)

        var check = 0
        for (j in 1..n)
        {
            check += Node.create(depth).check()
        }

        println("${n}\t trees of depth ${depth}\t check: ${check}")
    }

    println("long lived tree of depth ${maxDepth}\t check: ${longLivedTree.check()}")
}

class Node(val left: Node?, val right:Node?)
{
    fun check() :Int{
        var ret = 1
        if (left != null) {
            ret += left.check()
        }
        if (right != null) {
            ret += right.check()
        }
        return ret
    }

    companion object {
        fun create(n:Int):Node{
            if(n == 0){
                return Node(null, null)
            } 
            return Node(Node.create(n-1),Node.create(n-1))
        }
    }
}