import kotlin.math.*

const val MinDepth = 4

fun main(args: Array<String>) {
    val maxDepth = if (args.size > 0) max(args[0].toInt(), MinDepth + 2) else 10

    val stretchDepth = maxDepth + 1
    val stretchTree = Node.create(stretchDepth)
    stretchTree.calHash()
    println(
            "stretch tree of depth ${stretchDepth}\t root hash: ${stretchTree.getHash()} check: ${stretchTree.check()}"
    )

    val longLivedTree = Node.create(maxDepth)

    val nResults = (maxDepth - MinDepth) / 2 + 1
    for (i in 0..nResults - 1) {
        val depth = i * 2 + MinDepth
        val n = 1.shl(maxDepth - depth + MinDepth)

        var sum = 0L
        for (j in 1..n) {
            val tree = Node.create(depth)
            tree.calHash()
            sum += tree.getHash()
        }

        println("${n}\t trees of depth ${depth}\t root hash sum: ${sum}")
    }

    longLivedTree.calHash()
    println(
            "long lived tree of depth ${maxDepth}\t root hash: ${longLivedTree.getHash()} check: ${longLivedTree.check()}"
    )
}

class Node(val value: Long?, val left: Node?, val right: Node?) {
    var hash: Long? = null

    fun check(): Boolean {
        if (hash != null) {
            if (value != null) {
                return true
            }
            if (left != null && right != null) {
                return left.check() && right.check()
            }
            return false
        } else {
            return false
        }
    }

    fun calHash() {
        if (hash == null) {
            if (value != null) {
                hash = value
            } else if (left != null && right != null) {
                left.calHash()
                right.calHash()
                hash = left.getHash() + right.getHash()
            }
        }
    }

    fun getHash(): Long {
        if (hash != null) {
            return hash!!
        } else {
            return -1
        }
    }

    companion object {
        fun create(n: Int): Node {
            if (n == 0) {
                return Node(1, null, null)
            }
            val d = n - 1
            return Node(null, Node.create(d), Node.create(d))
        }
    }
}
