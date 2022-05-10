fun main(args: Array<String>) {
    val size = if (args.size > 0) args[0].toInt() else 100
    val n = if (args.size > 1) args[1].toInt() else 100
    val mod = (size * 10).toUInt()
    var hit = 0
    var missed = 0
    val rng0 = LCG(0.toUInt())
    val rng1 = LCG(1.toUInt())
    val lru = LRU(size)
    repeat(n) {
        val n0 = rng0.next() % mod
        lru.put(n0, n0)
        var n1 = rng1.next() % mod
        if (lru.get(n1) == null) {
            missed += 1
        } else {
            hit += 1
        }
    }
    println("$hit\n$missed")
}

class Pair<K, V>(var key: K, var value: V) {}

class LRU(val size: Int) {
    private val keys = HashMap<UInt, LinkedListNode<Pair<UInt, UInt>>>(size)
    private val entries = LinkedList<Pair<UInt, UInt>>()

    fun get(key: UInt): UInt? {
        val node = keys[key] ?: return null
        entries.moveToEnd(node)
        return node.data.value
    }

    fun put(key: UInt, value: UInt) {
        val node = keys[key]
        if (node != null) {
            node.data.value = value
            entries.moveToEnd(node)
        } else if (entries.len == size) {
            val head = entries.head!!
            keys.remove(head.data.key)
            head.data.key = key
            head.data.value = value
            entries.moveToEnd(head)
            keys[key] = head
        } else {
            keys[key] = entries.add(Pair(key, value))
        }
    }
}

class LCG(seed: UInt) {
    var _seed = seed
    public fun next(): UInt {
        lcg()
        return _seed
    }

    private fun lcg() {
        _seed = (A * _seed + C) % M
    }

    companion object {
        val A = 1103515245.toUInt()
        val C = 12345.toUInt()
        val M = (1.shl(31)).toUInt()
    }
}

class LinkedListNode<T>(var data: T) {
    var prev: LinkedListNode<T>? = null
    var next: LinkedListNode<T>? = null
}

class LinkedList<T> {
    var head: LinkedListNode<T>? = null
    var tail: LinkedListNode<T>? = null
    var len: Int = 0

    fun add(data: T): LinkedListNode<T> {
        val node = LinkedListNode<T>(data)
        addNode(node)
        len += 1
        return node
    }

    private fun addNode(node: LinkedListNode<T>) {
        if (head == null) {
            head = node
            node.prev = null
        } else if (tail != null) {
            node.prev = tail
            tail!!.next = node
        }
        tail = node
        node.next = null
    }

    private fun remove(node: LinkedListNode<T>) {
        if (head == node) {
            head = node.next
        }
        if (tail == node) {
            tail = node.prev
        }
        if (node.prev != null) {
            node.prev!!.next = node.next
        }
        if (node.next != null) {
            node.next!!.prev = node.prev
        }
    }

    fun moveToEnd(node: LinkedListNode<T>) {
        remove(node)
        addNode(node)
    }
}
