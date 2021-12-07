import kotlin.collections.*

fun main(args: Array<String>) {
    val n = if (args.size > 0) args[0].toInt() else 100
    var hit = 0
    var missed = 0
    val rng0 = LCG(0.toUInt())
    val rng1 = LCG(1.toUInt())
    val lru = LRU(10)
    val upperBound = 100.toUInt()
    repeat(n) {
        val n0 = rng0.next() % upperBound
        lru.put(n0, n0)
        var n1 = rng1.next() % upperBound
        if (lru.get(n1) == null) {
            missed += 1
        } else {
            hit += 1
        }
    }
    println("$hit\n$missed")
}

class LRU(size: Int) {
    public val size = size
    val map = LinkedHashMap<UInt, UInt>(size)

    public fun get(key: UInt): UInt? {
        val v = map.get(key)
        if (v != null) {
            map.remove(key)
            map.put(key, v)
        }

        return v
    }

    public fun put(key: UInt, value: UInt) {
        if (map.containsKey(key)) {
            map.remove(key)
        } else if (map.size == size) {
            map.remove(map.keys.first())
        }

        map.put(key, value)
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
