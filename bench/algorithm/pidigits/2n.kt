/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Based on Java version (1) by Isaac Gouy

   Contributed by Hank Grabowski
   Modified by hanabi1224
*/

import com.soywiz.kbignum.*

fun main(args: Array<String>) {
    val L = 10

    var n = args[0].toInt()
    var j = 0

    val digits = PiDigitSpigot()

    while (n > 0) {
        if (n >= L) {
            for (i in 0..L - 1) print(digits.next())
            j += L
        } else {
            for (i in 0..n - 1) print(digits.next())
            for (i in n..L - 1) print(" ")
            j += n
        }
        print("\t:")
        println(j)
        n -= L
    }
}

internal class PiDigitSpigot {
    var z: Transformation
    var x: Transformation
    var inverse: Transformation

    init {
        z = Transformation(1, 0, 0, 1)
        x = Transformation(0, 0, 0, 0)
        inverse = Transformation(0, 0, 0, 0)
    }

    operator fun next(): Int {
        val y = digit()
        if (isSafe(y)) {
            z = produce(y)
            return y
        } else {
            z = consume(x.next())
            return next()
        }
    }

    fun digit(): Int {
        return z.extract(3)
    }

    fun isSafe(digit: Int): Boolean {
        return digit == z.extract(4)
    }

    fun produce(i: Int): Transformation {
        return inverse.qrst(10, -10 * i, 0, 1).compose(z)
    }

    fun consume(a: Transformation): Transformation {
        return z.compose(a)
    }
}

internal class Transformation {
    var q: BigInt
    var r: BigInt
    var s: BigInt
    var t: BigInt
    var k: Int = 0

    constructor(q: Int, r: Int, s: Int, t: Int) {
        this.q = q.bi
        this.r = r.bi
        this.s = s.bi
        this.t = t.bi
        k = 0
    }

    constructor(q: BigInt, r: BigInt, s: BigInt, t: BigInt) {
        this.q = q
        this.r = r
        this.s = s
        this.t = t
        k = 0
    }

    operator fun next(): Transformation {
        k++
        q = k.bi
        r = (4 * k + 2).bi
        s = 0.bi
        t = (2 * k + 1).bi
        return this
    }

    fun extract(j: Int): Int {
        val bigj = j.bi
        val numerator = q.times(bigj).plus(r)
        val denominator = s.times(bigj).plus(t)
        return numerator.div(denominator).toInt()
    }

    fun qrst(q: Int, r: Int, s: Int, t: Int): Transformation {
        this.q = q.bi
        this.r = r.bi
        this.s = s.bi
        this.t = t.bi
        k = 0
        return this
    }

    fun compose(a: Transformation): Transformation {
        return Transformation(
                q.times(a.q),
                q.times(a.r).plus(r.times(a.t)),
                s.times(a.q).plus(t.times(a.s)),
                s.times(a.r).plus(t.times(a.t))
        )
    }
}
