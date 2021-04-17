/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 
   Based on Java version (1) by Isaac Gouy
   
   Contributed by Hank Grabowski
   Modified by hanabi1224
*/

import com.ionspin.kotlin.bignum.integer.*

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
    var q: BigInteger
    var r: BigInteger
    var s: BigInteger
    var t: BigInteger
    var k: Int = 0

    constructor(q: Int, r: Int, s: Int, t: Int) {
        this.q = q.toBigInteger()
        this.r = r.toBigInteger()
        this.s = s.toBigInteger()
        this.t = t.toBigInteger()
        k = 0
    }

    constructor(q: BigInteger, r: BigInteger, s: BigInteger, t: BigInteger) {
        this.q = q
        this.r = r
        this.s = s
        this.t = t
        k = 0
    }

    operator fun next(): Transformation {
        k++
        q = k.toBigInteger()
        r = (4 * k + 2).toBigInteger()
        s = 0.toBigInteger()
        t = (2 * k + 1).toBigInteger()
        return this
    }

    fun extract(j: Int): Int {
        val bigj = j.toBigInteger()
        val numerator = q.multiply(bigj).add(r)
        val denominator = s.multiply(bigj).add(t)
        return numerator.divide(denominator).intValue()
    }

    fun qrst(q: Int, r: Int, s: Int, t: Int): Transformation {
        this.q = q.toBigInteger()
        this.r = r.toBigInteger()
        this.s = s.toBigInteger()
        this.t = t.toBigInteger()
        k = 0
        return this
    }

    fun compose(a: Transformation): Transformation {
        return Transformation(
                q.multiply(a.q), q.multiply(a.r).add(r.multiply(a.t)), s.multiply(a.q).add(t.multiply(a.s)), s.multiply(a.r).add(t.multiply(a.t))
        )
    }
}
