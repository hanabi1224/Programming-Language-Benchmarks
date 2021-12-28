// Ported from 1.ts

import java.math.BigInteger

val ZERO = BigInteger.ZERO
val ONE = BigInteger.ONE
val TWO = BigInteger.TWO
val THREE = BigInteger("3")
val EIGHT = BigInteger("8")
val P = BigInteger("115792089237316195423570985008687907853269984665640564039457584007908834671663")
val N = BigInteger("115792089237316195423570985008687907852837564279074904382605163141518161494337")
val GX = BigInteger("55066263022277343669578718895168534326250603453777594175500187360389116729240")
val GY = BigInteger("32670510020758816978083085130507043184471273380659243275938904335757337482424")
val BETA =
        BigInteger("55594575648329892869085402983802832744385952214688224221778511981742606582254")
val POW_2_128 = BigInteger("340282366920938463463374607431768211456")
val A1 = BigInteger("64502973549206556628585045361533709077")
val B1 = BigInteger("-303414439467246543595250775667605759171")
val A2 = BigInteger("367917413016453100223835821029139468248")
val PRIVATE_KEY =
        BigInteger("20775598474904240222758871485654738649026525153462921990999819694398496339603")

fun main(args: Array<String>) {
    val n = if (args.size > 0) args[0].toInt() else 1
    var point = Point.BASE_P
    repeat(n) { point = point.multiply(PRIVATE_KEY) }
    println("${point.x.toString(16)},${point.y.toString(16)}")
}

fun mod(a: BigInteger, b: BigInteger = P): BigInteger {
    val r = a % b
    return if (r < BigInteger.ZERO) r + b else r
}

fun invert(number: BigInteger, modulo: BigInteger = P): BigInteger {
    var a = mod(number, modulo)
    var b = modulo
    var (x, y, u, v) = listOf(ZERO, ONE, ONE, ZERO)
    while (a != ZERO) {
        val q = b / a
        val r = b % a
        val m = x - u * q
        val n = y - v * q
        b = a
        a = r
        x = u
        y = v
        u = m
        v = n
    }
    return mod(x, modulo)
}

fun divNearest(a: BigInteger, b: BigInteger): BigInteger {
    return (a + b / TWO) / b
}

fun splitScalarEndo(k: BigInteger): Tuple {
    val b2 = A1
    val c1 = divNearest(b2 * k, N)
    val c2 = divNearest(-B1 * k, N)
    var k1 = mod(k - c1 * A1 - c2 * A2, N)
    var k2 = mod(-c1 * B1 - c2 * b2, N)
    val k1neg = k1 > POW_2_128
    val k2neg = k2 > POW_2_128
    if (k1neg) k1 = N - k1
    if (k2neg) k2 = N - k2
    return Tuple(k1neg, k1, k2neg, k2)
}

data class Tuple(val k1neg: Boolean, val k1: BigInteger, val k2neg: Boolean, val k2: BigInteger)

class JacobianPoint(val x: BigInteger, val y: BigInteger, val z: BigInteger) {
    public fun toAffine(): Point {
        val invZ = invert(z)
        val invZ2 = invZ.pow(2)
        val x = mod(x * invZ2)
        val y = mod(y * invZ * invZ2)
        return Point(x, y)
    }

    fun negate(): JacobianPoint {
        return JacobianPoint(x, mod(-y), z)
    }

    fun double(): JacobianPoint {
        val X1 = this.x
        val Y1 = this.y
        val Z1 = this.z
        val A = mod(X1.pow(2))
        val B = mod(Y1.pow(2))
        val C = mod(B.pow(2))
        val D = mod(TWO * (mod(mod((X1 + B).pow(2))) - A - C))
        val E = mod(THREE * A)
        val F = mod(E.pow(2))
        val X3 = mod(F - TWO * D)
        val Y3 = mod(E * (D - X3) - EIGHT * C)
        val Z3 = mod(TWO * Y1 * Z1)
        return JacobianPoint(X3, Y3, Z3)
    }

    fun add(other: JacobianPoint): JacobianPoint {
        val X1 = this.x
        val Y1 = this.y
        val Z1 = this.z
        val X2 = other.x
        val Y2 = other.y
        val Z2 = other.z
        if (X2 == ZERO || Y2 == ZERO) return this
        if (X1 == ZERO || Y1 == ZERO) return other
        val Z1Z1 = mod(Z1.pow(2))
        val Z2Z2 = mod(Z2.pow(2))
        val U1 = mod(X1 * Z2Z2)
        val U2 = mod(X2 * Z1Z1)
        val S1 = mod(Y1 * Z2 * Z2Z2)
        val S2 = mod(mod(Y2 * Z1) * Z1Z1)
        val H = mod(U2 - U1)
        val r = mod(S2 - S1)
        // H = 0 meaning it's the same point.
        if (H == ZERO) {
            if (r == ZERO) {
                return this.double()
            } else {
                return JacobianPoint.ZERO_J
            }
        }
        val HH = mod(H.pow(2))
        val HHH = mod(H * HH)
        val V = mod(U1 * HH)
        val X3 = mod(r.pow(2) - HHH - TWO * V)
        val Y3 = mod(r * (V - X3) - S1 * HHH)
        val Z3 = mod(Z1 * Z2 * H)
        return JacobianPoint(X3, Y3, Z3)
    }

    public fun multiplyUnsafe(n: BigInteger): JacobianPoint {
        var (k1neg, k1, k2neg, k2) = splitScalarEndo(n)
        var k1p = JacobianPoint.ZERO_J
        var k2p = JacobianPoint.ZERO_J
        var d = this
        while (k1 > ZERO || k2 > ZERO) {
            if (k1.testBit(0)) k1p = k1p.add(d)
            if (k2.testBit(0)) k2p = k2p.add(d)
            d = d.double()
            k1 = k1.shr(1)
            k2 = k2.shr(1)
        }
        if (k1neg) k1p = k1p.negate()
        if (k2neg) k2p = k2p.negate()
        k2p = JacobianPoint(mod(k2p.x * BETA), k2p.y, k2p.z)
        return k1p.add(k2p)
    }

    companion object {
        val ZERO_J = JacobianPoint(ZERO, ONE, ZERO)
        val BASE_J = JacobianPoint(GX, GY, ONE)
    }
}

class Point(val x: BigInteger, val y: BigInteger) {
    public fun toProjective(): JacobianPoint {
        return JacobianPoint(x, y, ONE)
    }

    public fun multiply(scalar: BigInteger): Point {
        return this.toProjective().multiplyUnsafe(scalar).toAffine()
    }

    companion object {
        val ZERO_P = Point(ZERO, ZERO)
        val BASE_P = Point(GX, GY)
    }
}
