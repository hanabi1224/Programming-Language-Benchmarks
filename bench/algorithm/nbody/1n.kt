/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Based on Java n-body by Mark C. Lewis, Chad Whipkey
   Feldbinder, and Tagir Valeev

   Contributed by Hank Grabowski
*/
import kotlin.math.*

fun main(args: Array<String>) {
    val n = if (args.size > 0) args[0].toInt() else 1000

    val bodies = NBodySystem()
    println(bodies.energy().toString(9))
    for (i in 0..n - 1) bodies.advance(0.01)
    println(bodies.energy().toString(9))
}

fun Double.toString(numOfDec: Int): String {
    var integerDigits = this.toInt()
    var floats = this - integerDigits
    var needReverseSign = floats < 0
    if (needReverseSign && integerDigits < 0) {
        integerDigits -= 1
        floats += 1
        needReverseSign = false
    }

    if (needReverseSign) {
        floats = -floats
    }

    val floatDigits = (floats * 10f.pow(numOfDec)).roundToInt()
    var result = "${integerDigits}.${floatDigits}"
    if (needReverseSign) {
        result = "-${result}"
    }

    return result
}

internal class NBodySystem {

    private val bodies: Array<Body>

    init {
        bodies = arrayOf(Body.sun(), Body.jupiter(), Body.saturn(), Body.uranus(), Body.neptune())

        var px = 0.0
        var py = 0.0
        var pz = 0.0
        for (i in 0..LENGTH - 1) {
            px += bodies[i].vx * bodies[i].mass
            py += bodies[i].vy * bodies[i].mass
            pz += bodies[i].vz * bodies[i].mass
        }
        bodies[0].offsetMomentum(px, py, pz)
    }

    fun advance(dt: Double) {
        val b = bodies
        for (i in 0..LENGTH - 1) {
            val iBody = b[i]
            val iMass = iBody.mass
            val ix = iBody.x
            val iy = iBody.y
            val iz = iBody.z
            var vx = iBody.vx
            var vy = iBody.vy
            var vz = iBody.vz

            for (j in i + 1..LENGTH - 1) {
                val jBody = b[j]
                val dx = ix - jBody.x
                val dy = iy - jBody.y
                val dz = iz - jBody.z

                val dSquared = dx * dx + dy * dy + dz * dz
                val distance = kotlin.math.sqrt(dSquared)
                val mag = dt / (dSquared * distance)

                val jMass = jBody.mass
                val mjMag = jMass * mag

                vx -= dx * mjMag
                vy -= dy * mjMag
                vz -= dz * mjMag

                val miMag = iMass * mag
                jBody.vx += dx * miMag
                jBody.vy += dy * miMag
                jBody.vz += dz * miMag
            }

            iBody.vx = vx
            iBody.vy = vy
            iBody.vz = vz

            iBody.x += dt * vx
            iBody.y += dt * vy
            iBody.z += dt * vz
        }
    }

    fun energy(): Double {
        var dx: Double
        var dy: Double
        var dz: Double
        var distance: Double
        var e = 0.0

        for (i in bodies.indices) {
            val iBody = bodies[i]
            e +=
                    0.5 *
                            iBody.mass *
                            (iBody.vx * iBody.vx + iBody.vy * iBody.vy + iBody.vz * iBody.vz)

            for (j in i + 1..bodies.size - 1) {
                val jBody = bodies[j]
                dx = iBody.x - jBody.x
                dy = iBody.y - jBody.y
                dz = iBody.z - jBody.z

                distance = kotlin.math.sqrt(dx * dx + dy * dy + dz * dz)
                e -= iBody.mass * jBody.mass / distance
            }
        }
        return e
    }

    companion object {
        private val LENGTH = 5
    }
}

internal class Body {

    var x: Double = 0.toDouble()
    var y: Double = 0.toDouble()
    var z: Double = 0.toDouble()
    var vx: Double = 0.toDouble()
    var vy: Double = 0.toDouble()
    var vz: Double = 0.toDouble()
    var mass: Double = 0.toDouble()

    fun offsetMomentum(px: Double, py: Double, pz: Double): Body {
        vx = -px / SOLAR_MASS
        vy = -py / SOLAR_MASS
        vz = -pz / SOLAR_MASS
        return this
    }

    companion object {
        val PI = 3.141592653589793
        val SOLAR_MASS = 4.0 * PI * PI
        val DAYS_PER_YEAR = 365.24

        fun jupiter(): Body {
            val p = Body()
            p.x = 4.84143144246472090e+00
            p.y = -1.16032004402742839e+00
            p.z = -1.03622044471123109e-01
            p.vx = 1.66007664274403694e-03 * DAYS_PER_YEAR
            p.vy = 7.69901118419740425e-03 * DAYS_PER_YEAR
            p.vz = -6.90460016972063023e-05 * DAYS_PER_YEAR
            p.mass = 9.54791938424326609e-04 * SOLAR_MASS
            return p
        }

        fun saturn(): Body {
            val p = Body()
            p.x = 8.34336671824457987e+00
            p.y = 4.12479856412430479e+00
            p.z = -4.03523417114321381e-01
            p.vx = -2.76742510726862411e-03 * DAYS_PER_YEAR
            p.vy = 4.99852801234917238e-03 * DAYS_PER_YEAR
            p.vz = 2.30417297573763929e-05 * DAYS_PER_YEAR
            p.mass = 2.85885980666130812e-04 * SOLAR_MASS
            return p
        }

        fun uranus(): Body {
            val p = Body()
            p.x = 1.28943695621391310e+01
            p.y = -1.51111514016986312e+01
            p.z = -2.23307578892655734e-01
            p.vx = 2.96460137564761618e-03 * DAYS_PER_YEAR
            p.vy = 2.37847173959480950e-03 * DAYS_PER_YEAR
            p.vz = -2.96589568540237556e-05 * DAYS_PER_YEAR
            p.mass = 4.36624404335156298e-05 * SOLAR_MASS
            return p
        }

        fun neptune(): Body {
            val p = Body()
            p.x = 1.53796971148509165e+01
            p.y = -2.59193146099879641e+01
            p.z = 1.79258772950371181e-01
            p.vx = 2.68067772490389322e-03 * DAYS_PER_YEAR
            p.vy = 1.62824170038242295e-03 * DAYS_PER_YEAR
            p.vz = -9.51592254519715870e-05 * DAYS_PER_YEAR
            p.mass = 5.15138902046611451e-05 * SOLAR_MASS
            return p
        }

        fun sun(): Body {
            val p = Body()
            p.mass = SOLAR_MASS
            return p
        }
    }
}
