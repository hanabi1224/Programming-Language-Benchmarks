import "os" for Process

var DaysPerYear = 365.24
var SolarMass = 4 * Num.pi * Num.pi

class Body {
    construct new(x, y, z, vx, vy, vz, mass) {
        _x=x
        _y=y
        _z=z
        _vx=vx
        _vy=vy
        _vz=vz
        _mass=mass
    }
    construct new(mass) {
        _x=0
        _y=0
        _z=0
        _vx=0
        _vy=0
        _vz=0
        _mass=mass
    }
    x { _x }
    x=(v) { _x = v }
    y { _y }
    y=(v) { _y = v }
    z { _z }
    z=(v) { _z = v }
    vx { _vx }
    vx=(v) { _vx = v }
    vy { _vy }
    vy=(v) { _vy = v }
    vz { _vz }
    vz=(v) { _vz = v }
    mass { _mass }
    offsetMomentum(px, py, pz) {
        _vx = -px / SolarMass
        _vy = -py / SolarMass
	    _vz = -pz / SolarMass
    }
}
class NBodySystem {
    construct new(array) {
        _array = array
        var px = 0
        var py = 0
        var pz = 0
        for (b in _array) {
            px = px + b.vx * b.mass
            py = py + b.vy * b.mass
            pz = pz + b.vz * b.mass
        }
        _array[0].offsetMomentum(px, py, pz)
    }

    advance (dt) {
        for (i in 0..._array.count) {
            var body = _array[i]
            for (j in (i+1)..._array.count) {
                var body2 = _array[j]
                var dx = body.x - body2.x
                var dy = body.y - body2.y
                var dz = body.z - body2.z

                var dSquared = dx*dx+dy*dy+dz*dz
                var distance = dSquared.sqrt
                var mag = dt / (dSquared * distance)

                var factor = body2.mass * mag
                body.vx = body.vx - dx * factor
                body.vy = body.vy - dy * factor
                body.vz = body.vz - dz * factor

                factor = body.mass * mag
                body2.vx = body2.vx + dx * factor
                body2.vy = body2.vy + dy * factor
                body2.vz = body2.vz + dz * factor
            }
        }
        for (body in _array) {
            body.x = body.x + dt * body.vx
            body.y = body.y + dt * body.vy
            body.z = body.z + dt * body.vz
        }
    }

    energy {
        var e = 0
        for (i in 0..._array.count) {
            var body = _array[i]
            e = e + 0.5 * body.mass * (body.vx*body.vx+body.vy*body.vy+body.vz*body.vz)
            for (j in (i+1)..._array.count) {
                var body2 = _array[j]
                var dx = body.x - body2.x
                var dy = body.y - body2.y
                var dz = body.z - body2.z
                var distance = (dx*dx+dy*dy+dz*dz).sqrt
                e = e - (body.mass * body2.mass) / distance
            }
        }
        return e
    }
}

var numToString = Fn.new() { |n|
    var d = n.round
    var f = (n.fraction * 1e9).round.abs
    var fStr = f.toString
    var nPadding = 9 - fStr.count
    (0...nPadding).each {
        fStr = "0%(fStr)"
    }
    return "%(d).%(fStr)"
} 

var sun = Body.new(SolarMass)
var jupiter = Body.new(
    4.84143144246472090e00,
    -1.16032004402742839e00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03 * DaysPerYear,
    7.69901118419740425e-03 * DaysPerYear,
    -6.90460016972063023e-05 * DaysPerYear,
    9.54791938424326609e-04 * SolarMass
)
var saturn = Body.new(
    8.34336671824457987e00,
    4.12479856412430479e00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03 * DaysPerYear,
    4.99852801234917238e-03 * DaysPerYear,
    2.30417297573763929e-05 * DaysPerYear,
    2.85885980666130812e-04 * SolarMass
)
var uranus = Body.new(
    1.28943695621391310e01,
    -1.51111514016986312e01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03 * DaysPerYear,
    2.37847173959480950e-03 * DaysPerYear,
    -2.96589568540237556e-05 * DaysPerYear,
    4.36624404335156298e-05 * SolarMass
)
var neptune = Body.new(
    1.53796971148509165e01,
    -2.59193146099879641e01,
    1.79258772950371181e-01,
    2.68067772490389322e-03 * DaysPerYear,
    1.62824170038242295e-03 * DaysPerYear,
    -9.51592254519715870e-05 * DaysPerYear,
    5.15138902046611451e-05 * SolarMass
)
var solarSystem = NBodySystem.new([sun, jupiter, saturn, uranus, neptune])

var n = 10
if (Process.allArguments.count > 2) {
    n = Num.fromString(Process.allArguments[2])
}
System.print(numToString.call(solarSystem.energy))
(0...n).each {
    solarSystem.advance(0.01)
}
System.print(numToString.call(solarSystem.energy))
