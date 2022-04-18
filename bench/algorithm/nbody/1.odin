package main

import "core:fmt"
import "core:math"
import "core:runtime"
import "core:strconv"

PI :: 3.141592653589793
SOLAR_MASS :: 4 * PI * PI
DAYS_PER_YEAR :: 365.24

Vec3 :: [3]f64
Body :: struct { pos: Vec3, velocity: Vec3, mass: f64 }
Sys :: [5]Body

init_body :: proc(x, y, z, vx, vy, vz, mass : f64) -> Body {
    return Body { [3]f64 {x, y, z}, [3]f64 {vx, vy, vz}*DAYS_PER_YEAR, mass*SOLAR_MASS }
}

init_sys :: proc() -> Sys {
    return Sys {
        // Sun
        init_body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
        // Jupiter
        init_body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01, 
            1.66007664274403694e-03, 7.69901118419740425e-03, -6.90460016972063023e-05,
            9.54791938424326609e-04),
        // Saturn
        init_body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
            -2.76742510726862411e-03, 4.99852801234917238e-03, 2.30417297573763929e-05,
            2.85885980666130812e-04),
        // Uranus
        init_body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
            2.96460137564761618e-03, 2.37847173959480950e-03, -2.96589568540237556e-05,
            4.36624404335156298e-05),
        // Neptune
        init_body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
            2.68067772490389322e-03, 1.62824170038242295e-03, -9.51592254519715870e-05,
            5.15138902046611451e-05),
    }
}

advance :: proc(sys: ^Sys, dt: f64) {
    for b, i in sys {
        v := b.velocity
        for j in (i+1)..<5 {
            b2 := &sys[j]
            dpos := b.pos - b2.pos
            distance_square := sum(dpos * dpos)
            distance := math.sqrt_f64(distance_square)
            mag := dt / (distance * distance_square)
            v -= dpos * (mag * b2.mass)
            b2.velocity += dpos * (mag * b.mass)
        }
        b.velocity = v
        b.pos += v * dt
    }
}

offset_momentum :: proc(sys: ^Sys) {
    p := Vec3 {}
    for b, _ in sys {
        p -= b.velocity * b.mass
    }
    sys[0].velocity = p / SOLAR_MASS
}

sum :: proc (v: Vec3) -> f64 {
    return v[0] + v[1] + v[2]
}

energy :: proc(sys: ^Sys) -> f64 {
    e := 0.0
    for b, i in sys {
        e += 0.5 * b.mass * sum(b.velocity * b.velocity)
        for j in (i+1)..<5 {
            b2 := sys[j]
            dpos := b.pos - b2.pos
            distance_square := sum(dpos * dpos)
            distance := math.sqrt_f64(distance_square)
            e -= b.mass * b2.mass / distance
        }
    }
    return e
}

main :: proc() {
    args := runtime.args__
    n := strconv.parse_int(auto_cast args[1]) or_else 1000
    
    sys := init_sys()
    offset_momentum(&sys)
    fmt.printf("%.9f\n", energy(&sys))
    for _ in 0..<n {
        advance(&sys, 0.01)
    }
    fmt.printf("%.9f\n", energy(&sys))
}
