/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Ralph Ganszky

   Added dummy element to tuple (because I read an article that this helps
   to block less writing back to memory) and store data in
   UnsafedMutablePointer to reduce overhead of array access.
   
   modified by Patrick Sy
*/

import Foundation

typealias Body = (
    r: (x: Double, y: Double, z: Double),
    v: (x: Double, y: Double, z: Double),
    m: Double, d: Double
)

let nPlanets: Int = 5
let solarMass: Double = 4 * Double.pi * Double.pi
let daysPerYear: Double = 365.24

let sun: Body = (
    r: (x: 0.0, y: 0.0, z: 0.0),
    v: (x: 0.0, y: 0.0, z: 0.0),
    m: solarMass, d: 0.0
)
let jupiter: Body = (
    r: (x: 4.8414314424647209,
        y: -1.16032004402742839,
        z: -0.103622044471123109),
    v: (x: 1.66007664274403694e-03 * daysPerYear,
        y: 7.69901118419740425e-03 * daysPerYear,
        z: -6.90460016972063023e-05 * daysPerYear),
    m: 9.54791938424326609e-04 * solarMass, d: 0.0
)
let saturn: Body = (
    r: (x: 8.34336671824457987,
       y: 4.12479856412430479,
       z: -4.03523417114321381e-01),
   v: (x: -2.76742510726862411e-03 * daysPerYear,
       y: 4.99852801234917238e-03 * daysPerYear,
       z: 2.30417297573763929e-05 * daysPerYear),
   m: 2.85885980666130812e-04 * solarMass, d: 0.0
)
let uranus: Body = (
    r: (x: 1.28943695621391310e+01,
       y: -1.51111514016986312e+01,
       z: -2.23307578892655734e-01),
   v: (x: 2.96460137564761618e-03 * daysPerYear,
       y: 2.37847173959480950e-03 * daysPerYear,
       z: -2.96589568540237556e-05 * daysPerYear),
   m: 4.36624404335156298e-05 * solarMass, d: 0.0
)
let neptune: Body = (
    r: (x: 1.53796971148509165e+01,
        y: -2.59193146099879641e+01,
        z: 1.79258772950371181e-01),
    v: (x: 2.68067772490389322e-03 * daysPerYear,
        y: 1.62824170038242295e-03 * daysPerYear,
        z: -9.51592254519715870e-05 * daysPerYear),
    m: 5.15138902046611451e-05 * solarMass, d: 0.0
)

let bodies: UnsafeMutablePointer<Body> = UnsafeMutablePointer<Body>.allocate(capacity: nPlanets)
defer {
    bodies.deallocate()
}
bodies[0] = sun
bodies[1] = jupiter
bodies[2] = saturn
bodies[3] = uranus
bodies[4] = neptune

func advance(_ bodies: UnsafeMutablePointer<Body>, n: Int, dt: Double) {
    for i in 0..<n {
        let iBody: Body = bodies[i]
        for j in i+1..<n {
            let jBody: Body = bodies[j]
            let dx: Double = iBody.r.x - jBody.r.x
            let dy: Double = iBody.r.y - jBody.r.y
            let dz: Double = iBody.r.z - jBody.r.z
            
            let dSquared: Double = dx*dx + dy*dy + dz*dz
            let mag: Double = dt / (dSquared * dSquared.squareRoot())
            // dSquared.squareRoot() == distance
            
            bodies[i].v = (bodies[i].v.x - dx * jBody.m * mag,
                           bodies[i].v.y - dy * jBody.m * mag,
                           bodies[i].v.z - dz * jBody.m * mag)
            bodies[j].v = (bodies[j].v.x + dx * iBody.m * mag,
                           bodies[j].v.y + dy * iBody.m * mag,
                           bodies[j].v.z + dz * iBody.m * mag)
        }
    }
    for i in 0..<n {
        bodies[i].r = (
            bodies[i].r.x + dt * bodies[i].v.x,
            bodies[i].r.y + dt * bodies[i].v.y,
            bodies[i].r.z + dt * bodies[i].v.z
        )
    }
}

func energy(_ bodies: UnsafeMutablePointer<Body>, n: Int) -> Double {
    var energy: Double = 0.0
    for i in 0..<n {
        let iBody: Body = bodies[i]
        energy += 0.5 * iBody.m * ( iBody.v.x * iBody.v.x +
                                    iBody.v.y * iBody.v.y +
                                    iBody.v.z * iBody.v.z)
        
        for j in i+1..<n {
            let jBody: Body = bodies[j]
            let dx: Double = iBody.r.x - jBody.r.x
            let dy: Double = iBody.r.y - jBody.r.y
            let dz: Double = iBody.r.z - jBody.r.z
            let distance: Double = (dx*dx + dy*dy + dz*dz).squareRoot()
            
            energy -= (iBody.m * jBody.m) / distance
        }
    }
    return energy
    
}

var n: Int = 1000
if CommandLine.argc > 1 {
    if let m: Int = Int(CommandLine.arguments[1]) {
        n = m
    }
}

// Adjust momentum of the sun
var p: (Double, Double, Double) = (0.0, 0.0, 0.0)

for i in 0..<nPlanets {
    p.0 += bodies[i].v.x * bodies[i].m
    p.1 += bodies[i].v.y * bodies[i].m
    p.2 += bodies[i].v.z * bodies[i].m
}
bodies[0].1 = (-p.0 / solarMass, -p.1 / solarMass, -p.2 / solarMass)

print(String(format: "%.9f", energy(bodies, n: nPlanets)))
for _ in 0..<n {
    advance(bodies, n: nPlanets, dt: 0.01)
}
print(String(format: "%.9f", energy(bodies, n: nPlanets)))