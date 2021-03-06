# From https://github.com/mrordinaire/nim-language-benchmarks-game/blob/master/nbody.nim
#
# The Computer Language Benchmarks Game
#
#  http://benchmarksgame.alioth.debian.org/
#
# based on the Java version at
# http://benchmarksgame.alioth.debian.org/u64q/benchmark.php?test=nbody&lang=java
#
# contributed by Do Nhat Minh
# https://github.com/def-/nim-benchmarksgame/blob/master/nbody_2.nim

import math, os, strutils

type
  Body = tuple[x, y, z, vx, vy, vz, mass: float64]
  NBody = array[0..4, Body]

const SolarMass = 4 * PI * PI
const DaysPerYear = 365.24'f64

var
  jupiter = (
    x: 4.84143144246472090e+00,
    y: -1.16032004402742839e+00,
    z: -1.03622044471123109e-01,
    vx: 1.66007664274403694e-03 * DaysPerYear,
    vy: 7.69901118419740425e-03 * DaysPerYear,
    vz: -6.90460016972063023e-05 * DaysPerYear,
    mass: 9.54791938424326609e-04 * SolarMass
  )
  saturn = (
    x: 8.34336671824457987e+00,
    y: 4.12479856412430479e+00,
    z: -4.03523417114321381e-01,
    vx: -2.76742510726862411e-03 * DaysPerYear,
    vy: 4.99852801234917238e-03 * DaysPerYear,
    vz: 2.30417297573763929e-05 * DaysPerYear,
    mass: 2.85885980666130812e-04 * SolarMass
  )
  uranus = (
    x: 1.28943695621391310e+01,
    y: -1.51111514016986312e+01,
    z: -2.23307578892655734e-01,
    vx: 2.96460137564761618e-03 * DaysPerYear,
    vy: 2.37847173959480950e-03 * DaysPerYear,
    vz: -2.96589568540237556e-05 * DaysPerYear,
    mass: 4.36624404335156298e-05 * SolarMass
  )
  neptune = (
    x: 1.53796971148509165e+01,
    y: -2.59193146099879641e+01,
    z: 1.79258772950371181e-01,
    vx: 2.68067772490389322e-03 * DaysPerYear,
    vy: 1.62824170038242295e-03 * DaysPerYear,
    vz: -9.51592254519715870e-05 * DaysPerYear,
    mass: 5.15138902046611451e-05 * SolarMass
  )
  sun = (x: 0'f64, y: 0'f64, z: 0'f64,
         vx: 0'f64, vy: 0'f64, vz: 0'f64, mass: SolarMass)
  bodies = [sun, jupiter, saturn, uranus, neptune]

proc offsetMomentum(body: var Body; px, py, pz: float64) =
  body.vx = -px / SolarMass
  body.vy = -py / SolarMass
  body.vz = -pz / SolarMass

proc advance(bodies: var NBody, dt: float64) =
  for i in low(bodies)..high(bodies):
    for j in i+1..high(bodies):
      let
        iBody = bodies[i]
        jBody = bodies[j]
        dx = iBody.x - jBody.x
        dy = iBody.y - jBody.y
        dz = iBody.z - jBody.z
        dSquared = dx * dx + dy * dy + dz * dz
        distance = sqrt(dSquared)
        mag = dt / (dSquared * distance)

      bodies[i].vx -= dx * jBody.mass * mag
      bodies[i].vy -= dy * jBody.mass * mag
      bodies[i].vz -= dz * jBody.mass * mag

      bodies[j].vx += dx * iBody.mass * mag
      bodies[j].vy += dy * iBody.mass * mag
      bodies[j].vz += dz * iBody.mass * mag

  for i in low(bodies)..high(bodies):
    bodies[i].x += dt * bodies[i].vx
    bodies[i].y += dt * bodies[i].vy
    bodies[i].z += dt * bodies[i].vz

proc energy(bodies: NBody): float64 =
  for i in low(bodies)..high(bodies):
    let (ix, iy, iz, ivx, ivy, ivz, imass) = bodies[i]
    result += 0.5 * imass * (ivx * ivx + ivy * ivy + ivz * ivz)
    for j in i+1..high(bodies):
      let
        (jx, jy, jz, _, _, _, jmass) = bodies[j]
        dx = ix - jx
        dy = iy - jy
        dz = iz - jz
        distance = sqrt(dx*dx + dy*dy + dz*dz)
      result -= (imass * jmass) / distance

var
  px, py, pz = 0'f64
  n = parseInt(paramStr(1))

for i in 1..high(bodies):
  px += bodies[i].vx * bodies[i].mass
  py += bodies[i].vy * bodies[i].mass
  pz += bodies[i].vz * bodies[i].mass

bodies[0].offsetMomentum(px, py, pz)

echo formatFloat(bodies.energy, ffDecimal, 9)
for i in 1..n:
  bodies.advance(0.01)
echo formatFloat(bodies.energy, ffDecimal, 9)