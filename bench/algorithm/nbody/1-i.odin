// contributed by Branimir Maksimovic

package main

import "core:fmt"
import "core:intrinsics"
import "core:math"
import "core:strconv"
import "core:os"

PI :: 3.141592653589793;
SOLAR_MASS :: 4 * PI * PI;
DAYS_PER_YEAR :: 365.24;

main :: proc(){
  initBodies()
  n := strconv.parse_int(os.args[1]) or_else 1000
  fmt.printf("%.9f\n", energy())
  for _ in 0..<n {
    advance(0.01)
  }
  fmt.printf("%.9f\n", energy())
}

Body:: struct {
    x, y, z, vx, vy, vz, mass: f64,
}
SIZE :: 5
bodies: #soa[SIZE]Body
initBodies::proc(){
  bodies[0]=sun()
  bodies[1]=jupiter()
  bodies[2]=saturn()
  bodies[3]=uranus()
  bodies[4]=neptune()
  px, py, pz:f64
  for value in bodies {
    px += value.vx * value.mass;
    py += value.vy * value.mass;
    pz += value.vz * value.mass;
  }
  sun := bodies[0]
  offsetMomentum(&sun,px,py,pz)
  bodies[0]=sun
}
advance::proc(dt:f64){
  N :: (SIZE-1)*SIZE/2
  dx,dy,dz,dts :#simd[2]f64
  dts = {dt,dt}
  Vector3 ::struct{dx,dy,dz:f64}
  rt:#soa[N]Vector3
  mag:[N]f64
  k:int
  for i in 0..<SIZE-1 {
    ibody := &bodies[i]
    for j in i+1..<SIZE {
      rt[k].dx = ibody.x - bodies[j].x
      rt[k].dy = ibody.y - bodies[j].y
      rt[k].dz = ibody.z - bodies[j].z
      k += 1
    }
  }
  for i:=0;i<N;i+=2{
      dx = intrinsics.unaligned_load(cast(^#simd[2]f64)&rt.dx[i])
      dy = intrinsics.unaligned_load(cast(^#simd[2]f64)&rt.dy[i])
      dz = intrinsics.unaligned_load(cast(^#simd[2]f64)&rt.dz[i])
      dsquared := dx*dx+dy*dy+dz*dz
      distance := intrinsics.sqrt(dsquared)
      dmag := dts/(dsquared*distance)
      intrinsics.unaligned_store(cast(^#simd[2]f64)&mag[i],dmag)
  }
  k = 0
  for i in 0..<SIZE-1 {
    ibody := &bodies[i]
    for j in i+1..<SIZE {
        ibody.vx -= rt[k].dx * bodies[j].mass * mag[k]
        ibody.vy -= rt[k].dy * bodies[j].mass * mag[k]
        ibody.vz -= rt[k].dz * bodies[j].mass * mag[k]

        bodies[j].vx += rt[k].dx * ibody.mass * mag[k]
        bodies[j].vy += rt[k].dy * ibody.mass * mag[k]
        bodies[j].vz += rt[k].dz * ibody.mass * mag[k]
        k += 1
    }
  }
  for i in 0..<SIZE {
    bodies[i].x += dt * bodies[i].vx
    bodies[i].y += dt * bodies[i].vy
    bodies[i].z += dt * bodies[i].vz
  }
}
energy::proc()->f64{
  e :f64
  for i in 0..<SIZE {
    ibody := bodies[i]
    dx,dy,dz,distance:f64
    e += 0.5 * ibody.mass * (ibody.vx*ibody.vx+ibody.vy*ibody.vy+ibody.vz*ibody.vz)
    for j in i+1..<SIZE {
      jbody := bodies[j]
      dx = ibody.x - jbody.x
      dy = ibody.y - jbody.y
      dz = ibody.z - jbody.z
      distance = math.sqrt(dx*dx+dy*dy+dz*dz)
      e -= (ibody.mass * jbody.mass) / distance
    }
  }
  return e
}

jupiter :: proc ()-> Body {
  return Body {
    x = 4.84143144246472090e+00,
    y = -1.16032004402742839e+00,
    z = -1.03622044471123109e-01,
    vx = 1.66007664274403694e-03 * DAYS_PER_YEAR,
    vy = 7.69901118419740425e-03 * DAYS_PER_YEAR,
    vz = -6.90460016972063023e-05 * DAYS_PER_YEAR,
    mass = 9.54791938424326609e-04 * SOLAR_MASS,
  }
}
saturn :: proc()-> Body {
  return Body{
    x = 8.34336671824457987e+00,
    y = 4.12479856412430479e+00,
    z = -4.03523417114321381e-01,
    vx = -2.76742510726862411e-03 * DAYS_PER_YEAR,
    vy = 4.99852801234917238e-03 * DAYS_PER_YEAR,
    vz = 2.30417297573763929e-05 * DAYS_PER_YEAR,
    mass = 2.85885980666130812e-04 * SOLAR_MASS,
  }
}
uranus :: proc()->Body {
  return Body{
    x = 1.28943695621391310e+01,
    y = -1.51111514016986312e+01,
    z = -2.23307578892655734e-01,
    vx = 2.96460137564761618e-03 * DAYS_PER_YEAR,
    vy = 2.37847173959480950e-03 * DAYS_PER_YEAR,
    vz = -2.96589568540237556e-05 * DAYS_PER_YEAR,
    mass = 4.36624404335156298e-05 * SOLAR_MASS,
  }
}
neptune :: proc()->Body{
  return Body{
    x = 1.53796971148509165e+01,
    y = -2.59193146099879641e+01,
    z = 1.79258772950371181e-01,
    vx = 2.68067772490389322e-03 * DAYS_PER_YEAR,
    vy = 1.62824170038242295e-03 * DAYS_PER_YEAR,
    vz = -9.51592254519715870e-05 * DAYS_PER_YEAR,
    mass = 5.15138902046611451e-05 * SOLAR_MASS,
  }
}
sun :: proc()->Body{
  return Body{
    mass = SOLAR_MASS,
  }
}
offsetMomentum::proc(self:^Body, px, py, pz:f64){
  self.vx = -px / SOLAR_MASS;
  self.vy = -py / SOLAR_MASS;
  self.vz = -pz / SOLAR_MASS;
}
