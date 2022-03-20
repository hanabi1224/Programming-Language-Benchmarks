// Based on https://github.com/ponylang/ponyc/blob/main/examples/n-body/n-body.pony

use "format"

class IEEE754
  let sign: U8
  let biased_exponent: I16
  let fraction: I64
  new create(f: F64) =>
    let bits = f.bits()
    sign = bits.shr(63).u8()
    biased_exponent = bits.shr(52).op_and(0x7ff).i16()
    fraction = bits.op_and(0xfffffffffffff).i64()

  fun significand() : I64 =>
    fraction.op_or(0x10000000000000)

  fun exponent() : I16 =>
    biased_exponent - 1023

  fun format() : String =>
    // TODO: use string builder
    var result = if sign == 0 then "" else "-" end
    let sig = significand()
    let exp = exponent() - 52
    let offset = (-exp).u64()
    result = result + sig.shr(offset).string() + "."
    let mask:U64 = 1.shl(offset) - 1
    var n:U64 = sig.u64().op_and(mask)
    var i:USize = 0
    var fractions:U64 = 0
    while i < 10 do
      n = n * 10
      if i < 9 then
        fractions = (fractions * 10) + n.shr(offset)
      else
        if n.shr(offset) >= 5 then
          fractions = fractions + 1
        end 
      end
      n = n.op_and(mask)
      i = i + 1
    end
    // BUG: This does not handle the 0.999999999999999 case
    result = result + fractions.string()
    result

class Body
  var x: F64
  var y: F64
  var z: F64

  var vx: F64
  var vy: F64
  var vz: F64

  var mass: F64

  let solar_mass: F64 = F64.pi() * F64.pi() * 4
  let days_per_year: F64 = 365.24

  new sun() =>
    x = 0; y = 0; z = 0
    vx = 0; vy = 0; vz = 0
    mass = 1
    _init()

  new jupiter() =>
    x = 4.8414314424647209
    y = -F64(1.16032004402742839)
    z = -F64(1.03622044471123109e-1)

    vx = 1.66007664274403694e-3
    vy = 7.69901118419740425e-3
    vz = -F64(6.90460016972063023e-5)

    mass = 9.54791938424326609e-4
    _init()

  new saturn() =>
    x = 8.34336671824457987
    y = 4.12479856412430479
    z = -F64(4.03523417114321381e-1)

    vx = -F64(2.76742510726862411e-3)
    vy = 4.99852801234917238e-3
    vz = 2.30417297573763929e-5

    mass = 2.85885980666130812e-4
    _init()

  new uranus() =>
    x = 1.28943695621391310e1
    y = -F64(1.51111514016986312e1)
    z = -F64(2.23307578892655734e-1)

    vx = 2.96460137564761618e-3
    vy = 2.37847173959480950e-3
    vz = -F64(2.96589568540237556e-5)

    mass = 4.36624404335156298e-5
    _init()

  new neptune() =>
    x = 1.53796971148509165e1
    y = -F64(2.59193146099879641e1)
    z = 1.79258772950371181e-1

    vx = 2.68067772490389322e-3
    vy = 1.62824170038242295e-3
    vz = -F64(9.51592254519715870e-5)

    mass = 5.15138902046611451e-5
    _init()

  // Calculate attraction with another body.
  fun ref attract(that: Body, dt: F64) =>
    let dx = x - that.x
    let dy = y - that.y
    let dz = z - that.z
    let d2 = (dx * dx) + (dy * dy) + (dz * dz)
    let mag = dt / (d2 * d2.sqrt())

    let mj_mag = mag * that.mass
    vx = vx - (dx * mj_mag)
    vy = vy - (dy * mj_mag)
    vz = vz - (dz * mj_mag)

    let mi_mag = mag * mass
    that.vx = that.vx + (dx * mi_mag)
    that.vy = that.vy + (dy * mi_mag)
    that.vz = that.vz + (dz * mi_mag)

  // Integrate new position.
  fun ref integrate(dt: F64) =>
    x = x + (vx * dt)
    y = y + (vy * dt)
    z = z + (vz * dt)

  // Kinetic energy: 0.5 m v^2
  fun ke(): F64 => 0.5 * mass * ((vx * vx) + (vy * vy) + (vz * vz))

  // Potential energy: m0 m1 / d
  fun pe(that: Body box): F64 =>
    let dx = x - that.x
    let dy = y - that.y
    let dz = z - that.z
    let d = ((dx * dx) + (dy * dy) + (dz * dz)).sqrt()
    (mass * that.mass) / d

  fun ref _init() =>
    vx = vx * days_per_year
    vy = vy * days_per_year
    vz = vz * days_per_year
    mass = mass * solar_mass

actor Main
  let system: Array[Body] = Array[Body](5)
  let sun: Body = Body.sun()
  let _env: Env

  new create(env: Env) =>
    _env = env

    let n = try
      env.args(1)?.usize()?
    else
      1000
    end

    // Initial system
    system
      .>push(sun)
      .>push(Body.jupiter())
      .>push(Body.saturn())
      .>push(Body.uranus())
      .>push(Body.neptune())

    offset_momentum()
    print_energy()

    var i: USize = 0
    while i < n do
      advance(0.01)
      i = i + 1
    end
    print_energy()

  fun ref advance(dt: F64) =>
    let count = system.size()
    var i: USize = 0

    while i < count do
      try
        let body = system(i)?
        var j = i + 1

        while j < count do
          body.attract(system(j)?, dt)
          j = j + 1
        end
      end

      i = i + 1
    end

    try
      i = 0
      while i < system.size() do
        let body = system(i)?
        body.integrate(dt)
        i = i + 1
      end
    end

  fun ref print_energy() =>
    _env.out.print(IEEE754(energy()).format())
    // _env.out.print(energy().bits().shr(52).string())
    // _env.out.print(Format.float[F64](energy() where width = 10, align = AlignRight, fmt = FormatBinary))

  fun ref energy(): F64 =>
    let count = system.size()
    var e: F64 = 0
    var i: USize = 0
    while i < count do
      try
        let body = system(i)?
        e = e + body.ke()
        var j = i + 1
        while j < count do
          e = e - body.pe(system(j)?)
          j = j + 1
        end
      end
      i = i + 1
    end
    e

  fun ref offset_momentum() =>
    var px: F64 = 0
    var py: F64 = 0
    var pz: F64 = 0

    try
      var i: USize = 0
      while i < system.size() do
        var body = system(i)?
        px = px - (body.vx * body.mass)
        py = py - (body.vy * body.mass)
        pz = pz - (body.vz * body.mass)
        i = i + 1
      end
    end

    sun.vx = px / sun.mass
    sun.vy = py / sun.mass
    sun.vz = pz / sun.mass
