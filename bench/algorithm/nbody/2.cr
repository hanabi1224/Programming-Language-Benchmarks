# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Optimized for Ruby by Jesse Millikan
# From version ported by Michael Neumann from the C gcc version,
# which was written by Christoph Bauer.
# ported from ruby to crystal by hanabi1224
# use StaticArray

alias F64x3 = Float64[3]

SOLAR_MASS    = 4_f64 * Math::PI**2
DAYS_PER_YEAR = 365.24_f64

def sum(a : F64x3) : Float64
  return a.reduce { |s, v| s + v }
end

def add(a : F64x3, b : F64x3) : F64x3
  return F64x3.new { |i| a.unsafe_fetch(i) + b.unsafe_fetch(i) }
end

def minus(a : F64x3, b : F64x3) : F64x3
  return F64x3.new { |i| a.unsafe_fetch(i) - b.unsafe_fetch(i) }
end

def mul(a : F64x3, b : F64x3) : F64x3
  return F64x3.new { |i| a.unsafe_fetch(i) * b.unsafe_fetch(i) }
end

def div(a : F64x3, b : F64x3) : F64x3
  return F64x3.new { |i| a.unsafe_fetch(i) / b.unsafe_fetch(i) }
end

class Planet
  property pos : F64x3
  property velocity : F64x3
  property mass

  def initialize(x : Float64, y : Float64, z : Float64, vx : Float64, vy : Float64, vz : Float64, mass : Float64)
    @pos = F64x3[x, y, z]
    @velocity = F64x3[vx * DAYS_PER_YEAR, vy * DAYS_PER_YEAR, vz * DAYS_PER_YEAR]
    @mass = Float64.new(mass * SOLAR_MASS)
  end

  def move_from_i(bodies, nbodies, dt, i)
    while i < nbodies
      b2 = bodies[i]
      d = minus(@pos, b2.pos)
      distance_square = sum(mul(d, d))
      distance = Math.sqrt(distance_square)
      mag = dt / (distance_square * distance)
      b_mass_mag, b2_mass_mag = @mass * mag, b2.mass * mag
      @velocity = minus(@velocity, d.map { |v| v * b2_mass_mag })
      b2.velocity = add(b2.velocity, d.map { |v| v * b_mass_mag })
      i += 1
    end
    @pos = add(@pos, @velocity.map { |v| v * dt })
  end
end

def energy(bodies)
  e = 0.0
  nbodies = bodies.size

  (0...nbodies).each do |i|
    b = bodies[i]
    e += 0.5 * b.mass * sum(mul(b.velocity, b.velocity))
    ((i + 1)...nbodies).each do |j|
      b2 = bodies[j]
      d = minus(b.pos, b2.pos)
      distance = Math.sqrt(sum(mul(d, d)))
      e -= (b.mass * b2.mass) / distance
    end
  end
  e
end

def offset_momentum(bodies)
  p = F64x3.new 0

  bodies.each do |b|
    m = b.mass
    p = minus(p, b.velocity.map { |v| v*m })
  end

  b = bodies[0]
  b.velocity = p.map { |v| v/SOLAR_MASS }
end

BODIES = [
  # sun
  Planet.new(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),

  # jupiter
  Planet.new(
    4.84143144246472090e+00,
    -1.16032004402742839e+00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03,
    7.69901118419740425e-03,
    -6.90460016972063023e-05,
    9.54791938424326609e-04),

  # saturn
  Planet.new(
    8.34336671824457987e+00,
    4.12479856412430479e+00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03,
    4.99852801234917238e-03,
    2.30417297573763929e-05,
    2.85885980666130812e-04),

  # uranus
  Planet.new(
    1.28943695621391310e+01,
    -1.51111514016986312e+01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03,
    2.37847173959480950e-03,
    -2.96589568540237556e-05,
    4.36624404335156298e-05),

  # neptune
  Planet.new(
    1.53796971148509165e+01,
    -2.59193146099879641e+01,
    1.79258772950371181e-01,
    2.68067772490389322e-03,
    1.62824170038242295e-03,
    -9.51592254519715870e-05,
    5.15138902046611451e-05),
]

n = ARGV.size > 0 ? ARGV[0].to_i : 1000

offset_momentum(BODIES)

puts "%.9f" % energy(BODIES)

nbodies = BODIES.size
dt = 0.01

n.times do
  i = 0
  while i < nbodies
    b = BODIES[i]
    b.move_from_i(BODIES, nbodies, dt, i + 1)
    i += 1
  end
end

puts "%.9f" % energy(BODIES)
