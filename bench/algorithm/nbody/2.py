import sys
import math


class Body(object):
    def __init__(self, p, v, m):
        (self.x, self.y, self.z) = p
        (self.vx, self.vy, self.vz) = v
        self.m = m


PI = 3.14159265358979323
SOLAR_MASS = 4 * PI * PI
DAYS_PER_YEAR = 365.24
BODIES = {
    'sun': Body([0.0, 0.0, 0.0], [0.0, 0.0, 0.0], SOLAR_MASS),

    'jupiter': Body([4.84143144246472090e+00,
                     -1.16032004402742839e+00,
                     -1.03622044471123109e-01],
                    [1.66007664274403694e-03 * DAYS_PER_YEAR,
                     7.69901118419740425e-03 * DAYS_PER_YEAR,
                     -6.90460016972063023e-05 * DAYS_PER_YEAR],
                    9.54791938424326609e-04 * SOLAR_MASS),

    'saturn': Body([8.34336671824457987e+00,
                    4.12479856412430479e+00,
                    -4.03523417114321381e-01],
                   [-2.76742510726862411e-03 * DAYS_PER_YEAR,
                    4.99852801234917238e-03 * DAYS_PER_YEAR,
                    2.30417297573763929e-05 * DAYS_PER_YEAR],
                   2.85885980666130812e-04 * SOLAR_MASS),

    'uranus': Body([1.28943695621391310e+01,
                    -1.51111514016986312e+01,
                    -2.23307578892655734e-01],
                   [2.96460137564761618e-03 * DAYS_PER_YEAR,
                    2.37847173959480950e-03 * DAYS_PER_YEAR,
                    -2.96589568540237556e-05 * DAYS_PER_YEAR],
                   4.36624404335156298e-05 * SOLAR_MASS),

    'neptune': Body([1.53796971148509165e+01,
                     -2.59193146099879641e+01,
                     1.79258772950371181e-01],
                    [2.68067772490389322e-03 * DAYS_PER_YEAR,
                     1.62824170038242295e-03 * DAYS_PER_YEAR,
                     -9.51592254519715870e-05 * DAYS_PER_YEAR],
                    5.15138902046611451e-05 * SOLAR_MASS)}

SYSTEM = list(BODIES.values())
N_BODIES = len(SYSTEM)


def advance(dt, bodies=SYSTEM):
    for i in range(N_BODIES):
        b1 = bodies[i]
        for j in range(i+1, N_BODIES):
            b2 = bodies[j]
            dx = b1.x - b2.x
            dy = b1.y - b2.y
            dz = b1.z - b2.z

            d_squared = dx*dx + dy*dy + dz*dz
            distance = math.sqrt(d_squared)

            mag = dt / (d_squared * distance)

            m2_multi_mag = b2.m * mag
            b1.vx -= dx * m2_multi_mag
            b1.vy -= dy * m2_multi_mag
            b1.vz -= dz * m2_multi_mag

            m1_multi_mag = b1.m * mag
            b2.vx += dx * m1_multi_mag
            b2.vy += dy * m1_multi_mag
            b2.vz += dz * m1_multi_mag
        b1.x += dt * b1.vx
        b1.y += dt * b1.vy
        b1.z += dt * b1.vz


def report_energy(bodies=SYSTEM, e=0.0):
    for i in range(N_BODIES):
        b1 = bodies[i]
        e += 0.5 * b1.m * (b1.vx * b1.vx + b1.vy * b1.vy + b1.vz * b1.vz)
        for j in range(i + 1, N_BODIES):
            b2 = bodies[j]
            dx = b1.x - b2.x
            dy = b1.y - b2.y
            dz = b1.z - b2.z
            distance = math.sqrt(dx*dx + dy*dy + dz*dz)
            e -= b1.m * b2.m / distance
    print(f"{e:.9f}")


def offset_momentum(ref, bodies=SYSTEM, px=0.0, py=0.0, pz=0.0):
    for b in bodies:
        px -= b.vx * b.m
        py -= b.vy * b.m
        pz -= b.vz * b.m
    ref.vx = px / SOLAR_MASS
    ref.vy = py / SOLAR_MASS
    ref.vz = pz / SOLAR_MASS


def main(n, ref='sun'):
    offset_momentum(BODIES[ref])
    report_energy()
    for i in range(n):
        advance(0.01)
    report_energy()


if __name__ == '__main__':
    main(int(sys.argv[1]))
