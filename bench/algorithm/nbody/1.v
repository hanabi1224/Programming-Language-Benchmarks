// Ported based on https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/nbody-go-3.html
import math
import strconv
import os

const (
	solar_mass    = 4.0 * math.pi * math.pi
	days_per_year = 365.24
	c_n           = 5
)

struct Body {
pub mut:
	x  f64
	y  f64
	z  f64
	vx f64
	vy f64
	vz f64
	m  f64
}

@[direct_array_access]
fn advance(sys []&Body, dt f64) {
	for i in 0 .. c_n {
		mut bi := sys[i]
		x := bi.x
		y := bi.y
		z := bi.z
		mut vx := bi.vx
		mut vy := bi.vy
		mut vz := bi.vz
		bi_m := bi.m
		for j in (i + 1) .. c_n {
			mut bj := sys[j]
			dx := x - bj.x
			dy := y - bj.y
			dz := z - bj.z

			dsquared := dx * dx + dy * dy + dz * dz
			distance := math.sqrt(dsquared)
			mag := (dt / (dsquared * distance))

			bj_m_mag := bj.m * mag
			vx -= dx * bj_m_mag
			vy -= dy * bj_m_mag
			vz -= dz * bj_m_mag

			bi_m_mag := bi_m * mag
			bj.vx += dx * bi_m_mag
			bj.vy += dy * bi_m_mag
			bj.vz += dz * bi_m_mag
		}
		bi.vx = vx
		bi.vy = vy
		bi.vz = vz

		bi.x += dt * vx
		bi.y += dt * vy
		bi.z += dt * vz
	}
}

@[direct_array_access]
fn offsetmomentum(sys []&Body) {
	mut px := f64(0)
	mut py := f64(0)
	mut pz := f64(0)
	for bi in sys {
		px -= bi.vx * bi.m
		py -= bi.vy * bi.m
		pz -= bi.vz * bi.m
	}
	mut sol := sys[0]
	sol.vx = px / solar_mass
	sol.vy = py / solar_mass
	sol.vz = pz / solar_mass
}

@[direct_array_access]
fn energy(sys []&Body) f64 {
	mut e := f64(0)
	for i := 0; i < c_n; i++ {
		bi := sys[i]
		e += 0.5 * bi.m * (bi.vx * bi.vx + bi.vy * bi.vy + bi.vz * bi.vz)
		for bj in sys[(i + 1)..] {
			dx := bi.x - bj.x
			dy := bi.y - bj.y
			dz := bi.z - bj.z
			distance := math.sqrt(dx * dx + dy * dy + dz * dz)
			e -= (bi.m * bj.m) / distance
		}
	}
	return e
}

fn arr_bodies() []&Body {
	return [
		&Body{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solar_mass},
		&Body{4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01, 1.66007664274403694e-03 * days_per_year, 7.69901118419740425e-03 * days_per_year, -6.90460016972063023e-05 * days_per_year, 9.54791938424326609e-04 * solar_mass},
		&Body{8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01, -2.76742510726862411e-03 * days_per_year, 4.99852801234917238e-03 * days_per_year, 2.30417297573763929e-05 * days_per_year, 2.85885980666130812e-04 * solar_mass},
		&Body{1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01, 2.96460137564761618e-03 * days_per_year, 2.37847173959480950e-03 * days_per_year, -2.96589568540237556e-05 * days_per_year, 4.36624404335156298e-05 * solar_mass},
		&Body{1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01, 2.68067772490389322e-03 * days_per_year, 1.62824170038242295e-03 * days_per_year, -9.51592254519715870e-05 * days_per_year, 5.15138902046611451e-05 * solar_mass},
	]
}

fn main() {
	mut n := 1000
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { 1000 }
	}

	bodies := arr_bodies()
	offsetmomentum(bodies)
	println('${energy(bodies):.9f}') //-0.169075164
	for _ in 0 .. n {
		advance(bodies, 0.01)
	}
	println('${energy(bodies):.9f}') //-0.169059907
}
