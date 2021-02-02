/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Translated from Christoph Bauer's nbody.c by Ian Martins
 */
typedef Planet = {
	var x:Float;
	var y:Float;
	var z:Float;
	var vx:Float;
	var vy:Float;
	var vz:Float;
	var mass:Float;
}

class App {
	private static var SOLAR_MASS = 4 * Math.PI * Math.PI;
	private static var DAYS_PER_YEAR = 365.24;
	private static var DT = 1e-2;
	private static var RECIP_DT = (1.0 / DT);

	private static function advance(bodies:Array<Planet>) {
		for (i in 0...bodies.length) {
			var b = bodies[i];
			for (j in (i + 1)...bodies.length) {
				var b2 = bodies[j];
				var dx = b.x - b2.x;
				var dy = b.y - b2.y;
				var dz = b.z - b2.z;
				var invDist = 1.0 / Math.sqrt(dx * dx + dy * dy + dz * dz);
				var mag = invDist * invDist * invDist;
				b.vx -= dx * b2.mass * mag;
				b.vy -= dy * b2.mass * mag;
				b.vz -= dz * b2.mass * mag;
				b2.vx += dx * b.mass * mag;
				b2.vy += dy * b.mass * mag;
				b2.vz += dz * b.mass * mag;
			}
		}
		for (b in bodies) {
			b.x += b.vx;
			b.y += b.vy;
			b.z += b.vz;
		}
	}

	private static function energy(bodies:Array<Planet>) {
		var e = 0.0;
		for (i in 0...bodies.length) {
			var b = bodies[i];
			e += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz);
			for (j in (i + 1)...bodies.length) {
				var b2 = bodies[j];
				var dx = b.x - b2.x;
				var dy = b.y - b2.y;
				var dz = b.z - b2.z;
				var distance = Math.sqrt(dx * dx + dy * dy + dz * dz);
				e -= (b.mass * b2.mass) / distance;
			}
		}
		return e;
	}

	private static function offsetMomentum(bodies:Array<Planet>) {
		var px = 0.0, py = 0.0, pz = 0.0;
		for (b in bodies) {
			px += b.vx * b.mass;
			py += b.vy * b.mass;
			pz += b.vz * b.mass;
		}
		bodies[0].vx = -px / SOLAR_MASS;
		bodies[0].vy = -py / SOLAR_MASS;
		bodies[0].vz = -pz / SOLAR_MASS;
	}

	/*
	 * Rescale certain properties of bodies. That allows doing
	 * consequential advance()'s as if dt were equal to 1.0.
	 *
	 * When all advances done, rescale bodies back to obtain correct energy.
	 */
	private static function scaleBodies(bodies:Array<Planet>, scale:Float) {
		for (b in bodies) {
			b.mass *= scale * scale;
			b.vx *= scale;
			b.vy *= scale;
			b.vz *= scale;
		}
	}

	inline private static function round(val:Float) {
		return Math.round(val * 1e9) / 1e9;
	}

	public static function main() {
		var bodies = new Array<Planet>();
		bodies.push({
			x: 0, // sun
			y: 0,
			z: 0,
			vx: 0,
			vy: 0,
			vz: 0,
			mass: SOLAR_MASS
		});
		bodies.push({
			x: 4.84143144246472090e+00, // jupiter
			y: -1.16032004402742839e+00,
			z: -1.03622044471123109e-01,
			vx: 1.66007664274403694e-03 * DAYS_PER_YEAR,
			vy: 7.69901118419740425e-03 * DAYS_PER_YEAR,
			vz: -6.90460016972063023e-05 * DAYS_PER_YEAR,
			mass: 9.54791938424326609e-04 * SOLAR_MASS
		});
		bodies.push({
			x: 8.34336671824457987e+00, // saturn
			y: 4.12479856412430479e+00,
			z: -4.03523417114321381e-01,
			vx: -2.76742510726862411e-03 * DAYS_PER_YEAR,
			vy: 4.99852801234917238e-03 * DAYS_PER_YEAR,
			vz: 2.30417297573763929e-05 * DAYS_PER_YEAR,
			mass: 2.85885980666130812e-04 * SOLAR_MASS
		});
		bodies.push({
			x: 1.28943695621391310e+01, // uranus
			y: -1.51111514016986312e+01,
			z: -2.23307578892655734e-01,
			vx: 2.96460137564761618e-03 * DAYS_PER_YEAR,
			vy: 2.37847173959480950e-03 * DAYS_PER_YEAR,
			vz: -2.96589568540237556e-05 * DAYS_PER_YEAR,
			mass: 4.36624404335156298e-05 * SOLAR_MASS
		});
		bodies.push({
			x: 1.53796971148509165e+01, // neptune
			y: -2.59193146099879641e+01,
			z: 1.79258772950371181e-01,
			vx: 2.68067772490389322e-03 * DAYS_PER_YEAR,
			vy: 1.62824170038242295e-03 * DAYS_PER_YEAR,
			vz: -9.51592254519715870e-05 * DAYS_PER_YEAR,
			mass: 5.15138902046611451e-05 * SOLAR_MASS
		});

		var n = Std.parseInt(Sys.args()[0]);
		offsetMomentum(bodies);
		Sys.println(round(energy(bodies)));
		scaleBodies(bodies, DT);
		for (i in 0...n)
			advance(bodies);
		scaleBodies(bodies, RECIP_DT);
		Sys.println(round(energy(bodies)));
	}
}
