using App.Extensions;

class Vec3 {
	public var x:Float;
	public var y:Float;
	public var z:Float;

	public inline function new(x, y, z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}

	public function product(b:Vec3):Float {
		return x * b.x + y * b.y + z * b.z;
	}

	public function minus0(b:Vec3):Void {
		x -= b.x;
		y -= b.y;
		z -= b.z;
	}

	public function minus(b:Vec3):Vec3 {
		return new Vec3(x - b.x, y - b.y, z - b.z);
	}

	public inline function add0(b:Vec3):Void {
		x += b.x;
		y += b.y;
		z += b.z;
	}

	public inline function add(b:Vec3):Vec3 {
		return new Vec3(x + b.x, y + b.y, z + b.z);
	}

	public inline function add2(b:Float):Vec3 {
		return new Vec3(x + b, y + b, z + b);
	}

	public inline function mul2(b:Float):Vec3 {
		return new Vec3(x * b, y * b, z * b);
	}
}

class Planet {
	public var pos:Vec3;
	public var velocity:Vec3;
	public var mass:Float;

	public inline function new(x, y, z, vx, vy, vz, mass) {
		this.pos = new Vec3(x, y, z);
		this.velocity = new Vec3(vx, vy, vz);
		this.mass = mass;
	}
}

class Extensions {
	inline public static function round9(val:Float) {
		return Math.round(val * 1e9) / 1e9;
	}
}

class App {
	private static var SOLAR_MASS = 4 * Math.PI * Math.PI;
	private static var DAYS_PER_YEAR = 365.24;
	private static var DT = 1e-2;
	private static var RECIP_DT = (1.0 / DT);

	private static function advance(bodies:Array<Planet>, dt:Float) {
		for (i in 0...bodies.length) {
			var b = bodies[i];
			var v = b.velocity;
			for (j in (i + 1)...bodies.length) {
				var b2 = bodies[j];
				var dpos = b.pos.minus(b2.pos);
				var distanceSquare = dpos.product(dpos);
				var distance = Math.sqrt(distanceSquare);
				var mag = dt / (distanceSquare * distance);
				v.minus0(dpos.mul2(b2.mass * mag));
				b2.velocity.add0(dpos.mul2(b.mass * mag));
			}
			b.velocity = v;
			b.pos.add0(v.mul2(dt));
		}
	}

	private static function energy(bodies:Array<Planet>) {
		var e = 0.0;
		for (i in 0...bodies.length) {
			var b = bodies[i];
			e += 0.5 * b.mass * b.velocity.product(b.velocity);
			for (j in (i + 1)...bodies.length) {
				var b2 = bodies[j];
				var dpos = b.pos.minus(b2.pos);
				e -= (b.mass * b2.mass) / Math.sqrt(dpos.product(dpos));
			}
		}
		return e;
	}

	private static function offsetMomentum(bodies:Array<Planet>) {
		var pos:Vec3 = new Vec3(0, 0, 0);
		for (b in bodies) {
			pos.minus0(b.velocity.mul2(b.mass));
		}
		var sol = bodies[0];
		sol.velocity = pos.mul2(1.0 / SOLAR_MASS);
	}

	public static function main() {
		var bodies = new Array<Planet>();
		// sun
		bodies.push(new Planet(0, 0, 0, 0, 0, 0, SOLAR_MASS));
		// jupiter
		bodies.push(new Planet(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01, 1.66007664274403694e-03 * DAYS_PER_YEAR,
			7.69901118419740425e-03 * DAYS_PER_YEAR, -6.90460016972063023e-05 * DAYS_PER_YEAR, 9.54791938424326609e-04 * SOLAR_MASS));
		// saturn
		bodies.push(new Planet(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01, -2.76742510726862411e-03 * DAYS_PER_YEAR,
			4.99852801234917238e-03 * DAYS_PER_YEAR, 2.30417297573763929e-05 * DAYS_PER_YEAR, 2.85885980666130812e-04 * SOLAR_MASS));
		// uranus
		bodies.push(new Planet(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01, 2.96460137564761618e-03 * DAYS_PER_YEAR,
			2.37847173959480950e-03 * DAYS_PER_YEAR, -2.96589568540237556e-05 * DAYS_PER_YEAR, 4.36624404335156298e-05 * SOLAR_MASS));
		// neptune
		bodies.push(new Planet(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01, 2.68067772490389322e-03 * DAYS_PER_YEAR,
			1.62824170038242295e-03 * DAYS_PER_YEAR, -9.51592254519715870e-05 * DAYS_PER_YEAR, 5.15138902046611451e-05 * SOLAR_MASS));
		var args = Sys.args();
		var n = args.length > 0 ? Std.parseInt(args[0]) : 1000;
		offsetMomentum(bodies);
		Sys.println(energy(bodies).round9());
		for (i in 0...n) {
			advance(bodies, DT);
		}
		Sys.println(energy(bodies).round9());
	}
}
