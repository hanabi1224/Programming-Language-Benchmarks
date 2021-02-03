/*
 * The Great Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Ian Martins
 * modified by hanabi1224
 */
class App {
	private static function eval_A(i:Int, j:Int) {
		return return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1);
	}

	private static function eval_A_times_u(N:Int, u:Array<Float>, Au:Array<Float>) {
		for (i in 0...N) {
			Au[i] = 0;
			for (j in 0...N)
				Au[i] += eval_A(i, j) * u[j];
		}
	}

	private static function eval_At_times_u(N:Int, u:Array<Float>, Au:Array<Float>) {
		for (i in 0...N) {
			Au[i] = 0;
			for (j in 0...N)
				Au[i] += eval_A(j, i) * u[j];
		}
	}

	private static function eval_AtA_times_u(N:Int, u:Array<Float>, AtAu:Array<Float>) {
		var v = new Array<Float>();
		eval_A_times_u(N, u, v);
		eval_At_times_u(N, v, AtAu);
	}

	public static function main() {
		var N = Std.parseInt(Sys.args()[0]);
		if (N == null)
			N = 2000;

		var u = new Array<Float>();
		var v = new Array<Float>();
		for (i in 0...N)
			u[i] = 1;
		for (i in 0...10) {
			eval_AtA_times_u(N, u, v);
			eval_AtA_times_u(N, v, u);
		}
		var vBv = 0.0;
		var vv = 0.0;
		for (i in 0...N) {
			vBv += u[i] * v[i];
			vv += v[i] * v[i];
		}
		Sys.println('${Std.string(Math.sqrt(vBv / vv)).substring(0, 11)}\n');
	}
}
