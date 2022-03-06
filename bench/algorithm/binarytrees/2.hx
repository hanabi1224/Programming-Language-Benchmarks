using App.Extensions;

typedef Node = {
	var left:Node;
	var right:Node;
}

function make(depth:Int):Node {
	if (depth < 1) {
		return {left: null, right: null,};
	}
	depth -= 1;
	return {left: make(depth), right: make(depth)};
}

class Extensions {
	static public inline function check(t:Node):Int {
		var sum = 1;
		if (t.left != null && t.right != null) {
			sum += t.left.check() + t.right.check();
		}
		return sum;
	}
}

class App {
	private static var MIN_DEPTH = 4;

	public static function main() {
		var args = Sys.args();
		var n = args.length > 0 ? Std.parseInt(args[0]) : 10;
		var maxDepth = MIN_DEPTH + 2 > n ? MIN_DEPTH + 2 : n;
		var stretchDepth = maxDepth + 1;
		var stretchTree = make(stretchDepth);
		Sys.println('stretch tree of depth $stretchDepth\t check: ${stretchTree.check()}');
		var longLivedTree = make(maxDepth);

		var depth = MIN_DEPTH;
		while (depth <= maxDepth) {
			var iteration = 1 << (maxDepth - depth + MIN_DEPTH);
			var sum = 0;
			for (i in 1...(iteration + 1)) {
				var tree = make(depth);
				sum += tree.check();
			}
			Sys.println('$iteration\t trees of depth $depth\t check: $sum');
			depth += 2;
		}

		Sys.println('long lived tree of depth $maxDepth\t check: ${longLivedTree.check()}');
	}
}
