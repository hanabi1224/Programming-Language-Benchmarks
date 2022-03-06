class Node {
	var left:Node;
	var right:Node;

	public inline function new(left, right) {
		this.left = left;
		this.right = right;
	}

	public function check():Int {
		var sum = 1;
		if (left != null && right != null) {
			sum += left.check() + right.check();
		}
		return sum;
	}
}

function make(depth:Int):Node {
	if (depth < 1) {
		return new Node(null, null);
	}
	depth -= 1;
	return new Node(make(depth), make(depth));
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
