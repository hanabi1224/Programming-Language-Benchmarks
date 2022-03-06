class Node {
	public var hash:Null<Int>;

	var value:Null<Int>;
	var left:Node;
	var right:Node;

	public inline function new(value, left, right) {
		this.value = value;
		this.left = left;
		this.right = right;
	}

	public function check():Bool {
		if (hash == null) {
			return false;
		} else if (value != null) {
			return true;
		} else {
			return left.check() && right.check();
		}
	}

	public function calHash():Void {
		if (hash == null) {
			if (value != null) {
				hash = value;
			} else {
				left.calHash();
				right.calHash();
				hash = left.hash + right.hash;
			}
		}
	}
}

function make(depth:Int):Node {
	if (depth < 1) {
		return new Node(1, null, null);
	}
	depth -= 1;
	return new Node(null, make(depth), make(depth));
}

class App {
	private static var MIN_DEPTH = 4;

	public static function main() {
		var args = Sys.args();
		var n = args.length > 0 ? Std.parseInt(args[0]) : 10;
		var maxDepth = MIN_DEPTH + 2 > n ? MIN_DEPTH + 2 : n;
		var stretchDepth = maxDepth + 1;
		var stretchTree = make(stretchDepth);
		stretchTree.calHash();
		Sys.println('stretch tree of depth $stretchDepth\t root hash: ${stretchTree.hash} check: ${stretchTree.check()}');
		var longLivedTree = make(maxDepth);

		var depth = MIN_DEPTH;
		while (depth <= maxDepth) {
			var iteration = 1 << (maxDepth - depth + MIN_DEPTH);
			var sum = 0;
			for (i in 1...(iteration + 1)) {
				var tree = make(depth);
				tree.calHash();
				sum += tree.hash;
			}
			Sys.println('$iteration\t trees of depth $depth\t root hash sum: $sum');
			depth += 2;
		}

		longLivedTree.calHash();
		Sys.println('long lived tree of depth $maxDepth\t root hash: ${longLivedTree.hash} check: ${longLivedTree.check()}');
	}
}
