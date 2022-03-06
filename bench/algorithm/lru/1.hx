class LinkedListNode<T> {
	public var data:T;
	public var prev:LinkedListNode<T>;
	public var next:LinkedListNode<T>;

	public inline function new(data) {
		this.data = data;
	}
}

class LinkedList<T> {
	public var len:Int = 0;

	var head:LinkedListNode<T>;
	var tail:LinkedListNode<T>;

	public inline function new() {}

	public function add(data:T):LinkedListNode<T> {
		var node = new LinkedListNode(data);
		__add_node(node);
		len += 1;
		return node;
	}

	private function __add_node(node:LinkedListNode<T>):Void {
		if (head == null) {
			head = node;
		} else if (tail != null) {
			node.prev = tail;
			tail.next = node;
		}
		tail = node;
		node.next = null;
	}

	private function __remove(node:LinkedListNode<T>):Void {
		if (head == node) {
			head = node.next;
		}
		if (tail == node) {
			tail = node.prev;
		}
		if (node.prev != null) {
			node.prev.next = node.next;
		}
		if (node.next != null) {
			node.next.prev = node.prev;
		}
	}

	public function move_to_end(node:LinkedListNode<T>):Void {
		__remove(node);
		__add_node(node);
	}

	public function pop_head():LinkedListNode<T> {
		if (head != null) {
			var tmp = head;
			head = tmp.next;
			len -= 1;
			return tmp;
		} else {
			return null;
		}
	}
}

class LCG {
	static var A = 1103515245;
	static var C = 12345;
	static var M = 1 << 31;

	var seed:Int;

	public inline function new(seed:Int) {
		this.seed = seed;
	}

	public function next():Int {
		seed = (1103515245 * seed + 12345) % M;
		if (seed < 0) {
			seed += M;
		}
		return seed;
	}
}

typedef Pair = {k:Int, v:Int};

class LRU {
	var size:Int;
	var _key_lookup:Map<Int, LinkedListNode<Pair>> = new Map();
	var _entries:LinkedList<Pair> = new LinkedList();

	public inline function new(size) {
		this.size = size;
	}

	public function get(key:Int):Null<Int> {
		var node = _key_lookup.get(key);
		if (node == null) {
			return null;
		}
		_entries.move_to_end(node);
		return node.data.v;
	}

	public function put(key:Int, value:Int) {
		var node = _key_lookup.get(key);
		if (node != null) {
			node.data.v = value;
			_entries.move_to_end(node);
			return;
		} else if (_entries.len == size) {
			var head = _entries.pop_head();
			_key_lookup.remove(head.data.k);
		}
		_key_lookup.set(key, _entries.add({k: key, v: value}));
	}
}

class App {
	private static var MIN_DEPTH = 4;

	public static function main() {
		var args = Sys.args();
		var size = args.length > 0 ? Std.parseInt(args[0]) : 100;
		var n = args.length > 1 ? Std.parseInt(args[1]) : 100;
		var mod = size * 10;
		var rng0 = new LCG(0);
		var rng1 = new LCG(1);
		var hit = 0;
		var missed = 0;
		var lru = new LRU(size);
		for (i in 0...n) {
			var n0 = rng0.next() % mod;
			lru.put(n0, n0);
			var n1 = rng1.next() % mod;
			// Sys.println('$n0, $n1');
			if (lru.get(n1) != null) {
				hit += 1;
			} else {
				missed += 1;
			}
		}
		Sys.println('$hit\n$missed');
	}
}
