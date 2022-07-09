use Map;

config const n = 100;
config const size = 100;

proc main() {
  const mod = (size * 10) : uint;
  var rng0 = new LCG(0);
  var rng1 = new LCG(1);
  var lru = new LRU(size);
  var hit = 0;
  var missed = 0;
  for i in 0..<n {
    var n0 = rng0.next() % mod;
    lru.put(n0, n0);
    var n1 = rng1.next() % mod;
    writeln(n0, ", ", n1);
    if lru.get(n1)[0] {
      hit += 1;
    } else {
      missed += 1;
    }
  }
  writeln(hit, "\n", missed);
}

class LCG {
  var seed : uint;
  proc init(seed : uint) {
    this.seed = seed;
  }

  proc next() : uint {
    seed = (1103515245 * seed + 12345) % 2147483648;
    return seed;
  }
}

class LinkedListNode {
  var data: (uint, uint);
  var prev: shared LinkedListNode?;
  var next: shared LinkedListNode?;
}

class LinkedList {
  var len = 0;
  var head, tail: shared LinkedListNode?;

  proc add(data: (uint, uint)) : shared LinkedListNode {
    var node = new shared LinkedListNode(data);
    __add_node(node);
    len += 1;
    return node;
  }

  proc __add_node(node: shared LinkedListNode) {
    if head == nil {
      head = node;
      node.prev = nil;
    } 
    else if tail != nil {
      node.prev = tail;
      tail!.next = node;
    }
    tail = node;
    node.next = nil;
  }

  proc __remove(node: shared LinkedListNode) {
    if head == node {
      head = node.next;
    }
    if tail == node {
      tail = node.prev;
    }
    if node.prev != nil {
      node.prev!.next = node.next;
    }
    if node.next != nil {
      node.next!.prev = node.prev;
    }
  }

  proc move_to_end(node: shared LinkedListNode) {
    __remove(node);
    __add_node(node);
  }
}

class LRU {
  var size: int;
  var keys: map(uint, shared LinkedListNode);
  var entries: shared LinkedList;

  proc init(size: int) {
    this.size = size;
    this.keys = new map(uint, shared LinkedListNode, initialCapacity=size);
    this.entries = new LinkedList();
  }

  proc get(key: uint): (bool,uint) {
    if this.keys.contains(key) {
      var node = this.keys.getValue(key);
      this.entries.move_to_end(node);
      return (true, node.data[1]);
    } else {
      return (false, 0 : uint);
    }
  }

  proc put(key: uint, value: uint) {
    if this.keys.contains(key) {
      var node = this.keys.getValue(key);
      node.data = (key, value);
      this.entries.move_to_end(node);
    } else if this.entries.len == this.size {
      var head = this.entries.head : LinkedListNode;
      head.data = (key, value);
      this.entries.move_to_end(head);
    } else {
      this.keys.set(key, this.entries.add((key, value)));
    }
  }
}
