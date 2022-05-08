config const n = 100;
config const size = 100;

proc main() {
  const mod = (size * 10) : uint;
  var rng0 = new LCG(0);
  var rng1 = new LCG(1);
  for i in 0..n {
    var n0 = rng0.next() % mod;
    var n1 = rng1.next() % mod;
    writeln(n0, ", ", n1);
  }
  var lln = new LinkedListNode(uint, 3);
  var ll = new LinkedList(uint);
  ll.add(1);
  var head = ll.pop_head();
  ll.move_to_end(head!);
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
  type t;
  var data: t;
  var prev: shared LinkedListNode(t)?;
  var next: shared LinkedListNode(t)?;
}

class LinkedList {
  type t;
  var len = 0;
  var head, tail: shared LinkedListNode(t)?;

  proc add(data:t) : LinkedListNode(t) {
    var node = new shared LinkedListNode(t, data);
    __add_node(node);
    len += 1;
    return node;
  }

  proc __add_node(node: LinkedListNode(t)) {
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

  proc __remove(node: LinkedListNode(t)) {
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

  proc move_to_end(node: LinkedListNode(t)) {
    __remove(node);
    __add_node(node);
  }

  proc pop_head() : LinkedListNode(t)? {
    if head == nil {
      return nil;
    }
    var tmp = head!;
    head = head!.next;
    len -= 1;
    return tmp;
  }
}
