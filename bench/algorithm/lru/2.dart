class LinkedListNode<T> {
  T data;
  LinkedListNode<T>? prev = null;
  LinkedListNode<T>? next = null;
  LinkedListNode(this.data);
}

class LinkedList<T> {
  LinkedListNode<T>? head = null;
  LinkedListNode<T>? tail = null;
  int len = 0;

  LinkedListNode<T> add(T data) {
    final node = new LinkedListNode(data);
    __add_node(node);
    len += 1;
    return node;
  }

  void __add_node(LinkedListNode<T> node) {
    if (head == null) {
      head = node;
      node.prev = null;
    } else if (tail != null) {
      node.prev = tail;
      tail!.next = node;
    }
    tail = node;
    node.next = null;
  }

  void __remove(LinkedListNode<T> node) {
    if (head == node) {
      head = node.next;
    }
    if (tail == node) {
      tail = node.prev;
    }
    if (node.prev != null) {
      node.prev!.next = node.next;
    }
    if (node.next != null) {
      node.next!.prev = node.prev;
    }
  }

  void move_to_end(LinkedListNode<T> node) {
    __remove(node);
    __add_node(node);
  }
}

class Pair<K, V> {
  K key;
  V value;
  Pair(this.key, this.value);
}

class LCG {
  int seed;
  LCG(int this.seed);
  int next() {
    _lcg();
    return seed;
  }

  void _lcg() {
    const A = 1103515245;
    const C = 12345;
    const M = 1 << 31;
    seed = (A * seed + C) % M;
  }
}

class LRU<K, V> {
  final int size;
  final Map<K, LinkedListNode<Pair<K, V>>> keys =
      Map<K, LinkedListNode<Pair<K, V>>>();
  final LinkedList<Pair<K, V>> entries = new LinkedList<Pair<K, V>>();
  LRU(int this.size);

  V? get(K key) {
    final node = keys[key];
    if (node == null) {
      return null;
    }
    entries.move_to_end(node);
    return node.data.value;
  }

  void put(K key, V value) {
    final node = keys[key];
    if (node != null) {
      node.data.value = value;
      entries.move_to_end(node);
    } else if (entries.len == size) {
      final head = entries.head;
      keys.remove(head!.data.key);
      head.data.key = key;
      head.data.value = value;
      entries.move_to_end(head);
      keys[key] = head;
    } else {
      keys[key] = entries.add(Pair(key, value));
    }
  }
}

void main(List<String> arguments) {
  final size = arguments.length > 0 ? int.parse(arguments[0]) : 100;
  final n = arguments.length > 0 ? int.parse(arguments[1]) : 1000;
  final mod = size * 10;
  var hit = 0;
  var missed = 0;
  final rng0 = LCG(0);
  final rng1 = LCG(1);
  final lru = LRU<int, int>(size);
  for (var i = 0; i < n; i++) {
    final n0 = rng0.next() % mod;
    lru.put(n0, n0);
    final n1 = rng1.next() % mod;
    if (lru.get(n1) == null) {
      missed += 1;
    } else {
      hit += 1;
    }
  }
  print("$hit\n$missed");
}
