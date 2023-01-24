import 'dart:collection';

class LCG {
  int seed;
  LCG(this.seed);
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
  final LinkedHashMap<K, V> map = LinkedHashMap<K, V>();
  LRU(this.size);

  V? get(K key) {
    final ret = map[key];
    if (ret != null) {
      map.remove(key);
      map[key] = ret;
    }
    return ret;
  }

  void put(K key, V value) {
    if (map.containsKey(key)) {
      map.remove(key);
    } else {
      if (map.length == size) {
        map.remove(map.keys.first);
      }
    }
    map[key] = value;
  }
}

void main(List<String> arguments) {
  final size = arguments.isNotEmpty ? int.parse(arguments[0]) : 100;
  final n = arguments.isNotEmpty ? int.parse(arguments[1]) : 1000;
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
  print('$hit\n$missed');
}
