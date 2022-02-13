import 'package:kt_dart/kt.dart';

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

class LRU {
  final int size;
  final KtLinkedMap map = KtLinkedMap.empty();
  LRU(int this.size);

  int? get(int key) {
    final ret = map.getOrDefault(key, null);
    if (ret != null) {
      map.remove(key);
      map.put(key, ret);
    }
    return ret;
  }

  void put(int key, int value) {
    if (map.containsKey(key)) {
      map.remove(key);
    } else {
      if (map.size == size) {
        map.remove(map.keys.first());
      }
    }
    map.put(key, value);
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
  final lru = LRU(size);
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
