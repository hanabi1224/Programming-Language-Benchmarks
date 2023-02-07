// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Christian Kleineidam
// based on the lua version by hanabi1224's lua implementation

Iterable<int> generate() sync*  {
  int i = 2;
  while (true) {
    yield i++;
  }
}

Iterable<int> filter(Iterator<int> it, int prime) sync* {
  while (true){
    if (it.moveNext()==false){
      print(it.current);
      return;
    }
    if (it.current % prime != 0){
      yield it.current;
    }
  }
}

void main(List<String> args) {
  int n = args.isNotEmpty ? int.parse(args[0]) : 1000;
  var it = generate().iterator;
  
  for (int i = 0; i < n; i++) {
    it.moveNext();
    print(it.current);
    it = filter(it, it.current).iterator;
  }
}
