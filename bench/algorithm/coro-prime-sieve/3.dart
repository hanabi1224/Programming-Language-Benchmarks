void main(List<String> arguments) async {
  final n = arguments.length > 0 ? int.parse(arguments[0]) : 5;
  var it = generate().iterator;
  for (var i = 0; i < n; i++) {
    it.moveNext();
    final prime = it.current;
    print(prime);
    it = filter(it, prime).iterator;
  }
}

Iterable<int> generate() sync* {
  for (var i = 2;; i++) {
    yield i;
  }
}

Iterable<int> filter(Iterator<int> input, int prime) sync* {
  while (true) {
    input.moveNext();
    final i = input.current;
    if (i % prime != 0) {
      yield i;
    }
  }
}
