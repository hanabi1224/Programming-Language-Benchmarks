import 'dart:async';

Future main(List<String> arguments) async {
  final n = arguments.length > 0 ? int.parse(arguments[0]) : 5;
  var stream = StreamIterator(generate());
  for (var i = 0; i < n; i++) {
    await stream.moveNext();
    var prime = stream.current;
    print(prime);
    stream = StreamIterator(filter(stream, prime));
  }
}

Stream<int> generate() async* {
  for (var i = 2;; i++) {
    yield await Future.microtask(() => i);
  }
}

Stream<int> filter(StreamIterator<int> input, int prime) async* {
  while (await input.moveNext()) {
    final i = input.current;
    if (i % prime != 0) {
      yield await Future.microtask(() => i);
    }
  }
}
