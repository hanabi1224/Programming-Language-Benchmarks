import 'dart:async';

Future main(List<String> arguments) async {
  final n = arguments.isNotEmpty ? int.parse(arguments[0]) : 5;

  var stream = StreamIterator(generate());

  for (var i = 0; i < n && await stream.moveNext(); i++) {
    var prime = stream.current;
    print(prime);

    stream = StreamIterator(filter(stream, prime));
  }
}

Stream<int> generate() async* {
  for (var i = 2;; i++) {
    yield i;
  }
}

Stream<int> filter(StreamIterator<int> input, int prime) async* {
  while (await input.moveNext()) {
    final i = input.current;
    if (i % prime != 0) {
      // Use of `await` to avoid Stack Overflow
      yield await Future.value(i);
    }
  }
}
