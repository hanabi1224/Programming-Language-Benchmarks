import 'dart:async';

Future main(List<String> arguments) async {
  final n = arguments.isNotEmpty ? int.parse(arguments[0]) : 5;

  var stream = StreamIterator(generate());

  for (var i = 0; i < n; i++) {
    if (await stream.moveNext()) {
      var prime = stream.current;
      print(prime);

      stream = StreamIterator(filter(stream, prime));
    } else {
      break;
    }
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
      yield i;
    }
  }
}
