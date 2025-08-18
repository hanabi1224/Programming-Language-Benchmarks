// Based on a version by hanabi1224 and kevmoo
// Modified by Christian Kleineidam

import 'dart:async';

Future<void> main(List<String> arguments) async {
  final n = arguments.isNotEmpty ? int.parse(arguments[0]) : 20;
  StreamIterator<int> stream = StreamIterator(generate());
  for (int i = 0; i < n; i++) {
    await stream.moveNext();
    print(stream.current);
    stream = StreamIterator(filter(stream, stream.current));
  }
}

Stream<int> generate() async* {
  for (int i = 2;; i++) {yield i;}
}

Stream<int> filter(StreamIterator<int> input, int prime) async* {
  while (await input.moveNext()) {
    if (input.current % prime != 0) {
      yield input.current;
    }
  }
}
