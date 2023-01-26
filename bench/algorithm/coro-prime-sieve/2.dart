import 'dart:isolate';

Future<void> main(List<String> arguments) async {
  final n = arguments.isNotEmpty ? int.parse(arguments[0]) : 5;
  await lastFilter(n);
}

Future<void> lastFilter(int n) async {
  final rx = ReceivePort();
  await Isolate.spawn(filter, FilterContext(rx.sendPort, n - 1));
  await for (int p in rx.cast()) {
    print(p);
    break;
  }
}

Future<void> filter(FilterContext ctx) async {
  final rx = ReceivePort();
  if (ctx.n > 1) {
    await Isolate.spawn(filter, FilterContext(rx.sendPort, ctx.n - 1));
  } else {
    await Isolate.spawn(generate, rx.sendPort);
  }
  var prime = -1;
  await for (int n in rx.cast()) {
    if (prime < 0) {
      prime = n;
      print('$prime');
    } else if (n % prime != 0) {
      ctx.outPort.send(n);
    }
  }
}

void generate(SendPort sender) {
  for (var i = 2;; i++) {
    sender.send(i);
  }
}

class FilterContext {
  SendPort outPort;
  int n;
  FilterContext(this.outPort, this.n);
}
