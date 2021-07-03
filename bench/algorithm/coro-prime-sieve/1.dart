import 'dart:isolate';

Future main(List<String> arguments) async {
  final n = arguments.length > 0 ? int.parse(arguments[0]) : 5;
  await lastFilter(n);
}

Future lastFilter(int n) async {
  var rx = ReceivePort();
  Isolate.spawn(filter, FilterContext(rx.sendPort, n - 1));
  await for (int p in rx.cast()) {
    print(p);
    break;
  }
}

Future filter(FilterContext ctx) async {
  var rx = ReceivePort();
  if (ctx.n > 1) {
    Isolate.spawn(filter, FilterContext(rx.sendPort, ctx.n - 1));
  } else {
    Isolate.spawn(generate, rx.sendPort);
  }
  int prime = -1;
  await for (int n in rx.cast()) {
    if (prime < 0) {
      prime = n;
      print("$prime");
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
  FilterContext(SendPort this.outPort, int this.n) {}
}
