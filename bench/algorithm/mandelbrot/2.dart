import 'dart:io';
import 'dart:isolate';
import 'dart:async';
import 'dart:typed_data';
import 'package:crypto/crypto.dart' as crypto;

void main(args) {
  int n = args.length > 0 ? int.parse(args[0]) : 200;
  // Ensure image_Width_And_Height are multiples of 8.
  n = (n + 7) ~/ 8 * 8;

  var threads = Platform.numberOfProcessors;
  final segmentFutures = <Future>[];

  var segmentSize = new List.filled(threads, n ~/ threads);
  segmentSize[0] += n % threads;

  int from = 0;
  for (int i = 0; i < threads; i++) {
    var len = segmentSize[i];
    var response = new ReceivePort();
    int localFrom = from;
    Future<Isolate> remote = Isolate.spawn(calculateSegment, response.sendPort);
    segmentFutures.add(remote.then((_) => response.first).then((sendPort) {
      ReceivePort response = new ReceivePort();
      sendPort.send(
          {'n': n, 'from': localFrom, 'len': len, 'port': response.sendPort});
      return response.first;
    }));
    from += len;
  }

  print('P4\n$n $n');

  Future.wait(segmentFutures).then((segments) {
    var buffer = BytesBuilder(copy: false);
    for (var segment in segments) {
      for (Uint8List line in segment) {
        buffer.add(line);
      }
    }
    var bytes = buffer.takeBytes();
    var hash = crypto.md5.convert(bytes);
    print("$hash");
  });
}

Uint8List calculateLine(int n, int y) {
  int lineLen = (n - 1) ~/ 8 + 1;

  var line = new Uint8List(lineLen);

  int xbyte = 0, bits = 1;
  double ci = y * 2.0 / n - 1.0;

  for (int x = 0; x < n; x++) {
    double cr = x * 2.0 / n - 1.5;
    if (bits > 0xff) {
      line[xbyte++] = bits;
      bits = 1;
    }
    double zr = cr, zi = ci, tr = cr * cr, ti = ci * ci;
    int i = 49;
    do {
      zi = zr * zi + zr * zi + ci;
      zr = tr - ti + cr;
      tr = zr * zr;
      ti = zi * zi;
    } while ((tr + ti <= 4.0) && (--i > 0));
    bits = (bits << 1) | (i == 0 ? 1 : 0);
  }
  while (bits < 0x100) bits = (bits << 1);
  line[xbyte] = bits;

  return line;
}

void calculateSegment(SendPort initialReplyTo) {
  var port = new ReceivePort();
  initialReplyTo.send(port.sendPort);
  port.listen((msg) {
    int n = msg['n'];
    int from = msg['from'];
    int len = msg['len'];
    SendPort replyTo = msg['port'];

    var lines = List<Uint8List>.filled(len, Uint8List(0));
    for (int i = 0; i < len; i++) {
      lines[i] = calculateLine(n, from + i);
    }
    replyTo.send(lines);
    port.close();
  });
}
