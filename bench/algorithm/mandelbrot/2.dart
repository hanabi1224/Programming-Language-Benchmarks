import 'dart:async';
import 'dart:io';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:crypto/crypto.dart' as crypto;

void main(List<String> args) {
  var n = args.isNotEmpty ? int.parse(args[0]) : 200;
  // Ensure image_Width_And_Height are multiples of 8.
  n = (n + 7) ~/ 8 * 8;

  final threads = Platform.numberOfProcessors;
  final segmentFutures = <Future<dynamic>>[];

  final segmentSize = List.filled(threads, n ~/ threads);
  segmentSize[0] += n % threads;

  var from = 0;
  for (var i = 0; i < threads; i++) {
    final len = segmentSize[i];
    final response = ReceivePort();
    final localFrom = from;
    final remote = Isolate.spawn(calculateSegment, response.sendPort);
    segmentFutures.add(remote.then((_) => response.first).then((sendPort) {
      final response = ReceivePort();
      sendPort.send(
          {'n': n, 'from': localFrom, 'len': len, 'port': response.sendPort});
      return response.first;
    }));
    from += len;
  }

  print('P4\n$n $n');

  Future.wait(segmentFutures).then((segments) {
    final buffer = BytesBuilder(copy: false);
    for (var segment in segments) {
      for (var line in segment as List<Uint8List>) {
        buffer.add(line);
      }
    }
    final bytes = buffer.takeBytes();
    final hash = crypto.md5.convert(bytes);
    print('$hash');
  });
}

Uint8List calculateLine(int n, int y) {
  final lineLen = (n - 1) ~/ 8 + 1;

  final line = Uint8List(lineLen);

  var xbyte = 0, bits = 1;
  final ci = y * 2.0 / n - 1.0;

  for (var x = 0; x < n; x++) {
    final cr = x * 2.0 / n - 1.5;
    if (bits > 0xff) {
      line[xbyte++] = bits;
      bits = 1;
    }
    var zr = cr, zi = ci, tr = cr * cr, ti = ci * ci;
    var i = 49;
    do {
      zi = zr * zi + zr * zi + ci;
      zr = tr - ti + cr;
      tr = zr * zr;
      ti = zi * zi;
    } while ((tr + ti <= 4.0) && (--i > 0));
    bits = (bits << 1) | (i == 0 ? 1 : 0);
  }
  while (bits < 0x100) {
    bits = bits << 1;
  }
  line[xbyte] = bits;

  return line;
}

void calculateSegment(SendPort initialReplyTo) {
  final port = ReceivePort();
  initialReplyTo.send(port.sendPort);
  port.listen((data) {
    final msg = data as Map;
    final n = msg['n'] as int;
    final from = msg['from'] as int;
    final len = msg['len'] as int;
    final replyTo = msg['port'] as SendPort;

    final lines = List<Uint8List>.filled(len, Uint8List(0));
    for (var i = 0; i < len; i++) {
      lines[i] = calculateLine(n, from + i);
    }
    replyTo.send(lines);
    port.close();
  });
}
