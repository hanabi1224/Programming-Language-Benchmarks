/* The Computer Language Benchmarks Game

   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Contributed by Philip Rogers
   Based on a javascript implementation by Jesse Millikan and Matt Baker
   Optimized and parallelized by Dwayne Slater
   + null safety    
*/

import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:typed_data';

class CodeList {
  Uint64List buffer;
  int length;
  int codeBuffer = 0;
  int codeBufferLen = 32;
  int codeLen = 0;

  CodeList(int initialCapacity)
      : buffer = Uint64List(initialCapacity),
        length = 0;

  void appendBuffer(int codes) {
    if (length == buffer.length) {
      final newBuffer = Uint64List(length * 2);
      newBuffer.setRange(0, length, buffer);
      buffer = newBuffer;
    }
    buffer[length++] = codes;
  }

  void add(int n) {
    codeBuffer = (codeBuffer << 2) | n;
    if ((--codeBufferLen) == 0) {
      appendBuffer(codeBuffer);
      codeBuffer = 0;
      codeBufferLen = 32;
    }
  }

  void flush() {
    codeLen = (length * 32) + (32 - codeBufferLen);
    while (codeBuffer != 0) {
      add(0);
    }
  }
}

String codeToString(int code, int len) {
  final b = Uint8List(len);
  for (var i = len - 1; i >= 0; i--) {
    const $A = 0x41;
    const $T = 0x54;
    const $C = 0x43;
    const $G = 0x47;
    b[i] = const [$A, $C, $T, $G][code & 3];
    code >>= 2;
  }
  return String.fromCharCodes(b);
}

Future<String> readInput() => stdin
    .transform(ascii.decoder)
    .transform(const LineSplitter())
    .skipWhile((line) => !line.startsWith('>THREE'))
    .skip(1)
    .takeWhile((line) => !line.startsWith('>'))
    .map((s) => s.toUpperCase())
    .join();

Future<CodeList> readCodes(String fileName) async {
  final file = File(fileName);
  final codeList = CodeList(1024 * 1024 * 4);
  final lines = await file.readAsLines();
  lines
      .skipWhile((line) => !line.startsWith('>THREE'))
      .skip(1)
      .takeWhile((line) => !line.startsWith('>'))
      .forEach((line) {
    final units = line.codeUnits;
    for (var i = 0; i < units.length; i++) {
      codeList.add((units[i] >> 1) & 3);
    }
  });
  codeList.flush();
  return codeList;
}

Map<String, int> frequency(CodeList codes, int length) {
  final freq = HashMap<int, int>();
  final shift = 64 - (length * 2);
  var window = codes.buffer[0];
  var next = codes.buffer[1];
  var count = codes.codeLen - length + 1;
  var cd = 32;
  var i = 2;
  while (count > 0) {
    freq[window >> shift] = (freq[window >> shift] ?? 0) + 1;

    window = (window << 2) | ((next >> 62) & 0x3);
    next <<= 2;
    count--;

    if ((--cd) == 0) {
      cd = 32;
      next = codes.buffer[i++];
    }
  }

  return Map.fromEntries(freq.entries.map((entry) => MapEntry(
      codeToString(entry.key.toUnsigned(length * 2), length), entry.value)));
}

void sort(CodeList codes, int length) {
  final freq = frequency(codes, length);
  final keys = freq.keys.toList();
  final n = codes.codeLen - length + 1;

  keys.sort((ak, bk) {
    final a = freq[ak] ?? 0;
    final b = freq[bk] ?? 0;
    return b - a;
  });

  for (final key in keys) {
    final count = ((freq[key] ?? 0) * 100 / n).toStringAsFixed(3);
    print('$key $count');
  }
  print('');
}

String find(CodeList codes, String string) {
  final freq = frequency(codes, string.length);
  return '${freq[string]}\t$string';
}

void main(List<String> arguments) async {
  final fileName = arguments.isNotEmpty ? arguments[0] : '25000_in';
  final sequence = await readCodes(fileName);
  final a = par(sequence, ['GGT', 'GGTA', 'GGTATT']);
  final b = par(sequence, ['GGTATTTTAATT']);
  final c = par(sequence, ['GGTATTTTAATTTATAGT']);
  sort(sequence, 1);
  sort(sequence, 2);
  (await a).forEach(print);
  (await b).forEach(print);
  (await c).forEach(print);
}

void findMultiple(List<dynamic> data) {
  final codes = data[1] as CodeList;
  final l = (data[2] as List<String>)
      .map((s) => find(codes, s))
      .toList(growable: false);
  (data[0] as SendPort).send(l);
}

Future<List<String>> par(CodeList codes, List<String> s) {
  final completer = Completer<List<String>>.sync();
  final recv = RawReceivePort(completer.complete);

  Isolate.spawn(findMultiple, [recv.sendPort, codes, s]);

  completer.future.whenComplete(recv.close);

  return completer.future;
}
