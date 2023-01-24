import 'dart:core';
import 'dart:io';

import 'package:tuple/tuple.dart';

Future main(List<String> arguments) async {
  final fileName = arguments.isNotEmpty ? arguments[0] : '25000_in';
  var content = await File(fileName).readAsString();
  final ilen = content.length;
  content = content.replaceAll(RegExp('>.*\n|\n'), '');
  final clen = content.length;
  for (final p in [
    'agggtaaa|tttaccct',
    '[cgt]gggtaaa|tttaccc[acg]',
    'a[act]ggtaaa|tttacc[agt]t',
    'ag[act]gtaaa|tttac[agt]ct',
    'agg[act]taaa|ttta[agt]cct',
    'aggg[acg]aaa|ttt[cgt]ccct',
    'agggt[cgt]aa|tt[acg]accct',
    'agggta[cgt]a|t[acg]taccct',
    'agggtaa[cgt]|[acg]ttaccct'
  ]) {
    final count = RegExp(p).allMatches(content).length;
    print('$p $count');
  }
  for (final t in [
    const Tuple2('tHa[Nt]', '<4>'),
    const Tuple2('aND|caN|Ha[DS]|WaS', '<3>'),
    const Tuple2('a[NSt]|BY', '<2>'),
    const Tuple2('<[^>]*>', '|'),
    const Tuple2(r'\|[^|][^|]*\|', '-'),
  ]) {
    content = content.replaceAll(RegExp(t.item1), t.item2);
  }
  print('\n$ilen\n$clen\n${content.length}');
}
