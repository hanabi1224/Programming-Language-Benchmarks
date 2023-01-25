import 'dart:convert';
import 'dart:io';
import 'package:crypto/crypto.dart' as crypto;

Future<void> main(List<String> arguments) async {
  final fileName = arguments.isNotEmpty ? arguments[0] : 'sample';
  final n = arguments.length > 1 ? int.parse(arguments[1]) : 3;
  final jsonStr = await File('$fileName.json').readAsString();
  final data = jsonDecode(jsonStr);
  const encoder = JsonEncoder();
  printHash(encoder.convert(data));
  final array = List.generate(n, (_) => jsonDecode(jsonStr), growable: false);
  printHash(encoder.convert(array));
}

void printHash(String json) {
  final hash = crypto.md5.convert(utf8.encode(json));
  print('$hash');
}
