import 'dart:convert';
import 'dart:io';
import 'package:crypto/crypto.dart' as crypto;

Future main(List<String> arguments) async {
  final fileName = arguments.length > 0 ? arguments[0] : "sample";
  final n = arguments.length > 1 ? int.parse(arguments[1]) : 3;
  final jsonStr = await File("$fileName.json").readAsString();
  final data = jsonDecode(jsonStr);
  final encoder = JsonEncoder();
  printHash(encoder.convert(data));
  final array = List.generate(n, (_) => jsonDecode(jsonStr), growable: false);
  printHash(encoder.convert(array));
}

void printHash(String json) {
  var hash = crypto.md5.convert(utf8.encode(json));
  print("$hash");
}
