import 'dart:convert';
import 'dart:io';
import 'package:crypto/crypto.dart' as crypto;

Future main(List<String> arguments) async {
  final fileName = arguments.length > 0 ? arguments[0] : "sample";
  final n = arguments.length > 1 ? int.parse(arguments[1]) : 3;
  final jsonStr = await File("$fileName.json").readAsString();
  var indent = " ";
  for (var i = 0; i < n; i++) {
    final data = jsonDecode(jsonStr);
    final encoder = JsonEncoder.withIndent(indent);
    indent += " ";
    final prettified = encoder.convert(data);
    var hash = crypto.md5.convert(utf8.encode(prettified));
    print("$hash");
  }
}
