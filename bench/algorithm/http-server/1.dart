import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;

final HttpClient _client = HttpClient();

Future<void> main(List<String> arguments) async {
  final n = arguments.isNotEmpty ? int.parse(arguments[0]) : 10;
  final port = 20000 + Random().nextInt(30000);
  final handler = const Pipeline().addHandler(_handlePostAsync);
  final server = await shelf_io.serve(handler, 'localhost', port);
  // print(port);
  var sum = 0;
  final api = Uri.parse('http://localhost:$port/');
  final tasks = <Future<int>>[];
  for (var i = 1; i <= n; i++) {
    tasks.add(_sendAsync(api, i));
  }
  for (var task in tasks) {
    sum += await task;
  }
  print(sum);
  await server.close(force: true);
}

Future<Response> _handlePostAsync(Request request) async {
  final jsonStr = await request.readAsString();
  final payload = jsonDecode(jsonStr) as Map;
  final value = payload['value'];
  return Response.ok('$value');
}

Future<int> _sendAsync(Uri api, int value) async {
  final payload = '{"value":$value}';
  while (true) {
    try {
      final request = await _client.postUrl(api);
      request.write(payload);
      await request.flush();
      final response = await request.close();
      final sb = StringBuffer();
      await for (var s in response.transform(utf8.decoder)) {
        sb.write(s);
      }
      final responseStr = sb.toString();
      return int.parse(responseStr);
    } catch (e) {}
  }
}
