import 'dart:convert';
import 'dart:io';
import 'dart:math';
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;

final HttpClient _client = new HttpClient();

Future main(List<String> arguments) async {
  final n = arguments.length > 0 ? int.parse(arguments[0]) : 10;
  final port = 30000 + Random().nextInt(10000);
  var handler = const Pipeline().addHandler(_handlePostAsync);
  var server = await shelf_io.serve(handler, 'localhost', port)
    ..autoCompress = true;
  // print(port);
  var sum = 0;
  final api = Uri.parse("http://localhost:$port/");
  final tasks = [];
  for (var i = 1; i <= n; i++) {
    tasks.add(_sendAsync(api, i));
  }
  for (Future<int> task in tasks) {
    sum += await task;
  }
  print(sum);
  await server.close(force: true);
}

Future<Response> _handlePostAsync(Request request) async {
  final jsonStr = await request.readAsString();
  final payload = jsonDecode(jsonStr);
  final value = payload["value"];
  return Response.ok("$value");
}

Future<int> _sendAsync(Uri api, int value) async {
  final payload = "{\"value\":$value}";
  while (true) {
    try {
      final request = await _client.postUrl(api);
      request.write(payload);
      await request.flush();
      final response = await request.close();
      var sb = StringBuffer();
      await for (var s in response.transform(utf8.decoder)) {
        sb.write(s);
      }
      var responseStr = sb.toString();
      return int.parse(responseStr);
    } catch (e) {}
  }
}
