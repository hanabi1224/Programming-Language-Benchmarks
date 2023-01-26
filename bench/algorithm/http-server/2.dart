import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:math';

import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;

final HttpClient _client = HttpClient();

Future<void> main(List<String> arguments) async {
  final n = arguments.isNotEmpty ? int.parse(arguments[0]) : 500;
  final port = 20000 + Random().nextInt(30000);
  // print(port);

  final serverReceivePort = ReceivePort();
  late final SendPort serverSendPort;

  final task = serverReceivePort.first.then((message) {
    serverSendPort = message as SendPort;
    serverSendPort.send(port);
  });

  await Isolate.spawn(_startServer, serverReceivePort.sendPort);
  await task;

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

  serverSendPort.send('close');
}

void _startServer(SendPort sendPort) {
  final receivePort = ReceivePort();
  sendPort.send(receivePort.sendPort);

  late HttpServer server;
  receivePort.listen((message) async {
    if (message is int) {
      final handler = const Pipeline().addHandler(_handlePostAsync);
      server = await shelf_io.serve(handler, 'localhost', message);
    } else if (message == 'close') {
      await server.close(force: true);
    }
  });
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
      final responseStr = await utf8.decodeStream(response);
      return int.parse(responseStr);
    } catch (e) {}
  }
}
