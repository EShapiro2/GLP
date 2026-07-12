// Host-side key-value blackboard for the IP end-to-end test.
//
// The two simulator agents coordinate through this server (simulators share
// the host network, so 127.0.0.1 on a simulator reaches the host):
//   PUT /<key>              store the request body under <key>
//   GET /<key>              200 + value, or 404 when unset
//   GET /wait/<key>?t=120   long-poll until <key> is set (or t seconds pass)
//
// Pure dart:io — run with: dart run tool/e2e_blackboard.dart [port]
import 'dart:async';
import 'dart:convert';
import 'dart:io';

Future<void> main(List<String> args) async {
  final port = args.isNotEmpty ? int.parse(args[0]) : 8787;
  final values = <String, String>{};
  final waiters = <String, List<Completer<String>>>{};

  final server = await HttpServer.bind(InternetAddress.anyIPv4, port);
  stdout.writeln('[blackboard] listening on ${server.address.address}:$port');

  Future<void> handle(HttpRequest req) async {
    try {
      final segments = req.uri.pathSegments;
      if (req.method == 'PUT' && segments.length == 1) {
        final key = segments[0];
        final value = await utf8.decodeStream(req);
        values[key] = value;
        stdout.writeln('[blackboard] PUT $key = '
            '${value.length > 60 ? '${value.substring(0, 60)}…' : value}');
        for (final waiter in waiters.remove(key) ?? const []) {
          waiter.complete(value);
        }
        req.response.statusCode = HttpStatus.ok;
      } else if (req.method == 'GET' &&
          segments.length == 2 &&
          segments[0] == 'wait') {
        final key = segments[1];
        final timeout =
            int.tryParse(req.uri.queryParameters['t'] ?? '120') ?? 120;
        final existing = values[key];
        if (existing != null) {
          req.response.write(existing);
        } else {
          final completer = Completer<String>();
          waiters.putIfAbsent(key, () => []).add(completer);
          try {
            final value =
                await completer.future.timeout(Duration(seconds: timeout));
            req.response.write(value);
          } on TimeoutException {
            req.response.statusCode = HttpStatus.requestTimeout;
          }
        }
      } else if (req.method == 'GET' && segments.length == 1) {
        final value = values[segments[0]];
        if (value == null) {
          req.response.statusCode = HttpStatus.notFound;
        } else {
          req.response.write(value);
        }
      } else {
        req.response.statusCode = HttpStatus.badRequest;
      }
    } catch (e) {
      stdout.writeln('[blackboard] error: $e');
      req.response.statusCode = HttpStatus.internalServerError;
    }
    await req.response.close();
  }

  // Handle each request concurrently — a parked long-poll must not block
  // other clients.
  await for (final req in server) {
    unawaited(handle(req));
  }
}
