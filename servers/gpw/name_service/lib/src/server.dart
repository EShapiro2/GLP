/// HTTP wiring: routes under /gpw/v1 onto [NameService].  The service binds
/// to localhost; TLS fronting is the web front's job (Stage 3, Caddy).
library;

import 'dart:convert';
import 'dart:io';

import 'service.dart';

const maxBodyBytes = 64 * 1024;

Future<HttpServer> serve(NameService service, InternetAddress host, int port) async {
  final server = await HttpServer.bind(host, port);
  server.listen((req) => _handle(service, req));
  return server;
}

Future<void> _handle(NameService service, HttpRequest req) async {
  Outcome outcome;
  try {
    outcome = await _route(service, req);
  } catch (e) {
    outcome = Outcome.error(500, 'internal error');
    stderr.writeln('gpw_name_service: ${req.method} ${req.uri.path}: $e');
  }
  req.response.statusCode = outcome.status;
  req.response.headers.contentType = ContentType.json;
  req.response.write(jsonEncode(outcome.body));
  await req.response.close();
}

Future<Outcome> _route(NameService service, HttpRequest req) async {
  final parts =
      req.uri.path.split('/').where((p) => p.isNotEmpty).toList();
  if (parts.length < 2 || parts[0] != 'gpw' || parts[1] != 'v1') {
    return Outcome.error(404, 'not found');
  }
  final rest = parts.sublist(2);

  if (rest.length == 1 && rest[0] == 'server-key' && req.method == 'GET') {
    return service.serverKeyInfo();
  }
  if (rest.isNotEmpty && rest[0] == 'names') {
    if (rest.length == 2 && req.method == 'GET') {
      return service.get(rest[1]);
    }
    if (rest.length == 2 && req.method == 'PUT') {
      return service.deposit(rest[1], await _json(req));
    }
    if (rest.length == 3 && req.method == 'POST' && rest[2] == 'repoint') {
      return service.repoint(rest[1], await _json(req));
    }
    if (rest.length == 3 && req.method == 'POST' && rest[2] == 'retire') {
      return service.retire(rest[1], await _json(req));
    }
  }
  return Outcome.error(404, 'not found');
}

Future<Object?> _json(HttpRequest req) async {
  final bytes = <int>[];
  await for (final chunk in req) {
    bytes.addAll(chunk);
    if (bytes.length > maxBodyBytes) {
      throw const FormatException('request body too large');
    }
  }
  try {
    return jsonDecode(utf8.decode(bytes));
  } on FormatException {
    return null; // Envelope.parse turns this into a 400.
  }
}
