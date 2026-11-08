// The debugging log service's web server (GLP/phones/docs/debugging.md, "The
// log service"): HOST (default 127.0.0.1), PORT (default 8095), DATA, the
// directory of the runs (default debuglog-data), DEBUGLOG_PASSWORD, which
// every request carries, and MAX_BATCH, the largest batch in bytes (default
// 64 MiB).  It does not start without a password.
import 'dart:io';

import 'package:debuglog_server/server.dart';
import 'package:debuglog_server/store.dart';

Future<void> main() async {
  final env = Platform.environment;
  final password = env['DEBUGLOG_PASSWORD'] ?? '';
  if (password.isEmpty) {
    stderr.writeln('debuglog: DEBUGLOG_PASSWORD is not set; see README, "The server"');
    exit(2);
  }
  final host = env['HOST'] ?? '127.0.0.1';
  final port = int.tryParse(env['PORT'] ?? '') ?? 8095;
  final data = Directory(env['DATA'] ?? 'debuglog-data');
  await data.create(recursive: true);
  final max = int.tryParse(env['MAX_BATCH'] ?? '') ?? 64 << 20;
  final service = DebuglogService(LogStore(data), password, maxBatch: max);
  final server = await HttpServer.bind(host, port);
  stdout.writeln('debuglog at $host:$port/log, runs in ${data.absolute.path}, batches up to $max bytes');
  await for (final q in server) {
    service.handle(q);
  }
}
