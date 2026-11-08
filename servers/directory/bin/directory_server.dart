// The Grassroots Directory Service's web server (GNA Section 7): the page,
// the lookups and the verification links, over the store in DATA (default
// directory.json).  The directory's agent, which takes the persons' List,
// List unlisted and Unlist and their links, joins it when GNA gives its
// messages.
import 'dart:io';

import 'package:directory_server/mail.dart';
import 'package:directory_server/server.dart';
import 'package:directory_server/store.dart';

Future<void> main() async {
  final env = Platform.environment;
  final host = env['HOST'] ?? '127.0.0.1';
  final port = int.tryParse(env['PORT'] ?? '') ?? 8090;
  final base = env['BASE_URL'] ?? 'http://$host:$port';
  final smtp = SmtpConfig.fromEnvironment();
  final dir = DirectoryService(Store(file: File(env['DATA'] ?? 'directory.json')),
      smtp == null ? LogMailer() : SmtpMailer(smtp), base);
  final server = await HttpServer.bind(host, port);
  stdout.writeln('directory at $base/directory, listening on $host:$port, mail ${smtp == null ? 'logged, not sent' : 'through ${smtp.host}:${smtp.port}'}');
  await for (final q in server) {
    dir.handle(q);
  }
}
