// The warranties of packages (lib/warranty.dart), for tools/make.py and
// phone/tool/packages.py: on stdin a JSON list of requests, each
// {id, files, text, schemas, verdict, out?}; on stdout a JSON list of answers,
// each {warranty} or {error}.  Where a request names a directory out, the unit
// compiled and what its harness checks it against are written there too
// (warranty.dart's artefactFile and trustFile).
//
// The keys: GAS_COMPILER_KEY and GAS_CREATOR_KEY, files each holding an
// Ed25519 seed as hex, made where there is none (default
// servers/gas/keys/compiler.key and creator.key, gitignored).  The tree:
// GLP_PROGRAMS (default the repository's programs/).
import 'dart:convert';
import 'dart:io';

import 'package:gas_core/warranty.dart';

Future<void> main() async {
  final here = File(Platform.script.toFilePath()).parent.parent.parent.path; // servers/gas
  final env = Platform.environment;
  final programs = env['GLP_PROGRAMS'] ?? Directory('$here/../../programs').absolute.path;
  final compiler = keyAt(env['GAS_COMPILER_KEY'] ?? '$here/keys/compiler.key');
  final creator = keyAt(env['GAS_CREATOR_KEY'] ?? '$here/keys/creator.key');
  final requests = jsonDecode(await utf8.decodeStream(stdin)) as List;
  final answers = <Object?>[];
  for (final r in requests.cast<Map>()) {
    try {
      answers.add({
        'warranty': warrant(
          programs: programs,
          id: r['id'] as String,
          files: (r['files'] as Map).cast<String, Object?>(),
          text: r['text'] as String,
          schemas: r['schemas'] as String,
          verdict: r['verdict'] as String,
          compiler: compiler,
          creator: creator,
          out: r['out'] as String?,
        )
      });
    } on Object catch (e) {
      answers.add({'error': '$e'});
    }
  }
  // The engine writes its notes to stdout: the answers go after a marker.
  stdout.writeln('%% warranties');
  stdout.writeln(jsonEncode(answers));
}
