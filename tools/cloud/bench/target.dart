// target.dart: the measured process.  Drives the GLP engine as the REPL does and handshakes with
// client.dart through files in <work>: ready -> (go) -> load -> loaded -> (go2) -> goal -> done -> (quit).
//   dart run --observe=0 --disable-service-auth-codes --no-pause-isolates-on-exit target.dart <work> <file|dir> <path> [goal]
import 'dart:io';
import 'package:glp_runtime/engine/glp_engine.dart';

Future<void> waitFor(String path) async {
  while (!File(path).existsSync()) { await Future.delayed(const Duration(milliseconds: 50)); }
}

Future<void> main(List<String> args) async {
  final work = args[0], mode = args[1], path = args[2];
  final goal = args.length > 3 && args[3].isNotEmpty ? args[3] : null;
  final root = File('/home/user/glp/programs/self.glp').absolute.path;
  final engine = GlpEngine(rootSelfGlpPath: root);
  engine.maxCycles = 1000000000;
  File('$work/ready').writeAsStringSync('');
  await waitFor('$work/go');
  final sw = Stopwatch()..start();
  if (mode == 'dir') { engine.loadProgram(path); } else { engine.loadFile(path); }
  final tLoad = sw.elapsedMilliseconds;
  File('$work/loaded').writeAsStringSync('$tLoad');
  var tGoal = 0; var status = '';
  if (goal != null) {
    await waitFor('$work/go2');
    final t = sw.elapsedMilliseconds;
    final r = await engine.runGoal(goal);
    tGoal = sw.elapsedMilliseconds - t;
    status = '${r.status} ${r.error ?? ''}';
  }
  File('$work/done').writeAsStringSync('load $tLoad ms; goal $tGoal ms; $status');
  await waitFor('$work/quit');
  exit(0);
}
