/// Does a committable later clause fire when an earlier clause's head suspends
/// on an unbound argument? (strictTypes=false: we care about runtime selection,
/// not the harness's mode annotations.)
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  test('NetIn clause commits while UserIn unbound (clause selection)', () async {
    final path =
        File('../programs/tests/clause_select_probe.glp').absolute.path;
    final engine = GlpEngine(
        rootSelfGlpPath: File('../programs/self.glp').absolute.path)
      ..strictTypes = false;
    final out = <String>[];
    engine.runtime.outputCallback = out.add;
    engine.loadSource(File(path).readAsStringSync(), filename: path);

    final r = await engine.runGoal('go');
    // ignore: avoid_print
    print('STATUS: ${r.status}  OUT: $out');

    final gotNet = out.any((l) => l.contains('got_net(hello)'));
    expect(gotNet, isTrue,
        reason: 'committable NetIn clause should fire despite unbound UserIn');
  }, skip: 'Retired 2026-06-22: rotted Issue-15 isolation scaffolding — '
      'clause_select_probe.glp type-errors (and the goal-check now rejects it). '
      'Issue 15 was resolved as a typing/legacy-fixture artifact and its probe '
      '(probe15) eliminated. See known-issues Issue 15.');
}
