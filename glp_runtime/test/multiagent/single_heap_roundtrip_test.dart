/// Single-heap discriminator: the REAL agent/4 + ui_mediator/5 clauses, with
/// only the transport swapped (in-heap pipe instead of MAD). Same heap => the
/// cold-call response variable is a local writer, never a `_w` remote ref.
///
///   completes (connected both sides) => UI .glp correct, cross-isolate `_w`
///     writer-return is the fault (madGLP runtime).
///   strands (no connected)            => escrow polarity bug in the UI .glp.
///
/// strictTypes=false: the harness loads the real clauses across sources (so
/// agent/4 resolves at runtime) and tolerates the play's isolated-typecheck
/// mode warnings. Runtime executes per the `?` annotations regardless.
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  test('single-heap cold-call: connect -> accept -> connected on both sides',
      () async {
    final dir = '../programs/tests/agent_roundtrip';
    final engine = GlpEngine(
        rootSelfGlpPath: File('../programs/self.glp').absolute.path)
      ..strictTypes = false;

    final out = <String>[];
    engine.runtime.outputCallback = out.add;

    for (final f in [
      'self.glp',
      'typed_social_agent.glp',
      'typed_ui_mediator.glp',
      'play_single_heap_roundtrip.glp',
    ]) {
      final path = File('$dir/$f').absolute.path;
      engine.loadSource(File(path).readAsStringSync(), filename: path);
    }

    final result = await engine.runGoal('go');
    // ignore: avoid_print
    print('STATUS: ${result.status}');
    // ignore: avoid_print
    print('OUTPUT:\n${out.join('\n')}');

    final aliceConnected = out.any((l) => l.contains('connected(bob)'));
    final bobConnected = out.any((l) => l.contains('connected(alice)'));
    expect(bobConnected, isTrue, reason: 'bob emits connected(alice)');
    expect(aliceConnected, isTrue, reason: 'alice emits connected(bob)');
  });
}
