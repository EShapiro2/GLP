/// widowed/1 (the GLP paper f577e20, appendix-guards.tex, the guard table's
/// two rows and the paragraph on link ends after it): widowed(X) succeeds
/// when the reader paired to the writer X occurs in no goal of the agent and
/// in no link end the agent holds, the goal being reduced not counted, and
/// widowed(X?) when the writer paired to the reader X? does.  A goal whose
/// guard finds the pair occurring suspends, and is re-tried at quiescence
/// once it occurs nowhere (GlpRuntime.wakeWidowed).  The checker admits the
/// guard in either mode under the rule of glp.tex, Remark "Guards and SRSW":
/// "if X occurs in a widowed guard, X? occurs in the head, X in the guard,
/// and neither elsewhere in the clause; and if X? occurs in a widowed guard,
/// X occurs in the head, X? in the guard, and neither elsewhere in the
/// clause" (Definition "SRSW of a Guarded Clause").
///
/// Fixture: programs/tests/typed/widowed.glp.  The link ends' side, a
/// variable sent to another agent occurring in its link end until the end is
/// retired, is test/multiagent/epochs_test.dart's.
@Timeout.none
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

GlpEngine _engine() {
  final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
  expect(
      engine.loadFile(File('../programs/tests/typed/widowed.glp').absolute.path),
      isTrue);
  return engine;
}

/// The value a binding prints as: `widowed` for `Const(widowed)`.
String _v(Object? term) =>
    '$term'.replaceAllMapped(RegExp(r'^Const\((.*)\)$'), (m) => m[1]!);

void main() {
  group('widowed(X): the reader paired to the writer occurs nowhere', () {
    test('succeeds once the goal holding W? has ended', () async {
      final r =
          await _engine().runGoal('watch(W, O), holder(W?, T?), release(T)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['O']), 'widowed');
    });

    test('suspends while a goal holds W?', () async {
      final r = await _engine().runGoal('watch(W, O), holder(W?, T?)');
      expect(r.status, ExecutionStatus.suspended, reason: '${r.error}');
      expect(r.bindings['O'], isNull);
    });
  });

  group('widowed(X?): the writer paired to the reader occurs nowhere', () {
    // Its success in one agent no well-typed program brings about, the
    // checker refusing a head that leaves an output unassigned: the writer
    // goes with a link end retired, at a crash of the agent that holds it,
    // test/multiagent/epochs_test.dart.
    test('a reader whose writer is assigned is not widowed', () async {
      final r = await _engine().runGoal('wait(R?, O), assign(R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect('${r.bindings['O']}', contains('got'));
    });

    test('suspends while a goal holds R', () async {
      final r = await _engine().runGoal('wait(R?, O), owner(R, T?)');
      expect(r.status, ExecutionStatus.suspended, reason: '${r.error}');
      expect(r.bindings['O'], isNull);
    });
  });

  group('the checker: the variable in the guard, its pair in the head, '
      'neither elsewhere', () {
    late Directory dir;
    setUpAll(() {
      dir = Directory('../programs/tests').createTempSync('glp_widowed_');
    });
    tearDownAll(() => dir.deleteSync(recursive: true));

    /// Load [source] as a program under the root; the diagnostic it is
    /// refused with, or null where it loads.
    String? refusal(String name, String source) {
      final path = '${dir.path}/$name.glp';
      File(path).writeAsStringSync(source);
      try {
        expect(GlpEngine(rootSelfGlpPath: _rootSelf).loadFile(path), isTrue);
        return null;
      } catch (e) {
        return '$e';
      }
    }

    test('widowed(X) with X? once in the head loads', () {
      expect(
          refusal('w_ok', '''
procedure p(Constant, Constant).
p(X?, a) :- widowed(X) | true.
'''),
          isNull);
    });

    test('widowed(X?) with X once in the head loads', () {
      expect(
          refusal('r_ok', '''
procedure p(Constant?, Constant).
p(X, a) :- widowed(X?) | true.
'''),
          isNull);
    });

    test('widowed(X) with X? also in the body is refused', () {
      expect(
          refusal('w_body', '''
procedure p(Constant, Constant).
p(X?, Y?) :- widowed(X) | q(X?, Y).

procedure q(Constant?, Constant).
q(V, V?).
'''),
          contains('widowed'));
    });

    test('widowed(X) with X? nowhere in the head is refused', () {
      expect(
          refusal('w_nohead', '''
procedure p(Constant).
p(a) :- widowed(X) | true.
'''),
          contains('widowed'));
    });

    test('widowed(X?) with X also in the body is refused', () {
      expect(
          refusal('r_body', '''
procedure p(Constant?, Constant).
p(X, Y?) :- widowed(X?) | q(X, Y).

procedure q(Constant, Constant).
q(a, b).
'''),
          contains('widowed'));
    });

    test('widowed(X?) with X in the body, not the head, is refused', () {
      expect(
          refusal('r_nohead', '''
procedure p(Constant).
p(Y?) :- widowed(X?) | q(X, Y).

procedure q(Constant, Constant).
q(a, b).
'''),
          contains('widowed'));
    });
  });
}
