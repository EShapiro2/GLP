/// widowed_writer/1 and widowed_reader/1 (the GLP paper 07835e1,
/// appendix-guards.tex, the guard table's two rows and the paragraph on link
/// ends after it): widowed_writer(X) succeeds when the reader paired to the
/// writer X occurs in no goal of the agent and in no link end the agent
/// holds, the goal being reduced not counted, and widowed_reader(X?) when the
/// writer paired to the reader X? does; each is declared in its one mode,
/// `procedure widowed_writer(_).` and `procedure widowed_reader(_?).`.  A
/// goal whose guard finds the pair occurring suspends, and is re-tried at
/// quiescence once it occurs nowhere (GlpRuntime.wakeWidowed).  The checker
/// holds each guard to the rule of glp.tex, Remark "Guards and SRSW": "if X
/// occurs in a widowed_writer guard, X? occurs in the head, X in the guard,
/// and neither elsewhere in the clause; and if X? occurs in a widowed_reader
/// guard, X occurs in the head, X? in the guard, and neither elsewhere in the
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
  group('widowed_writer(X): the reader paired to the writer occurs nowhere', () {
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

    // The guard looks through the agent's goals, and a goal may hold an
    // integer of any magnitude: the look reads it as any other term (sGLP
    // #12, 2026-11-12: an integer of magnitude 2^62 or more aborted it, and
    // a seed split_random_seed/3 gives is such an integer).
    for (final n in [
      '5',
      '4611686018427387903',
      '4611686018427387904',
      '9223372036854775807',
      '-4611686018427387904',
      '-4611686018427387905',
      '-9223372036854775807',
    ]) {
      test('suspends while a goal holds W? and the integer $n', () async {
        final r = await _engine().runGoal('watch(W, O), keeper(W?, $n, T?)');
        expect(r.status, ExecutionStatus.suspended, reason: '${r.error}');
        expect(r.bindings['O'], isNull);
      });
    }

    test('succeeds once the goal holding W? and a large integer has ended',
        () async {
      final r = await _engine().runGoal(
          'watch(W, O), keeper(W?, 4611686018427387904, T?), release(T)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['O']), 'widowed');
    });
  });

  group('widowed_reader(X?): the writer paired to the reader occurs nowhere', () {
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

    test('widowed_writer(X) with X? once in the head loads', () {
      expect(
          refusal('w_ok', '''
procedure p(Constant, Constant).
p(X?, a) :- widowed_writer(X) | true.
'''),
          isNull);
    });

    test('widowed_reader(X?) with X once in the head loads', () {
      expect(
          refusal('r_ok', '''
procedure p(Constant?, Constant).
p(X, a) :- widowed_reader(X?) | true.
'''),
          isNull);
    });

    test('widowed_writer(X?), the reader where the declaration takes a '
        'writer, is refused by the checker; widowed_reader(X) likewise', () {
      expect(
          refusal('w_mode', '''
procedure p(Constant?, Constant).
p(X, a) :- widowed_writer(X?) | true.
'''),
          contains('widowed_writer'));
      expect(
          refusal('r_mode', '''
procedure p(Constant, Constant).
p(X?, a) :- widowed_reader(X) | true.
'''),
          contains('widowed_reader'));
    });

    test('widowed_writer(X) with X? also in the body is refused', () {
      expect(
          refusal('w_body', '''
procedure p(Constant, Constant).
p(X?, Y?) :- widowed_writer(X) | q(X?, Y).

procedure q(Constant?, Constant).
q(V, V?).
'''),
          contains('widowed'));
    });

    test('widowed_writer(X) with X? nowhere in the head is refused', () {
      expect(
          refusal('w_nohead', '''
procedure p(Constant).
p(a) :- widowed_writer(X) | true.
'''),
          contains('widowed'));
    });

    test('widowed_reader(X?) with X also in the body is refused', () {
      expect(
          refusal('r_body', '''
procedure p(Constant?, Constant).
p(X, Y?) :- widowed_reader(X?) | q(X, Y).

procedure q(Constant, Constant).
q(a, b).
'''),
          contains('widowed'));
    });

    test('widowed_reader(X?) with X in the body, not the head, is refused', () {
      expect(
          refusal('r_nohead', '''
procedure p(Constant).
p(Y?) :- widowed_reader(X?) | q(X, Y).

procedure q(Constant, Constant).
q(a, b).
'''),
          contains('widowed'));
    });
  });
}
