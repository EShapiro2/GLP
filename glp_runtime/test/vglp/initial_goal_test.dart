// glp_runtime/test/vglp/initial_goal_test.dart
//
// The initial goal of a vGLP program: "every call of a volitional procedure,
// in a clause body or in the initial goal, holds a writer at each output
// position" (vGLP, sections/vglp.tex, Definition "Volitional Procedure,
// Interactive Type, Interactive Term, Ordinary Clause, vGLP Program"), and
// "the GLP module that forms the program's initial goal imports that module's
// compiled procedures" (sections/parts/canonical-compilation.tex, Definition
// "Canonical Compilation").  The clause-body half is canonical_test's; this
// is the initial goal's, by the same rule (canonical.dart,
// checkWritersAtOutputs) over one record of the volitional procedures read
// off the .vglp source (volitionalRecord), in the two places the initial goal
// is written:
//
//   - a clause of the GLP module that imports the compiled procedures --- the
//     entry the host posts, the bridge's goal M # q(...) in its body ---
//     checked when the program links (program_linker.dart);
//   - a goal posted directly (TGLP modules.tex, "Type-Compatible Attestation
//     Between Agents": the initial goal posted to the runtime "is
//     type-checked before execution as a body goal"), checked where it is
//     (glp_engine.dart, _checkGoalWellTyped).  A posted goal names an entry
//     point, and the only entry point that is a volitional procedure is one
//     of a compiled module loaded as a single-module program, whose
//     procedures are its entry points: a directory program's are its
//     self.glp's procedures, which are ordinary.
//
// IGLP's Task 1 of GLP 2026-10-13 08:11 UTC; GLP's answer of 2026-10-13
// 09:20 UTC, item 2.  The programs are written under programs/tests/vglp/ for
// the run and removed after, as elicitation_test's are.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/program_linker.dart' show emitVglpSources;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;

final _root = File('../programs/self.glp').absolute.path;

/// One volitional procedure, ask/2, its question a card in writer mode: the
/// program shows the peer and hands the person the writer of the answer; its
/// second argument, Stream(YesNo), is an output position.
const _asker = '''
Peer  ::= Constant.
YesNo ::= yes ; no.
Card  ::= card(Peer, YesNo?).

exported procedure (Card)*ask(Peer?, Stream(YesNo)).
(card(P?, A))*ask(P, [A?]) :- ground(P?) | true.
''';

/// The GLP module that forms the initial goal: the host's entry, ask_init/3,
/// whose body calls the compiled ask/2 with [answers] at its output
/// position.
String _self(String answers) => '''
Peer     ::= Constant.
YesNo    ::= yes ; no.
Card     ::= card(Peer, YesNo?).
Question ::= card_w(Card).
Ask(Q)   ::= ask(Constant, Q).
Drawn    ::= card_w(_) ; YesNo.

imported procedure asker#ask(Peer?, Stream(YesNo), Stream(Ask(Question))).
imported procedure asker#dispatch(Stream(Ask(Question))?,
    PersonChannel(Drawn)?, Channel(Stream(_), Stream(_))).

exported procedure ask_init(Constant?, PersonIn(Drawn)?, Stream(_)?).
ask_init(_, Gs, _) :-
    asker # ask(bob, $answers, Asks),
    asker # dispatch(Asks?, ch(Gs?, Ds), _),
    send_to_user(Ds?).
''';

/// A fixture program under programs/tests/vglp/, its entry module calling
/// ask/2 with [answers].
Directory _fixture(String answers) {
  final d = Directory('../programs/tests/vglp/initgoal_fixture_${pid}_'
      '${DateTime.now().microsecondsSinceEpoch}')
    ..createSync();
  File('${d.path}/asker.vglp').writeAsStringSync(_asker);
  File('${d.path}/self.glp').writeAsStringSync(_self(answers));
  return d;
}

/// The refusal of the call [call], in [where], at output position 2 holding
/// [term].
Matcher _refusal(String call, String calledAs, String where, String term) =>
    allOf(
        contains('The call $call of the volitional procedure $calledAs/2, '
            '$where, holds $term at output position 2, which is not a '
            'writer'),
        contains('every call of a volitional procedure, in a clause body or '
            'in the initial goal, holds a writer at each output position '
            '(vGLP, Definition "Volitional Procedure, Interactive Type, '
            'Interactive Term, Ordinary Clause, vGLP Program")'));

void main() {
  group('a clause of the GLP module forming the initial goal, at link time',
      () {
    final made = <Directory>[];
    Directory fixture(String answers) {
      final d = _fixture(answers);
      made.add(d);
      return d;
    }
    tearDown(() {
      for (final d in made) {
        if (d.existsSync()) d.deleteSync(recursive: true);
      }
      made.clear();
    });

    test('a writer at the output position: the program loads', () {
      final d = fixture('_');
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(engine.loadProgram(d.absolute.path), isTrue);
    });

    test('a constant at the output position: refused, naming the call, the '
        'clause and the position', () {
      final d = fixture('[yes]');
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(
          () => engine.loadProgram(d.absolute.path),
          throwsA(predicate((e) =>
              _refusal(
                      'asker # ask(bob, [yes], Asks)',
                      'asker#ask',
                      'in the clause ask_init(_, Gs, _) at line 13 of '
                          '${d.absolute.path}/self.glp',
                      '[yes]')
                  .matches(e.toString(), {}))));
    });

    test('a reader at the output position: refused, by the type check of '
        'the module, which precedes the link', () {
      // A reader where the declaration's mode is the writer's is ill-moded
      // (TGLP), and the module is refused in step 2, before step 4 reaches
      // the rule.
      final d = fixture('A?');
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(
          () => engine.loadProgram(d.absolute.path),
          throwsA(predicate((e) => e
              .toString()
              .contains('Variable mode mismatch: reader requires'))));
    });

    test('the compiled module committed in the source\'s place: its record '
        'is read off the source beside it, and the call refused alike', () {
      final d = fixture('[yes]');
      // The committed module is the compiler's own output, :emit's; the
      // loader then links it, and skips the source.
      final written =
          emitVglpSources(d.absolute.path, rootSelfGlpPath: _root);
      expect(written, ['${d.absolute.path}/asker.glp']);
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(
          () => engine.loadProgram(d.absolute.path),
          throwsA(predicate((e) => e
              .toString()
              .contains('holds [yes] at output position 2, which is not a '
                  'writer'))));
    });
  });

  group('a goal posted directly', () {
    late Directory d;
    late GlpEngine engine;
    setUp(() {
      d = _fixture('_');
      emitVglpSources(d.absolute.path, rootSelfGlpPath: _root);
      engine = GlpEngine(rootSelfGlpPath: _root);
      // The compiled module as a single-module program: its procedures, ask/3
      // among them, are its entry points.
      expect(engine.loadFile('${d.absolute.path}/asker.glp'), isTrue);
    });
    tearDown(() {
      if (d.existsSync()) d.deleteSync(recursive: true);
    });

    test('a constant at the output position: refused, naming the call, the '
        'goal and the position', () {
      expect(
          () => engine.postGoal('ask(bob, [yes], Asks)'),
          throwsA(isA<GoalRefused>().having(
              (e) => e.message,
              'message',
              _refusal('ask(bob, [yes], Asks)', 'ask',
                  'in the posted goal ask(bob, [yes], Asks)', '[yes]'))));
    });

    test('a constant at the output position of one conjunct: refused', () {
      expect(
          () => engine.postGoal('ask(bob, D, A1), ask(carol, [no], A2)'),
          throwsA(isA<GoalRefused>().having((e) => e.message, 'message',
              contains('The call ask(carol, [no], A2) of the volitional '
                  'procedure ask/2'))));
    });

    test('a writer at the output position: admitted', () async {
      final r = await engine.runGoal('ask(bob, D, Asks)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
    });
  });
}
