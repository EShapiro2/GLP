// glp_runtime/test/vglp/program_compilation_test.dart
//
// The canonical compilation end to end: a vGLP module to one self-contained
// GLP module.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation".

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/vglp/mediator.dart';
import 'package:glp_runtime/vglp/program_compilation.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setRootScopeEnvironmentSource;

const _programs = '../programs';

void main() {
  final rootSelfGlp = File('$_programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    setRootScopeEnvironmentSource(rootSelfGlp.readAsStringSync());
  }

  final mediator = MediatorSource.fromDirectory('$_programs/vglp');

  CompiledProgram compile(String source) => compileProgram(
      Parser(Lexer(source).tokenize(), vglp: true).parseModule(), mediator);

  group('one self-contained module', () {
    const src = '''
IntroChannel   ::= Channel(Stream(Constant), Stream(Constant)).
Response       ::= accept(IntroChannel) ; no.
Decision       ::= yes ; no.
ColdCallAnswer ::= response(Response?).
UserContent    ::= response(Response).
UserInMsg      ::= msg(Constant, Constant, UserContent)
                 ; decision(Decision, Constant, ColdCallAnswer).
UserInStream   ::= [] ; [UserInMsg | UserInStream].
ColdCallOffer  ::= offer(Constant).

procedure respond(ColdCallOffer?, Response, UserInStream).
*(Answer=yes, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true
*(no) true.
*(Answer=no, From?)
respond(offer(From), Resp?, [decision(Answer?, From?, response(Resp))]) :-
    ground(From?) | true.
''';

    test('it carries the source types, the added types and the mediator\'s',
        () {
      final out = compile(src).source;
      expect(out, contains('UserInStream ::='));       // the source's own
      expect(out, contains('Xs_respond_1 ::= xs_respond_1(Decision).'));  // added
      expect(out, contains('Answer ::= Xs_respond_1 ; Xs_respond_2.'));
      expect(out, contains('Reply_respond_1 ::= then(Xs_respond_1) ; else.'));
      expect(out, contains('Reply_respond_2 ::= then(Xs_respond_2).'));
      expect(out, contains('Escrow ::= esc_respond_1(Reply_respond_1?) ; '
          'esc_respond_2(Reply_respond_2?).'));
      expect(out, contains('UserAnswer ::= answer(ReqId, Answer).'));  // instantiated
    });

    test('no type parameter survives the emission', () {
      final out = compile(src).source;
      expect(out, isNot(contains('(A)')));
      expect(out, isNot(contains('(A, E, X)')));
      expect(out, isNot(contains('(E, X)')));
      expect(out, isNot(contains('PendingList(E)')));
    });

    test('every compiled procedure is declared immediately above its clauses',
        () {
      final out = compile(src).source;
      final lines = out.split('\n');
      final declLine =
          lines.indexWhere((l) => l.startsWith('procedure respond('));
      expect(declLine, greaterThan(-1));
      expect(lines[declLine + 1], startsWith('respond('));
    });

    test('the mediator comes with it, clauses and declarations', () {
      final out = compile(src).source;
      for (final p in ['med', 'timer', 'deadline', 'answer', 'close', 'drop',
          'med_split']) {
        expect(out, contains('procedure $p('), reason: '$p is missing');
      }
      // abort/3 stays generic in the slot's reply type.
      expect(out, contains('procedure(R) abort('));
    });

    test('the pending table\'s program clauses come ahead of the search clauses',
        () {
      final out = compile(src).source;
      final own = out.indexOf('answer(ReqId, xs_respond_1(X1), ');
      final search = out.indexOf('otherwise | answer(');
      expect(own, greaterThan(-1));
      expect(search, greaterThan(own));
      expect(out, contains('answer(ReqId, xs_respond_2(X1), '));
      // close only for the clause with an else-branch
      expect(out, contains('close(ReqId, [pending(Id, esc_respond_1(R?)) | Ps], Ps?)'));
      expect(out, isNot(contains('esc_respond_2(R?)) | Ps], Ps?) :- (ReqId? =?= Id?) | R = else')));
    });

    test('the slots are typed by their clauses\' reply types', () {
      final out = compile(src).source;
      expect(out, contains('Slot(Reply_respond_1)?, Slot(Reply_respond_2)?)'));
      expect(out, contains('Slot(R) ::= none ; ask(R, ReqId).'));
    });

    test('the emitted module parses back as GLP', () {
      final out = compile(src).source;
      final back = Parser(Lexer(out).tokenize()).parseModule();
      expect(back.procedures, isNotEmpty);
      // Nothing volition-guarded survives: GLP is vGLP without volition guards.
      for (final p in back.procedures) {
        for (final c in p.clauses) {
          expect(c.isVolitionGuarded, isFalse);
        }
      }
    });

    test('display declarations are carried through unchanged', () {
      final out = compile('''
$src
display respond *(Answer=yes, From?) : panel(inbox), label("Accept"), transient.
''');
      expect(out.source, contains('display respond'));
      expect(out.source, contains('label("Accept")'));
      // An atom is printed bare and a string literal keeps its quotes, as the
      // source had them: a declaration printed panel("inbox") would no longer
      // match its clause's guard.
      expect(out.source, contains('panel(inbox)'));
      expect(out.source, contains('*(Answer=yes, From?)'));
      expect(out.source, isNot(contains('"inbox"')));
      expect(out.source, isNot(contains('"yes"')));
    });
  });

  group('the deployed sources', () {
    // FIVE, and a list written from memory misses one: cssn/child_agent.vglp
    // arrived on 2026-08-16 and coins/coins_agent.vglp on 2026-09-03.  They
    // PARSE as vGLP.  coins/coins_agent.vglp also COMPILES and RUNS ---
    // Section N2 of test/run_all_tests.sh loads its emitted coins_agent.glp
    // and runs the village market; the others do not yet compile, the edits
    // being with their owners — social/graph and grassapp SGSG's, the two
    // cssn sources CSSN's.  Each parse test becomes the compilation test as
    // its source is repaired.
    for (final path in [
      'social/graph/agent.vglp',
      'grassapp/grassapp_agent.vglp',
      'cssn/agent.vglp',
      'cssn/child_agent.vglp',
      'coins/coins_agent.vglp',
    ]) {
      test('$path parses as vGLP', () {
        final file = File('$_programs/$path');
        expect(file.existsSync(), isTrue, reason: '$path is not on disc');
        final m = Parser(Lexer(file.readAsStringSync()).tokenize(), vglp: true)
            .parseModule();
        expect(m.procedures, isNotEmpty);
        expect(
            m.procedures
                .expand((p) => p.clauses)
                .any((c) => c.isVolitionGuarded),
            isTrue);
      });
    }
  });
}
