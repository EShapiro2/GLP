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
      expect(out, contains('Xs_respond_1 ::= xs(Decision).'));  // added
      expect(out, contains('Answer ::= Xs_respond_1 ; Xs_respond_2.'));
      expect(out, contains('Reply ::= then(Answer) ; else.'));  // instantiated
    });

    test('no type parameter survives the emission', () {
      final out = compile(src).source;
      expect(out, isNot(contains('(A)')));
      expect(out, isNot(contains('(A, X)')));
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
      for (final p in ['med', 'timer', 'deadline', 'lookup', 'aborts',
          'med_split', 'grant_then', 'grant_else']) {
        expect(out, contains('procedure $p('), reason: '$p is missing');
      }
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
    });
  });

  group('the three deployed sources', () {
    // They PARSE as vGLP.  They do not yet COMPILE: each carries volition
    // guards the compilation cannot type, reported to their owners —
    // social/graph and grassapp to SGSG, cssn to CSSN.  Two causes: a guard
    // naming a writer whose reader occurs nowhere in the clause, which
    // Definition "Guarded Clause, ..." does not admit and whose abbreviation
    // is *(yes, Other?); and, in grassapp, a declaration over Stream(X) with
    // no parameter list, so the answer writer sits at an abstract position.
    // The compilation of all three goes in here when the sources are repaired.
    for (final path in [
      'social/graph/agent.vglp',
      'grassapp/grassapp_agent.vglp',
      'cssn/agent.vglp',
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
