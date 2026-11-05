// glp_runtime/test/compiler/one_goal_reader_test.dart
//
// One reader of an atom, in a head, a guard and a body alike (GLP-Spec
// appendix-lp.tex, Definition "Logic Programs Syntax"; GLP #11 Cowork,
// Code_inbox 2026-11-03 16:40 UTC, item 7): `:=`, `=..`, `..=` and `=` are
// atoms with infix functors, read the same in every position, and which calls
// a guard admits is checked after reading, not a subset of the reader's.  A
// clause head is held to an atom.  Until 2026-11-05 three readers, one per
// position, admitted different subsets: `_ := E` in a head only, `f(a) =.. L`
// in a head and a body only, `X? < Y?` in a guard only.

import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:test/test.dart';

Clause _clause(String source) =>
    Parser(Lexer(source).tokenize()).parseModule().procedures.single.clauses.single;

CompileError _refusal(String source) {
  try {
    Parser(Lexer(source).tokenize()).parseModule();
  } on CompileError catch (e) {
    return e;
  }
  fail('parsed: $source');
}

void main() {
  group('the infix functors read the same in every position', () {
    test('f(a) =.. L in a head, a guard and a body', () {
      final head = _clause('f(a) =.. L.').head;
      expect(head.functor, '=..');
      expect(head.args[0], isA<StructTerm>());
      final guard = _clause('p(L) :- f(a) =.. L | true.').guards!.single;
      expect(guard.predicate, '=..');
      expect(guard.args[0], isA<StructTerm>());
      final body = _clause('p(L) :- f(a) =.. L.').body!.single;
      expect(body.functor, '=..');
      expect(body.args[0], isA<StructTerm>());
    });

    test('_ := E in a guard and a body (a clause begins with a variable, a reader, an atom or procedure, which the module reader holds, so no clause has it as its head)', () {
      for (final source in ['p :- _ := 1 + 2 | true.', 'p :- _ := 1 + 2.']) {
        final c = _clause(source);
        final atom = c.guards != null ? c.guards!.single.predicate : c.body!.single.functor;
        final args = c.guards != null ? c.guards!.single.args : c.body!.single.args;
        expect(atom, ':=', reason: source);
        expect(args[0], isA<UnderscoreTerm>(), reason: source);
      }
    });

    test('X = a in a guard and a body, and X? < 3 in a body as in a guard', () {
      expect(_clause('p(X) :- X = a | true.').guards!.single.predicate, '=');
      expect(_clause('p(X) :- X = a.').body!.single.functor, '=');
      expect(_clause('p(X) :- X? < 3 | true.').guards!.single.predicate, '<');
      expect(_clause('p(X) :- X? < 3.').body!.single.functor, '<');
    });
  });

  group('a clause head is an atom', () {
    for (final (what, source) in [
      ('a cross-module call', 'm # p.'),
      ('a spawn', 'p @ alice.'),
    ]) {
      test('$what is refused as a head', () {
        expect(_refusal(source).message, contains('A clause head is an atom'));
      });
    }
  });
}
