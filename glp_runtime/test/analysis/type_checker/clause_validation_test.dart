// glp_runtime/test/analysis/type_checker/clause_validation_test.dart
//
// TGLP sections/typed-glp.tex, "Anonymous variables" (SRSW Relaxations):
// "In a clause head, where a produced position carries an output placeholder
// rather than a writer, an anonymous variable is written `_?` there and
// denotes an output the clause never produces; complementation
// (Definition "Moded Head") makes it the fresh writer of the previous
// sentence.  An anonymous reader at a consumed position remains forbidden: a
// reader with no writer is a goal that can never be satisfied."
//
// Until TGLP dddf684 the checker refused `_?` in head, body and guard alike.
// The discriminator is the position's mode, not the spelling.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';

/// Type-check [source] as one module: its type definitions and procedure
/// declarations build the environment, its clauses are checked against it.
TypeCheckResult check(String source) {
  final module = Parser(Lexer(source).tokenize()).parseModule();
  final clauses = module.procedures.expand((p) => p.clauses).toList();
  return TypeChecker(buildTypeEnvironment(module)).check(clauses);
}

/// The one message the refusal carries, as it has carried it all along.
const refusal = '_? (anonymous reader) is not permitted in program clauses';

void main() {
  group('anonymous readers', () {
    test('`_?` at a produced position of a clause head is accepted', () {
      // InnerChannel's second field is InnerStream?, so under a consumed
      // InnerChannel? it is a produced position: the output placeholder the
      // clause never produces.
      final r = check('''
        Inner ::= probe(String?) ; hello.
        InnerStream ::= [] ; [Inner | InnerStream].
        InnerChannel ::= ch(InnerStream, InnerStream?).
        procedure read_inner(InnerStream?).
        read_inner([hello | S]) :- read_inner(S?).
        read_inner([]).
        procedure read_end(InnerChannel?).
        read_end(ch(In, _?)) :- read_inner(In?).
      ''');
      expect(r.errors, isEmpty);
      expect(r.isWellTyped, isTrue);
    });

    test('`_?` at a produced position of a head list tail is accepted', () {
      final r = check('''
        Greeting ::= hello(String).
        GreetStream ::= [] ; [Greeting | GreetStream].
        procedure say(String?, GreetStream).
        say(Me, [hello(Me?) | _?]).
      ''');
      expect(r.errors, isEmpty);
      expect(r.isWellTyped, isTrue);
    });

    test('`_?` at a consumed position of a clause head is refused', () {
      final r = check('''
        Greeting ::= hello(String).
        GreetStream ::= [] ; [Greeting | GreetStream].
        procedure heard(GreetStream?).
        heard([hello(_?) | _]).
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal));
    });

    test('`_?` in a body goal is refused', () {
      final r = check('''
        Greeting ::= hello(String).
        GreetStream ::= [] ; [Greeting | GreetStream].
        procedure listen(GreetStream?).
        listen([]).
        procedure start(GreetStream?).
        start(S) :- listen(_?).
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal));
    });

    test('`_?` in a guard is refused', () {
      final r = check('''
        procedure known_here(String?).
        known_here(_).
        procedure keep(String?).
        keep(X) :- known_here(_?) | true.
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal));
    });

    test('a named anonymous reader at a consumed position is refused', () {
      final r = check('''
        Greeting ::= hello(String).
        GreetStream ::= [] ; [Greeting | GreetStream].
        procedure heard(GreetStream?).
        heard([hello(_W?) | _]).
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message,
          equals('_W? (anonymous reader) is not permitted in program clauses'));
    });
  });
}
