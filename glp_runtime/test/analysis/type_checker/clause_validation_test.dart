// glp_runtime/test/analysis/type_checker/clause_validation_test.dart
//
// GLP-Spec sections/glp.tex, Remark "Anonymous Variables" (297db75), and TGLP
// sections/full/typed-anonymous-variables.tex, "Anonymous variables"
// (2424f4c): "Anonymous readers (`_?`, `_Name?`) are not permitted in a
// clause, in the head or the body: a reader whose writer is never written is a
// goal that can never be satisfied, so a clause writes every writer it holds,
// closing a stream it does not extend.  The initial state of an agent
// (Section maGLP) is a goal, and the runtime writes its anonymous readers."
//
// From TGLP dddf684 until 2026-10-14 the checker admitted `_?` at a produced
// head position; it now refuses an anonymous reader at every position of
// every clause, whatever the position's mode.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_clause.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';

/// Type-check [source] as one module: its type definitions and procedure
/// declarations build the environment, its clauses are checked against it.
TypeCheckResult check(String source) {
  final module = Parser(Lexer(source).tokenize()).parseModule();
  final clauses = module.procedures.expand((p) => p.clauses).toList();
  return TypeChecker(buildTypeEnvironment(module)).check(clauses);
}

/// The refusal of the anonymous reader spelt [spelling].
String refusal([String spelling = '_?']) =>
    '$spelling (anonymous reader) is not permitted in program clauses';

void main() {
  group('anonymous readers in a clause', () {
    test('`_?` at a produced position of a clause head is refused', () {
      // InnerChannel's second field is InnerStream?, so under a consumed
      // InnerChannel? it is a produced position.
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
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal()));
    });

    test('`_?` at a produced head list tail is refused', () {
      final r = check('''
        Greeting ::= hello(String).
        GreetStream ::= [] ; [Greeting | GreetStream].
        procedure say(String?, GreetStream).
        say(Me, [hello(Me?) | _?]).
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal()));
    });

    test('`_?` at a produced head argument is refused', () {
      final r = check('''
        procedure give(Integer?, Integer).
        give(_, _?).
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal()));
    });

    test('the clause closing the stream it does not extend is accepted', () {
      // What the refused clause above is to write instead.
      final r = check('''
        Greeting ::= hello(String).
        GreetStream ::= [] ; [Greeting | GreetStream].
        procedure say(String?, GreetStream).
        say(Me, [hello(Me?)]).
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
      expect(r.errors.first.message, equals(refusal()));
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
      expect(r.errors.first.message, equals(refusal()));
    });

    test('`_?` in a guard is refused', () {
      final r = check('''
        procedure known_here(String?).
        known_here(_).
        procedure keep(String?).
        keep(X) :- known_here(_?) | true.
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal()));
    });

    test('`_Name?` at a produced position of a clause head is refused', () {
      final r = check('''
        Inner ::= probe(String?) ; hello.
        InnerStream ::= [] ; [Inner | InnerStream].
        InnerChannel ::= ch(InnerStream, InnerStream?).
        procedure read_inner(InnerStream?).
        read_inner([hello | S]) :- read_inner(S?).
        read_inner([]).
        procedure read_end(InnerChannel?).
        read_end(ch(In, _Out?)) :- read_inner(In?).
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal('_Out?')));
    });

    test('`_Name?` at a consumed position of a clause head is refused', () {
      final r = check('''
        Greeting ::= hello(String).
        GreetStream ::= [] ; [Greeting | GreetStream].
        procedure heard(GreetStream?).
        heard([hello(_W?) | _]).
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal('_W?')));
    });

    test('`_Name?` in a body goal is refused', () {
      final r = check('''
        Greeting ::= hello(String).
        GreetStream ::= [] ; [Greeting | GreetStream].
        procedure listen(GreetStream?).
        listen([]).
        procedure start(GreetStream?).
        start(S) :- listen(_In?).
      ''');
      expect(r.errors, isNotEmpty);
      expect(r.errors.first.message, equals(refusal('_In?')));
    });
  });

  group('the initial state of an agent', () {
    test('agent(ch(_?,_),ch(_?,_)) is a goal, and its `_?` is admitted', () {
      // GLP-Spec maGLP: "The initial state of all agents is the atomic goal
      // agent(ch(_?,_),ch(_?,_))", whose anonymous readers the runtime
      // writes.  It is checked as a goal ([checkGoal]), not validated as a
      // clause.
      const source = '''
        Msg ::= hello.
        Msgs ::= [] ; [Msg | Msgs].
        Channel ::= ch(Msgs, Msgs?).
        procedure agent(Channel?, Channel?).
        agent(ch(In, []), ch(Net, [])) :- drain(In?), drain(Net?).
        procedure drain(Msgs?).
        drain([hello | Xs]) :- drain(Xs?).
        drain([]).
      ''';
      final module = Parser(Lexer(source).tokenize()).parseModule();
      final env = buildTypeEnvironment(module);
      final clauses = module.procedures.expand((p) => p.clauses).toList();
      expect(TypeChecker(env).check(clauses).errors, isEmpty);

      final goal = Parser(Lexer("'_q_' :- agent(ch(_?, _), ch(_?, _)).")
              .tokenize())
          .parseModule()
          .procedures
          .single
          .clauses
          .single
          .body!;
      final r = checkGoal(goal, buildProgramDFA(env), env);
      expect(r.errors.map((e) => e.toString()),
          everyElement(isNot(contains('anonymous reader'))));
      expect(r.isWellTyped, isTrue);
    });
  });
}
