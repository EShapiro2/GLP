// glp_runtime/test/analysis/type_checker/guard_meet_test.dart
//
// A guard atom narrows the head occurrence it tests.
// Spec: TGLP (Moded-Types), sections/typed-glp.tex, "Type checking of guards":
//
//   "A guard atom that tests the type of a head occurrence narrows it.  Let S be
//    the type of the occurrence and T the type declared for the position it
//    occupies in the guard.  The guard atom is well-typed if the MEET of S and T
//    --- the type whose paths are the paths of both, again a type, since
//    alternatives are distinguished by their top-level functor --- is non-empty,
//    and the occurrence has that meet as its type in the body, where condition 3
//    of Definition (Well-Typed Clause) is applied to it.  Otherwise a guard that
//    discriminates a union is unwritable."

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/well_typed_clause.dart'
    show constantTypedVariables;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';

void main() {
  group('a guard narrows the head occurrence it tests', () {
    test('the verify_install / module shape loads, M being Module in the body',
        () {
      // `module(M?)` at an argument of type `Content ::= String ; Module` tests
      // the case its clause is for: the meet is `Module?`, and it is `Module?`
      // that condition 3(b) compares against the body's `Module?`.  Asking
      // `Content <: Module` instead refuses the clause.
      final result = checkSource('''
Content ::= String ; Module.

procedure is_module(Module?).
is_module(_) :- true.

procedure use_module(Module?, Module).
use_module(M, M?).

procedure verify_install(Content?, Module).
verify_install(M, V?) :- is_module(M?) | use_module(M?, V).
''');

      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });

    test('a guard on a type wider than the occurrence narrows nothing', () {
      // A guard declared over a union at an `Integer?` occurrence: the meet is
      // `Integer?`, the narrower of the two, so the body still sees `Integer?`.
      final result = checkSource('''
Const ::= Integer ; Real ; String.

procedure a_constant(Const?).
a_constant(_) :- true.

procedure takes_integer(Integer?).
takes_integer(_) :- true.

procedure k(Integer?).
k(K) :- a_constant(K?) | takes_integer(K?).
''');

      expect(
          result.errors
              .where((e) => e.message.contains('not dual across clause'))
              .toList(),
          isEmpty);
    });

    test('a guard whose meet with the occurrence is empty is refused', () {
      // `String?` and `Integer?` share no path, so no term satisfies both: the
      // guard can never succeed and the clause is not well-typed.
      final result = checkSource('''
procedure an_integer(Integer?).
an_integer(0).

procedure a_string(String?, String).
a_string(S, S?).

procedure bad(String?, String).
bad(S, T?) :- an_integer(S?) | a_string(S?, T).
''');

      expect(result.errors.any((e) => e.message.contains('the meet is empty')),
          isTrue,
          reason: result.errors.map((e) => e.message).join('\n'));
    });
  });

  // TGLP typed-glp.tex, "Type checking of guards" (TGLP 30d8ac2): "A negated
  // guard narrows nothing.  ~g succeeds where g fails, which tells what the
  // value is not, and the type a clause may rely on is what it is; so the
  // occurrence keeps its type in the body, and the guard atom is well-typed
  // whatever the tested type, an empty meet meaning that the guard succeeds
  // always rather than that the clause is ill-typed."
  group('a negated guard narrows nothing', () {
    test('~g on an occurrence whose meet with g\'s type is empty is well-typed',
        () {
      // `Integer?` and `Module?` share no term: `is_module(X?)` would be
      // refused, and `~is_module(X?)` succeeds always.  The module_guard.glp
      // shape (A28, B), which was refused as an empty meet; `Verdict` stands
      // for its `Constant`, which is the root scope's and not in scope here.
      final result = checkSource('''
Verdict ::= not_module.

procedure is_module(Module?).
is_module(_) :- true.

procedure test_not_module(Integer?, Verdict).
test_not_module(X, not_module) :- ~is_module(X?) | true.
''');

      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });

    test('the occurrence keeps its type in the body', () {
      // `~is_module(M?)` tells that M is not a Module, not what it is: M is
      // still `Content?` in the body, where `use_content` accepts it.
      final result = checkSource('''
Content ::= String ; Module.

procedure is_module(Module?).
is_module(_) :- true.

procedure use_content(Content?, Content).
use_content(C, C?).

procedure pass_on(Content?, Content).
pass_on(M, V?) :- ~is_module(M?) | use_content(M?, V).
''');

      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });

    test('a negated guard does not narrow to the case the clause excludes', () {
      // Narrowing as if `~is_module(M?)` held positively gives M the type
      // `Module?` in the body --- the very case this clause is NOT for --- and
      // `use_module` would accept it.  Kept at `Content?`, condition 3(b)
      // refuses it: the head receives `Content`, which is not within `Module`.
      final result = checkSource('''
Content ::= String ; Module.

procedure is_module(Module?).
is_module(_) :- true.

procedure use_module(Module?, Module).
use_module(M, M?).

procedure wrong(Content?, Module).
wrong(M, V?) :- ~is_module(M?) | use_module(M?, V).
''');

      expect(
          result.errors.any((e) =>
              e.message.contains('Variable pair (M, M?)') &&
              e.message.contains('Content')),
          isTrue,
          reason: result.errors.map((e) => e.message).join('\n'));
      expect(result.errors.any((e) => e.message.contains('the meet is empty')),
          isFalse);
    });

    test('a negated guard gives no occurrence a constant type', () {
      // The relaxation "Readers of constant types" is asked of the type each
      // occurrence has, a guard's off the guard atom.  A positive guard at an
      // `Integer?` position gives X that type; a negated one tells only that X
      // is not an Integer, and licenses nothing.
      Set<String> constantIn(String clauseSource) {
        final module = Parser(Lexer('''
Pair ::= p(Integer, Integer) ; q.
procedure an_integer(Integer?).
an_integer(_) :- true.
procedure twice(Pair?, Pair, Pair).
$clauseSource
''').tokenize()).parseModule();
        final env = buildModuleTypeEnvironment(module);
        final dfa = buildProgramDFA(env);
        final clause = module.procedures
            .singleWhere((p) => p.name == 'twice')
            .clauses
            .single;
        return constantTypedVariables(clause, dfa, env);
      }

      expect(constantIn('twice(X, X?, X?) :- an_integer(X?) | true.'),
          contains('X'));
      expect(constantIn('twice(X, X?, X?) :- ~an_integer(X?) | true.'),
          isNot(contains('X')));
    });
  });
}
