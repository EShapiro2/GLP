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
}
