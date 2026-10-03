// glp_runtime/test/analysis/type_checker/head_body_occurrences_test.dart
//
// Condition 3(b) of Definition (Well-Typed Clause) is checked for EVERY body
// occurrence of a variable whose pair is in the head, not the first alone.
// Spec: TGLP (Moded-Types), sections/well-typing.tex, Definition (Well-Typed
// Clause), condition 3: "For every variable pair X and X? in C ... (b) If one
// occurs in the head and the other in the body, they have the same type"
// (relaxed to subtyping, def:well-typed-clause-subtyping); and
// sections/typed-glp.tex, SRSW*: a reader of a constant type may occur more
// than once, its paired writer once.  Each such body occurrence is a pair with
// the head's.
//
// Until 2026-09-27 the checker kept the first body occurrence only
// (putIfAbsent), and a second at a type the head's is not within went
// unchecked.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setRootScopeEnvironmentSource;

const _decls = '''
Colour ::= red ; green.

procedure takes_colour(Colour?).
takes_colour(_).

procedure takes_int(Integer?).
takes_int(_).

procedure takes_constant(Constant?).
takes_constant(_).
''';

void main() {
  // The root self.glp is d_1 of every scope (TGLP modules.tex, Definition
  // "Root, Scope"), and Constant is its.  Until 2026-10-03 these sources were
  // checked in an empty root scope, Constant undefined, and passed only because
  // an undefined name in a declaration was read as a type parameter.
  setRootScopeEnvironmentSource(
      File('../programs/self.glp').readAsStringSync());

  group('a head variable read twice in the body', () {
    test('is refused where the second body occurrence is not within the head\'s '
        'type', () {
      final result = checkSource('''
$_decls
procedure p(Colour?).
p(C) :- takes_colour(C?), takes_int(C?).
''');
      final messages = result.errors.map((e) => e.message).toList();
      expect(
          messages.any((m) =>
              m.contains('Variable pair (C, C?)') &&
              m.contains('takes_int/1') &&
              m.contains('Colour') &&
              m.contains('Integer')),
          isTrue,
          reason: 'the second occurrence, at takes_int/1, is compared with the '
              'head: $messages');
    });

    test('is refused where the first body occurrence is the one out of type',
        () {
      final result = checkSource('''
$_decls
procedure p(Colour?).
p(C) :- takes_int(C?), takes_colour(C?).
''');
      expect(
          result.errors.map((e) => e.message).any((m) =>
              m.contains('Variable pair (C, C?)') && m.contains('takes_int/1')),
          isTrue);
    });

    test('loads where every body occurrence is within the head\'s type', () {
      final result = checkSource('''
$_decls
procedure p(Colour?).
p(C) :- takes_colour(C?), takes_constant(C?).
''');
      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });
  });
}
