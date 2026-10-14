// glp_runtime/test/analysis/union_inverted_argument_test.dart
//
// TGLP typed-glp.tex: "An alternative in a type definition may also be a type
// name S, providing type union for types with disjoint top-level functors";
// appendix-type-automaton.tex, "Type definition transitions": "If A_j
// references another type S or S?: transitions are inherited from S or S?
// respectively".  A term of the named type is read by the union as by S: an
// argument S declares mode-inverted keeps its mode under the union.
//
// Regression guard: the moded head (moded_head.dart, _getSubtermModes and
// _getListSubtermModes) found a term's constructor only among the union's own
// alternatives, so a term of a type the union names was given its parent's
// mode at every argument, and a mode-inverted argument of it disagreed with
// the automaton, which inherits S's transitions (program_dfa.dart,
// _addTypeTransitions): "No transition for probe(1,1):↓ from state Pg?"
// (Code #8's subagent, 2026-10-14, the opaque-types merge, a mini-app's own
// type the union of its root channel's messages and its person's items).

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart'
    show checkSource;
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;

String _errors(result) => result.errors.map((e) => e.message).join('\n');

void main() {
  final scope = rootScope(File('../programs/self.glp').absolute.path);

  group('a union naming a type with a mode-inverted argument', () {
    test('reads its term as the named type does, at a consumed position', () {
      const direct = '''
PV ::= value ; unreachable.
PM ::= probe(PV?) ; other.

procedure r(PM?, Integer).
r(probe(W?), 1) :- W = unreachable.
r(other, 2).
''';
      const union = '''
PV ::= value ; unreachable.
PM ::= probe(PV?).
Pg ::= PM ; other.

procedure r(Pg?, Integer).
r(probe(W?), 1) :- W = unreachable.
r(other, 2).
''';
      final d = checkSource(direct, ancestorScope: scope);
      expect(d.isWellTyped, isTrue, reason: _errors(d));
      final u = checkSource(union, ancestorScope: scope);
      expect(u.isWellTyped, isTrue, reason: _errors(u));
    });

    test('and inside a stream of the union', () {
      final u = checkSource('''
PV ::= value(String) ; unreachable.
PM ::= hello(String) ; probe(PV?).
U ::= opened(String) ; greeted(String).
Pg ::= PM ; out(Stream(U)) ; U.

procedure r(Stream(Pg)?, Integer).
r([probe(W?) | _], 1) :- W = unreachable.
r([_ | _], 2) :- otherwise | true.
r([], 0).
''', ancestorScope: scope);
      expect(u.isWellTyped, isTrue, reason: _errors(u));
    });

    test('the wrong mode at the inverted argument is refused as it is directly',
        () {
      final u = checkSource('''
PV ::= value ; unreachable.
PM ::= probe(PV?).
Pg ::= PM ; other.

procedure r(Pg?, Integer).
r(probe(W), 1) :- W? = unreachable.
r(other, 2).
''', ancestorScope: scope);
      expect(u.isWellTyped, isFalse);
      expect(_errors(u), contains('mode mismatch'));
    });
  });
}
