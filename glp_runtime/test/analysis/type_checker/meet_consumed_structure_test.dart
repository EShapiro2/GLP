// glp_runtime/test/analysis/type_checker/meet_consumed_structure_test.dart
//
// The meet of two consumed types with a structure alternative.
//
// TGLP typed-glp.tex, "Type checking of guards": "the meet of S and T---the
// type whose paths are the paths of both, again a type, since alternatives
// are distinguished by their top-level functor".
//
// Until 2026-11-11 the meet of `A?` and `B?`, where an alternative with
// arguments survives and the meet is neither type whole, had every argument
// of that alternative at the opposite mode: a path of neither (meet.dart,
// _rebuild; found by GLP #15 while adding Term).
library;

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/meet.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/subtyping.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';

TypeRef _ref(String name, {bool input = false}) =>
    TypeRef(name, 0, 0, isInput: input);

void main() {
  // A ::= f(Integer) ; g(Integer).   B ::= f(Integer) ; h.
  // C ::= f(Integer).                the meet of A and B, as a type
  TypeEnvironment env() => TypeEnvironment({
        'A': TypeDef('A', [
          StructAlt('f', [_ref('Integer')], 0, 0),
          StructAlt('g', [_ref('Integer')], 0, 0),
        ], 0, 0),
        'B': TypeDef('B', [
          StructAlt('f', [_ref('Integer')], 0, 0),
          ConstantAlt('h', 0, 0),
        ], 0, 0),
        'C': TypeDef('C', [
          StructAlt('f', [_ref('Integer')], 0, 0),
        ], 0, 0),
      }, {});

  test('the meet of two produced types keeps the argument produced', () {
    final e = env();
    final dfa = buildProgramDFA(e);
    final m = meetOfTypes(dfa.getState('A'), dfa.getState('B'), dfa, e)!;
    expect(m.isDual, isFalse);
    expect(
        dfa.getAutomaton(m.name).transition(
            m, TransitionLabel.functor('f', 1, 1, mode: Mode.produce)),
        dfa.getState('Integer'));
    expect(isSubtype(m, dfa.getState('C'), dfa), isTrue);
    expect(isSubtype(dfa.getState('C'), m, dfa), isTrue);
  });

  test('the meet of two consumed types keeps the argument consumed', () {
    final e = env();
    final dfa = buildProgramDFA(e);
    final m = meetOfTypes(dfa.getState('A?'), dfa.getState('B?'), dfa, e)!;
    expect(m.isDual, isTrue);
    final automaton = dfa.getAutomaton(m.name);
    expect(
        automaton.transition(
            m, TransitionLabel.functor('f', 1, 1, mode: Mode.consume)),
        dfa.getState('Integer?'));
    expect(
        automaton.transition(
            m, TransitionLabel.functor('f', 1, 1, mode: Mode.produce)),
        isNull);
    // The same type as C?, by its automaton.
    final out = dfa.getState(m.baseName);
    expect(isSubtype(out, dfa.getState('C'), dfa), isTrue);
    expect(isSubtype(dfa.getState('C'), out, dfa), isTrue);
  });

  test('a consumed type met with one above it is that type itself', () {
    final e = env();
    final dfa = buildProgramDFA(e);
    expect(meetOfTypes(dfa.getState('C?'), dfa.getState('A?'), dfa, e),
        dfa.getState('C?'));
    expect(meetOfTypes(dfa.getState('A?'), dfa.getState('C?'), dfa, e),
        dfa.getState('C?'));
  });
}
