// glp_runtime/test/analysis/type_checker/same_atom_occurrences_test.dart
//
// Two occurrences of one variable in one atom are compared by their type
// automata, not by the names of their types.  Type identity is structural:
// "two types with the same automaton bind the parameter consistently whatever
// their names or defining modules, and two types with different automata
// conflict however alike their names" (TGLP parameterized-types.tex, after
// Definition (Instantiation)).
//
// Until 2026-09-29 the checker required the two occurrences to carry the same
// type NAME (InconsistentVariableError), and refused a reader of a constant
// type read twice in one call at two names of one type.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

const _decls = '''
Colour ::= red ; green.
Hue ::= red ; green.
Shade ::= red ; blue.

procedure two_same(Colour?, Hue?).
two_same(_, _).

procedure two_other(Colour?, Shade?).
two_other(_, _).
''';

void main() {
  group('a reader read twice in one atom', () {
    test('loads at two names of one type', () {
      final result = checkSource('''
$_decls
procedure p(Colour?).
p(C) :- two_same(C?, C?).
''');
      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });

    test('is refused at two types with different automata', () {
      final result = checkSource('''
$_decls
procedure p(Colour?).
p(C) :- two_other(C?, C?).
''');
      final messages = result.errors.map((e) => e.message).toList();
      expect(
          messages.any((m) =>
              m.contains('Variable C? has inconsistent types') &&
              m.contains('Colour?') &&
              m.contains('Shade?')),
          isTrue,
          reason: '$messages');
    });
  });
}
