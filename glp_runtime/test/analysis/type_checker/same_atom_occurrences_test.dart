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
//
// Since 2026-10-14 no rule makes the occurrences one type at all (GLP,
// 2026-10-14 08:34 UTC: "TGLP has no rule that every occurrence of one
// variable has one type"): each occurrence is typed by its own position (TGLP
// well-typing.tex, Definition "Type Assignment"), and each is paired with the
// head's under condition 3(b) of Definition "Well-Typed Clause with
// Subtyping": "the dual of the type of the head occurrence is a subtype of
// the dual of the type of the body occurrence".  The second case is refused
// by 3(b), Colour not being below Shade.

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

    test('is refused at a type the head\'s is not within, by condition 3(b)',
        () {
      final result = checkSource('''
$_decls
procedure p(Colour?).
p(C) :- two_other(C?, C?).
''');
      final messages = result.errors.map((e) => e.message).toList();
      expect(
          messages.any((m) =>
              m.contains('Variable pair (C, C?) not dual across clause') &&
              m.contains('the head occurrence receives Colour, which is not '
                  'within what the body occurrence accepts (Shade)')),
          isTrue,
          reason: '$messages');
    });

    test('loads at two types the head\'s is within', () {
      final result = checkSource('''
$_decls
Wide ::= red ; green ; blue.
procedure two_wide(Colour?, Wide?).
two_wide(_, _).
procedure p(Colour?).
p(C) :- two_wide(C?, C?).
''');
      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });
  });
}
