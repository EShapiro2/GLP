// glp_runtime/test/analysis/type_checker/body_body_occurrences_test.dart
//
// Condition 3(a) of Definition (Well-Typed Clause) is checked for EVERY body
// occurrence of a reader whose writer is in the body, not the first alone.
// Spec: TGLP (Moded-Types), sections/well-typing.tex, Definition (Well-Typed
// Clause), condition 3: "For every variable pair X and X? in C: (a) If both
// occur in the head, or both occur in the body, their types are dual"
// (relaxed to subtyping for body/body pairs, def:well-typed-clause-subtyping);
// and sections/typed-glp.tex, SRSW*: a reader of a constant type may occur more
// than once, its paired writer once.  Each such body occurrence is a pair with
// the writer's.
//
// Until 2026-09-29 the checker kept the first body occurrence of each key only
// (putIfAbsent), and a second at a type the writer's is not within went
// unchecked; condition 3(b) had the same fault until 2026-09-27
// (head_body_occurrences_test.dart).  A goal is a body, and is checked alike.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

const _decls = '''
Colour ::= red ; green.

procedure gives_colour(Colour).
gives_colour(red).

procedure takes_colour(Colour?).
takes_colour(_).

procedure takes_int(Integer?).
takes_int(_).

procedure takes_constant(Constant?).
takes_constant(_).
''';

bool _refusesPair(List<String> messages, String v) => messages.any((m) =>
    m.contains('Variable pair ($v, $v?)') &&
    m.contains('Colour') &&
    m.contains('Integer'));

void main() {
  group('a body writer read twice in the body', () {
    test('is refused where the second reader occurrence is out of type', () {
      final result = checkSource('''
$_decls
procedure p.
p :- gives_colour(C), takes_colour(C?), takes_int(C?).
''');
      final messages = result.errors.map((e) => e.message).toList();
      expect(_refusesPair(messages, 'C'), isTrue,
          reason: 'the second reader, at takes_int/1, is compared with the '
              'writer at gives_colour/1: $messages');
    });

    test('is refused where the first reader occurrence is out of type', () {
      final result = checkSource('''
$_decls
procedure p.
p :- gives_colour(C), takes_int(C?), takes_colour(C?).
''');
      expect(
          _refusesPair(result.errors.map((e) => e.message).toList(), 'C'),
          isTrue);
    });

    test('loads where every reader occurrence is within the writer\'s type',
        () {
      final result = checkSource('''
$_decls
procedure p.
p :- gives_colour(C), takes_colour(C?), takes_constant(C?).
''');
      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });
  });
}
