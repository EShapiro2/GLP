/// A call to an imported declaration that names type parameters is
/// instantiated at the call, as a local call is, and never checked at the
/// declaration's wildcard copy.
///
/// TGLP `modules.tex`, "Cross-module type checking": "Where the imported
/// declaration names type parameters, as an exported one may, the call
/// instantiates them as a local call does (Definition (Instantiation)): a
/// parameter the importing module holds open stays open across the module
/// boundary and is fixed at the call, the clauses of the called procedure being
/// those of the linked program."
///
/// Until 2026-09-27 `_checkRemoteGoal` checked `M # p(...)` against the wildcard
/// copy `env.procedures['M#p/n']`, so the body occurrence at a parameter
/// position was typed `_`: a forwarding clause that holds the parameter open
/// was refused against its own abstract type, and a call fixing the parameter
/// to one type while the clause hands out another was accepted.
library;

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

const _import = '''
Colour ::= red ; green ; blue.
imported procedure(Y) worker#tie(Y, Y?).
''';

void main() {
  group('a call to a parameterised import is instantiated at the call', () {
    test('a forwarding clause holding the parameter open checks', () {
      final result = checkSource('''
$_import
procedure(Y) fwd(Y, Y?).
fwd(A?, B) :- worker # tie(A, B?).
''');
      expect(result.errors.map((e) => e.message), isEmpty);
    });

    test('a call fixing the parameter to the clause\'s type checks', () {
      final result = checkSource('''
$_import
procedure go(Colour, Colour?).
go(A?, B) :- worker # tie(A, B?).
''');
      expect(result.errors.map((e) => e.message), isEmpty);
    });

    test('a call fixing the parameter to Colour where the head receives '
        'Integer is refused', () {
      final result = checkSource('''
$_import
procedure mix(Colour, Integer?).
mix(A?, B) :- worker # tie(A, B?).
''');
      final messages = result.errors.map((e) => e.message).toList();
      expect(messages, isNotEmpty);
      expect(
          messages.any((m) =>
              m.contains('Variable pair (B, B?)') && m.contains('Colour')),
          isTrue,
          reason: 'the pair B is refused at the instantiation Y = Colour, '
              'not at the wildcard: $messages');
    });
  });
}
