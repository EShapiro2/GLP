// glp_runtime/test/analysis/type_checker/abstract_type_certification_test.dart
//
// A parameterised procedure is certified by its abstract instance, its clauses
// well-typed with subtyping, an abstract type below itself alone.
//
// Specification: TGLP parameterized-types.tex, Section "Modular Checking via
// Abstract Parameters":
//
//   Definition "Abstract Type": "An abstract type is a type-automaton state
//   with no outgoing transitions, distinct from the wildcard _.  As the
//   endpoint of a simple prefix it matches only itself (Definition Prefix
//   Acceptance), and not the wildcard."
//
//   Definition "Parametrically Well-Typed": "The abstract instance of a
//   parameterised procedure declaration replaces each type parameter by a
//   distinct abstract type.  A parameterised procedure is parametrically
//   well-typed if, by its abstract instance, its clauses are well-typed with
//   subtyping (Definition "Well-Typed Clause with Subtyping") and every input
//   path is accepted by some clause."
//
// So by Definition "Subtyping" an abstract type is a subtype of itself and of
// nothing else --- not of `_`, not of another abstract type --- and nothing
// else is a subtype of it; a concrete pair is related by subtyping, so `:=`
// arithmetic on an `Integer` variable certifies, `Integer` being below `Exp`.
// Fixtures: programs/tests/strict_certification/.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart'
    show rootScope, mergeSelfGlpFileIntoScope;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final _root = File('../programs/self.glp').absolute.path;
final _programs = Directory('../programs').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/strict_certification/$name').absolute.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root);

String _refusal(String name) {
  try {
    _engine().loadFile(_fixture(name));
  } catch (e) {
    return e.toString();
  }
  return '';
}

// `checkSource` checks in the root scope's types alone, without the root
// `self.glp`, so `Stream` is stated here as the root states it.
const _stream = 'Stream(X) ::= [] ; [X | Stream(X)].\n';

const _note = '''
Msg ::= m(String).
procedure note(Msg?, Stream(Msg)).
note(X, [X?]).
procedure(T) id(T?, T).
id(X, X?).
''';

List<String> _errors(String source) =>
    checkSource('$_stream$source').errors.map((e) => e.message).toList();

void main() {
  group('the abstract instance, well-typed with subtyping', () {
    test('a body pair handing the parameter to a concrete type: not '
        'parametrically well-typed', () {
      // B is a writer at id/2's T = M and B? a reader at note/2's Msg?: the
      // abstract type M is below no concrete type.
      final errors = _errors('''
$_note
procedure(M) fwd2(M?, Stream(Msg)).
fwd2(A, Out?) :- id(A?, B), note(B?, Out).
''');
      expect(errors, hasLength(1));
      expect(errors.single,
          contains('fwd2/2 is not parametrically well-typed'));
      expect(errors.single,
          contains(r'writer type $abstract_M is not a subtype of Msg'));
    });

    test('a head/body pair handing the parameter to a concrete type: not '
        'parametrically well-typed', () {
      // A in the head at M?, A? in note/2's goal at Msg?.
      final errors = _errors('''
$_note
procedure(M) fwd(M?, Stream(Msg)).
fwd(A, Out?) :- note(A?, Out).
''');
      expect(errors, hasLength(1));
      expect(errors.single, contains('fwd/2 is not parametrically well-typed'));
      expect(errors.single, contains(r'receives $abstract_M'));
    });

    test('two parameters are two distinct abstract types', () {
      // The head pairs a reader at X with its writer at Y.
      final errors = _errors('''
procedure(X, Y) swap(X?, Y).
swap(A, A?).
''');
      expect(errors, hasLength(1));
      expect(errors.single, contains('swap/2 is not parametrically well-typed'));
    });

    test('a parameter is not below the wildcard: a variable at X handed '
        'where the callee declares `_`, not certified, with the message', () {
      final errors = _errors('''
procedure sink(_?).
sink(_).
procedure(X) pass(X?).
pass(A) :- sink(A?).
''');
      expect(errors, hasLength(1));
      expect(errors.single, contains('pass/1 is not parametrically well-typed'));
      expect(
          errors.single,
          contains(r'the head occurrence receives $abstract_X, which is not '
              'within what the body occurrence accepts (_)'));
      expect(errors.single, contains('compilation rejects the program'));
    });

    test('a concrete type handed where the callee declares `_` is below it: '
        'certified', () {
      expect(_errors('''
procedure sink(_?).
sink(_).
procedure(X) pass_len(Stream(X)?, Integer?).
pass_len(_, N) :- sink(N?).
'''), isEmpty);
    });

    test('`:=` arithmetic on a variable of a concrete type: certified, '
        '`Integer` being below `Exp`', () {
      final scope = rootScope(_root);
      expect(checkSource('''
procedure(X) count(Stream(X)?, Integer?, Integer).
count([], A, A?).
count([_|Xs], A, N?) :- A1 := A? + 1, count(Xs?, A1?, N).
''', ancestorScope: scope).errors.map((e) => e.message), isEmpty);
    });

    test('a procedure handing its parameter through is certified', () {
      expect(_errors('''
procedure(T) id(T?, T).
id(X, X?).
procedure(T) relay2(T?, T).
relay2(A, B?) :- id(A?, B).
'''), isEmpty);
    });

    test('a procedure the abstract route does not certify is checked at the '
        'instantiation its program makes, and passes there', () {
      expect(_errors('''
$_note
procedure(M) fwd2(M?, Stream(Msg)).
fwd2(A, Out?) :- id(A?, B), note(B?, Out).
procedure go(Msg?, Stream(Msg)).
go(X, Out?) :- fwd2(X?, Out).
'''), isEmpty);
    });
  });

  group("sGLP's length/3 and nth/3 (programs/sglp/self.glp)", () {
    final scope = mergeSelfGlpFileIntoScope(
        rootScope(_root), File('../programs/sglp/self.glp').absolute.path,
        root: _programs);
    for (final key in ['length/3', 'nth/3']) {
      test('$key is parametrically well-typed', () {
        // Defined with clauses by sglp/self.glp's own layer, so the answer is
        // its certification and not the default for a key outside the scope.
        expect(scope.paramProcDecls.containsKey(key), isTrue);
        expect(scope.scopeLayers[key]?.clauses[key], isNotEmpty);
        expect(scopeProcedureIsParametric(scope, key), isTrue);
      });
    }
  });

  group('a call leaving a parameter open', () {
    test('to a procedure relating it to a concrete type: refused', () {
      final err = _refusal('cert_open.glp');
      expect(err, contains('No instantiation of fwd2/2 is found for the call'));
      expect(err, contains('fwd2/2 is not parametrically well-typed'));
    });

    test('to a procedure handing one parameter to another: refused', () {
      final err = _refusal('cert_cast.glp');
      expect(err, contains('No instantiation of cast/2 is found for the call'));
      expect(err, contains('cast/2 is not parametrically well-typed'));
    });

    test('a parameter handed to `_`, uninstantiated: refused', () {
      final err = _refusal('cert_wild.glp');
      expect(err, contains('pass/1 is not parametrically well-typed'));
      expect(err, contains('compilation rejects the program'));
    });

    test('to a procedure handing it through: checked open, and run', () async {
      final engine = _engine()..loadFile(_fixture('cert_kept.glp'));
      final r = await engine.runGoal('go(X)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      final x = engine.runtime.heap.dereference(r.bindings['X']!);
      expect(x, isA<rt.ConstTerm>());
      expect((x as rt.ConstTerm).value, 5);
    });
  });
}
