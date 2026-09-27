// glp_runtime/test/analysis/type_checker/abstract_route_test.dart
//
// The abstract instance is one way to certify a parameterised procedure, not
// the only one.
// Spec: TGLP (Moded-Types), sections/parameterized-types.tex, "Modular Checking
// via Abstract Parameters":
//
//   "A parametrically well-typed procedure is thus checked once and certified
//    for every instantiation ...  A procedure that inspects a parameter ... is
//    not parametrically well-typed, and is checked per instantiation. ...
//    Where a program contains a parameterised procedure that no call in it
//    instantiates and that is not parametrically well-typed, compilation
//    rejects the program."
//
// So a procedure whose abstract instance is not well-typed is checked at each
// instantiation its program makes, and refused only where there is none.
// Until 2026-09-27 a failed abstract instance was reported and the procedure
// refused whatever its instantiations ("Decision 1"), which refused
// `handle_response/6` of `programs/tests/agent_roundtrip`.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

// `checkSource` checks in the root scope's types alone, without the root
// `self.glp`, so `Stream` and `merge/3` are stated here as the root `self.glp`
// states them.
//
// `join/3` inspects no parameter, but it merges a stream of `AMsg` into its
// `Stream(X)`, so by its abstract instance the head occurrence of `As`
// (`AStream`) is not within what `merge/3` accepts there
// (`Stream<$abstract_X>`): it is not parametrically well-typed.  At `X = Msg`,
// `AStream` is within `Stream<Msg>`, and every clause is well-typed.
const _join = '''
Stream(X) ::= [] ; [X | Stream(X)].
Msg ::= a ; b.
AMsg ::= a.
AStream ::= [] ; [AMsg | AStream].
MsgStream ::= [] ; [Msg | MsgStream].

procedure(X) merge(Stream(X)?, Stream(X)?, Stream(X)).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], Ys, Ys?).
merge(Xs, [], Xs?).

procedure(X) join(AStream?, Stream(X)?, Stream(X)).
join(As, In, Out?) :- merge(In?, As?, Out).
''';

void main() {
  group('a procedure that fails its abstract instance', () {
    test('is checked at the instantiation its program makes, and passes there',
        () {
      final result = checkSource('''
$_join
procedure go(AStream?, MsgStream?, MsgStream).
go(As, In, Out?) :- join(As?, In?, Out).
''');
      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });

    test('is refused where no call instantiates it, and the refusal says why',
        () {
      final result = checkSource(_join);
      final messages = result.errors.map((e) => e.message).toList();
      expect(messages, hasLength(1));
      expect(messages.single, contains('join/3 is not parametrically well-typed'));
      expect(messages.single, contains('its abstract instance is not well-typed'));
      expect(messages.single,
          contains('no call in the program instantiates it'));
    });

    test('is refused at an instantiation that is not well-typed', () {
      // At `X = Integer` the head occurrence of `As` is not within
      // `Stream<Integer>`: checked there, the instantiation refuses it.
      final result = checkSource('''
$_join
IntStream ::= [] ; [Integer | IntStream].
procedure go(AStream?, IntStream?, IntStream).
go(As, In, Out?) :- join(As?, In?, Out).
''');
      expect(result.errors, isNotEmpty);
      expect(
          result.errors
              .where((e) => e.message.contains('parametrically well-typed')),
          isEmpty);
    });
  });

  test('a procedure that passes its abstract instance is certified uncalled',
      () {
    final result = checkSource('''
Stream(X) ::= [] ; [X | Stream(X)].
procedure(X) pdrain(Stream(X)?).
pdrain([]).
pdrain([_|Xs]) :- pdrain(Xs?).
''');
    expect(result.errors.map((e) => e.message).toList(), isEmpty);
  });
}
