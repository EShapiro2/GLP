// glp_runtime/test/analysis/type_checker/instantiation_types_test.dart
//
// A call to a parameterised procedure is checked by the expansion of its
// instantiation, even where that expansion names a type not yet built.
// Spec: TGLP (Moded-Types), sections/parameterized-types.tex, Definition
// (Instantiation):
//
//   "A map theta from those parameters to types of the program is an
//    instantiation of A if C and the clauses of q are well-typed when q's
//    declaration is replaced by its expansion under theta ..."
//
// C, the calling clause, is part of what the instantiation must make
// well-typed.  Until 2026-09-27 a call whose instantiation named a type not yet
// built --- `Stream<Msg>` below, which nothing but the call names --- was checked
// for its modes alone, and the calling clause was never checked again once the
// closure had built the type: typed_social_agent.glp's agent/4 handed
// handle_response/6 a Constant where it takes a Key, and loaded.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

// `checkSource` checks in the root scope's types alone, without the root
// `self.glp`, so `Stream` is stated here as the root `self.glp` states it.
// `MsgList` is a named list, `Stream<Msg>` by structural identity, so a call
// passing one fixes `X = Msg` and names `Stream<Msg>`, which no declaration
// names and so no expansion has built.
const _tagged = '''
Stream(X) ::= [] ; [X | Stream(X)].
Msg ::= a ; b.
MsgList ::= [] ; [Msg | MsgList].

procedure(X) tagged(Integer?, Stream(X)?, Stream(X)).
tagged(_, Xs, Xs?).
''';

void main() {
  group('a call whose instantiation names a type not yet built', () {
    test('is refused where the caller hands a position the wrong type', () {
      final result = checkSource('''
$_tagged
procedure go(String?, MsgList?, MsgList).
go(S, In, Out?) :- tagged(S?, In?, Out).
''');
      final messages = result.errors.map((e) => e.message).toList();
      expect(
          messages.any((m) =>
              m.contains('Variable pair (S, S?)') &&
              m.contains('String') &&
              m.contains('Integer')),
          isTrue,
          reason: 'the head receives String where tagged/3 takes Integer: $messages');
    });

    test('loads where the caller hands every position its type', () {
      final result = checkSource('''
$_tagged
procedure go(Integer?, MsgList?, MsgList).
go(N, In, Out?) :- tagged(N?, In?, Out).
''');
      expect(result.errors.map((e) => e.message).toList(), isEmpty);
    });

    test('is refused where the stream it passes is not of the instantiation',
        () {
      // X is fixed at Msg by the input stream; the output is a stream of
      // Integer, which is not within Stream<Msg>.
      final result = checkSource('''
$_tagged
IntList ::= [] ; [Integer | IntList].
procedure go(Integer?, MsgList?, IntList).
go(N, In, Out?) :- tagged(N?, In?, Out).
''');
      expect(result.errors, isNotEmpty);
    });
  });
}
