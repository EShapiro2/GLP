// glp_runtime/test/analysis/type_checker/call_instantiation_order_test.dart
//
// The body goals in no order: a clause's verdict does not depend on the order
// of its goals.
//
// Specification: TGLP (Moded-Types) cc4a891, appendix-implementation-notes.tex,
// "The instantiation of a call": "Definition~\ref{def:instantiation} asks that
// an instantiation exist and orders nothing; the checker reads the sites of a
// call over the whole clause, the body goals in no order, ...".  GLP's task of
// 2026-10-02 13:38 UTC.  The engine-run cases are in call_instantiation_test.dart.
//
// Until 2026-10-02 a call's instantiation was inferred from the occurrences
// typed before it, so copy/2 below, whose output is typed only by the merge
// after it, had no instantiation in every order that put it first.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

// `checkSource` checks in the root scope's types alone, without the root
// `self.glp`, so the stream type and merge/3 are stated here as the root
// `self.glp` states them.
const _decls = '''
Stream(X) ::= [] ; [X | Stream(X)].
procedure(X) merge(Stream(X)?, Stream(X)?, Stream(X)).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], Ys, Ys?).
merge(Xs, [], Xs?).

Msg ::= msg(Integer).
procedure(FM, NIM) copy(Stream(FM)?, Stream(NIM)).
copy([msg(K)|Xs], [msg(K?)|Ys?]) :- copy(Xs?, Ys).
copy([], []).
procedure src(Stream(Msg)).
src([]).
procedure snk(Stream(Msg)?).
snk([]).
snk([_|Xs]) :- snk(Xs?).
''';

List<List<String>> _orders(List<String> xs) => xs.length <= 1
    ? [xs]
    : [
        for (var i = 0; i < xs.length; i++)
          for (final rest in _orders([...xs]..removeAt(i))) [xs[i], ...rest]
      ];

List<String> _messages(String source) =>
    checkSource(source).errors.map((e) => e.message).toList();

void main() {
  test('every order of a body is well-typed: copy\'s input typed by the goal '
      'before or after it, its output by the merge', () {
    const goals = ['src(S)', 'copy(S?, T)', 'merge(T?, [], U)', 'snk(U?)'];
    for (final order in _orders(goals)) {
      final source = '$_decls\nprocedure go.\ngo :- ${order.join(', ')}.\n';
      expect(_messages(source), isEmpty, reason: order.join(', '));
    }
  });

  test('every order of a body is refused alike where copy\'s input is a '
      'constructed term: no site supplies FM and copy\'s clauses fix none', () {
    const goals = ['copy([msg(1)], T)', 'merge(T?, [], U)', 'snk(U?)'];
    final verdicts = <String>{};
    for (final order in _orders(goals)) {
      final source = '$_decls\nprocedure go.\ngo :- ${order.join(', ')}.\n';
      final messages = _messages(source);
      expect(
          messages.where((m) => m.contains(
              'No instantiation of copy/2 is found for the call '
              'copy([msg(1)|[]], T): no site of the call supplies a type for '
              'FM')),
          isNotEmpty,
          reason: order.join(', '));
      verdicts.add(messages.length.toString());
    }
    expect(verdicts, hasLength(1));
  });
}
