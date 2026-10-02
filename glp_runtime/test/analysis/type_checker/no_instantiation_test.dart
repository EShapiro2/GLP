// glp_runtime/test/analysis/type_checker/no_instantiation_test.dart
//
// A call to a parameterised procedure that no instantiation makes well-typed
// is a type error.
// Spec: TGLP (Moded-Types), sections/parameterized-types.tex, Definition
// "Instantiation":
//
//   "A map theta from those parameters to types of the program is an
//    instantiation of A if C and the clauses of q are well-typed
//    (Definition "Well-Typed Clause") when q's declaration is replaced by its
//    expansion under theta, and every input path of that declaration is
//    accepted by some clause of q."
//
// and sections/well-typing.tex, Definition "Well-Typed Clause with Subtyping",
// condition 3, which relates each argument variable to its other occurrence in
// the calling clause.  A `Stream` handed where `OpenStream(X)?` or
// `NonEmptyList(X)?` is read carries `[]`, which no expansion of either has, so
// no map of `X` makes the call well-typed.
//
// Until 2026-10-02 a call whose instantiation the checker did not infer was
// checked for its modes alone, so these calls loaded: list_to_bst.glp:14 handed
// split_at/5 the Stream(X) its head reads (GLP 2026-10-02 02:03 UTC).  A call
// whose arguments fix no parameter where it stands, but which has an
// instantiation --- a fresh writer a later goal types, a subtype of the
// declared stream --- is not refused.

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';

// `checkSource` checks in the root scope's types alone, without the root
// `self.glp`, so the stream types and `merge/3` are stated here as the root
// `self.glp` states them (`NonEmptyList` as list_to_bst.glp states it).
const _streams = '''
Stream(X) ::= [] ; [X | Stream(X)].
OpenStream(X) ::= [X | Stream(X)].
NonEmptyList(X) ::= [X | Stream(X)].
''';

const _merge = '''
procedure(X) merge(Stream(X)?, Stream(X)?, Stream(X)).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], Ys, Ys?).
merge(Xs, [], Xs?).
''';

// The shape of list_to_bst.glp's split_at/5 call: the caller's head reads a
// Stream(X), and the callee takes a NonEmptyList(X)? there and its element at
// a bare X, which the caller fills from a `_` field of its own output.
const _pick = '''
BST ::= empty ; node(_, BST, BST).

procedure(X) top(Stream(X)?, BST).
top(Xs, node(Mid?, empty, empty)) :- pick(Xs?, Mid).

procedure(X) pick(NonEmptyList(X)?, X).
pick([X|_], X?).
''';

const _nonempty = '''
procedure(X) nonempty(OpenStream(X)?, String).
nonempty([_|_], yes).
''';

List<String> _messages(String source) =>
    checkSource(source).errors.map((e) => e.message).toList();

void main() {
  group('a call no instantiation makes well-typed is refused', () {
    test('a parametric caller handing the NonEmptyList(X)? the Stream(X) it '
        'reads (list_to_bst\'s split_at call)', () {
      final messages = _messages('$_streams$_pick');
      expect(messages, hasLength(1));
      expect(messages.single,
          contains('No instantiation of pick/2 for the call pick(Xs?, Mid)'));
      expect(messages.single,
          contains('argument 1, Xs?, holds Stream<\$abstract_X>, which no '
              'expansion of NonEmptyList(X)? accepts'));
      expect(messages.single, contains('Definition "Instantiation"'));
    });

    test('and at the instantiation a caller of the parametric caller makes',
        () {
      final messages = _messages('''
$_streams$_pick
procedure go(Stream(Integer)?, BST).
go(Xs, T?) :- top(Xs?, T).
''');
      expect(
          messages.where((m) => m.contains(
              'No instantiation of pick/2 for the call pick(Xs?, Mid): '
              'argument 1, Xs?, holds Stream<Integer>')),
          isNotEmpty);
    });

    test('a parametric caller handing a Stream(X) to OpenStream(X)?', () {
      final messages = _messages('''
$_streams$_nonempty
procedure(X) check(Stream(X)?, String).
check(Xs, R?) :- nonempty(Xs?, R).
''');
      expect(messages, hasLength(1));
      expect(
          messages.single,
          contains('No instantiation of nonempty/2 for the call '
              'nonempty(Xs?, R): argument 1, Xs?, holds '
              'Stream<\$abstract_X>, which no expansion of OpenStream(X)? '
              'accepts'));
    });

    test('a monomorphic caller handing a Stream(Integer) to OpenStream(X)?',
        () {
      final messages = _messages('''
$_streams$_nonempty
procedure check(Stream(Integer)?, String).
check(Xs, R?) :- nonempty(Xs?, R).
''');
      expect(messages, hasLength(1));
      expect(
          messages.single,
          contains('No instantiation of nonempty/2 for the call '
              'nonempty(Xs?, R): argument 1, Xs?, holds Stream<Integer>, '
              'which no expansion of OpenStream(X)? accepts'));
    });

    test('a writer whose head occurrence hands out less than every expansion '
        'produces', () {
      final messages = _messages('''
$_streams
procedure(X) produce(Stream(X)).
produce([]).

procedure out(OpenStream(Integer)).
out(Xs?) :- produce(Xs).
''');
      expect(messages, hasLength(1));
      expect(
          messages.single,
          contains('No instantiation of produce/1 for the call produce(Xs): '
              'argument 1, Xs, is to hold OpenStream<Integer>, and no '
              'expansion of Stream(X) is within it'));
    });
  });

  group('a call that has an instantiation is not refused', () {
    test('merge/3 of streams of integers', () {
      expect(
          _messages('''
$_streams$_merge
procedure join(Stream(Integer)?, Stream(Integer)?, Stream(Integer)).
join(Xs, Ys, Zs?) :- merge(Xs?, Ys?, Zs).
'''),
          isEmpty);
    });

    test('an OpenStream(Integer) handed where Stream(X)? is read, X fixed by '
        'nothing else', () {
      // OpenStream<Integer> is within Stream<Integer>: X = Integer is an
      // instantiation, although no equation of the call states it.
      expect(
          _messages('''
$_streams
procedure(X) len(Stream(X)?, Integer).
len([], 0).
len([_|Xs], N?) :- len(Xs?, M), N := M? + 1.

procedure count(OpenStream(Integer)?, Integer).
count(Xs, N?) :- len(Xs?, N).
'''),
          isEmpty);
    });

    test('a fresh writer a later goal types', () {
      // At fresh(Ys) nothing types Ys; consume/1 types it after, and
      // X = Integer is an instantiation.
      expect(
          _messages('''
$_streams
procedure(X) fresh(Stream(X)).
fresh([]).

procedure consume(Stream(Integer)?).
consume([]).
consume([_|Xs]) :- consume(Xs?).

procedure go.
go :- fresh(Ys), consume(Ys?).
'''),
          isEmpty);
    });
  });
}
