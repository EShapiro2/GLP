import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  test('SRSW violation: repeated variable should be rejected', () {
    print('\nTesting SRSW violation: same(f(X, X))');

    final compiler = GlpCompiler();

    expect(() => compiler.compile('same(f(X, X)).'), throwsException);
    print('✅ Correctly rejected repeated variable');
  });

  test('Anonymous variable _ in head argument compiles without SRSW error', () {
    print('\nTesting anonymous variable in head argument');

    final compiler = GlpCompiler();

    // _ as a writer argument with no reader should compile without SRSW error
    final source = '''
procedure foo(_?, _).
foo(X, _) :- ground(X?) | true.
''';

    final program = compiler.compile(source);
    expect(program, isNotNull);
    expect(program.ops.length, greaterThan(0));
    print('✅ Anonymous variable _ compiles correctly');
    print('   Generated ${program.ops.length} instructions');
  });

  test('Anonymous variable _ passes SRSW where named variable would fail', () {
    print('\nTesting _ vs named variable in head');

    final compiler = GlpCompiler();

    // This should FAIL - Result has no reader
    final badSource = '''
procedure foo(_?, _).
foo(X, Result) :- ground(X?) | true.
''';

    expect(() => compiler.compile(badSource), throwsException,
        reason: 'Result with no reader should fail SRSW');
    print('✅ Named variable correctly rejected (no reader)');

    // This should PASS - _ has no SRSW requirements
    final goodSource = '''
procedure foo(_?, _).
foo(X, _) :- ground(X?) | true.
''';

    final program = compiler.compile(goodSource);
    expect(program, isNotNull);
    print('✅ _ correctly accepted (anonymous)');
  });

  test('SRSW rejects guard-only readers without groundness', () {
    print('\nTesting SRSW rejects guard-only readers without groundness');

    final compiler = GlpCompiler();

    // This should FAIL - X only appears in guard that doesn't imply groundness
    // (known/1 checks if bound, but doesn't guarantee ground for SRSW purposes)
    // Actually, let's use a custom guard that doesn't mark ground
    // The simplest case: otherwise doesn't ground anything
    final badSource = '''
foo(X) :- otherwise | bar.
''';

    expect(() => compiler.compile(badSource), throwsException,
        reason: 'otherwise does not ground X, so X has no reader');
    print('✅ Guard-only readers without groundness correctly rejected');
  });

  // ===========================================================================
  // Readers of constant types (TGLP typed-glp.tex, "Readers of constant types").
  // Definition (Constant Type): "A type is a constant type if each of its
  // alternatives is a constant, one of the primitive types Integer, Real, String
  // and Module, or a constant type."  Proposition "Readers of Constant Types"
  // licenses several occurrences of such a reader, its paired writer occurring
  // once, and the relaxation "holds wherever the occurrences sit --- in the head,
  // nested within an argument, or in the body".  The question is asked of the
  // type each occurrence has, and not of a list of five type names read at the
  // top-level type of a head argument.
  // ===========================================================================

  test('a reader twice at a user-defined union of constants loads', () {
    // `Colour ::= red ; green ; blue.` qualifies exactly as `String` does: each
    // of its alternatives is a constant.  The old list of five names held no
    // user-defined type, so this was refused.
    final program = GlpCompiler().compile('''
Colour ::= red ; green ; blue.
procedure paint(Colour?, Colour, Colour).
paint(C, C?, C?).
''');
    expect(program, isNotNull);
  });

  test('a reader twice at a nested Integer position loads', () {
    // `X` sits inside `p(...)`, not at the top level of a head argument, so the
    // type name read off the declaration was `Pair` and never `Integer`.  `Pair`
    // is no constant type --- it carries a functor --- but the OCCURRENCE's type
    // is `Integer`, and that is what the relaxation is asked of.
    final program = GlpCompiler().compile('''
Pair ::= p(Integer, Integer).
procedure dup(Pair?, Integer, Integer).
dup(p(X, _), X?, X?).
''');
    expect(program, isNotNull);
  });

  test('a body variable read twice loads', () {
    // Both occurrences are in the body: nothing about the head decides them.
    final program = GlpCompiler().compile('''
procedure src(Integer).
src(1).

procedure use(Integer?, Integer?, Integer).
use(_, _, 0).

procedure go(Integer).
go(R?) :- src(N), use(N?, N?, R).
''');
    expect(program, isNotNull);
  });

  test('a reader twice at a type carrying a functor is refused', () {
    // `Stream ::= [] ; [_|Stream].` has a cons alternative, which carries a
    // functor, so it is no constant type and licenses nothing.
    expect(
        () => GlpCompiler().compile('''
Stream ::= [] ; [_|Stream].
procedure share(Stream?, Stream, Stream).
share(S, S?, S?).
'''),
        throwsException);
  });

  test('a reader twice at a stream of a constant element is refused', () {
    // `IntStream ::= [] ; [Integer|IntStream].` ends every path at `[]` or at
    // `Integer`, with no wildcard and no mode inversion, so the ground-type
    // sentence of TGLP b32934d admitted it and this LOADED --- measured
    // 2026-09-23.  A producer binds a functor before its arguments, so a cons
    // cell is a value carrying a writer in its tail: the paper replaced that
    // sentence with Definition (Constant Type), under which a cons alternative
    // disqualifies the type outright.
    expect(
        () => GlpCompiler().compile('''
IntStream ::= [] ; [Integer|IntStream].
procedure share(IntStream?, IntStream?, IntStream?).
share(_, _, _).
procedure split(IntStream?).
split(S) :- share(S?, S?, S?).
'''),
        throwsException);
  });

  // ===========================================================================
  // TGLP glp.tex, Remark "Guards and SRSW": only a guard whose SUCCESS implies
  // that X? is ground licenses several occurrences of X and X?.
  // ===========================================================================

  test('ground/1 licenses a reader twice in the body', () {
    final program = GlpCompiler().compile('''
procedure pair(_?, _?, _).
pair(_, _, done).
procedure k(_?, _).
k(X, Y?) :- ground(X?) | pair(X?, X?, Y).
''');
    expect(program, isNotNull);
  });

  // ===========================================================================
  // Guards the catalogue gives "Ground: no" (GLP-Spec appendix-guards.tex):
  // `compound(f(X?))`, `list([X?])` and `unknown(X?)` succeed with X? unbound,
  // so none implies groundness and none licenses several occurrences of X?
  // (TGLP glp.tex, Remark "Guards and SRSW").  Until 2026-09-29 the analyzer
  // marked all three grounded, and each refused clause below loaded.
  // ===========================================================================

  for (final g in ['compound', 'list', 'unknown']) {
    test('$g/1 licenses no reader twice in the body', () {
      expect(
          () => GlpCompiler().compile('''
procedure pair(_?, _?, _).
pair(_, _, done).
procedure k(_?, _).
k(X, Y?) :- $g(X?) | pair(X?, X?, Y).
'''),
          throwsA(predicate((e) => e.toString().contains('SRSW'),
              'an SRSW violation')));
    });
  }

  test('integer/1, "Ground: yes", still licenses a reader twice', () {
    final program = GlpCompiler().compile('''
procedure pair(_?, _?, _).
pair(_, _, done).
procedure k(_?, _).
k(X, Y?) :- integer(X?) | pair(X?, X?, Y).
''');
    expect(program, isNotNull);
  });

  // ===========================================================================
  // real/1, "Ground: yes" (GLP-Spec appendix-guards.tex, 12be29b; GLP #3
  // Cowork, 2026-10-02 22:19 UTC).  No engine is built in this file, so the
  // root scope is empty and the guard's declaration types nothing: the
  // analyzer's mark alone licenses X? twice.  Its runtime cases are
  // test/engine/real_guard_test.dart.
  // ===========================================================================

  test('real/1, "Ground: yes", licenses a reader twice', () {
    final program = GlpCompiler().compile('''
procedure pair(_?, _?, _).
pair(_, _, done).
procedure k(_?, _).
k(X, Y?) :- real(X?) | pair(X?, X?, Y).
''');
    expect(program, isNotNull);
  });

  test('real/1 licenses no writer twice: X occurs once, as ever', () {
    expect(
        () => GlpCompiler().compile('''
procedure w(_?, _).
w(X, Y?) :- real(X?) | Y = s(X).
'''),
        throwsA(predicate(
            (e) => '$e'.contains(
                'Writer variable "X" occurs 2 times; a writer occurs once, '
                'whatever the guards'),
            'the writer twice refused')));
  });
}
