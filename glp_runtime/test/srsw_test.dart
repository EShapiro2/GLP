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
  // Readers of ground types (TGLP typed-glp.tex, "Readers of ground types"):
  // "A reader whose type admits only ground terms may occur more than once in a
  // clause, its paired writer occurring once ... and the relaxation holds
  // wherever the occurrences sit --- in the head, nested within an argument, or
  // in the body."  The question is decided from the type automaton, at every
  // occurrence, and not from a list of five type names read at the top-level
  // type of a head argument.
  // ===========================================================================

  test('a reader twice at a user-defined union of constants loads', () {
    // `Colour ::= red ; green ; blue.` qualifies exactly as `String` does: every
    // path of it ends at a constant, with no wildcard and no mode inversion.
    // The old list of five names held no user-defined type, so this was refused.
    final program = GlpCompiler().compile('''
Colour ::= red ; green ; blue.
procedure paint(Colour?, Colour, Colour).
paint(C, C?, C?).
''');
    expect(program, isNotNull);
  });

  test('a reader twice at a nested Integer position loads', () {
    // `X` sits inside `p(...)`, not at the top level of a head argument, so the
    // type name read off the declaration was `Pair` and never `Integer`.
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

  test('a reader twice at a type that is not ground is still refused', () {
    // `Stream ::= [] ; [_|Stream].` has a wildcard on the way, so it admits more
    // than ground terms and licenses nothing.
    expect(
        () => GlpCompiler().compile('''
Stream ::= [] ; [_|Stream].
procedure share(Stream?, Stream, Stream).
share(S, S?, S?).
'''),
        throwsException);
  });
}
