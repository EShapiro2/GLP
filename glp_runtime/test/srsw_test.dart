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
    print('\nTesting anonymous variable in head: _ := X / Y');

    final compiler = GlpCompiler();

    // This clause uses _ as first argument - should compile without error
    // The _ is a writer that nobody reads (used in abort clauses)
    // X and Y are grounded by number() guards, so guard readers count for SRSW
    final source = '''
_ := X / Y :-
  number(X?), number(Y?), Y? =:= 0 |
  abort("Division by zero").
''';

    final program = compiler.compile(source);
    expect(program, isNotNull);
    expect(program.ops.length, greaterThan(0));
    print('✅ Anonymous variable _ compiles correctly');
    print('   Generated ${program.ops.length} instructions');
  });

  test('Anonymous variable _ passes SRSW where Result? would fail', () {
    print('\nTesting _ vs Result? in abort clause');

    final compiler = GlpCompiler();

    // This should FAIL - Result? has no writer
    final badSource = '''
Result? := X / Y :-
  number(X?), number(Y?), Y? =:= 0 |
  abort("Division by zero").
''';

    expect(() => compiler.compile(badSource), throwsException,
        reason: 'Result? with no writer should fail SRSW');
    print('✅ Result? correctly rejected (no writer)');

    // This should PASS - _ has no SRSW requirements
    // X and Y are grounded by number() guards, so guard readers count
    final goodSource = '''
_ := X / Y :-
  number(X?), number(Y?), Y? =:= 0 |
  abort("Division by zero").
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
}
