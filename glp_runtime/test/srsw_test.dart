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
}
