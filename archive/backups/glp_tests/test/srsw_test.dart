import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  test('SRSW violation: repeated variable should be rejected', () {
    print('\nTesting SRSW violation: same(f(X, X))');

    final compiler = GlpCompiler();
    
    expect(() => compiler.compile('same(f(X, X)).'), throwsException);
    print('✅ Correctly rejected repeated variable');
  });
}
