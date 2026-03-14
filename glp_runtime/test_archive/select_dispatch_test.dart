import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  group('_select/1 dispatch table generation', () {
    test('module with no exports produces no _select/1', () {
      final compiler = GlpCompiler();
      final program = compiler.compile('''
procedure foo(Integer?).
foo(X) :- ground(X?) | true.
''');
      expect(program.labels.containsKey('_select/1'), isFalse);
    });

    test('module with one export produces _select/1', () {
      final compiler = GlpCompiler();
      final program = compiler.compile('''
exported procedure process(Any?).
process(X) :- otherwise | consume(X?).

procedure consume(Any?).
consume(_).
''');
      expect(program.labels.containsKey('_select/1'), isTrue);
    });

    test('module with multiple exports produces _select/1', () {
      final compiler = GlpCompiler();
      final program = compiler.compile('''
exported procedure foo(Any?).
foo(X) :- ground(X?) | true.

exported procedure bar(Any?).
bar(X) :- ground(X?) | true.
''');
      expect(program.labels.containsKey('_select/1'), isTrue);
    });

    test('imported procedures do not generate _select/1', () {
      final compiler = GlpCompiler();
      final program = compiler.compile('''
imported procedure other#foo(Integer?).
procedure bar(Integer?).
bar(X) :- ground(X?) | true.
''');
      expect(program.labels.containsKey('_select/1'), isFalse);
    });
  });
}
