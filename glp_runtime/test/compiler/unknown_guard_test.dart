/// An unknown guard is refused at compile time (weeding round three, item 11).
///
/// A guard is a guard predicate of the catalogue (GLP-Spec appendix-guards.tex)
/// or a defined guard, a unit clause the partial evaluator unfolds before code
/// generation (Defined guard predicates).  What reaches the code generator is
/// therefore a catalogue guard the runtime evaluates or nothing, and nothing is
/// refused there.  Until 2026-10-02 the runtime printed a [WARN] for an unknown
/// guard and failed the clause at run time.  These compile the source directly,
/// with no type check before it, which on a load refuses an undeclared guard
/// predicate first.
library;

import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:test/test.dart';

Matcher _refusedWith(String text) => throwsA(isA<CompileError>()
    .having((e) => e.message, 'message', contains(text)));

void main() {
  test('a guard no catalogue row and no unit clause names is refused', () {
    expect(() => GlpCompiler().compile('p(X?) :- fail | X = done.\n'),
        _refusedWith('Unknown guard predicate fail/0'));
  });

  test('a catalogue guard at an arity it does not have is refused', () {
    expect(() => GlpCompiler().compile('p(X, Y?) :- integer(X?, 2) | Y = a.\n'),
        _refusedWith('Unknown guard predicate integer/2'));
  });

  // no_readers/1 on a term is a guard the runtime evaluates (GLP #3 Cowork,
  // 2026-10-02 15:31 UTC, B9; no_readers_guard_test.dart), where this test
  // asserted codegen's refusal of it before the two branches met.
  test('no_readers/1 on a term compiles, the runtime evaluating it', () {
    expect(
        GlpCompiler().compile('p(X, Y?) :- no_readers(f(X?)) | Y = a.\n'),
        isA<BytecodeProgram>());
  });

  test('a catalogue guard compiles', () {
    expect(GlpCompiler().compile('p(X, Y?) :- integer(X?) | Y = a.\n'),
        isA<BytecodeProgram>());
  });
}
