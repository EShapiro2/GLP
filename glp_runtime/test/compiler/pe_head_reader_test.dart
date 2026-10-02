// glp_runtime/test/compiler/pe_head_reader_test.dart
//
// The partial evaluator's term matching against a unit clause's HEAD READER.
// Spec: GLP-Spec sections/appendix-term-matching.tex, Definition "Term
// Matching", with T1 the goal term and T2 the head term (the Remark after it):
//
//   T1 \ T2        | Writer X2  | Reader X2? | Term f2/n2
//   Writer X1      | fail       | X1 := X2?  | X1 := T2
//   Reader X1?     | X2 := X1?  | fail       | suspend on X1?
//   Term f1/n1     | X2 := T1   | fail       | fail if f1 != f2 or n1 != n2
//
// So a head reader against a constant, a structure, a list or a goal reader
// FAILS, and a defined guard whose unfolding meets one "can never succeed"
// (GLP-Spec appendix-guards.tex, "Defined guard predicates": the call is
// "unfolded to the term matching of T1 with S1, ..., Tn with Sn").  Until
// 2026-10-02 the partial evaluator bound the head reader to the term, and
// aliased the two readers and suspended.  GLP's task of 2026-10-01 23:58 UTC,
// item 1 (task (a) of 2026-10-01 13:06 UTC).  The two copies ---
// partial_evaluator.dart's, which the engine and the linker run, and
// analyzer.dart's, which GlpCompiler runs --- are both asked.

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart';

/// `pick(X?, X)`: its first argument is a head reader.
const _pick = 'pick(X?, X).\n';

String _peError(String source) {
  final module = Parser(Lexer(source).tokenize()).parseModule();
  try {
    PartialEvaluator().transformDefinedGuards(
        Program(module.procedures, module.line, module.column));
  } catch (e) {
    return e.toString();
  }
  return '';
}

String _compilerError(String source) {
  try {
    GlpCompiler().compile(source);
  } catch (e) {
    return e.toString();
  }
  return '';
}

void main() {
  final failing = {
    'a constant': 't(A) :- pick(a, A?) | true.\n',
    'a structure': 't(A) :- pick(f(1), A?) | true.\n',
    'a list': 't(A) :- pick([1], A?) | true.\n',
    'a goal reader': 't(A, B) :- pick(B?, A?) | true.\n',
  };

  for (final MapEntry(key: what, value: clause) in failing.entries) {
    test('a head reader against $what fails: partial_evaluator.dart', () {
      final e = _peError(_pick + clause);
      expect(e, contains('can never succeed'), reason: e);
      expect(e, contains('head reader'), reason: e);
    });

    test('a head reader against $what fails: analyzer.dart', () {
      final e = _compilerError(_pick + clause);
      expect(e, contains('can never succeed'), reason: e);
      expect(e, contains('head reader'), reason: e);
    });
  }

  test('a goal writer against a head reader is assigned, as the table has it',
      () {
    // Row "Writer X1", column "Reader X2?": X1 := X2?.  The guard reduces.
    const clause = 't(A, B?) :- pick(B, A?) | true.\n';
    expect(_peError(_pick + clause), isEmpty);
  });
}
