// glp_runtime/test/compiler/pe_head_writer_test.dart
//
// The partial evaluator's term matching of a call WRITER against a unit
// clause's HEAD WRITER.
// Spec: GLP-Spec sections/appendix-term-matching.tex, Definition "Term
// Matching", with T1 the goal term and T2 the head term (the Remark after it):
//
//   T1 \ T2        | Writer X2  | Reader X2? | Term f2/n2
//   Writer X1      | fail       | X1 := X2?  | X1 := T2
//   Reader X1?     | X2 := X1?  | fail       | suspend on X1?
//   Term f1/n1     | X2 := T1   | fail       | fail if f1 != f2 or n1 != n2
//
// So a call writer against a head writer FAILS, and a defined guard whose
// unfolding meets one "can never succeed" (GLP-Spec appendix-guards.tex,
// "Defined guard predicates": the call is "unfolded to the term matching of T1
// with S1, ..., Tn with Sn").  An anonymous variable is a writer of its own
// (glp.tex, Remark "Anonymous Variables"), on either side.  Until 2026-10-02
// both partial evaluators aliased the unit clause's writer to the call's, and
// passed an anonymous variable on either side over.  GLP's task (GLP #3
// Cowork, 2026-10-02 08:40 UTC, G).  The two copies --- partial_evaluator.dart's,
// which the engine and the linker run, and analyzer.dart's, which GlpCompiler
// runs --- are both asked.

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart';

/// `put(X, X?)`: its first argument is a head writer, its second a head reader.
const _put = 'put(X, X?).\n';

/// `any(_)`: its one argument is an anonymous head writer.
const _any = 'any(_).\n';

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
    'a call writer against a head writer': _put + 't(B?, A?) :- put(A, B) | true.\n',
    'an anonymous call writer against a head writer':
        _put + 't(B?) :- put(_, B) | true.\n',
    'a call writer against an anonymous head writer':
        _any + 't(A?) :- any(A) | true.\n',
    'a call writer against a head writer nested in a structure':
        'wrap(f(X), X?).\n' 't(B?, A?) :- wrap(f(A), B) | true.\n',
  };

  for (final MapEntry(key: what, value: source) in failing.entries) {
    test('$what fails: partial_evaluator.dart', () {
      final e = _peError(source);
      expect(e, contains('can never succeed'), reason: e);
      expect(e, contains('head writer'), reason: e);
    });

    test('$what fails: analyzer.dart', () {
      final e = _compilerError(source);
      expect(e, contains('can never succeed'), reason: e);
      expect(e, contains('head writer'), reason: e);
    });
  }

  // What the table assigns, in the writer's column and row, still reduces.
  final reducing = {
    // Row "Reader X1?", column "Writer X2": X2 := X1?.
    'a call reader against a head writer': _put + 't(A, B?) :- put(A?, B) | true.\n',
    // Row "Term f1/n1", column "Writer X2": X2 := T1.
    'a call term against a head writer': _put + 't(B?) :- put(f(1), B) | true.\n',
    // A call reader, and a call term, against an anonymous head writer.
    'a call reader against an anonymous head writer':
        _any + 't(A) :- any(A?) | true.\n',
    'a call term against an anonymous head writer': _any + 't :- any(a) | true.\n',
  };

  for (final MapEntry(key: what, value: source) in reducing.entries) {
    test('$what reduces: partial_evaluator.dart', () {
      expect(_peError(source), isEmpty);
    });

    test('$what reduces: analyzer.dart', () {
      expect(_compilerError(source), isEmpty);
    });
  }
}
