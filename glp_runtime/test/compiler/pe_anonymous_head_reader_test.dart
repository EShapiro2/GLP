// glp_runtime/test/compiler/pe_anonymous_head_reader_test.dart
//
// The partial evaluator's term matching against a unit clause's ANONYMOUS
// head reader `_?`.  TGLP typed-glp.tex, "Anonymous variables": at a produced
// head position `_?` "denotes an output the clause never produces", the
// placeholder `Out?` of a writer the clause never names; so it is a head
// reader, matched by GLP-Spec appendix-term-matching.tex, Definition "Term
// Matching", column "Reader X2?": a goal writer is assigned it, a goal reader
// and a goal term fail.  A defined guard is "unfolded to the term matching of
// T1 with S1, ..., Tn with Sn" (GLP-Spec appendix-guards.tex, "Defined guard
// predicates"), so a call meeting `_?` with a term or a reader can never
// succeed.  Until 2026-10-02 both copies passed over any anonymous variable
// on either side, so `pick2(A?, 2)` reduced against `pick2(1, _?)`.  GLP's
// task of 2026-10-02 08:40 UTC, B.  The two copies --- partial_evaluator.dart's,
// which the engine and the linker run, and analyzer.dart's, which GlpCompiler
// runs --- are both asked, as pe_head_reader_test.dart asks them of a named
// head reader.

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart';

/// Unit clauses with `_?` at an argument, inside a structure, as a list's
/// tail, and written with a name, `_X?`.
const _units =
    'pick2(1, _?).\n'
    'pick3(ch(_, _?)).\n'
    'pick4([1 | _?]).\n'
    'pick5(1, _X?).\n';

String _peError(String source) {
  final module = Parser(Lexer(source).tokenize()).parseModule();
  try {
    PartialEvaluator().transformDefinedGuards(
      Program(module.procedures, module.line, module.column),
    );
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
    'a constant at an argument': 't(A) :- pick2(A?, 2) | true.\n',
    'a structure at an argument': 't(A) :- pick2(A?, f(2)) | true.\n',
    'a list at an argument': 't(A) :- pick2(A?, [2]) | true.\n',
    'a goal reader at an argument': 't(A, B) :- pick2(A?, B?) | true.\n',
    'a constant inside a structure': 't :- pick3(ch(1, 2)) | true.\n',
    'a constant as a list tail': 't :- pick4([1 | []]) | true.\n',
    'a constant at a named `_X?`': 't(A) :- pick5(A?, 2) | true.\n',
  };

  for (final MapEntry(key: what, value: clause) in failing.entries) {
    test('`_?` against $what fails: partial_evaluator.dart', () {
      final e = _peError(_units + clause);
      expect(e, contains('can never succeed'), reason: e);
      expect(e, contains('head reader'), reason: e);
    });

    test('`_?` against $what fails: analyzer.dart', () {
      final e = _compilerError(_units + clause);
      expect(e, contains('can never succeed'), reason: e);
      expect(e, contains('head reader'), reason: e);
    });
  }

  final assigned = {
    'at an argument': 't(A, B?) :- pick2(A?, B) | true.\n',
    'inside a structure': 't(B?) :- pick3(ch(1, B)) | true.\n',
    'as a list tail': 't(B?) :- pick4([1 | B]) | true.\n',
    'at a named `_X?`': 't(A, B?) :- pick5(A?, B) | true.\n',
  };

  for (final MapEntry(key: where, value: clause) in assigned.entries) {
    test('a goal writer $where is assigned `_?`, as the table has it: '
        'partial_evaluator.dart', () {
      expect(_peError(_units + clause), isEmpty);
    });

    test('a goal writer $where is assigned `_?`, as the table has it: '
        'analyzer.dart', () {
      expect(_compilerError(_units + clause), isEmpty);
    });
  }
}
