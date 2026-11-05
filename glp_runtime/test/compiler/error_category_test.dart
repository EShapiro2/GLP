// glp_runtime/test/compiler/error_category_test.dart
//
// Every phase that raises a CompileError has a category, so that every
// diagnostic prints with its category's prefix (GLP #11 Cowork, Code_inbox
// 2026-11-03 16:40 UTC, item 10: no paper names the categories; give every
// phase one).  Until 2026-11-05 the loader's, the partial evaluator's, the
// type check's and the clause validation's diagnostics printed with none.

import 'package:glp_runtime/compiler/error.dart';
import 'package:test/test.dart';

void main() {
  for (final (phase, prefix) in [
    ('lexer', '[lexical] '),
    ('parser', '[syntax] '),
    ('analyzer', '[semantic] '),
    ('partial_evaluator', '[semantic] '),
    ('validation', '[semantic] '),
    ('typecheck', '[type] '),
    ('loader', '[loader] '),
    ('codegen', '[codegen] '),
  ]) {
    test('a diagnostic of the phase $phase prints with the prefix $prefix', () {
      final e = CompileError('the message', 3, 4, phase: phase);
      expect(e.category, isNotNull);
      expect(e.toString(), startsWith('${prefix}the message'));
    });
  }

  test('a category given is kept as given', () {
    final e = CompileError('m', 1, 1, phase: 'loader', category: ErrorCategory.syntax);
    expect(e.category, ErrorCategory.syntax);
  });
}
