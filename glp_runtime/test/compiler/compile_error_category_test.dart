/// GlpCompiler keeps a CompileError's category when it rethrows it.
///
/// `GlpCompiler.compileWithMetadata` rethrows every CompileError with the
/// source attached, so that its message shows the line.  Until 2026-10-02 it
/// passed the error's category as the phase, by its name --- `lexical`,
/// `syntax`, `semantic`, `codegen` --- and `CompileError` maps a phase to a
/// category by the phase's name --- `lexer`, `parser`, `analyzer`, `codegen`
/// --- so every category but `codegen` came back null: a lexical, syntax or
/// semantic error from the compiler had no category (Integration's report of
/// 2026-10-02 10:07 UTC, D; GLP #3 Cowork's task of 2026-10-02 13:00 UTC).
/// no_guard_negation_test.dart could assert the parser's category only on the
/// parser itself, and not through the compiler.  The rethrow now passes the
/// category itself.
library;

import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:test/test.dart';

/// The CompileError [GlpCompiler] raises on [source]; fails the test if it
/// compiles.
CompileError _refusal(String source) {
  try {
    GlpCompiler().compile(source);
  } on CompileError catch (e) {
    return e;
  }
  fail('compiled: $source');
}

void main() {
  group('a CompileError rethrown by GlpCompiler keeps its category', () {
    test('lexical: an unterminated string', () {
      final e = _refusal('p(X?) :- q("abc, X).\n');
      expect(e.category, ErrorCategory.lexical, reason: '$e');
      expect('$e', startsWith('[lexical] Unterminated string'));
    });

    test('syntax: guard negation, the parser\'s refusal', () {
      final e = _refusal('''
procedure d(Integer?).
d(1).
procedure use(Integer?, _).
use(X, R?) :- ~d(X?) | R = no.
''');
      expect(e.category, ErrorCategory.syntax, reason: '$e');
      expect(e.message, contains('"~" is not GLP syntax'));
      expect(e.line, 4);
    });

    test('semantic: a writer twice in a head, the analyzer\'s refusal', () {
      final e = _refusal('h(same(To), To).\n');
      expect(e.category, ErrorCategory.semantic, reason: '$e');
      expect('$e', startsWith('[semantic] '));
    });

    test('codegen: an unlinked cross-module call, the code generator\'s '
        'refusal (kept before too)', () {
      final e = _refusal('p :- m # q.\n');
      expect(e.category, ErrorCategory.codegen, reason: '$e');
    });

    test('the source still rides with it: the message shows the line', () {
      final e = _refusal('p(X?) :- q("abc, X).\n');
      expect(e.source, isNotNull);
      expect('$e', contains('p(X?) :- q("abc, X).'));
    });

    test('and the category is the one the phase gave the error first', () {
      // The same source through the lexer alone.
      CompileError? direct;
      try {
        Parser(Lexer('p(X?) :- q("abc, X).\n').tokenize()).parse();
      } on CompileError catch (e) {
        direct = e;
      }
      expect(direct, isNotNull);
      expect(_refusal('p(X?) :- q("abc, X).\n').category, direct!.category);
    });
  });

  group('CompileError', () {
    test('a phase still names its category', () {
      expect(CompileError('m', 1, 1, phase: 'lexer').category,
          ErrorCategory.lexical);
      expect(CompileError('m', 1, 1, phase: 'parser').category,
          ErrorCategory.syntax);
      expect(CompileError('m', 1, 1, phase: 'analyzer').category,
          ErrorCategory.semantic);
      expect(CompileError('m', 1, 1, phase: 'codegen').category,
          ErrorCategory.codegen);
    });

    test('a category given is kept, over the phase', () {
      expect(
          CompileError('m', 1, 1, category: ErrorCategory.semantic).category,
          ErrorCategory.semantic);
      expect(
          CompileError('m', 1, 1,
                  phase: 'lexer', category: ErrorCategory.syntax)
              .category,
          ErrorCategory.syntax);
    });
  });
}
