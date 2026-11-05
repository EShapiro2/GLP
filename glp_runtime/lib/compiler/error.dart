/// Error categories for compiler diagnostics
enum ErrorCategory {
  lexical,    // Invalid characters, unterminated strings
  syntax,     // Malformed clauses, missing delimiters
  semantic,   // SRSW violations, undefined variables, the partial evaluator's and the clause validation's refusals
  type,       // The type checker's refusals
  loader,     // A program or module refused by the loader or the linker
  codegen,    // Internal compiler errors during bytecode generation
}

/// Compilation error with source location
class CompileError implements Exception {
  final String message;
  final int line;
  final int column;
  final String? source;
  final ErrorCategory? category;

  /// [category] is kept as given; without one, it is the category of [phase],
  /// named by the phase that raised the error: `lexer`, `parser`, `analyzer`,
  /// `partial_evaluator`, `validation`, `typecheck`, `loader` and `codegen`,
  /// every phase the code names having a category, so that every diagnostic
  /// prints with its category's prefix (GLP #11 Cowork, Code_inbox 2026-11-03
  /// 16:40 UTC, item 10; until 2026-11-05 the last four printed with none).
  /// A caller passing an error on gives its category, not a phase: GlpCompiler
  /// passed the category's name as the phase until 2026-10-02, which no phase
  /// is named, and lost every category but codegen.
  CompileError(
    this.message,
    this.line,
    this.column,
    {this.source, String? phase, ErrorCategory? category}
  ) : category =
          category ?? (phase != null ? _categoryFromPhase(phase) : null);

  static ErrorCategory? _categoryFromPhase(String phase) {
    switch (phase) {
      case 'lexer': return ErrorCategory.lexical;
      case 'parser': return ErrorCategory.syntax;
      case 'analyzer': return ErrorCategory.semantic;
      case 'partial_evaluator': return ErrorCategory.semantic;
      case 'validation': return ErrorCategory.semantic;
      case 'typecheck': return ErrorCategory.type;
      case 'loader': return ErrorCategory.loader;
      case 'codegen': return ErrorCategory.codegen;
      default: return null;
    }
  }

  @override
  String toString() {
    final categoryName = category != null ? '[${category.toString().split('.').last}] ' : '';
    final loc = 'Line $line, Column $column';

    if (source != null) {
      final lines = source!.split('\n');
      if (line > 0 && line <= lines.length) {
        final sourceLine = lines[line - 1];
        final pointer = ' ' * (column - 1) + '^';
        return '$categoryName$message\n$loc:\n$sourceLine\n$pointer';
      }
    }

    return '$categoryName$message at $loc';
  }
}
