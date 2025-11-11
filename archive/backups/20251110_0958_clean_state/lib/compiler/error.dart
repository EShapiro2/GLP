/// Error categories for compiler diagnostics
enum ErrorCategory {
  lexical,    // Invalid characters, unterminated strings
  syntax,     // Malformed clauses, missing delimiters
  semantic,   // SRSW violations, undefined variables
  codegen,    // Internal compiler errors during bytecode generation
}

/// Compilation error with source location
class CompileError implements Exception {
  final String message;
  final int line;
  final int column;
  final String? source;
  final ErrorCategory? category;

  CompileError(
    this.message,
    this.line,
    this.column,
    {this.source, String? phase}
  ) : category = phase != null ? _categoryFromPhase(phase) : null;

  static ErrorCategory? _categoryFromPhase(String phase) {
    switch (phase) {
      case 'lexer': return ErrorCategory.lexical;
      case 'parser': return ErrorCategory.syntax;
      case 'analyzer': return ErrorCategory.semantic;
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
