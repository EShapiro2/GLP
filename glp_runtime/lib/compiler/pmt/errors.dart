/// PMT Error: Represents a mode/SRSW violation detected during PMT checking

class PmtError {
  final String message;
  final int line;
  final int column;

  PmtError(this.message, this.line, this.column);

  @override
  String toString() => 'PMT Error at $line:$column: $message';

  @override
  bool operator ==(Object other) =>
      other is PmtError &&
      message == other.message &&
      line == other.line &&
      column == other.column;

  @override
  int get hashCode => Object.hash(message, line, column);
}

/// Exception thrown when PMT checking fails
class PmtErrors implements Exception {
  final List<PmtError> errors;

  PmtErrors(this.errors);

  @override
  String toString() {
    if (errors.isEmpty) return 'PmtErrors: (none)';
    return 'PmtErrors:\n${errors.map((e) => '  $e').join('\n')}';
  }
}
