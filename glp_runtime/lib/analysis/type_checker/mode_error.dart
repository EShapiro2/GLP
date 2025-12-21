// lib/analysis/type_checker/mode_error.dart
//
// Mode checking error types for moded type system.

import 'package:glp_runtime/compiler/ast.dart';
import 'mode.dart';

/// Base class for mode checking errors
class ModeError implements Exception {
  final String message;
  final int line;
  final int column;

  ModeError(this.message, this.line, this.column);

  /// Writer (X) appears at output position where reader expected
  ///
  /// Output positions expect readers (X?) because the callee will write.
  /// Finding a writer (X) means the caller is trying to write at an output
  /// position, which violates the mode discipline.
  factory ModeError.writerAtOutput({
    required String variable,
    required String predicate,
    required int argIndex,
    required int line,
    required int column,
  }) {
    return ModeError(
      'Mode error: writer $variable at output position in $predicate argument $argIndex\n'
      'Output positions expect readers (X?) since callee writes',
      line,
      column,
    );
  }

  /// Reader (X?) appears at input position where writer expected
  ///
  /// Input positions expect writers (X) because the caller will provide value.
  /// Finding a reader (X?) means the caller is trying to read at an input
  /// position, which violates the mode discipline.
  factory ModeError.readerAtInput({
    required String variable,
    required String predicate,
    required int argIndex,
    required int line,
    required int column,
  }) {
    return ModeError(
      'Mode error: reader $variable at input position in $predicate argument $argIndex\n'
      'Input positions expect writers (X) since caller provides value',
      line,
      column,
    );
  }

  /// Call site mode mismatch
  ///
  /// At a call site, caller and callee modes must be complementary:
  /// - Callee expects input (Type?) → caller must provide writer (X)
  /// - Callee expects output (Type) → caller must provide reader (X?)
  factory ModeError.callModeMismatch({
    required String predicate,
    required int argIndex,
    required Mode expectedMode,
    required Mode actualMode,
    required int line,
    required int column,
  }) {
    return ModeError(
      'Mode error: $predicate argument $argIndex mode mismatch\n'
      'Expected: $expectedMode, Found: $actualMode',
      line,
      column,
    );
  }

  @override
  String toString() => 'Mode error at line $line, column $column: $message';
}

/// Analysis error base class (if not already defined elsewhere)
/// This allows ModeError to integrate with other analysis errors
abstract class AnalysisError implements Exception {
  String get message;
  int get line;
  int get column;
}

/// Make ModeError also extend AnalysisError for consistency
class ModeAnalysisError extends ModeError implements AnalysisError {
  ModeAnalysisError(super.message, super.line, super.column);

  factory ModeAnalysisError.writerAtOutput({
    required String variable,
    required String predicate,
    required int argIndex,
    required int line,
    required int column,
  }) {
    return ModeAnalysisError(
      'Mode error: writer $variable at output position in $predicate argument $argIndex\n'
      'Output positions expect readers (X?) since callee writes',
      line,
      column,
    );
  }

  factory ModeAnalysisError.readerAtInput({
    required String variable,
    required String predicate,
    required int argIndex,
    required int line,
    required int column,
  }) {
    return ModeAnalysisError(
      'Mode error: reader $variable at input position in $predicate argument $argIndex\n'
      'Input positions expect writers (X) since caller provides value',
      line,
      column,
    );
  }

  factory ModeAnalysisError.callModeMismatch({
    required String predicate,
    required int argIndex,
    required Mode expectedMode,
    required Mode actualMode,
    required int line,
    required int column,
  }) {
    return ModeAnalysisError(
      'Mode error: $predicate argument $argIndex mode mismatch\n'
      'Expected: $expectedMode, Found: $actualMode',
      line,
      column,
    );
  }
}
