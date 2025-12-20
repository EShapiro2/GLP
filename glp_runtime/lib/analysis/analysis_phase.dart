// lib/analysis/analysis_phase.dart
//
// Interface for analysis phases that can run independently or as part of compilation.
// Phases include: Type Checking, SRSW Checking, Defined Guards expansion.

/// Base class for analysis errors
class AnalysisError {
  final String phase;
  final String message;
  final int line;
  final int column;
  final String? context;
  
  AnalysisError({
    required this.phase,
    required this.message,
    required this.line,
    required this.column,
    this.context,
  });
  
  @override
  String toString() {
    final loc = 'line $line, column $column';
    final ctx = context != null ? '\n    $context' : '';
    return '[$phase] $message at $loc$ctx';
  }
  
  /// Error severity
  bool get isError => true;
}

/// Analysis warning (non-fatal)
class AnalysisWarning extends AnalysisError {
  AnalysisWarning({
    required super.phase,
    required super.message,
    required super.line,
    required super.column,
    super.context,
  });
  
  @override
  bool get isError => false;
}

/// Shared context passed between analysis phases
class AnalysisContext {
  /// Type environment (populated by type parsing)
  dynamic typeEnvironment;
  
  /// Variable information (populated by SRSW analysis)
  Map<String, dynamic> variableInfo = {};
  
  /// Expanded guards (populated by defined guards phase)
  Map<String, dynamic> expandedGuards = {};
  
  /// Any additional phase-specific data
  final Map<String, dynamic> data = {};
}

/// Result of running analysis
class AnalysisResult {
  final List<AnalysisError> errors;
  final AnalysisContext context;
  
  AnalysisResult(this.errors, this.context);
  
  /// True if no errors (warnings are OK)
  bool get success => errors.where((e) => e.isError).isEmpty;
  
  /// Just the actual errors (not warnings)
  List<AnalysisError> get actualErrors => errors.where((e) => e.isError).toList();
  
  /// Just the warnings
  List<AnalysisError> get warnings => errors.where((e) => !e.isError).toList();
  
  @override
  String toString() {
    if (errors.isEmpty) {
      return 'Analysis successful (no errors or warnings)';
    }
    final sb = StringBuffer();
    final errs = actualErrors;
    final warns = warnings;
    if (errs.isNotEmpty) {
      sb.writeln('Errors (${errs.length}):');
      for (final e in errs) {
        sb.writeln('  $e');
      }
    }
    if (warns.isNotEmpty) {
      sb.writeln('Warnings (${warns.length}):');
      for (final w in warns) {
        sb.writeln('  $w');
      }
    }
    return sb.toString();
  }
}

/// Interface for analysis phases
abstract class AnalysisPhase {
  /// Name of this phase (for error reporting)
  String get name;
  
  /// Run this analysis phase on the AST
  /// Returns list of errors/warnings found
  List<AnalysisError> analyze(dynamic ast, AnalysisContext ctx);
}

/// Runs multiple analysis phases
class AnalysisRunner {
  final List<AnalysisPhase> phases;
  
  AnalysisRunner(this.phases);
  
  /// Run all phases on the AST
  /// If stopOnError is true, stops at first phase with errors
  AnalysisResult run(dynamic ast, {bool stopOnError = false}) {
    final ctx = AnalysisContext();
    final allErrors = <AnalysisError>[];
    
    for (final phase in phases) {
      final errors = phase.analyze(ast, ctx);
      allErrors.addAll(errors);
      
      if (stopOnError && errors.any((e) => e.isError)) {
        break;
      }
    }
    
    return AnalysisResult(allErrors, ctx);
  }
  
  /// Run only specific phases by name
  AnalysisResult runPhases(dynamic ast, List<String> phaseNames) {
    final ctx = AnalysisContext();
    final allErrors = <AnalysisError>[];
    
    for (final phase in phases) {
      if (phaseNames.contains(phase.name)) {
        final errors = phase.analyze(ast, ctx);
        allErrors.addAll(errors);
      }
    }
    
    return AnalysisResult(allErrors, ctx);
  }
}

// =============================================================================
// Standard analysis phases
// =============================================================================

/// Type checking phase
class TypeCheckPhase implements AnalysisPhase {
  final String? sourceCode;  // Optional source for type parsing

  TypeCheckPhase({this.sourceCode});

  @override
  String get name => 'type';

  @override
  List<AnalysisError> analyze(dynamic ast, AnalysisContext ctx) {
    // Import the type checker components
    // This is a placeholder - actual implementation requires imports

    // For now, return empty list
    // Full implementation in separate integration file
    return [];
  }
}

/// SRSW checking phase
class SRSWCheckPhase implements AnalysisPhase {
  @override
  String get name => 'srsw';
  
  @override
  List<AnalysisError> analyze(dynamic ast, AnalysisContext ctx) {
    // Implementation would wrap existing SRSW checker
    // For now, placeholder
    return [];
  }
}

/// Defined guards expansion phase
class DefinedGuardsPhase implements AnalysisPhase {
  @override
  String get name => 'guards';
  
  @override
  List<AnalysisError> analyze(dynamic ast, AnalysisContext ctx) {
    // Implementation would expand defined guards
    // For now, placeholder
    return [];
  }
}

/// Create the standard analysis runner with all phases
AnalysisRunner createStandardRunner() {
  return AnalysisRunner([
    TypeCheckPhase(),
    SRSWCheckPhase(),
    DefinedGuardsPhase(),
  ]);
}
