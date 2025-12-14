/// PMT SRSW Checker: Verifies Single-Reader/Single-Writer constraint
///
/// SRSW Rules:
/// - Each variable must have exactly 1 writer occurrence
/// - Each variable must have at least 1 reader occurrence
/// - Multiple reader occurrences allowed only if variable is grounded by a guard
///
/// Supports multiple mode alternatives (union of mode declarations).
/// A clause is valid if it satisfies SRSW for at least one declared mode.

import '../ast.dart';
import 'errors.dart';
import 'mode_table.dart';
import 'occurrence.dart';

class PmtChecker {
  final ModeTable modeTable;
  final OccurrenceClassifier classifier;

  PmtChecker(this.modeTable) : classifier = OccurrenceClassifier(modeTable);

  /// Check all clauses in a procedure
  List<PmtError> checkProcedure(Procedure proc) {
    final errors = <PmtError>[];

    final allModes = modeTable.getAllModes(proc.name, proc.arity);
    if (allModes == null || allModes.isEmpty) {
      // No mode declaration — skip checking
      return errors;
    }

    for (final clause in proc.clauses) {
      errors.addAll(checkClauseAgainstModes(clause, allModes));
    }

    return errors;
  }

  /// Check a single clause against multiple mode alternatives
  ///
  /// Clause is valid if it satisfies SRSW for at least one mode.
  List<PmtError> checkClauseAgainstModes(Clause clause, List<List<Mode>> allModes) {
    if (allModes.length == 1) {
      // Single mode — use standard checking
      return checkClause(clause, allModes.first);
    }

    // Multiple modes — try each until one succeeds
    List<PmtError>? bestErrors;

    for (final modes in allModes) {
      final errors = checkClause(clause, modes);
      if (errors.isEmpty) {
        // This mode works — clause is valid
        return [];
      }
      // Track the mode with fewest errors
      if (bestErrors == null || errors.length < bestErrors.length) {
        bestErrors = errors;
      }
    }

    // No mode matched — return errors from best match, with note about alternatives
    if (bestErrors != null && bestErrors.isNotEmpty) {
      // Add a note that this is a multimodal predicate
      final modeStrings = allModes.map((m) =>
        '(${m.map((mode) => mode == Mode.reader ? '?' : '').join(', ')})'
      ).join(' | ');
      return [
        PmtError(
          'Clause does not match any declared mode. Available modes: $modeStrings',
          clause.line,
          clause.column,
        ),
        ...bestErrors,
      ];
    }

    return bestErrors ?? [];
  }

  /// Check a single clause against a specific mode declaration
  List<PmtError> checkClause(Clause clause, List<Mode> modes) {
    final errors = <PmtError>[];

    // Classify all variable occurrences
    final occurrences = classifier.classifyClause(clause, modes);

    // Group by variable name
    final byVar = groupByVariable(occurrences);

    // Extract grounded variables from guards
    final groundedVars = _extractGroundedVars(clause);

    // Verify SRSW for each variable
    for (final entry in byVar.entries) {
      final varName = entry.key;
      final occs = entry.value;
      final counts = countOccurrences(occs);

      // Check writer occurrences
      if (counts.writers == 0) {
        errors.add(PmtError(
          'Variable $varName has no writer occurrence',
          occs.first.line,
          occs.first.column,
        ));
      } else if (counts.writers > 1) {
        // Find the second writer for error location
        final secondWriter = occs
            .where((o) => o.type == OccurrenceType.writer)
            .skip(1)
            .first;
        errors.add(PmtError(
          'Variable $varName has ${counts.writers} writer occurrences (expected 1)',
          secondWriter.line,
          secondWriter.column,
        ));
      }

      // Check reader occurrences
      if (counts.readers == 0) {
        errors.add(PmtError(
          'Variable $varName has no reader occurrence',
          occs.first.line,
          occs.first.column,
        ));
      } else if (counts.readers > 1 && !groundedVars.contains(varName)) {
        // Find the second reader for error location
        final secondReader = occs
            .where((o) => o.type == OccurrenceType.reader)
            .skip(1)
            .first;
        errors.add(PmtError(
          'Variable $varName has ${counts.readers} reader occurrences; add ground($varName) guard',
          secondReader.line,
          secondReader.column,
        ));
      }
    }

    return errors;
  }

  /// Extract variables that are grounded by guards
  ///
  /// Guards that imply groundness (matching analyzer.dart):
  /// - ground/1: explicit groundness
  /// - Type guards (arity 1): number, integer, float, atom, string, list, tuple, compound, var, nonvar
  /// - is_mutual_ref/1
  /// - unknown/1
  /// - Comparison guards (arity 2): <, >, =<, >=, =:=, =\=
  /// - =?=/2: ground equality
  Set<String> _extractGroundedVars(Clause clause) {
    final grounded = <String>{};

    if (clause.guards == null) return grounded;

    // Type-checking guards (arity 1)
    const typeCheckOps = {
      'ground', 'number', 'integer', 'float', 'atom', 'string',
      'list', 'tuple', 'compound', 'var', 'nonvar',
      'is_mutual_ref', 'unknown',
    };

    // Comparison guards (arity 2)
    const comparisonOps = {'<', '>', '=<', '>=', '=:=', r'=\=', '=?='};

    for (final guard in clause.guards!) {
      // Arity-1 guards that imply groundness
      if (typeCheckOps.contains(guard.predicate) && guard.args.length == 1) {
        _collectVarNames(guard.args[0], grounded);
      }

      // Arity-2 guards that imply groundness (both args)
      if (comparisonOps.contains(guard.predicate) && guard.args.length == 2) {
        _collectVarNames(guard.args[0], grounded);
        _collectVarNames(guard.args[1], grounded);
      }
    }

    return grounded;
  }

  /// Collect variable names from a term (recursively)
  void _collectVarNames(Term term, Set<String> out) {
    if (term is VarTerm) {
      out.add(term.name);
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _collectVarNames(arg, out);
      }
    } else if (term is ListTerm) {
      if (term.head != null) _collectVarNames(term.head!, out);
      if (term.tail != null) _collectVarNames(term.tail!, out);
    }
    // ConstTerm, UnderscoreTerm — no variables
  }
}
