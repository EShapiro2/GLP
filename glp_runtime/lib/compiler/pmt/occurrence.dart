/// PMT Occurrence Classifier: Classifies variable occurrences as reader or writer
///
/// Classification rules (syntactic):
/// - Variable with `?` suffix (e.g., `X?`) → **reader** occurrence
/// - Variable without `?` suffix (e.g., `X`) → **writer** occurrence
///
/// The syntactic annotation in source code is authoritative for SRSW checking.
/// Mode declarations are used for separate mode consistency validation.

import '../ast.dart';
import 'mode_table.dart';

/// Type of variable occurrence
enum OccurrenceType { writer, reader }

/// A single variable occurrence with its classification and location
class Occurrence {
  final String variable;
  final OccurrenceType type;
  final int line;
  final int column;

  Occurrence(this.variable, this.type, this.line, this.column);

  @override
  String toString() => '$variable:${type.name}@$line:$column';

  @override
  bool operator ==(Object other) =>
      other is Occurrence &&
      variable == other.variable &&
      type == other.type &&
      line == other.line &&
      column == other.column;

  @override
  int get hashCode => Object.hash(variable, type, line, column);
}

/// Classifies variable occurrences in clauses based on mode declarations
class OccurrenceClassifier {
  final ModeTable modeTable;

  OccurrenceClassifier(this.modeTable);

  /// Classify all variable occurrences in a clause
  ///
  /// [clause] - The clause to analyze
  /// [headModes] - The modes for the clause's predicate (from mode table)
  ///
  /// Returns a list of all variable occurrences with their classifications.
  List<Occurrence> classifyClause(Clause clause, List<Mode> headModes) {
    final occurrences = <Occurrence>[];

    // Classify head arguments
    _classifyHead(clause.head, headModes, occurrences);

    // Classify body goals
    if (clause.body != null) {
      for (final goal in clause.body!) {
        _classifyGoal(goal, occurrences);
      }
    }

    // Classify guard arguments (guards are read-only, so all variables are readers)
    if (clause.guards != null) {
      for (final guard in clause.guards!) {
        _classifyGuard(guard, occurrences);
      }
    }

    return occurrences;
  }

  /// Classify variables in clause head
  void _classifyHead(Atom head, List<Mode> headModes, List<Occurrence> out) {
    for (int i = 0; i < head.args.length && i < headModes.length; i++) {
      // Collect variables using syntactic annotations (headModes unused for now)
      _collectVariables(head.args[i], out);
    }
  }

  /// Classify variables in a body goal
  void _classifyGoal(Goal goal, List<Occurrence> out) {
    // Skip remote goals for now (would need cross-module mode lookup)
    if (goal is RemoteGoal) {
      return;
    }

    // Handle SpawnGoal by processing its inner goal
    if (goal is SpawnGoal) {
      _classifyGoal(goal.innerGoal, out);
      return;
    }

    // Collect variables from all arguments using syntactic annotations
    for (final arg in goal.args) {
      _collectVariables(arg, out);
    }
  }

  /// Classify variables in a guard
  void _classifyGuard(Guard guard, List<Occurrence> out) {
    for (final arg in guard.args) {
      _collectVariables(arg, out);
    }
  }

  /// Recursively collect variable occurrences from a term using syntactic annotations
  void _collectVariables(Term term, List<Occurrence> out) {
    if (term is VarTerm) {
      // Skip named anonymous variables (Section 9 of typed-glp-manual)
      // Variables starting with _ are anonymous and exempt from SRSW
      if (term.name.startsWith('_')) {
        return;
      }
      // Use syntactic annotation: X? is reader, X is writer
      final occType = term.isReader ? OccurrenceType.reader : OccurrenceType.writer;
      out.add(Occurrence(term.name, occType, term.line, term.column));
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _collectVariables(arg, out);
      }
    } else if (term is ListTerm) {
      if (term.head != null) {
        _collectVariables(term.head!, out);
      }
      if (term.tail != null) {
        _collectVariables(term.tail!, out);
      }
    }
    // ConstTerm, UnderscoreTerm — no variables to collect
  }
}

/// Group occurrences by variable name
Map<String, List<Occurrence>> groupByVariable(List<Occurrence> occurrences) {
  final result = <String, List<Occurrence>>{};
  for (final occ in occurrences) {
    result.putIfAbsent(occ.variable, () => []).add(occ);
  }
  return result;
}

/// Count writer and reader occurrences for a variable
({int writers, int readers}) countOccurrences(List<Occurrence> occurrences) {
  int writers = 0;
  int readers = 0;
  for (final occ in occurrences) {
    if (occ.type == OccurrenceType.writer) {
      writers++;
    } else {
      readers++;
    }
  }
  return (writers: writers, readers: readers);
}
