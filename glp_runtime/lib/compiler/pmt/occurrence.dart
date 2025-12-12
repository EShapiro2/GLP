/// PMT Occurrence Classifier: Classifies variable occurrences as reader or writer
///
/// Classification rules (head inverts, body preserves):
/// - Head reader arg (`T?`) → variable is **writer** occurrence
/// - Head writer arg (`T`)  → variable is **reader** occurrence
/// - Body reader arg (`T?`) → variable is **reader** occurrence
/// - Body writer arg (`T`)  → variable is **writer** occurrence

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
      final argMode = headModes[i];
      // Head inverts: reader arg → writer occurrence, writer arg → reader occurrence
      final occType = (argMode == Mode.reader)
          ? OccurrenceType.writer
          : OccurrenceType.reader;
      _collectVariables(head.args[i], occType, out);
    }
  }

  /// Classify variables in a body goal
  void _classifyGoal(Goal goal, List<Occurrence> out) {
    // Skip remote goals for now (would need cross-module mode lookup)
    if (goal is RemoteGoal) {
      // For remote goals, we'd need to look up the target module's modes
      // For now, skip classification (or could treat as unknown)
      return;
    }

    // Look up modes for this goal
    final goalModes = modeTable.getModes(goal.functor, goal.arity);
    if (goalModes == null) {
      // No mode declaration for this predicate - skip classification
      // (Could also treat all args as unknown/unclassified)
      return;
    }

    for (int i = 0; i < goal.args.length && i < goalModes.length; i++) {
      final argMode = goalModes[i];
      // Body preserves: reader arg → reader occurrence, writer arg → writer occurrence
      final occType = (argMode == Mode.reader)
          ? OccurrenceType.reader
          : OccurrenceType.writer;
      _collectVariables(goal.args[i], occType, out);
    }
  }

  /// Classify variables in a guard
  /// Guards only read values, so all variable occurrences are readers
  void _classifyGuard(Guard guard, List<Occurrence> out) {
    for (final arg in guard.args) {
      _collectVariables(arg, OccurrenceType.reader, out);
    }
  }

  /// Recursively collect variable occurrences from a term
  void _collectVariables(Term term, OccurrenceType type, List<Occurrence> out) {
    if (term is VarTerm) {
      out.add(Occurrence(term.name, type, term.line, term.column));
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _collectVariables(arg, type, out);
      }
    } else if (term is ListTerm) {
      if (term.head != null) {
        _collectVariables(term.head!, type, out);
      }
      if (term.tail != null) {
        _collectVariables(term.tail!, type, out);
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
