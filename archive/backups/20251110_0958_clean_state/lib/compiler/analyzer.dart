import 'ast.dart';
import 'error.dart';

/// Variable information for semantic analysis
class VariableInfo {
  final String name;
  final bool isWriter;

  // Occurrence tracking
  int writerOccurrences = 0;
  int readerOccurrences = 0;

  // First occurrence location
  AstNode? firstOccurrence;

  // Register assignment (filled during analysis)
  int? registerIndex;

  // For readers: the paired writer name
  String? pairedWriter;

  // Variable classification
  bool isTemporary = false;  // X register (doesn't cross calls)
  bool isPermanent = false;  // Y register (survives across calls)

  VariableInfo(this.name, this.isWriter);

  bool get isSRSWValid {
    // Writer can occur once as writer
    if (isWriter && writerOccurrences > 1) return false;

    // Reader can occur once (unless ground guard allows multiple)
    // For now, simple check: reader occurs at most once
    // TODO: Allow multiple reader occurrences with ground guard
    if (!isWriter && readerOccurrences > 1) return false;

    return true;
  }

  @override
  String toString() => 'VariableInfo($name, writer×$writerOccurrences, reader×$readerOccurrences, reg=$registerIndex)';
}

/// Variable table for a single clause
class VariableTable {
  final Map<String, VariableInfo> _vars = {};

  // Track guard context
  bool _hasGroundGuard = false;
  final Set<String> _groundedVars = {};

  void recordWriterOccurrence(String name, AstNode node) {
    final info = _vars.putIfAbsent(name, () => VariableInfo(name, true));
    info.writerOccurrences++;
    info.firstOccurrence ??= node;
  }

  void recordReaderOccurrence(String name, AstNode node) {
    final writerName = name;  // Reader X? pairs with writer X

    // Ensure writer exists or create it
    final writerInfo = _vars.putIfAbsent(writerName, () => VariableInfo(writerName, true));

    // Record reader occurrence
    writerInfo.readerOccurrences++;
    writerInfo.firstOccurrence ??= node;
  }

  void markGrounded(String varName) {
    _groundedVars.add(varName);
  }

  bool isGrounded(String varName) => _groundedVars.contains(varName);

  void verifySRSW() {
    for (final info in _vars.values) {
      // Check writer occurrences
      if (info.writerOccurrences > 1) {
        throw CompileError(
          'SRSW violation: Writer variable "${info.name}" occurs ${info.writerOccurrences} times in clause',
          info.firstOccurrence?.line ?? 0,
          info.firstOccurrence?.column ?? 0,
          phase: 'analyzer'
        );
      }

      // Check reader occurrences (unless grounded)
      if (info.readerOccurrences > 1 && !isGrounded(info.name)) {
        throw CompileError(
          'SRSW violation: Reader variable "${info.name}?" occurs ${info.readerOccurrences} times without ground guard',
          info.firstOccurrence?.line ?? 0,
          info.firstOccurrence?.column ?? 0,
          phase: 'analyzer'
        );
      }
    }
  }

  List<VariableInfo> getAllVars() => _vars.values.toList();
  VariableInfo? getVar(String name) => _vars[name];

  @override
  String toString() => 'VariableTable(${_vars.length} vars)';
}

/// Annotated AST nodes with semantic information

class AnnotatedProgram {
  final Program ast;
  final List<AnnotatedProcedure> procedures;

  AnnotatedProgram(this.ast, this.procedures);
}

class AnnotatedProcedure {
  final Procedure ast;
  final String name;
  final int arity;
  final List<AnnotatedClause> clauses;

  // Code generation metadata (filled during codegen)
  int? entryPC;         // κ (kappa) - entry point for this procedure
  String? entryLabel;   // e.g., "merge/3"

  AnnotatedProcedure(this.ast, this.name, this.arity, this.clauses);

  String get signature => '$name/$arity';

  @override
  String toString() => 'AnnotatedProcedure($signature, ${clauses.length} clauses)';
}

class AnnotatedClause {
  final Clause ast;
  final VariableTable varTable;

  // Clause metadata
  bool hasGuards;
  bool hasBody;

  AnnotatedClause(this.ast, this.varTable, {this.hasGuards = false, this.hasBody = false});

  @override
  String toString() => 'AnnotatedClause(guards=$hasGuards, body=$hasBody, vars=${varTable.getAllVars().length})';
}

/// Semantic analyzer for GLP programs
class Analyzer {
  AnnotatedProgram analyze(Program program) {
    final annotatedProcs = <AnnotatedProcedure>[];

    for (final proc in program.procedures) {
      annotatedProcs.add(_analyzeProcedure(proc));
    }

    return AnnotatedProgram(program, annotatedProcs);
  }

  AnnotatedProcedure _analyzeProcedure(Procedure proc) {
    final annotatedClauses = <AnnotatedClause>[];

    for (final clause in proc.clauses) {
      annotatedClauses.add(_analyzeClause(clause));
    }

    return AnnotatedProcedure(proc, proc.name, proc.arity, annotatedClauses);
  }

  AnnotatedClause _analyzeClause(Clause clause) {
    final varTable = VariableTable();

    // Analyze head
    _analyzeAtom(clause.head, varTable);

    // Analyze guards (if present)
    final hasGuards = clause.guards != null && clause.guards!.isNotEmpty;
    if (hasGuards) {
      for (final guard in clause.guards!) {
        _analyzeGuard(guard, varTable);
      }
    }

    // Analyze body (if present)
    final hasBody = clause.body != null && clause.body!.isNotEmpty;
    if (hasBody) {
      for (final goal in clause.body!) {
        _analyzeGoal(goal, varTable);
      }
    }

    // Verify SRSW constraint
    varTable.verifySRSW();

    // Assign register indices
    _assignRegisters(varTable);

    return AnnotatedClause(clause, varTable, hasGuards: hasGuards, hasBody: hasBody);
  }

  void _analyzeAtom(Atom atom, VariableTable varTable) {
    for (final arg in atom.args) {
      _analyzeTerm(arg, varTable);
    }
  }

  void _analyzeGoal(Goal goal, VariableTable varTable) {
    for (final arg in goal.args) {
      _analyzeTerm(arg, varTable);
    }
  }

  void _analyzeGuard(Guard guard, VariableTable varTable) {
    // Special handling for ground/1 and known/1
    if (guard.predicate == 'ground' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm && arg.isReader) {
        // ground(X?) allows multiple reader occurrences
        varTable.markGrounded(arg.name);
      }
    }

    // Analyze guard arguments
    for (final arg in guard.args) {
      _analyzeTerm(arg, varTable);
    }
  }

  void _analyzeTerm(Term term, VariableTable varTable) {
    if (term is VarTerm) {
      if (term.isReader) {
        varTable.recordReaderOccurrence(term.name, term);
      } else {
        varTable.recordWriterOccurrence(term.name, term);
      }
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _analyzeTerm(arg, varTable);
      }
    } else if (term is ListTerm) {
      if (term.head != null) {
        _analyzeTerm(term.head!, varTable);
      }
      if (term.tail != null) {
        _analyzeTerm(term.tail!, varTable);
      }
    }
    // ConstTerm and UnderscoreTerm have no variables to track
  }

  void _assignRegisters(VariableTable varTable) {
    int nextIndex = 0;

    for (final info in varTable.getAllVars()) {
      // For now, all variables are temporaries (X registers)
      // TODO: Analyze variable lifetimes to distinguish X vs Y registers
      info.isTemporary = true;
      info.registerIndex = nextIndex++;
    }
  }
}
