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
    // Revised SRSW: Each variable must have exactly one writer AND at least one reader
    // Exception: Ground guard allows multiple reader occurrences

    // Must have exactly one writer occurrence
    if (writerOccurrences != 1) return false;

    // Must have at least one reader occurrence
    if (readerOccurrences == 0) return false;

    // Multiple readers allowed only with ground guard (checked in verifySRSW)
    // This property doesn't have access to ground info, so we allow it here
    // and verifySRSW will enforce the ground guard requirement

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

      // Check for complete reader/writer pairs (revised SRSW requirement)
      // Each variable must have exactly one writer AND at least one reader
      if (info.writerOccurrences != 1) {
        throw CompileError(
          'SRSW violation: Variable "${info.name}" must have exactly one writer occurrence (found ${info.writerOccurrences})',
          info.firstOccurrence?.line ?? 0,
          info.firstOccurrence?.column ?? 0,
          phase: 'analyzer'
        );
      }

      if (info.readerOccurrences == 0) {
        throw CompileError(
          'SRSW violation: Variable "${info.name}" has writer but no reader (must have at least one reader)',
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
  int _freshVarCounter = 0;

  Analyzer();

  AnnotatedProgram analyze(Program program) {
    // Step 1: Find unit clauses (defined guard predicates)
    final unitClauses = _findUnitClauses(program);

    // Step 2: Transform defined guards to equality goals
    final transformedProgram = _transformDefinedGuards(program, unitClauses);

    // Step 3: Analyze transformed program
    final annotatedProcs = <AnnotatedProcedure>[];

    for (final proc in transformedProgram.procedures) {
      annotatedProcs.add(_analyzeProcedure(proc));
    }

    return AnnotatedProgram(transformedProgram, annotatedProcs);
  }

  /// Find all unit clauses in the program.
  /// A unit clause is a procedure with exactly one clause, no guards, and no body.
  /// Returns map from "name/arity" to the head arguments.
  Map<String, List<Term>> _findUnitClauses(Program program) {
    final result = <String, List<Term>>{};

    for (final proc in program.procedures) {
      if (proc.clauses.length == 1) {
        final clause = proc.clauses.first;
        final hasGuards = clause.guards != null && clause.guards!.isNotEmpty;
        final hasBody = clause.body != null && clause.body!.isNotEmpty;

        if (!hasGuards && !hasBody) {
          // This is a unit clause
          result['${proc.name}/${proc.arity}'] = clause.head.args;
        }
      }
    }

    return result;
  }

  /// Transform defined guards to equality goals.
  /// For guard p(S1,...,Sn) with unit clause p(T1,...,Tn),
  /// replace with equality guards T1=S1, T2=S2, ..., Tn=Sn.
  Program _transformDefinedGuards(Program program, Map<String, List<Term>> unitClauses) {
    if (unitClauses.isEmpty) return program;

    final transformedProcs = <Procedure>[];

    for (final proc in program.procedures) {
      final transformedClauses = <Clause>[];

      for (final clause in proc.clauses) {
        transformedClauses.add(_transformClause(clause, unitClauses));
      }

      transformedProcs.add(Procedure(proc.name, proc.arity, transformedClauses, proc.line, proc.column));
    }

    return Program(transformedProcs, program.line, program.column);
  }

  /// Transform a single clause by unfolding any defined guards.
  Clause _transformClause(Clause clause, Map<String, List<Term>> unitClauses) {
    if (clause.guards == null || clause.guards!.isEmpty) {
      return clause;
    }

    final newGuards = <Guard>[];
    bool anyTransformed = false;

    for (final guard in clause.guards!) {
      final signature = '${guard.predicate}/${guard.args.length}';
      final unitClauseArgs = unitClauses[signature];

      if (unitClauseArgs != null) {
        // This is a defined guard - unfold to equalities
        anyTransformed = true;
        final equalityGuards = _unfoldDefinedGuard(guard.args, unitClauseArgs, guard.line, guard.column);
        newGuards.addAll(equalityGuards);
      } else {
        // Not a defined guard - keep as is
        newGuards.add(guard);
      }
    }

    if (!anyTransformed) {
      return clause;
    }

    return Clause(
      clause.head,
      guards: newGuards.isEmpty ? null : newGuards,
      body: clause.body,
      line: clause.line,
      column: clause.column,
    );
  }

  /// Unfold a defined guard call to equality guards.
  /// call args: S1, S2, ..., Sn (from the guard call)
  /// unit clause args: T1, T2, ..., Tn (from the unit clause head)
  /// Result: guards T1'=S1, T2'=S2, ..., Tn'=Sn where Ti' has fresh variables
  List<Guard> _unfoldDefinedGuard(List<Term> callArgs, List<Term> unitClauseArgs, int line, int column) {
    // Create fresh variable mapping for unit clause variables
    final varMapping = <String, String>{};

    // Rename variables in unit clause args to fresh names
    final renamedArgs = <Term>[];
    for (final arg in unitClauseArgs) {
      renamedArgs.add(_renameVariables(arg, varMapping, line, column));
    }

    // Create equality guards: Ti = Si
    final guards = <Guard>[];
    for (int i = 0; i < callArgs.length && i < renamedArgs.length; i++) {
      guards.add(Guard('=', [renamedArgs[i], callArgs[i]], line, column));
    }

    return guards;
  }

  /// Rename variables in a term to fresh names.
  /// Updates varMapping with new names for each variable encountered.
  Term _renameVariables(Term term, Map<String, String> varMapping, int line, int column) {
    if (term is VarTerm) {
      final freshName = varMapping.putIfAbsent(term.name, () => '_G${_freshVarCounter++}');
      return VarTerm(freshName, term.isReader, term.line, term.column);
    } else if (term is StructTerm) {
      final newArgs = term.args.map((arg) => _renameVariables(arg, varMapping, line, column)).toList();
      return StructTerm(term.functor, newArgs, term.line, term.column);
    } else if (term is ListTerm) {
      final newHead = term.head != null ? _renameVariables(term.head!, varMapping, line, column) : null;
      final newTail = term.tail != null ? _renameVariables(term.tail!, varMapping, line, column) : null;
      return ListTerm(newHead, newTail, term.line, term.column);
    } else if (term is UnderscoreTerm) {
      // Anonymous variables get fresh names too (each _ is independent)
      final freshName = '_G${_freshVarCounter++}';
      return VarTerm(freshName, false, term.line, term.column);
    } else {
      // ConstTerm - return as is
      return term;
    }
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

    // Verify SRSW constraint - all GLP code must satisfy SRSW
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

    // Type-checking guards implicitly test groundness
    // Per spec: type tests require bound values, which are ground by definition
    final typeCheckOps = ['number', 'integer', 'float', 'atom', 'string', 'list', 'tuple', 'compound', 'var', 'nonvar'];
    if (typeCheckOps.contains(guard.predicate) && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        // Mark the writer as grounded (readers of X are allowed multiple times)
        varTable.markGrounded(arg.name);
      }
    }

    // is_mutual_ref/1 guard marks argument as ground (MutualRefTerm can be read multiple times)
    if (guard.predicate == 'is_mutual_ref' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        varTable.markGrounded(arg.name);
      }
    }

    // writer/1 and reader/1 guards mark argument as ground
    // Unbound variables are safe to read multiple times (always return same reference)
    if ((guard.predicate == 'writer' || guard.predicate == 'reader') && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        varTable.markGrounded(arg.name);
      }
    }

    // Comparison guards implicitly test groundness of both operands
    // Per spec: comparison guards require both operands to be bound numeric values,
    // which means they're ground. This allows multiple reader occurrences.
    final comparisonOps = ['<', '>', '=<', '>=', '=:=', '=\\='];
    if (comparisonOps.contains(guard.predicate) && guard.args.length == 2) {
      for (final arg in guard.args) {
        if (arg is VarTerm) {
          // Mark the writer as grounded (readers of X are allowed multiple times)
          varTable.markGrounded(arg.name);
        }
      }
    }

    // Ground equality guard marks both arguments as grounded
    // =?= succeeds only if both arguments are ground and equal
    if (guard.predicate == '=?=' && guard.args.length == 2) {
      for (final arg in guard.args) {
        if (arg is VarTerm) {
          varTable.markGrounded(arg.name);
        }
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
