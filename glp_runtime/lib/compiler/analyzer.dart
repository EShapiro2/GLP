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
  final PartialEvaluator _partialEvaluator = PartialEvaluator();

  Analyzer();

  AnnotatedProgram analyze(Program program, {bool generateReduce = false}) {
    // First: transform defined guards via partial evaluation
    // This must happen BEFORE SRSW analysis
    final transformed = _partialEvaluator.transformDefinedGuards(program);

    // Second: auto-generate reduce/2 clauses for metainterpretation
    // Generated for all files by default, except those with -stdlib. declaration
    final withReduce = generateReduce
        ? _generateReduceClauses(transformed)
        : transformed;

    final annotatedProcs = <AnnotatedProcedure>[];

    for (final proc in withReduce.procedures) {
      annotatedProcs.add(_analyzeProcedure(proc));
    }

    return AnnotatedProgram(withReduce, annotatedProcs);
  }

  /// Generate reduce/2 clauses for all procedures in the program
  /// Each source clause generates a corresponding reduce/2 clause:
  /// - H.           -> reduce(H, true).
  /// - H :- B.      -> reduce(H, B).
  /// - H :- G | B.  -> reduce(H, B) :- G | true.
  Program _generateReduceClauses(Program program) {
    // Don't generate reduce for reduce/2 itself (avoid infinite recursion)
    final sourceClauses = <Clause>[];
    for (final proc in program.procedures) {
      if (proc.name == 'reduce' && proc.arity == 2) continue;
      sourceClauses.addAll(proc.clauses);
    }

    if (sourceClauses.isEmpty) {
      return program; // Nothing to generate
    }

    // Generate reduce/2 clauses
    final reduceClauses = <Clause>[];
    for (final clause in sourceClauses) {
      reduceClauses.add(_generateReduceClause(clause));
    }

    // Check if reduce/2 already exists (user-defined)
    final existingReduceIdx = program.procedures.indexWhere(
      (p) => p.name == 'reduce' && p.arity == 2
    );

    final newProcedures = List<Procedure>.from(program.procedures);

    if (existingReduceIdx >= 0) {
      // Append to existing reduce/2
      final existing = newProcedures[existingReduceIdx];
      final mergedClauses = [...existing.clauses, ...reduceClauses];
      newProcedures[existingReduceIdx] = Procedure(
        'reduce', 2, mergedClauses,
        existing.line, existing.column
      );
    } else {
      // Create new reduce/2 procedure
      final firstClause = reduceClauses.first;
      newProcedures.add(Procedure(
        'reduce', 2, reduceClauses,
        firstClause.line, firstClause.column
      ));
    }

    return Program(newProcedures, program.line, program.column);
  }

  /// Generate a single reduce/2 clause from a source clause
  Clause _generateReduceClause(Clause source) {
    final head = source.head;
    final guards = source.guards;
    final body = source.body;
    final line = head.line;
    final col = head.column;

    // Convert head atom to term for reduce/2
    final headTerm = _atomToTerm(head);

    // Body term for reduce/2: 'true' for facts, original body for rules
    Term bodyTerm;
    if (body == null || body.isEmpty) {
      bodyTerm = ConstTerm('true', line, col);
    } else {
      bodyTerm = _goalsToTerm(body, line, col);
    }

    // reduce(Head, Body)
    final reduceHead = Atom('reduce', [headTerm, bodyTerm], line, col);

    // If original had guards, keep them with 'true' body
    // reduce(H, B) :- G | true.
    List<Goal>? reduceBody;
    if (guards != null && guards.isNotEmpty) {
      reduceBody = [Goal('true', [], line, col)];
    }

    return Clause(
      reduceHead,
      guards: guards,
      body: reduceBody,
      line: line,
      column: col,
    );
  }

  /// Convert an Atom to a Term (StructTerm or ConstTerm for 0-arity)
  Term _atomToTerm(Atom atom) {
    if (atom.args.isEmpty) {
      return ConstTerm(atom.functor, atom.line, atom.column);
    }
    return StructTerm(atom.functor, atom.args, atom.line, atom.column);
  }

  /// Convert a list of goals to a single term (conjunction)
  Term _goalsToTerm(List<Goal> goals, int line, int col) {
    if (goals.isEmpty) {
      return ConstTerm('true', line, col);
    }
    if (goals.length == 1) {
      return _goalToTerm(goals.first);
    }
    // Right-associative conjunction: (A, (B, C))
    var result = _goalToTerm(goals.last);
    for (var i = goals.length - 2; i >= 0; i--) {
      result = StructTerm(',', [_goalToTerm(goals[i]), result], line, col);
    }
    return result;
  }

  /// Convert a Goal to a Term
  Term _goalToTerm(Goal goal) {
    if (goal.args.isEmpty) {
      return ConstTerm(goal.functor, goal.line, goal.column);
    }
    return StructTerm(goal.functor, goal.args, goal.line, goal.column);
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

  // Guards that can be negated with ~
  static const _negatableGuards = {
    // Type guards
    'ground', 'known', 'unknown', 'integer', 'number', 'atom', 'string',
    'constant', 'compound', 'tuple', 'list', 'is_list',
    // Equality
    '=?=',
  };

  // Guards that cannot be negated (due to type-error semantics or special behavior)
  static const _nonNegatableGuards = {
    // Arithmetic (type error on non-numeric)
    '<', '>', '=<', '>=', '=:=', '=\\=',
    // Control
    'otherwise',
    // Time
    'wait', 'wait_until',
  };

  // Body-only constructs that are NOT valid guards
  static const _invalidInGuardPosition = {
    'true',   // true is body-only, not a guard
    'false',  // false is body-only
    'fail',   // fail is body-only
  };

  void _analyzeGuard(Guard guard, VariableTable varTable) {
    // Reject body-only constructs in guard position
    if (_invalidInGuardPosition.contains(guard.predicate)) {
      throw CompileError(
        '"${guard.predicate}" is not a guard - it can only appear in body position',
        guard.line,
        guard.column,
        phase: 'analyzer'
      );
    }
    // Validate guard negation
    if (guard.negated) {
      // Check if guard is negatable
      if (_nonNegatableGuards.contains(guard.predicate)) {
        throw CompileError(
          'Guard "${guard.predicate}" cannot be negated (type-error semantics)',
          guard.line,
          guard.column,
          phase: 'analyzer'
        );
      }
      // Note: defined guards (unit clauses) cannot be negated - this would require
      // checking if the guard predicate is a user-defined unit clause, which we
      // defer to runtime or codegen phase for now
    }

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

    // unknown/1 guard marks argument as ground for SRSW purposes
    // Unbound variables are safe to read multiple times (always return same reference)
    if (guard.predicate == 'unknown' && guard.args.length == 1) {
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

    // unknown/1 and known/1 are meta-tests (test if variable is unbound/bound)
    // Their arguments should NOT count as reader occurrences for SRSW
    // The variable must still have a writer and reader elsewhere
    if ((guard.predicate == 'unknown' || guard.predicate == 'known') && guard.args.length == 1) {
      return;  // Don't record this occurrence
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

// ============================================================================
// PARTIAL EVALUATION FOR DEFINED GUARDS
// ============================================================================

/// Result of compile-time GLP unification for partial evaluation
sealed class UnifyResult {}

class UnifySuccess extends UnifyResult {
  final Map<String, Term> substitution;
  UnifySuccess(this.substitution);
}

class UnifyFail extends UnifyResult {
  final String reason;
  UnifyFail(this.reason);
}

class UnifySuspend extends UnifyResult {
  final Set<String> unboundReaders;
  UnifySuspend(this.unboundReaders);
}

/// Partial evaluator for defined guards
class PartialEvaluator {
  int _varCounter = 0;

  /// Entry point: transform all defined guards in a program.
  /// Call this before SRSW analysis.
  Program transformDefinedGuards(Program program) {
    final unitClauses = _collectUnitClauses(program);

    if (unitClauses.isEmpty) {
      return program; // No unit clauses, nothing to transform
    }

    List<Procedure> transformedProcedures = [];

    for (final procedure in program.procedures) {
      List<Clause> transformedClauses = [];

      for (final clause in procedure.clauses) {
        final transformed = _transformClause(clause, unitClauses);
        transformedClauses.add(transformed);
      }

      transformedProcedures.add(Procedure(
        procedure.name,
        procedure.arity,
        transformedClauses,
        procedure.line,
        procedure.column,
      ));
    }

    return Program(transformedProcedures, program.line, program.column);
  }

  /// Collect unit clauses from program.
  /// Returns map from "name/arity" to list of head arguments.
  /// A unit clause has exactly one clause, no guards, and no body (or body is just `true`).
  Map<String, List<Term>> _collectUnitClauses(Program program) {
    final Map<String, List<Term>> unitClauses = {};

    for (final proc in program.procedures) {
      // Must have exactly one clause
      if (proc.clauses.length != 1) continue;

      final clause = proc.clauses.first;

      // Must have no guards
      if (clause.guards != null && clause.guards!.isNotEmpty) continue;

      // Must have no body, or body is empty, or body is just `true`
      if (clause.body != null && clause.body!.isNotEmpty) {
        // Check if body is just `true`
        if (clause.body!.length == 1 &&
            clause.body![0].functor == 'true' &&
            clause.body![0].args.isEmpty) {
          // Body is just `true`, this is a unit clause
        } else {
          continue; // Has real body goals
        }
      }

      // This is a unit clause
      final key = '${proc.name}/${proc.arity}';
      unitClauses[key] = clause.head.args;
    }

    return unitClauses;
  }

  /// Transform a clause by reducing defined guards.
  /// Returns transformed clause.
  /// Throws CompileError if guard cannot be reduced (suspend) or always fails.
  Clause _transformClause(Clause clause, Map<String, List<Term>> unitClauses) {
    if (clause.guards == null || clause.guards!.isEmpty) {
      return clause; // No guards, nothing to transform
    }

    var currentHead = clause.head;
    var currentGuards = List<Guard>.from(clause.guards!);
    var currentBody = clause.body != null ? List<Goal>.from(clause.body!) : null;
    bool changed = true;

    // Fixpoint iteration - keep processing until no more defined guards
    while (changed) {
      changed = false;
      List<Guard> remainingGuards = [];

      for (int i = 0; i < currentGuards.length; i++) {
        final guard = currentGuards[i];
        final key = '${guard.predicate}/${guard.args.length}';

        if (unitClauses.containsKey(key)) {
          // This is a defined guard - reduce it
          if (guard.negated) {
            throw CompileError(
              'Defined guard "${guard.predicate}" cannot be negated',
              guard.line,
              guard.column,
              phase: 'analyzer'
            );
          }

          // Rename unit clause variables to fresh names
          final renamedArgs = _renameUnitClauseVars(unitClauses[key]!);

          // Unify guard arguments with unit clause arguments
          final result = _glpUnifyForPE(guard.args, renamedArgs);

          switch (result) {
            case UnifyFail(:final reason):
              throw CompileError(
                'Defined guard "${guard.predicate}(${guard.args.join(", ")})" can never succeed.\n'
                '  Unit clause: ${guard.predicate}(${unitClauses[key]!.join(", ")})\n'
                '  Reason: $reason\n'
                '  This clause is unreachable.',
                guard.line,
                guard.column,
                phase: 'analyzer'
              );

            case UnifySuspend(:final unboundReaders):
              throw CompileError(
                'Cannot reduce defined guard "${guard.predicate}(${guard.args.join(", ")})" at compile time.\n'
                '  Unit clause: ${guard.predicate}(${unitClauses[key]!.join(", ")})\n'
                '  Unbound readers: ${unboundReaders.map((r) => "$r?").join(", ")}\n'
                '  Defined guards must be fully reducible at compile time.',
                guard.line,
                guard.column,
                phase: 'analyzer'
              );

            case UnifySuccess(:final substitution):
              // Apply substitution to head
              currentHead = _applySubstitutionToAtom(currentHead, substitution);

              // Apply substitution to remaining guards (not yet processed)
              final restGuards = currentGuards.sublist(i + 1)
                  .map((g) => _applySubstitutionToGuard(g, substitution))
                  .toList();

              // Apply to already-collected remaining guards
              remainingGuards = remainingGuards
                  .map((g) => _applySubstitutionToGuard(g, substitution))
                  .toList();

              // Apply substitution to body
              if (currentBody != null) {
                currentBody = currentBody
                    .map((g) => _applySubstitutionToGoal(g, substitution))
                    .toList();
              }

              // Update guards list and restart
              currentGuards = [...remainingGuards, ...restGuards];
              changed = true;
              break; // restart the while loop
          }

          if (changed) break; // restart outer loop
        } else {
          // Not a defined guard - keep it
          remainingGuards.add(guard);
        }
      }

      if (!changed) {
        // No more defined guards to reduce
        currentGuards = remainingGuards;
      }
    }

    return Clause(
      currentHead,
      guards: currentGuards.isEmpty ? null : currentGuards,
      body: currentBody,
      line: clause.line,
      column: clause.column,
    );
  }

  /// Rename variables in unit clause arguments to fresh names.
  /// IMPORTANT: Underscores (_) are NOT renamed - they stay as underscores.
  List<Term> _renameUnitClauseVars(List<Term> args) {
    // First, collect all variable names in the unit clause
    final varNames = <String>{};
    for (final arg in args) {
      _collectVarNames(arg, varNames);
    }

    // Build renaming map (skip underscores)
    final Map<String, String> renaming = {};
    for (final name in varNames) {
      if (name != '_') {
        renaming[name] = '_PE\$${_varCounter++}';
      }
    }

    // Apply renaming to all args
    return args.map((arg) => _applyRenaming(arg, renaming)).toList();
  }

  /// Collect all variable names in a term
  void _collectVarNames(Term term, Set<String> names) {
    if (term is VarTerm) {
      names.add(term.name);
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _collectVarNames(arg, names);
      }
    } else if (term is ListTerm) {
      if (term.head != null) _collectVarNames(term.head!, names);
      if (term.tail != null) _collectVarNames(term.tail!, names);
    }
  }

  /// Apply variable renaming to a term recursively
  Term _applyRenaming(Term term, Map<String, String> renaming) {
    if (term is VarTerm) {
      if (term.name == '_') {
        // Underscore stays as underscore
        return UnderscoreTerm(term.line, term.column);
      }
      if (renaming.containsKey(term.name)) {
        return VarTerm(renaming[term.name]!, term.isReader, term.line, term.column);
      }
      return term;
    } else if (term is StructTerm) {
      return StructTerm(
        term.functor,
        term.args.map((a) => _applyRenaming(a, renaming)).toList(),
        term.line,
        term.column,
      );
    } else if (term is ListTerm) {
      return ListTerm(
        term.head != null ? _applyRenaming(term.head!, renaming) : null,
        term.tail != null ? _applyRenaming(term.tail!, renaming) : null,
        term.line,
        term.column,
      );
    } else if (term is UnderscoreTerm) {
      return term;
    } else {
      return term; // ConstTerm unchanged
    }
  }

  /// GLP unification for partial evaluation.
  /// callArgs: arguments from the guard call
  /// unitArgs: arguments from the unit clause (already renamed)
  UnifyResult _glpUnifyForPE(List<Term> callArgs, List<Term> unitArgs) {
    if (callArgs.length != unitArgs.length) {
      return UnifyFail('Arity mismatch: ${callArgs.length} vs ${unitArgs.length}');
    }

    Map<String, Term> substitution = {};
    Set<String> suspensionSet = {};

    // Phase 1: Collection - process each argument pair
    for (int i = 0; i < callArgs.length; i++) {
      final result = _unifyTerms(callArgs[i], unitArgs[i], substitution, suspensionSet);
      if (result != null) {
        return result; // Failure
      }
    }

    // Phase 2: Resolution - check if suspended readers are resolved
    Set<String> unresolvedReaders = {};
    for (final readerName in suspensionSet) {
      // Reader X? suspends if X is not in substitution domain
      if (!substitution.containsKey(readerName)) {
        unresolvedReaders.add(readerName);
      }
    }

    if (unresolvedReaders.isNotEmpty) {
      return UnifySuspend(unresolvedReaders);
    }

    // Resolve substitution chains
    final resolved = _resolveSubstitution(substitution);
    return UnifySuccess(resolved);
  }

  /// Unify two terms, updating substitution and suspension set.
  /// Returns UnifyFail on structural mismatch, null on success.
  UnifyResult? _unifyTerms(
    Term callArg,
    Term unitArg,
    Map<String, Term> subst,
    Set<String> suspSet
  ) {
    // Handle underscore on either side - always succeeds, no binding
    if (_isUnderscore(callArg) || _isUnderscore(unitArg)) {
      return null; // success, continue
    }

    // Case: call arg is writer (VarTerm, not reader)
    if (callArg is VarTerm && !callArg.isReader) {
      if (unitArg is VarTerm && !unitArg.isReader) {
        // Writer vs Writer: alias unit writer to call writer
        subst[unitArg.name] = callArg;
      } else if (unitArg is VarTerm && unitArg.isReader) {
        // Writer vs Reader in unit clause - unusual but handle it
        // The reader refers to a writer that should be aliased
        subst[unitArg.name] = callArg;
      } else {
        // Writer vs constant/structure: bind call writer to unit arg
        subst[callArg.name] = unitArg;
      }
      return null;
    }

    // Case: call arg is reader
    if (callArg is VarTerm && callArg.isReader) {
      final writerName = callArg.name; // X? refers to writer X

      if (unitArg is VarTerm && !unitArg.isReader) {
        // Reader vs Writer: alias unit writer to call writer
        subst[unitArg.name] = VarTerm(writerName, false, callArg.line, callArg.column);
      } else if (unitArg is VarTerm && unitArg.isReader) {
        // Reader vs Reader: both suspend on same thing, alias
        subst[unitArg.name] = VarTerm(writerName, false, callArg.line, callArg.column);
        suspSet.add(writerName);
      } else {
        // Reader vs constant/structure: add to suspension set
        // Record what it should match - bind the writer to the unit arg
        suspSet.add(writerName);
        if (subst.containsKey(writerName)) {
          // Check structural compatibility
          final existing = subst[writerName]!;
          final compatResult = _checkCompatible(existing, unitArg, subst, suspSet);
          if (compatResult != null) return compatResult;
        } else {
          subst[writerName] = unitArg;
        }
      }
      return null;
    }

    // Case: call arg is constant
    if (callArg is ConstTerm) {
      if (unitArg is ConstTerm) {
        if (callArg.value == unitArg.value) {
          return null; // match
        } else {
          return UnifyFail('Constant mismatch: ${callArg.value} vs ${unitArg.value}');
        }
      } else if (unitArg is VarTerm && !unitArg.isReader) {
        // Constant vs Writer: bind unit writer to constant
        subst[unitArg.name] = callArg;
        return null;
      } else if (unitArg is VarTerm && unitArg.isReader) {
        // Constant vs Reader in unit clause - unusual
        subst[unitArg.name] = callArg;
        return null;
      } else {
        return UnifyFail('Constant ${callArg.value} cannot match structure $unitArg');
      }
    }

    // Case: call arg is structure
    if (callArg is StructTerm) {
      if (unitArg is StructTerm) {
        if (callArg.functor != unitArg.functor || callArg.args.length != unitArg.args.length) {
          return UnifyFail('Functor mismatch: ${callArg.functor}/${callArg.args.length} vs ${unitArg.functor}/${unitArg.args.length}');
        }
        // Recurse on arguments
        for (int i = 0; i < callArg.args.length; i++) {
          final result = _unifyTerms(callArg.args[i], unitArg.args[i], subst, suspSet);
          if (result != null) return result;
        }
        return null;
      } else if (unitArg is VarTerm && !unitArg.isReader) {
        subst[unitArg.name] = callArg;
        return null;
      } else if (unitArg is VarTerm && unitArg.isReader) {
        subst[unitArg.name] = callArg;
        return null;
      } else {
        return UnifyFail('Structure ${callArg.functor} cannot match $unitArg');
      }
    }

    // Case: call arg is list
    if (callArg is ListTerm) {
      if (unitArg is ListTerm) {
        // Both nil
        if (callArg.isNil && unitArg.isNil) {
          return null;
        }
        // One nil, one not
        if (callArg.isNil != unitArg.isNil) {
          return UnifyFail('List structure mismatch: nil vs non-nil');
        }
        // Both non-nil - recurse on head and tail
        if (callArg.head != null && unitArg.head != null) {
          final headResult = _unifyTerms(callArg.head!, unitArg.head!, subst, suspSet);
          if (headResult != null) return headResult;
        }
        if (callArg.tail != null && unitArg.tail != null) {
          final tailResult = _unifyTerms(callArg.tail!, unitArg.tail!, subst, suspSet);
          if (tailResult != null) return tailResult;
        }
        return null;
      } else if (unitArg is VarTerm && !unitArg.isReader) {
        subst[unitArg.name] = callArg;
        return null;
      } else if (unitArg is VarTerm && unitArg.isReader) {
        subst[unitArg.name] = callArg;
        return null;
      } else {
        return UnifyFail('List cannot match $unitArg');
      }
    }

    return UnifyFail('Unhandled case: ${callArg.runtimeType} vs ${unitArg.runtimeType}');
  }

  /// Check if two terms are structurally compatible
  UnifyResult? _checkCompatible(
    Term existing,
    Term newTerm,
    Map<String, Term> subst,
    Set<String> suspSet
  ) {
    // Simple structural check
    if (existing is ConstTerm && newTerm is ConstTerm) {
      if (existing.value != newTerm.value) {
        return UnifyFail('Incompatible bindings: ${existing.value} vs ${newTerm.value}');
      }
      return null;
    }
    if (existing is StructTerm && newTerm is StructTerm) {
      if (existing.functor != newTerm.functor || existing.args.length != newTerm.args.length) {
        return UnifyFail('Incompatible structures: ${existing.functor} vs ${newTerm.functor}');
      }
      // Could recurse here for deeper check, but for now accept
      return null;
    }
    // For now, accept other combinations (variables get resolved later)
    return null;
  }

  bool _isUnderscore(Term term) {
    return term is UnderscoreTerm || (term is VarTerm && term.name == '_');
  }

  /// Resolve substitution chains.
  /// If σ = {X → Y, Y → f(Z)}, result is {X → f(Z), Y → f(Z)}
  Map<String, Term> _resolveSubstitution(Map<String, Term> subst) {
    Map<String, Term> resolved = {};
    for (final entry in subst.entries) {
      resolved[entry.key] = _resolveTerm(entry.value, subst, {});
    }
    return resolved;
  }

  /// Resolve a term by following variable chains
  Term _resolveTerm(Term term, Map<String, Term> subst, Set<String> visited) {
    if (term is VarTerm) {
      if (visited.contains(term.name)) {
        // Cycle - return as is
        return term;
      }
      if (subst.containsKey(term.name)) {
        visited.add(term.name);
        final resolved = _resolveTerm(subst[term.name]!, subst, visited);
        // Preserve reader status if resolving to another variable
        if (term.isReader && resolved is VarTerm && !resolved.isReader) {
          return VarTerm(resolved.name, true, resolved.line, resolved.column);
        }
        return resolved;
      }
      return term;
    }
    if (term is StructTerm) {
      return StructTerm(
        term.functor,
        term.args.map((a) => _resolveTerm(a, subst, {...visited})).toList(),
        term.line,
        term.column,
      );
    }
    if (term is ListTerm) {
      if (term.isNil) return term;
      return ListTerm(
        term.head != null ? _resolveTerm(term.head!, subst, {...visited}) : null,
        term.tail != null ? _resolveTerm(term.tail!, subst, {...visited}) : null,
        term.line,
        term.column,
      );
    }
    return term; // Constants, underscores, etc.
  }

  /// Apply substitution to a term
  Term _applySubstitution(Term term, Map<String, Term> subst) {
    if (term is VarTerm) {
      if (term.name == '_') return term; // underscore unchanged
      final varName = term.name;
      if (subst.containsKey(varName)) {
        final replacement = subst[varName]!;
        // If original was reader and replacement is a writer var, make it reader
        if (term.isReader && replacement is VarTerm && !replacement.isReader) {
          return VarTerm(replacement.name, true, replacement.line, replacement.column);
        }
        return _applySubstitution(replacement, subst);
      }
      return term;
    }
    if (term is StructTerm) {
      return StructTerm(
        term.functor,
        term.args.map((a) => _applySubstitution(a, subst)).toList(),
        term.line,
        term.column,
      );
    }
    if (term is ListTerm) {
      if (term.isNil) return term;
      return ListTerm(
        term.head != null ? _applySubstitution(term.head!, subst) : null,
        term.tail != null ? _applySubstitution(term.tail!, subst) : null,
        term.line,
        term.column,
      );
    }
    if (term is UnderscoreTerm) {
      return term;
    }
    return term; // ConstTerm unchanged
  }

  /// Apply substitution to an Atom (head)
  Atom _applySubstitutionToAtom(Atom atom, Map<String, Term> subst) {
    return Atom(
      atom.functor,
      atom.args.map((a) => _applySubstitution(a, subst)).toList(),
      atom.line,
      atom.column,
    );
  }

  /// Apply substitution to a Guard
  Guard _applySubstitutionToGuard(Guard guard, Map<String, Term> subst) {
    return Guard(
      guard.predicate,
      guard.args.map((a) => _applySubstitution(a, subst)).toList(),
      guard.line,
      guard.column,
      negated: guard.negated,
    );
  }

  /// Apply substitution to a Goal
  Goal _applySubstitutionToGoal(Goal goal, Map<String, Term> subst) {
    return Goal(
      goal.functor,
      goal.args.map((a) => _applySubstitution(a, subst)).toList(),
      goal.line,
      goal.column,
    );
  }
}
