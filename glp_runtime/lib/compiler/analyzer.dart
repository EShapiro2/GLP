import 'ast.dart';
import 'error.dart';
import 'partial_evaluator.dart' show unitClausesOf;
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/program_dfa.dart' show ProgramDFA, buildProgramDFA, UnknownTypeError;
import '../analysis/type_checker/well_typed_clause.dart' show repeatableReaderVariables;

/// Variable information for semantic analysis
class VariableInfo {
  final String name;
  final bool isWriter;

  // Occurrence tracking (total - for register allocation, etc.)
  int writerOccurrences = 0;
  int readerOccurrences = 0;
  
  // Head/body occurrences only (for SRSW validation)
  // Guard occurrences do NOT count toward SRSW satisfaction per spec
  int writerOccurrencesHeadBody = 0;
  int readerOccurrencesHeadBody = 0;

  // Writer occurrences in the clause head alone: a writer licensed to occur
  // more than once must occur here (GLP-Spec glp.tex, Remark "Guards and
  // SRSW": "if X? occurs in a guard, its paired writer X must occur in the
  // head"), and twice here the diagnostic names the clause
  // ([VariableTable.collectSRSWViolations]).
  int writerOccurrencesHead = 0;

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

  /// Check if this variable is anonymous (name starts with _)
  bool get isAnonymous => name.startsWith('_');

  bool get isSRSWValid {
    // Anonymous variables (names starting with _) are exempt from SRSW
    // They denote fresh writers with no paired reader
    if (isAnonymous) return true;

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
  final Set<String> _groundedVars = {};

  // The variables a groundness-implying guard grounds at an occurrence of the
  // READER X?, which is what the relaxation of the writer is conditioned on:
  // "if the success of a guard implies that X? is bound to a ground term, then
  // both X and X? may occur multiple times in the clause" (GLP-Spec glp.tex,
  // Remark "Guards and SRSW", 831f315), guard arguments being "readers paired
  // to head writers" (Definition "Guarded Clause").
  final Set<String> _groundedByReaderVars = {};

  // The variables an occurrence of which has a type licensing a repeated
  // reader: a CONSTANT TYPE (TGLP typed-glp.tex, "Readers of constant types")
  // or `MutualRef` ("A reader of type MutualRef may also occur more than
  // once", typed-glp.tex, 350eb7d).  Such a reader may occur more than once in
  // the clause, ITS PAIRED WRITER OCCURRING ONCE.
  final Set<String> _typeLicensedVars = {};

  /// The head of the clause this table was built from, which a diagnostic
  /// names.
  Atom? head;

  /// Record a writer occurrence.
  /// [inHeadOrBody]: true if in head or body (counts toward SRSW), false if in guard
  /// [inHead]: true if in the clause head
  /// Anonymous variables (names starting with '_') are not tracked for SRSW.
  void recordWriterOccurrence(String name, AstNode node,
      {bool inHeadOrBody = true, bool inHead = false}) {
    // Skip anonymous variables - they don't participate in SRSW
    if (name.startsWith('_')) return;

    final info = _vars.putIfAbsent(name, () => VariableInfo(name, true));
    info.writerOccurrences++;
    if (inHeadOrBody) {
      info.writerOccurrencesHeadBody++;
    }
    if (inHead) {
      info.writerOccurrencesHead++;
    }
    info.firstOccurrence ??= node;
  }

  /// Record a reader occurrence.
  /// [inHeadOrBody]: true if in head or body (counts toward SRSW), false if in guard
  /// Anonymous variables (names starting with '_') are not tracked for SRSW.
  void recordReaderOccurrence(String name, AstNode node, {bool inHeadOrBody = true}) {
    // Skip anonymous variables - they don't participate in SRSW
    if (name.startsWith('_')) return;
    
    final writerName = name;  // Reader X? pairs with writer X

    // Ensure writer exists or create it
    final writerInfo = _vars.putIfAbsent(writerName, () => VariableInfo(writerName, true));

    // Record reader occurrence
    writerInfo.readerOccurrences++;
    if (inHeadOrBody) {
      writerInfo.readerOccurrencesHeadBody++;
    }
    writerInfo.firstOccurrence ??= node;
  }

  /// Mark [varName] grounded by a groundness-implying guard; [byReader] when
  /// the guard's occurrence is the reader X?.
  void markGrounded(String varName, {required bool byReader}) {
    _groundedVars.add(varName);
    if (byReader) _groundedByReaderVars.add(varName);
  }

  bool isGrounded(String varName) => _groundedVars.contains(varName);

  /// Whether the WRITER may occur more than once: "if the success of a guard
  /// implies that X? is bound to a ground term, then both X and X? may occur
  /// multiple times in the clause" (GLP-Spec glp.tex, Remark "Guards and
  /// SRSW", 831f315; TGLP glp.tex, the Remark of the same name) --- in the head
  /// as in the body.  The guard's occurrence is the reader X?, and X occurs in
  /// the head: "if X? occurs in a guard, its paired writer X must occur in the
  /// head" (the same Remark).  The groundness-implying guards are those
  /// [Analyzer._analyzeGuard] marks.  The type licence of a constant type or
  /// `MutualRef` is the reader's alone, SRSW* being SRSW "with that same
  /// permission, its paired writer occurring once" (TGLP typed-glp.tex), so it
  /// does not reach here.
  bool allowsMultipleWriters(String varName) =>
      _groundedByReaderVars.contains(varName) &&
      (_vars[varName]?.writerOccurrencesHead ?? 0) > 0;

  /// Mark a variable whose type at some occurrence licenses a repeated reader:
  /// a constant type (TGLP typed-glp.tex, Definition "Constant Type") or
  /// `MutualRef` (typed-glp.tex, 350eb7d).
  void markTypeLicensed(String varName) {
    _typeLicensedVars.add(varName);
  }

  bool isTypeLicensed(String varName) => _typeLicensedVars.contains(varName);

  /// Whether the READER may occur more than once.  Two relaxations reach here
  /// and both license it: a groundness-implying guard (TGLP glp.tex
  /// rem:guards-srsw) and the type of an occurrence --- a constant type
  /// (typed-glp.tex, Proposition "Readers of Constant Types") or `MutualRef`
  /// ("A reader of type MutualRef may also occur more than once").
  bool allowsMultipleReaders(String varName) =>
      isGrounded(varName) || isTypeLicensed(varName);

  /// Verify SRSW constraints and return list of violations (empty if valid)
  /// 
  /// SRSW rules:
  /// - Each variable must have exactly one writer
  /// - Each variable must have at least one reader
  /// - Guard occurrences count toward SRSW IF the guard implies groundness
  ///   (e.g., number(X?), integer(X?), ground(X?) mark X as grounded)
  /// - Grounded variables can be read multiple times safely
  List<String> collectSRSWViolations() {
    final violations = <String>[];

    for (final info in _vars.values) {
      // Skip anonymous variables (names starting with _) - they are exempt from SRSW
      // Each occurrence denotes a fresh writer with no paired reader
      if (info.isAnonymous) continue;

      // Check writer occurrences.  SRSW requires SO, "every variable occurs in
      // it at most once" (GLP-Spec glp.tex, Definitions "Single-Occurrence (SO)
      // Invariant" and "GLP Program"), and the one relaxation of the writer is
      // the guard's: "if the success of a guard implies that X? is bound to a
      // ground term, then both X and X? may occur multiple times in the
      // clause" (Remark "Guards and SRSW", 831f315, Udi's sentence restored on
      // 2026-10-12; TGLP glp.tex, the same Remark) --- in the head as in the
      // body ([allowsMultipleWriters]).  Without that guard the writer occurs
      // once; twice in the HEAD the diagnostic names the clause.  From
      // 2026-10-02 (b4cd4191, 905a9ed8, on GLP-Spec bbff21d, undone by
      // 831f315) a writer twice was refused whatever the guards.
      if (info.writerOccurrences > 1 && !allowsMultipleWriters(info.name)) {
        // A groundness-implying guard that grounds X here, but not as the
        // reader X? with X in the head, licenses no repeated writer.
        final why = isGrounded(info.name)
            ? '; a groundness-implying guard licenses a repeated writer X only '
                'through its reader X?, with X in the head'
            : ' without a groundness-implying guard';
        if (info.writerOccurrencesHead > 1) {
          final line = head?.line ?? info.firstOccurrence?.line ?? 0;
          final clause = head != null ? ' of the clause $head' : '';
          violations.add(
            'Line $line: Writer variable "${info.name}" occurs ${info.writerOccurrencesHead} times in the head$clause$why'
          );
        } else {
          final line = info.firstOccurrence?.line ?? 0;
          violations.add(
            'Line $line: Writer variable "${info.name}" occurs ${info.writerOccurrences} times$why'
          );
        }
      }

      // Check reader occurrences.  Guard occurrences do not count toward SRSW
      // satisfaction, so the count is readerOccurrencesHeadBody.  A reader may
      // occur more than once under either relaxation: a groundness-implying
      // guard, or the type of an occurrence --- a constant type or MutualRef
      // (TGLP typed-glp.tex, "Readers of constant types").
      if (info.readerOccurrencesHeadBody > 1 && !allowsMultipleReaders(info.name)) {
        final line = info.firstOccurrence?.line ?? 0;
        violations.add(
          'Line $line: Reader variable "${info.name}?" occurs ${info.readerOccurrencesHeadBody} times without a groundness-implying guard or a constant or MutualRef type'
        );
      }

      // Check for complete reader/writer pairs
      // Each variable must have exactly one writer
      if (info.writerOccurrences == 0) {
        final line = info.firstOccurrence?.line ?? 0;
        violations.add(
          'Line $line: Variable "${info.name}" has no writer (must have exactly one)'
        );
      }

      // Each variable must have at least one reader
      // Guard occurrences always count toward SRSW pairing (per spec Section 1.3, Remark 1)
      // A reader may appear once in guard AND once in body (two total allowed without groundness)
      if (info.readerOccurrences == 0 && info.writerOccurrences > 0) {
        final line = info.firstOccurrence?.line ?? 0;
        violations.add(
          'Line $line: Variable "${info.name}" has no reader (must have exactly one)'
        );
      }
    }

    return violations;
  }

  /// Legacy method - throws on first violation (for backwards compatibility)
  void verifySRSW() {
    final violations = collectSRSWViolations();
    if (violations.isNotEmpty) {
      // Find first violation's line for the error location
      final firstVar = _vars.values.firstWhere(
        (v) {
          if (v.writerOccurrences > 1 && !allowsMultipleWriters(v.name)) return true;
          if (v.readerOccurrences > 1 && !allowsMultipleReaders(v.name)) return true;
          if (v.writerOccurrences == 0) return true;
          if (v.readerOccurrences == 0 && v.writerOccurrences > 0) return true;
          return false;
        },
        orElse: () => _vars.values.first,
      );
      throw CompileError(
        'SRSW violation: ${violations.first.replaceFirst(RegExp(r'^Line \d+: '), '')}',
        firstVar.firstOccurrence?.line ?? 0,
        firstVar.firstOccurrence?.column ?? 0,
        phase: 'analyzer'
      );
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

  /// The scope the program is checked in.  The SRSW relaxation of a typed
  /// program is decided on the type each occurrence has (TGLP typed-glp.tex,
  /// "Readers of ground types"), and a type has a definition only in a scope,
  /// so without one no clause carries the relaxation.
  TypeEnvironment? _typeEnv;
  ProgramDFA? _dfa;
  bool _dfaBuilt = false;

  Analyzer();

  AnnotatedProgram analyze(Program program, {
    List<ProcDecl>? procDeclarations,
    TypeEnvironment? typeEnv,
  }) {
    _typeEnv = typeEnv;
    _dfa = null;
    _dfaBuilt = false;

    // STEP 1: Run SRSW validation on the ORIGINAL program
    // This must happen BEFORE partial evaluation, because partial eval removes defined guards.
    // Guard readers (like X? in send(X?, Ch?, Ch1)) must be counted for SRSW pairing.
    //
    // Every program that reaches here is checked, a linked program included:
    // it is the object checked (TGLP modules.tex §Compilation), and its alias
    // clauses satisfy SRSW by construction — each argument threaded at its
    // declared polarity, a writer in the head and its paired reader in the body
    // at each consumed argument, a reader in the head and its paired writer in
    // the body at each produced one (program_linker.dart, _makeAliasClause).
    // Until 2026-09-18 a `skipGlobalSRSW` flag let a linked program bypass the
    // pass on the claim that the aliases could not satisfy it.
    _checkSRSWAndGuards(program);

    // STEP 2: Transform defined guards via partial evaluation
    // After SRSW validation passes, we can safely transform defined guards.
    final transformed =
        _partialEvaluator.transformDefinedGuards(program, scope: _typeEnv);

    // No reduce/2 clauses are generated: a program that needs reduce/2 writes
    // it, as the book's meta-interpreters do.  Until 2026-10-02 every module
    // but a system one, and every linked program, had reduce(H, B) clauses
    // generated here for each of its clauses, after the SRSW pass and the type
    // check, so neither saw them --- and the object compiled was not the object
    // checked (Coordination #1, 2026-09-18).

    // STEP 3: Annotate clauses (register assignment) on the transformed program
    // SRSW already validated, so skip SRSW checking here
    final annotatedProcs = <AnnotatedProcedure>[];
    for (final proc in transformed.procedures) {
      final (annotatedProc, _) = _analyzeProcedureCollectingErrors(proc, skipSRSW: true);
      annotatedProcs.add(annotatedProc);
    }

    return AnnotatedProgram(transformed, annotatedProcs);
  }

  /// The SRSW pass and the guard check of [program], with no compilation:
  /// each clause satisfies SRSW (GLP-Spec glp.tex, Definition "GLP Program,
  /// Goals", with Remark "Guards and SRSW"), the relaxations of a typed
  /// program decided in [typeEnv], and calls only guard predicates in its
  /// guards (Definition "Guarded Clause").  Step 1 of [analyze], the pass
  /// every program compiled runs, given to the linker so that each module of
  /// a program's tree is checked by it, reached or not (TGLP modules.tex,
  /// Compilation, second step: "each module is checked against its
  /// scope---for SRSW, its guards and its types---exactly as a single-module
  /// program is, whether or not an entry point reaches it").  A procedure is
  /// named in a violation as [procedurePrefix] followed by its name: the
  /// linker passes the name step 3 gives it.
  ///
  /// Throws [CompileError] where a clause violates either.
  void checkSRSWAndGuards(Program program,
      {TypeEnvironment? typeEnv, String procedurePrefix = ''}) {
    _typeEnv = typeEnv;
    _dfa = null;
    _dfaBuilt = false;
    _checkSRSWAndGuards(program, procedurePrefix: procedurePrefix);
  }

  void _checkSRSWAndGuards(Program program, {String procedurePrefix = ''}) {
    final allViolations = <String>[];
    for (final proc in program.procedures) {
      allViolations.addAll(
          _collectSRSWViolationsForProcedure(proc, procedurePrefix));
    }

    // Report SRSW violations early (before partial evaluation)
    if (allViolations.isNotEmpty) {
      final message = 'SRSW violations found:\n${allViolations.map((v) => '  • $v').join('\n')}';
      throw CompileError(message, 0, 0, phase: 'analyzer');
    }
  }

  /// Collect SRSW violations for a procedure (for early validation before partial eval)
  List<String> _collectSRSWViolationsForProcedure(Procedure proc,
      [String procedurePrefix = '']) {
    final allViolations = <String>[];

    for (final clause in proc.clauses) {
      final varTable = VariableTable();

      // Analyze head
      _analyzeAtom(clause.head, varTable);

      // Analyze guards (if present) - THIS IS KEY: guard readers ARE counted
      if (clause.guards != null && clause.guards!.isNotEmpty) {
        for (final guard in clause.guards!) {
          _analyzeGuard(guard, varTable);
        }
      }

      // Analyze body (if present)
      if (clause.body != null && clause.body!.isNotEmpty) {
        for (final goal in clause.body!) {
          _analyzeGoal(goal, varTable);
        }
      }

      // Collect SRSW violations
      final violations = _srswViolations(clause, varTable);
      final contextViolations = violations.map((v) => '$procedurePrefix${proc.name}/${proc.arity}: $v').toList();
      allViolations.addAll(contextViolations);
    }

    return allViolations;
  }

  /// The clause's SRSW violations, the relaxations of a typed program applied.
  ///
  /// The type-based relaxation is consulted only where the counts have
  /// something to answer for: it costs a walk of the clause against the type
  /// automaton, and a clause with no multiple occurrence has nothing to relax.
  List<String> _srswViolations(Clause clause, VariableTable varTable) {
    var violations = varTable.collectSRSWViolations();
    if (violations.isEmpty) return violations;
    if (!_markTypeLicensedVars(clause, varTable)) return violations;
    violations = varTable.collectSRSWViolations();
    return violations;
  }

  /// Analyze procedure and collect SRSW violations instead of throwing
  (AnnotatedProcedure, List<String>) _analyzeProcedureCollectingErrors(Procedure proc, {bool skipSRSW = false}) {
    final annotatedClauses = <AnnotatedClause>[];
    final allViolations = <String>[];

    for (final clause in proc.clauses) {
      final (annotatedClause, violations) = _analyzeClauseCollectingErrors(
        clause, proc.name, proc.arity, skipSRSW: skipSRSW);
      annotatedClauses.add(annotatedClause);
      allViolations.addAll(violations);
    }

    return (AnnotatedProcedure(proc, proc.name, proc.arity, annotatedClauses), allViolations);
  }

  /// Analyze clause and collect SRSW violations instead of throwing
  /// [skipSRSW]: if true, skip SRSW validation (the program's was validated
  /// before partial evaluation)
  (AnnotatedClause, List<String>) _analyzeClauseCollectingErrors(
      Clause clause, String procName, int procArity, {bool skipSRSW = false}) {
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

    // Collect SRSW violations (unless skipped for auto-generated clauses)
    final violations = skipSRSW ? <String>[] : _srswViolations(clause, varTable);
    final contextViolations = violations.map((v) => '$procName/$procArity: $v').toList();

    // Assign register indices (even if there are violations, for partial analysis)
    _assignRegisters(varTable);

    return (AnnotatedClause(clause, varTable, hasGuards: hasGuards, hasBody: hasBody), contextViolations);
  }

  /// Analyze a clause head.
  void _analyzeAtom(Atom atom, VariableTable varTable) {
    varTable.head = atom;
    for (final arg in atom.args) {
      _analyzeTerm(arg, varTable, inHead: true);
    }
  }

  void _analyzeGoal(Goal goal, VariableTable varTable) {
    for (final arg in goal.args) {
      _analyzeTerm(arg, varTable);
    }
  }

  // Body-only constructs that are NOT valid guards
  static const _invalidInGuardPosition = {
    'true',   // true is body-only, not a guard
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

    // Special handling for ground/1
    // ground(X?) implies the argument is fully ground (no unbound variables)
    if (guard.predicate == 'ground' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        // ground(X?) licenses X and X? more than once (Remark "Guards and
        // SRSW")
        varTable.markGrounded(arg.name, byReader: arg.isReader);
      }
    }

    // Type-checking guards whose success implies the argument is ground: the
    // catalogue's "Ground: yes" (GLP-Spec appendix-guards.tex; TGLP glp.tex,
    // Remark "Guards and SRSW").  `compound` and `list` are "Ground: no" ---
    // `compound(f(X?))` and `list([X?])` succeed with X? unbound --- and mark
    // nothing (GLP, 2026-09-28).  `atom` and `tuple` are not in the catalogue
    // and are not guards: removed from the runtime on 2026-10-01 (GLP, approved
    // by Udi), with this table and the partial evaluator.  `real` is "Ground:
    // yes" since GLP-Spec 12be29b (GLP #3 Cowork, 2026-10-02 22:19 UTC).
    // Note: var/nonvar removed (don't guarantee groundness), float removed (not implemented)
    final typeCheckOps = ['number', 'integer', 'real', 'string', 'constant'];
    if (typeCheckOps.contains(guard.predicate) && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        // Mark X grounded: X and X? may occur more than once.
        varTable.markGrounded(arg.name, byReader: arg.isReader);
      }
    }

    // module/1 guard marks argument as ground (ModuleTerm is ground)
    if (guard.predicate == 'module' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        varTable.markGrounded(arg.name, byReader: arg.isReader);
      }
    }

    // `is_mutual_ref` marks nothing: the catalogue gives it "Ground: no"
    // (GLP-Spec appendix-guards.tex).  A repeated `Ref?` is licensed by the
    // occurrence's TYPE --- "A reader of type MutualRef may also occur more than
    // once" (TGLP typed-glp.tex, 350eb7d) --- in [_markTypeLicensedVars], where
    // the guard's own occurrence has the `MutualRef?` it narrows `Ref?` to.
    // Until 2026-10-01 it marked its argument grounded, which licensed a
    // repeated WRITER as well, and the root self.glp's mwm_main/2 and mwm1/4
    // read Ref? twice on that mark alone (GLP, 2026-10-01).

    // `unknown` marks nothing: the catalogue gives it "Ground: no" (GLP-Spec
    // appendix-guards.tex) --- it succeeds on an unbound variable --- so it
    // licenses no repeated occurrence (TGLP glp.tex, Remark "Guards and SRSW";
    // GLP, 2026-09-28).  Until 2026-09-29 it marked its argument grounded.

    // `wait` and `wait_until` mark nothing: the catalogue's time-guard table
    // has no Ground column (GLP-Spec appendix-guards.tex, Time guards), so
    // neither licenses a repeated occurrence (TGLP glp.tex, Remark "Guards and
    // SRSW").  A repeated `D?` of type Number is licensed by its type, a
    // constant type, as any reader of one is (TGLP typed-glp.tex, "Readers of
    // constant types").  Until 2026-10-02 each marked its argument grounded,
    // which licensed a repeated writer as well.

    // Comparison guards implicitly test groundness of both operands
    // Per spec: comparison guards require both operands to be bound numeric values,
    // which means they're ground. This allows multiple reader occurrences.
    // Arguments may be complex arithmetic expressions, so extract all reader variables.
    final comparisonOps = ['<', '>', '=<', '>=', '=:=', '=\\=', '@<'];
    if (comparisonOps.contains(guard.predicate) && guard.args.length == 2) {
      for (final arg in guard.args) {
        _extractAndMarkGroundedVars(arg, varTable);
      }
    }

    // Ground equality.  =?= "succeeds if both arguments are ground and equal",
    // Ground "yes (both)", and marks both (GLP-Spec appendix-guards.tex,
    // bbff21d).  =?\= marks nothing, Ground "no": it "succeeds if no readers
    // substitution makes them ground and equal", f(a, Z?) =?\= f(b, W?) with
    // Z? and W? unbound among its successes.  Until 2026-10-02 it marked both,
    // so neq_pair(X, Y, pair(X?, X?, Y?, Y?)) :- X? =?\= Y? | true loaded, and
    // its call neq_pair(f(a, Z?), f(b, W?), P) bound P to a term holding one
    // unbound variable twice (Integration #4 Code, 2026-10-02 10:52 UTC).
    if (guard.predicate == '=?=' && guard.args.length == 2) {
      for (final arg in guard.args) {
        if (arg is VarTerm) {
          varTable.markGrounded(arg.name, byReader: arg.isReader);
        }
      }
    }

    // Analyze guard arguments
    // Per spec: guard occurrences do NOT count toward SRSW validation.
    // A reader appearing only in guards does not satisfy the pairing requirement.
    // Pass inHeadOrBody: false so these occurrences are tracked but don't count for SRSW.
    for (final arg in guard.args) {
      _analyzeTerm(arg, varTable, inHeadOrBody: false);
    }
  }

  /// Extract all reader variables from a term and mark them as grounded.
  /// Used for arithmetic comparison guards where arguments may be complex expressions.
  ///
  /// A list is walked along its spine in a loop and its elements by recursive
  /// calls, so the depth used is the nesting of the term's elements and not the
  /// length of a list (TGLP well-typing.tex, Definition "Well-Typed Clause",
  /// puts no bound on a term's size).
  void _extractAndMarkGroundedVars(Term term, VariableTable varTable) {
    Term? t = term;
    while (t is ListTerm) {
      if (t.head != null) {
        _extractAndMarkGroundedVars(t.head!, varTable);
      }
      t = t.tail;
    }
    if (t is VarTerm) {
      // Mark the variable grounded: X and X? may occur more than once.
      varTable.markGrounded(t.name, byReader: t.isReader);
    } else if (t is StructTerm) {
      // Recurse into structure arguments (e.g., X? + 1 has args [X?, 1])
      for (final arg in t.args) {
        _extractAndMarkGroundedVars(arg, varTable);
      }
    }
    // ConstTerm and UnderscoreTerm have no variables to extract
  }

  /// Analyze a term, recording variable occurrences.
  /// [inHeadOrBody]: true if analyzing head or body (counts for SRSW),
  ///                 false if analyzing guards (does not count for SRSW)
  /// [inHead]: true if analyzing the clause head
  ///
  /// A list is walked along its spine in a loop and its elements by recursive
  /// calls, in the order the recursion took them, so the depth used is the
  /// nesting of the term's elements and not the length of a list.
  void _analyzeTerm(Term term, VariableTable varTable,
      {bool inHeadOrBody = true, bool inHead = false}) {
    Term? t = term;
    while (t is ListTerm) {
      if (t.head != null) {
        _analyzeTerm(t.head!, varTable,
            inHeadOrBody: inHeadOrBody, inHead: inHead);
      }
      t = t.tail;
    }
    if (t is VarTerm) {
      // Skip anonymous variables (names starting with '_') - exempt from SRSW
      // Each occurrence denotes a fresh writer with no paired reader
      if (t.name.startsWith('_')) return;

      if (t.isReader) {
        varTable.recordReaderOccurrence(t.name, t, inHeadOrBody: inHeadOrBody);
      } else {
        varTable.recordWriterOccurrence(t.name, t,
            inHeadOrBody: inHeadOrBody, inHead: inHead);
      }
    } else if (t is StructTerm) {
      for (final arg in t.args) {
        _analyzeTerm(arg, varTable, inHeadOrBody: inHeadOrBody, inHead: inHead);
      }
    }
    // A ConstTerm reached here is in argument position, and therefore data.
    // Until 2026-07-31 a reserved-name check stood here; the rule now restricts
    // underscore-prefixed names in CALL POSITION only, and leaves the prefix
    // unrestricted as data — as a message tag, or as a member of a type union
    // (TGLP appendix-root-self.tex, app:system-mode; GLP-Spec
    // appendix-guards.tex, "Naming and admission of body kernels"). Call and
    // definition position are checked by the loader, in primitive_layer.dart.
    // UnderscoreTerm has no variables to track
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

  /// Mark the clause's variables an occurrence of which has a type licensing a
  /// repeated reader --- a CONSTANT TYPE, or `MutualRef` ("A reader of type
  /// MutualRef may also occur more than once", TGLP typed-glp.tex, 350eb7d) ---
  /// and answer whether any was marked.
  ///
  /// The question is asked of the type each occurrence has, at every occurrence
  /// (TGLP typed-glp.tex, "Readers of constant types": the relaxation "holds
  /// wherever the occurrences sit --- in the head, nested within an argument, or
  /// in the body --- since it rests on what the term is and not on where it is
  /// read"), so a body variable read twice, a head variable at a nested
  /// `Integer` position and a head argument whose type is a user-defined union
  /// of constants are one case and not three.  Until 2026-09-23 the decision was
  /// a fixed list of five type NAMES --- Integer, Real, Number, String, Constant
  /// --- consulted at the top-level type name of a head argument alone, so
  /// `Colour ::= red ; green ; blue.` did not qualify where `String` did, and
  /// nothing nested and nothing in the body qualified at all.
  bool _markTypeLicensedVars(Clause clause, VariableTable varTable) {
    final env = _typeEnv;
    if (env == null) return false;
    final dfa = _programDfa(env);
    if (dfa == null) return false;
    final Set<String> licensed;
    try {
      licensed = repeatableReaderVariables(clause, dfa, env);
    } on UnknownTypeError {
      // A type the scope does not carry: the type checker reports it; nothing
      // is relaxed on a type that cannot be resolved.
      return false;
    }
    if (licensed.isEmpty) return false;
    for (final name in licensed) {
      varTable.markTypeLicensed(name);
    }
    return true;
  }

  /// The program DFA of [env], built once per [analyze].
  ProgramDFA? _programDfa(TypeEnvironment env) {
    if (_dfaBuilt) return _dfa;
    _dfaBuilt = true;
    try {
      _dfa = buildProgramDFA(env);
    } on UnknownTypeError {
      _dfa = null;
    }
    return _dfa;
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
  ///
  /// The defined guards are [scope]'s unit clauses and the program's own, the
  /// program's shadowing the scope's: the scope is the one the program was
  /// checked in, passed in with it ([Analyzer.analyze]'s `typeEnv`); a linked
  /// program carries the root self.glp's among its own procedures.
  Program transformDefinedGuards(Program program, {TypeEnvironment? scope}) {
    final unitClauses = {
      ...unitClausesOf(scope?.scopeClauses ?? const {}),
      ..._collectUnitClauses(program)
    };

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
  /// A unit clause has exactly one clause, no guards and no body (GLP-Spec
  /// appendix-guards.tex, Defined guard predicates); a clause whose body is
  /// `true` is not one and defines no guard (until 2026-10-02 it counted).
  Map<String, List<Term>> _collectUnitClauses(Program program) {
    final Map<String, List<Term>> unitClauses = {};

    for (final proc in program.procedures) {
      // Must have exactly one clause
      if (proc.clauses.length != 1) continue;

      final clause = proc.clauses.first;

      // Must have no guards
      if (clause.guards != null && clause.guards!.isNotEmpty) continue;

      // Must have no body
      if (clause.body != null && clause.body!.isNotEmpty) continue;

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
  /// IMPORTANT: Anonymous variables (names starting with '_') are NOT renamed - 
  /// they stay as-is since each occurrence is independent.
  List<Term> _renameUnitClauseVars(List<Term> args) {
    // First, collect all variable names in the unit clause
    final varNames = <String>{};
    for (final arg in args) {
      _collectVarNames(arg, varNames);
    }

    // Build renaming map (skip anonymous variables - those starting with '_')
    // Note: PE-generated vars must NOT start with underscore, or they'll be treated as anonymous
    final Map<String, String> renaming = {};
    for (final name in varNames) {
      if (!name.startsWith('_')) {
        renaming[name] = 'PE_${_varCounter++}';
      }
    }

    // Apply renaming to all args
    return args.map((arg) => _applyRenaming(arg, renaming)).toList();
  }

  /// Collect all variable names in a term
  ///
  /// A list is walked along its spine in a loop and its elements by recursive
  /// calls, so the depth used is the nesting of the term's elements and not the
  /// length of a list (TGLP well-typing.tex, Definition "Well-Typed Clause",
  /// puts no bound on a term's size).
  void _collectVarNames(Term term, Set<String> names) {
    Term? t = term;
    while (t is ListTerm) {
      if (t.head != null) _collectVarNames(t.head!, names);
      t = t.tail;
    }
    if (t is VarTerm) {
      names.add(t.name);
    } else if (t is StructTerm) {
      for (final arg in t.args) {
        _collectVarNames(arg, names);
      }
    }
  }

  /// Apply variable renaming to a term recursively
  ///
  /// A list is walked along its spine in a loop and its elements by recursive
  /// calls, so the depth used is the nesting of the term's elements and not the
  /// length of a list (TGLP well-typing.tex, Definition "Well-Typed Clause",
  /// puts no bound on a term's size).
  Term _applyRenaming(Term term, Map<String, String> renaming) {
    if (term is ListTerm) {
      final cells = <ListTerm>[];
      final heads = <Term?>[];
      Term? t = term;
      while (t is ListTerm) {
        cells.add(t);
        heads.add(t.head != null ? _applyRenaming(t.head!, renaming) : null);
        t = t.tail;
      }
      Term? renamed = t != null ? _applyRenaming(t, renaming) : null;
      for (var i = cells.length - 1; i >= 0; i--) {
        renamed = ListTerm(heads[i], renamed, cells[i].line, cells[i].column);
      }
      return renamed!;
    }
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

  /// Set subst[key] = value, propagating to any existing alias.
  /// If subst[key] was previously a VarTerm (alias), also bind that variable.
  void _substSet(Map<String, Term> subst, String key, Term value) {
    if (subst.containsKey(key)) {
      final old = subst[key]!;
      if (old is VarTerm && !old.isReader && value is! VarTerm) {
        if (!subst.containsKey(old.name)) {
          subst[old.name] = value;
        }
      }
    }
    subst[key] = value;
  }

  /// Unify two terms, updating substitution and suspension set.
  /// Returns UnifyFail on structural mismatch, null on success.
  UnifyResult? _unifyTerms(
    Term callArg,
    Term unitArg,
    Map<String, Term> subst,
    Set<String> suspSet
  ) {
    // Two non-empty lists, matched along their spines in a loop and their
    // elements by recursive calls, as the list case below matches them: so
    // the depth used is the nesting of the terms' elements and not the length
    // of a list (TGLP well-typing.tex, Definition "Well-Typed Clause", puts no
    // bound on a term's size).
    while (callArg is ListTerm &&
        unitArg is ListTerm &&
        !callArg.isNil &&
        !unitArg.isNil) {
      if (callArg.head != null && unitArg.head != null) {
        final headResult = _unifyTerms(callArg.head!, unitArg.head!, subst, suspSet);
        if (headResult != null) return headResult;
      }
      if (callArg.tail == null || unitArg.tail == null) return null;
      callArg = callArg.tail!;
      unitArg = unitArg.tail!;
    }

    // Handle underscore on either side - always succeeds, no binding
    if (_isAnonymous(callArg) || _isAnonymous(unitArg)) {
      // Except a writer against a writer: an anonymous variable is a writer
      // of its own (GLP-Spec glp.tex, Remark "Anonymous Variables"), and a
      // call writer against a head writer fails (appendix-term-matching.tex,
      // row "Writer X1", column "Writer X2").  Until 2026-10-02 it was passed
      // over.
      if (_isWriterTerm(callArg) && _isWriterTerm(unitArg)) {
        return UnifyFail('Writer $callArg cannot match the head writer $unitArg');
      }
      return null; // success, continue
    }

    // Case: call arg is writer (VarTerm, not reader)
    if (callArg is VarTerm && !callArg.isReader) {
      if (unitArg is VarTerm && !unitArg.isReader) {
        // Call writer vs head writer: FAIL.  GLP-Spec appendix-term-matching.tex,
        // Definition "Term Matching": row "Writer X1", column "Writer X2".
        // Until 2026-10-02 the unit writer was aliased to the call writer.
        return UnifyFail(
            'Writer ${callArg.name} cannot match the head writer ${unitArg.name}');
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
        // Goal reader vs head reader: FAIL.  GLP-Spec appendix-term-matching.tex,
        // Definition "Term Matching": row "Reader X1?", column "Reader X2?".
        // Until 2026-10-02 the two were aliased and the goal suspended.
        return UnifyFail(
            'Reader $writerName? cannot match the head reader ${unitArg.name}?');
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
        _substSet(subst, unitArg.name, callArg);
        return null;
      } else if (unitArg is VarTerm && unitArg.isReader) {
        // Constant vs head reader: FAIL (appendix-term-matching.tex, row "Term
        // f1/n1", column "Reader X2?"; a constant is f/0).  Until 2026-10-02 the
        // head reader was bound to the constant.
        return UnifyFail(
            'Constant ${callArg.value} cannot match the head reader ${unitArg.name}?');
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
        _substSet(subst, unitArg.name, callArg);
        return null;
      } else if (unitArg is VarTerm && unitArg.isReader) {
        // Structure vs head reader: FAIL (appendix-term-matching.tex, row "Term
        // f1/n1", column "Reader X2?").  Until 2026-10-02 the head reader was
        // bound to the structure.
        return UnifyFail(
            'Structure ${callArg.functor} cannot match the head reader ${unitArg.name}?');
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
        // Both non-nil - recurse on head and tail (the loop at the top of
        // this function matches two non-empty lists before this case)
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
        _substSet(subst, unitArg.name, callArg);
        return null;
      } else if (unitArg is VarTerm && unitArg.isReader) {
        // List vs head reader: FAIL (appendix-term-matching.tex, row "Term
        // f1/n1", column "Reader X2?"; a list is '[]'/0 or '.'/2).  Until
        // 2026-10-02 the head reader was bound to the list.
        return UnifyFail('List cannot match the head reader ${unitArg.name}?');
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

  /// Check if a term is an anonymous variable.
  /// Anonymous variables are UnderscoreTerm or any VarTerm whose name starts with '_'.
  bool _isAnonymous(Term term) {
    return term is UnderscoreTerm || (term is VarTerm && term.name.startsWith('_'));
  }

  /// A writer occurrence, named or anonymous.
  bool _isWriterTerm(Term term) =>
      (term is UnderscoreTerm && !term.isReader) ||
      (term is VarTerm && !term.isReader);

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
  ///
  /// A list is walked along its spine in a loop and its elements by recursive
  /// calls, each given a copy of [visited] as the recursion gave it, so the
  /// depth used is the nesting of the term's elements and not the length of a
  /// list (TGLP well-typing.tex, Definition "Well-Typed Clause", puts no bound
  /// on a term's size).
  Term _resolveTerm(Term term, Map<String, Term> subst, Set<String> visited) {
    if (term is ListTerm && !term.isNil) {
      final cells = <ListTerm>[];
      final heads = <Term?>[];
      Term? t = term;
      while (t is ListTerm && !t.isNil) {
        cells.add(t);
        heads.add(
            t.head != null ? _resolveTerm(t.head!, subst, {...visited}) : null);
        t = t.tail;
      }
      Term? resolved = t != null ? _resolveTerm(t, subst, {...visited}) : null;
      for (var i = cells.length - 1; i >= 0; i--) {
        resolved = ListTerm(heads[i], resolved, cells[i].line, cells[i].column);
      }
      return resolved!;
    }
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
    return term; // Constants, underscores, the empty list, etc.
  }

  /// Apply substitution to a term
  ///
  /// A list is walked along its spine in a loop and its elements by recursive
  /// calls, so the depth used is the nesting of the term's elements and not the
  /// length of a list (TGLP well-typing.tex, Definition "Well-Typed Clause",
  /// puts no bound on a term's size).
  Term _applySubstitution(Term term, Map<String, Term> subst) {
    if (term is ListTerm && !term.isNil) {
      final cells = <ListTerm>[];
      final heads = <Term?>[];
      Term? t = term;
      while (t is ListTerm && !t.isNil) {
        cells.add(t);
        heads.add(t.head != null ? _applySubstitution(t.head!, subst) : null);
        t = t.tail;
      }
      Term? applied = t != null ? _applySubstitution(t, subst) : null;
      for (var i = cells.length - 1; i >= 0; i--) {
        applied = ListTerm(heads[i], applied, cells[i].line, cells[i].column);
      }
      return applied!;
    }
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
