import 'ast.dart';
import 'error.dart';
import 'partial_evaluator.dart' show PartialEvaluator;
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

  // Reader occurrences in the clause head alone: where the writer occurs in
  // a widowed_writer guard, its reader occurs here and nowhere else (the GLP paper
  // f577e20, Remark "Guards and SRSW").
  int readerOccurrencesHead = 0;

  // First occurrence location
  AstNode? firstOccurrence;

  // Register assignment (filled during analysis)
  int? registerIndex;

  VariableInfo(this.name, this.isWriter);

  /// Check if this variable is anonymous (name starts with _)
  bool get isAnonymous => name.startsWith('_');

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
  void recordReaderOccurrence(String name, AstNode node,
      {bool inHeadOrBody = true, bool inHead = false}) {
    // Skip anonymous variables - they don't participate in SRSW
    if (name.startsWith('_')) return;
    
    final writerName = name;  // Reader X? pairs with writer X

    // Ensure writer exists or create it
    final writerInfo = _vars.putIfAbsent(writerName, () => VariableInfo(writerName, true));

    // Record reader occurrence
    writerInfo.readerOccurrences++;
    if (inHead) writerInfo.readerOccurrencesHead++;
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

  // The variables occurring in a widowed guard, each as the mode it occurs
  // there in: the writer X (`widowed_writer(X)`) or the reader X?
  // (`widowed_reader(X?)`) (the GLP paper 07835e1, glp.tex, Remark "Guards
  // and SRSW": "if X occurs in a widowed_writer guard, X? occurs in the
  // head, X in the guard, and neither elsewhere in the clause; and if X?
  // occurs in a widowed_reader guard, X occurs in the head, X? in the guard,
  // and neither elsewhere in the clause"; Definition "SRSW of a Guarded
  // Clause": the iff of a variable and its paired one holds but for such a
  // variable).
  final Map<String, bool> _widowedVars = {};

  /// Mark [varName] as occurring in a widowed_writer guard, as a writer, or,
  /// where [asReader], in a widowed_reader guard, as a reader.
  void markWidowed(String varName, {required bool asReader}) =>
      _widowedVars[varName] = asReader;

  bool isWidowed(String varName) => _widowedVars.containsKey(varName);

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

      // A variable of a widowed guard: the variable in the guard, its pair
      // in the head, and neither elsewhere in the clause (the GLP paper
      // 07835e1, Remark "Guards and SRSW" and Definition "SRSW of a Guarded
      // Clause").  `widowed_writer(X)`: X? once in the head, X in the guard
      // and nowhere else; `widowed_reader(X?)`: X once in the head, X? in
      // the guard and nowhere else.
      final widowedAsReader = _widowedVars[info.name];
      if (widowedAsReader != null) {
        final line = info.firstOccurrence?.line ?? 0;
        if (!widowedAsReader) {
          // widowed_writer(X): the writer in the guard, the reader in the head.
          if (info.readerOccurrencesHead != 1 ||
              info.readerOccurrencesHeadBody != 1) {
            violations.add('Line $line: Reader variable "${info.name}?" of a '
                'widowed_writer guard over "${info.name}" must occur once, '
                'in the head, and nowhere else in the clause');
          }
          if (info.writerOccurrencesHeadBody != 0 ||
              info.writerOccurrences != 1) {
            violations.add('Line $line: Writer variable "${info.name}" of a '
                'widowed_writer guard occurs in the guard and nowhere else '
                'in the clause');
          }
        } else {
          // widowed_reader(X?): the reader in the guard, the writer in the head.
          if (info.writerOccurrencesHead != 1 ||
              info.writerOccurrencesHeadBody != 1 ||
              info.writerOccurrences != 1) {
            violations.add('Line $line: Writer variable "${info.name}" of a '
                'widowed_reader guard over "${info.name}?" must occur once, '
                'in the head, and nowhere else in the clause');
          }
          if (info.readerOccurrencesHeadBody != 0 ||
              info.readerOccurrences != 1) {
            violations.add('Line $line: Reader variable "${info.name}?" of a '
                'widowed_reader guard occurs in the guard and nowhere else '
                'in the clause');
          }
        }
        continue;
      }

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
      // Guard occurrences count toward SRSW pairing (GLP-Spec glp.tex, Remark
      // "Guards and SRSW": a reader X? in a guard is paired to the writer X in
      // the head)
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
      annotatedProcs.add(_annotateProcedure(proc));
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
      // Guard readers ARE counted ([_tableOf]).
      final varTable = _tableOf(clause);

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

  /// Annotate a procedure's clauses: each clause's variable table with its
  /// registers assigned.  The program's SRSW was validated before partial
  /// evaluation, so none is checked here.
  AnnotatedProcedure _annotateProcedure(Procedure proc) {
    final annotatedClauses = <AnnotatedClause>[];

    for (final clause in proc.clauses) {
      final varTable = _tableOf(clause);
      final hasGuards = clause.guards != null && clause.guards!.isNotEmpty;
      final hasBody = clause.body != null && clause.body!.isNotEmpty;

      // Assign register indices
      _assignRegisters(varTable);

      annotatedClauses.add(
          AnnotatedClause(clause, varTable, hasGuards: hasGuards, hasBody: hasBody));
    }

    return AnnotatedProcedure(proc, proc.name, proc.arity, annotatedClauses);
  }

  /// The variable table of [clause]: its head, its guards and its body
  /// walked in that order, every occurrence recorded.
  VariableTable _tableOf(Clause clause) {
    final varTable = VariableTable();

    // Analyze head
    _analyzeAtom(clause.head, varTable);

    // Analyze guards (if present)
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

    return varTable;
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

  // The type-checking guards whose success implies the argument is ground
  // ([_analyzeGuard]).
  static const _typeCheckOps = ['number', 'integer', 'real', 'string', 'constant'];

  // The comparison guards, which ground both operands ([_analyzeGuard]).
  static const _comparisonOps = ['<', '>', '=<', '>=', '=:=', '=\\=', '@<'];

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

    // widowed_writer(X) and widowed_reader(X?): the variable in the guard,
    // its pair in the head, neither elsewhere
    // ([VariableTable.collectSRSWViolations]; the GLP paper 07835e1, Remark
    // "Guards and SRSW").  The mode of the argument is the declaration's,
    // `procedure widowed_writer(_).` and `procedure widowed_reader(_?).`,
    // which the type checker holds it to.
    if ((guard.predicate == 'widowed_writer' ||
            guard.predicate == 'widowed_reader') &&
        guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        varTable.markWidowed(arg.name,
            asReader: guard.predicate == 'widowed_reader');
      }
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
    if (_typeCheckOps.contains(guard.predicate) && guard.args.length == 1) {
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
    if (_comparisonOps.contains(guard.predicate) && guard.args.length == 2) {
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
        varTable.recordReaderOccurrence(t.name, t,
            inHeadOrBody: inHeadOrBody, inHead: inHead);
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
