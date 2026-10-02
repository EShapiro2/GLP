import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/bytecode/runner.dart'
    show BytecodeProgram, runtimeGuards;
import 'ast.dart';
import 'analyzer.dart';
import 'error.dart';
import 'result.dart';

/// Code generation context
class CodeGenContext {
  // Bytecode accumulator: the instruction objects, labels among them
  final List<bc.Op> instructions = [];

  // Label management
  final Map<String, int> labels = {};
  final List<String> pendingLabels = [];  // Labels waiting to be placed

  // Temporary variable allocation
  int nextTempVar = 10;  // Start temps at 10 to avoid collision with argument registers
  final Map<String, int> tempAllocation = {};

  // Current procedure context
  String? currentProcedure;
  int currentClauseIndex = 0;

  // Phase tracking
  bool inHead = false;
  bool inGuard = false;
  bool inBody = false;

  // Track variables seen in head (for GetVariable vs GetValue)
  final Set<String> seenHeadVars = {};

  int get currentPC => instructions.length;

  void emit(bc.Op instruction) {
    instructions.add(instruction);
  }

  void emitLabel(String label) {
    // Record label position in map
    labels[label] = currentPC;
    // Emit the Label instruction
    instructions.add(bc.Label(label));
  }

  int allocateTemp() => nextTempVar++;

  void resetTemps(int variableCount, int argCount) {
    // Temp registers share the operand space with argument slots (0..argCount-1)
    // and clause variables (0..variableCount-1); start them above both so a temp
    // index can never alias a real argument slot. The floor of 10 preserves the
    // historical numbering for procedures of arity <= 10 (their bytecode is
    // unchanged); arity >= 11 pushes temps above the argument slots, which is
    // what makes high-arity clause dispatch correct — the code-format spec makes
    // argSlot an unbounded clen, not a 0..9 register.
    final base = variableCount > argCount ? variableCount : argCount;
    nextTempVar = base > 10 ? base : 10;
    tempAllocation.clear();
  }
}

/// Code generator - transforms annotated AST to bytecode
class CodeGenerator {
  BytecodeProgram generate(AnnotatedProgram program) {
    final result = generateWithMetadata(program);
    return result.program;
  }

  CompilationResult generateWithMetadata(AnnotatedProgram program) {
    final ctx = CodeGenContext();
    final variableMap = <String, int>{};

    // Generate code for each procedure
    for (final proc in program.procedures) {
      _generateProcedure(proc, ctx);

      // Collect variable mappings from the first procedure (for goals)
      // This captures variables used in queries like "merge([1,2,3], [a,b], Xs)."
      if (proc == program.procedures.first) {
        for (final clause in proc.clauses) {
          for (final varInfo in clause.varTable.getAllVars()) {
            if (varInfo.registerIndex != null) {
              variableMap[varInfo.name] = varInfo.registerIndex!;
            }
          }
        }
      }
    }

    // Build final bytecode program using runner's BytecodeProgram
    // It will auto-index labels from Label instructions
    final bytecode = BytecodeProgram(ctx.instructions);

    return CompilationResult(bytecode, variableMap);
  }

  void _generateProcedure(AnnotatedProcedure proc, CodeGenContext ctx) {
    ctx.currentProcedure = proc.signature;

    // Entry label: "p/1", "merge/3", etc.
    final entryLabel = proc.signature;
    ctx.emitLabel(entryLabel);

    // Record entry PC (κ)
    proc.entryPC = ctx.currentPC;
    proc.entryLabel = entryLabel;

    // Generate each clause
    for (int i = 0; i < proc.clauses.length; i++) {
      ctx.currentClauseIndex = i;
      final isLastClause = (i == proc.clauses.length - 1);

      final clause = proc.clauses[i];
      final nextLabel = isLastClause
          ? '${entryLabel}_end'
          : '${entryLabel}_c${i + 1}';

      _generateClause(clause, ctx, nextLabel, isLastClause);
    }

    // End of procedure
    ctx.emitLabel('${entryLabel}_end');
    ctx.emit(bc.NoMoreClauses());  // Suspend if U non-empty, else fail
  }

  void _generateClause(AnnotatedClause clause, CodeGenContext ctx, String nextLabel, bool isLastClause) {
    ctx.resetTemps(clause.varTable.getAllVars().length, clause.ast.head.arity);
    ctx.seenHeadVars.clear();  // Clear head variable tracking for new clause

    // Emit label for non-first clauses
    if (ctx.currentClauseIndex > 0) {
      ctx.emitLabel('${ctx.currentProcedure}_c${ctx.currentClauseIndex}');
    }

    // CLAUSE_TRY: Initialize Si=∅, σ̂w=∅
    ctx.emit(bc.ClauseTry());

    // HEAD PHASE
    ctx.inHead = true;
    ctx.inGuard = false;
    ctx.inBody = false;

    _generateHead(clause.ast.head, clause.varTable, ctx);

    // GUARD PHASE (if present)
    if (clause.hasGuards && clause.ast.guards != null) {
      ctx.inHead = false;
      ctx.inGuard = true;

      for (final guard in clause.ast.guards!) {
        _generateGuard(guard, clause.varTable, ctx);
      }
    }

    // COMMIT: If we reach this point, Si must be empty, so commit
    ctx.emit(bc.Commit());  // Apply σ̂w, enter BODY phase

    // BODY PHASE
    ctx.inHead = false;
    ctx.inGuard = false;
    ctx.inBody = true;

    if (clause.hasBody && clause.ast.body != null) {
      // SPECIAL CASE: Body is just "true" - treat like a fact (no spawn)
      if (clause.ast.body!.length == 1 &&
          clause.ast.body![0].functor == 'true' &&
          clause.ast.body![0].arity == 0) {
        // Just succeed without spawning
        ctx.emit(bc.Proceed());
      } else {
        // Normal body with real goals
        _generateBody(clause.ast.body!, clause.varTable, ctx);
      }
    } else {
      // Empty body: just proceed
      ctx.emit(bc.Proceed());
    }
  }

  void _generateHead(Atom head, VariableTable varTable, CodeGenContext ctx) {
    // Process each head argument
    for (int i = 0; i < head.args.length; i++) {
      final arg = head.args[i];
      _generateHeadArgument(arg, i, varTable, ctx);
    }
  }

  /// A named anonymous variable, `_X` or `_X?`, as the `_` or `_?` it is: TGLP
  /// typed-glp.tex, "Anonymous variables": "An anonymous variable is any
  /// variable whose name begins with `_` ... Each occurrence denotes a fresh
  /// writer with no paired reader".  The analyzer keeps no register for one,
  /// so it is compiled where `_` is, each occurrence a variable of its own.
  /// Until 2026-10-02 it was looked up as a named variable and refused,
  /// "Undefined variable: _X".
  static Term _anonymousAsUnderscore(Term term) =>
      term is VarTerm && term.name.startsWith('_')
          ? UnderscoreTerm(term.line, term.column, isReader: term.isReader)
          : term;

  void _generateHeadArgument(Term term, int argSlot, VariableTable varTable, CodeGenContext ctx) {
    term = _anonymousAsUnderscore(term);
    if (term is VarTerm) {
      // Get variable register index
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      // Check if this is the first occurrence in the head
      final baseVarName = term.name.endsWith('?') ? term.name.substring(0, term.name.length - 1) : term.name;
      final isFirstOccurrence = !ctx.seenHeadVars.contains(baseVarName);

      if (isFirstOccurrence) {
        // First occurrence: emit GetVariable
        ctx.emit(bc.GetVariable(regIndex, argSlot, isReader: term.isReader));
        ctx.seenHeadVars.add(baseVarName);
      } else {
        // Subsequent occurrence: emit GetValue
        ctx.emit(bc.GetValue(regIndex, argSlot, isReader: term.isReader));
      }

    } else if (term is ConstTerm) {
      // Constant in head: match with head_constant
      ctx.emit(bc.HeadConstant(term.value, argSlot));

    } else if (term is ListTerm) {
      if (term.isNil) {
        // Empty list: head_nil
        ctx.emit(bc.HeadNil(argSlot));
      } else {
        // Non-empty list [H|T]: treat as structure '.'(H, T)
        ctx.emit(bc.HeadStructure('.', 2, argSlot));

        // Process head element
        if (term.head != null) {
          _generateStructureElement(term.head!, varTable, ctx, inHead: true);
        }

        // Process tail element
        if (term.tail != null) {
          _generateStructureElement(term.tail!, varTable, ctx, inHead: true);
        }
      }

    } else if (term is StructTerm) {
      // FIX: For structures as direct HEAD arguments, extract first then match
      // This avoids overlapping HeadStructure operations

      // Step 1: Extract the argument into a temp register.
      // tempReg is freshly allocated (first occurrence), so get_variable
      // captures the argument into clauseVars without sigmaHat binding; it is
      // the polarity-carrying get_variable of §4.2 (D3 wire format).
      final tempReg = ctx.allocateTemp();
      ctx.emit(bc.GetVariable(tempReg, argSlot, isReader: false));

      // Step 2: Match the structure at the temp register (not argSlot!)
      ctx.emit(bc.HeadStructure(term.functor, term.arity, tempReg));

      // FCP AM: Process ALL arguments inline using Push/Pop for nested structures
      // _generateStructureElement already has correct Push/Pop logic (lines 335-361)
      for (final subArg in term.args) {
        _generateStructureElement(subArg, varTable, ctx, inHead: true);
      }

    } else if (term is UnderscoreTerm) {
      if (term.isReader) {
        // `_?`: "an output the clause never produces" (TGLP typed-glp.tex,
        // "Anonymous variables"), the placeholder `Out?` of a variable whose
        // writer occurs nowhere in the clause --- a head reader of a variable
        // of its own.  So the table's column "Reader X2?" matches it
        // (GLP-Spec appendix-term-matching.tex, Definition "Term Matching"):
        // a goal writer is assigned it, a goal reader and a goal term fail.
        // It was compiled as `_` is, to nothing, so `p(_, _?)` took `p(1, 2)`.
        ctx.emit(bc.GetVariable(ctx.allocateTemp(), argSlot, isReader: true));
      }
      // `_`: a head writer whose value is discarded; the argument is simply
      // not extracted.
    }
  }

  void _generateStructureElement(Term term, VariableTable varTable, CodeGenContext ctx, {required bool inHead}) {
    // Called during structure traversal (S register in use)
    term = _anonymousAsUnderscore(term);

    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      // UnifyVariable in the occurrence's syntactic mode
      ctx.emit(bc.UnifyVariable(regIndex, isReader: term.isReader));

    } else if (term is ConstTerm) {
      // Constant at position S
      ctx.emit(bc.UnifyConstant(term.value));

    } else if (term is ListTerm) {
      if (term.isNil) {
        // Nil is atomic constant - same for HEAD and BODY modes
        ctx.emit(bc.UnifyConstant('nil'));
      } else {
        // Non-empty list: use Push/UnifyStructure/Pop pattern
        if (inHead) {
          final saveReg = ctx.allocateTemp();
          ctx.emit(bc.Push(saveReg));
          ctx.emit(bc.UnifyStructure('.', 2));
          if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: true);
          if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: true);
          ctx.emit(bc.Pop(saveReg));
          // FCP AM: After Pop, must place nested structure at S and increment
          ctx.emit(bc.UnifyVariable(saveReg, isReader: false));
        } else {
          // WRITE mode (BODY): building nested structure within argument structure
          final tempReg = ctx.allocateTemp();
          ctx.emit(bc.PutStructure('.', 2, tempReg));
          if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: inHead);
          if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: inHead);
          ctx.emit(bc.UnifyVariable(tempReg, isReader: false));
        }
      }

    } else if (term is StructTerm) {
      // Nested structure: use Push/UnifyStructure/Pop pattern (FCP AM design)
      if (inHead) {
        final saveReg = ctx.allocateTemp();
        ctx.emit(bc.Push(saveReg));
        ctx.emit(bc.UnifyStructure(term.functor, term.arity));
        for (final subArg in term.args) {
          _generateStructureElement(subArg, varTable, ctx, inHead: true);
        }
        ctx.emit(bc.Pop(saveReg));
        // FCP AM: After Pop, must place nested structure at S and increment
        ctx.emit(bc.UnifyVariable(saveReg, isReader: false));
      } else {
        // WRITE mode
        final tempReg = ctx.allocateTemp();
        ctx.emit(bc.PutStructure(term.functor, term.arity, tempReg));
        for (final subArg in term.args) {
          _generateStructureElement(subArg, varTable, ctx, inHead: inHead);
        }
        ctx.emit(bc.UnifyVariable(tempReg, isReader: false));
      }

    } else if (term is UnderscoreTerm) {
      if (term.isReader) {
        // `_?` in a head structure: a head reader of a variable of its own, as
        // at an argument above.  unify_void, which `_` compiles to, passes
        // over whatever the goal holds here, and in a structure built for a
        // goal writer places a fresh writer where the clause's output
        // placeholder stands.
        ctx.emit(bc.UnifyVariable(ctx.allocateTemp(), isReader: true));
      } else {
        // `_` in a head structure: a fresh writer, its value discarded.
        ctx.emit(bc.UnifyVoid(count: 1));
      }
    }
  }

  void _generateGuard(Guard guard, VariableTable varTable, CodeGenContext ctx) {
    // Special built-in guards
    if (guard.predicate == 'ground' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        final varInfo = varTable.getVar(arg.name);
        if (varInfo != null) {
          ctx.emit(bc.Ground(varInfo.registerIndex!));
          return;
        }
      }
    }

    if (guard.predicate == 'known' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        final varInfo = varTable.getVar(arg.name);
        if (varInfo != null) {
          ctx.emit(bc.Known(varInfo.registerIndex!));
          return;
        }
      }
    }

    if (guard.predicate == 'no_readers' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        final varInfo = varTable.getVar(arg.name);
        if (varInfo != null) {
          ctx.emit(bc.NoReaders(varInfo.registerIndex!));
          return;
        }
      }
    }

    if (guard.predicate == 'otherwise' && guard.args.isEmpty) {
      ctx.emit(bc.Otherwise());
      return;
    }

    // Ground equality guard: X =?= Y with both operands variables is the
    // ground equality instruction (0x45).  An operand that is not a variable
    // takes the generic guard call below.  So does X =?\= Y, whatever its
    // operands: 0x45 has no negated operand (IGLP code-format-fragment.tex,
    // 9b45225), and =?\= is called by name, a builtin guard of the runtime.
    if (guard.predicate == '=?=' && guard.args.length == 2) {
      final leftArg = guard.args[0];
      final rightArg = guard.args[1];
      if (leftArg is VarTerm && rightArg is VarTerm) {
        final leftInfo = varTable.getVar(leftArg.name);
        final rightInfo = varTable.getVar(rightArg.name);
        if (leftInfo != null && rightInfo != null) {
          ctx.emit(bc.GroundEqual(
            leftInfo.registerIndex!,
            rightInfo.registerIndex!,
          ));
          return;
        }
      }
    }

    // Generic guard predicate call (runtime evaluation).  A guard that is not
    // one the runtime evaluates is refused here, at compile time: defined
    // guards were unfolded before code generation (GLP-Spec appendix-guards
    // .tex, Defined guard predicates), so what reaches here names a guard
    // predicate of the catalogue or nothing.  Until 2026-10-02 the runtime
    // printed a [WARN] for an unknown guard and failed the clause.
    final signature = '${guard.predicate}/${guard.args.length}';
    if (!runtimeGuards.contains(signature)) {
      throw CompileError(
        'Unknown guard predicate $signature: no guard of the catalogue '
            '(GLP-Spec appendix-guards.tex) has that name and arity, and no '
            'unit clause defines it as a guard',
        guard.line,
        guard.column,
        phase: 'codegen',
      );
    }
    // Setup arguments, then call guard
    for (int i = 0; i < guard.args.length; i++) {
      _generatePutArgument(guard.args[i], i, varTable, ctx);
    }

    ctx.emit(bc.Guard(guard.predicate, guard.args.length));
  }

  void _generateBody(List<Goal> goals, VariableTable varTable, CodeGenContext ctx) {
    for (int i = 0; i < goals.length; i++) {
      final goal = goals[i];

      // A cross-module call M # G is resolved to a local call when the
      // program is linked (TGLP modules.tex, Compilation, fourth step), so one
      // reaching the generator was never linked, and is refused.
      if (goal is RemoteGoal) {
        throw CompileError(
          'Cross-module call "${goal.staticModuleName} # ${goal.goal}" reached '
          'the code generator unresolved: a '
          'cross-module call becomes a local call when its program is linked, '
          'so the program that holds it is loaded as a directory program.',
          goal.line,
          goal.column,
          phase: 'codegen'
        );
      }

      // Special handling for SpawnGoal (Goal@AgentId)
      // In dGLP mode, ignore the @AgentId annotation and just run the inner goal
      if (goal is SpawnGoal) {
        final innerGoal = goal.innerGoal;
        // Setup arguments for inner goal
        for (int j = 0; j < innerGoal.args.length; j++) {
          _generatePutArgument(innerGoal.args[j], j, varTable, ctx);
        }
        // Spawn the inner goal (ignoring agent annotation)
        final procedureLabel = '${innerGoal.functor}/${innerGoal.arity}';
        ctx.emit(bc.Spawn(procedureLabel, innerGoal.arity));
        continue;
      }

      // Setup arguments in A registers
      for (int j = 0; j < goal.args.length; j++) {
        _generatePutArgument(goal.args[j], j, varTable, ctx);
      }

      // ALWAYS spawn (tail recursion removed - all goals spawned)
      final procedureLabel = '${goal.functor}/${goal.arity}';  // Full signature
      ctx.emit(bc.Spawn(procedureLabel, goal.arity));
    }

    // After spawning all goals, emit proceed to terminate parent
    ctx.emit(bc.Proceed());
  }

  void _generatePutArgument(Term term, int argSlot, VariableTable varTable, CodeGenContext ctx) {
    term = _anonymousAsUnderscore(term);
    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      // PutVariable in the occurrence's mode
      ctx.emit(bc.PutVariable(regIndex, argSlot, isReader: term.isReader));

    } else if (term is ConstTerm) {
      // Constant: put bound writer with reader
      ctx.emit(bc.PutBoundConst(term.value, argSlot));

    } else if (term is ListTerm) {
      if (term.isNil) {
        ctx.emit(bc.PutBoundNil(argSlot));
      } else {
        // Build list structure as '.'(H, T)
        ctx.emit(bc.PutStructure('.', 2, argSlot));
        if (term.head != null) _generateArgumentStructureElement(term.head!, varTable, ctx);
        if (term.tail != null) _generateArgumentStructureElement(term.tail!, varTable, ctx);
      }

    } else if (term is StructTerm) {
      // Build structure
      ctx.emit(bc.PutStructure(term.functor, term.arity, argSlot));
      for (final arg in term.args) {
        _generateArgumentStructureElement(arg, varTable, ctx);
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable: create fresh unbound writer
      final tempReg = ctx.allocateTemp();
      ctx.emit(bc.PutVariable(tempReg, argSlot, isReader: false));
    }
  }

  // Helper for building structure elements INSIDE argument structures
  // This is different from _generateStructureElement which is for HEAD/GUARD unification
  void _generateArgumentStructureElement(Term term, VariableTable varTable, CodeGenContext ctx) {
    term = _anonymousAsUnderscore(term);
    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }
      final regIndex = varInfo.registerIndex!;
      // Emit unify instruction to add variable to structure
      ctx.emit(bc.UnifyVariable(regIndex, isReader: term.isReader));

    } else if (term is ConstTerm) {
      // Add constant to structure
      ctx.emit(bc.UnifyConstant(term.value));

    } else if (term is ListTerm) {
      if (term.isNil) {
        ctx.emit(bc.UnifyConstant('nil'));  // Empty list
      } else {
        // Non-empty list: build structurally as a './2' cons cell, ground or
        // not. FCP builds compound terms with allocate_list_cell and never
        // folds them into a constant — constants are primitive only
        // (§cf-primitives). Building structurally keeps every *Constant operand
        // atomic so the code-format artefact can encode it. Pattern: [H|T]
        // becomes '.'(H, T).
        ctx.emit(bc.PutStructure('.', 2, ctx.allocateTemp())); // nested: temp register (FCP-style), not the unencodable -1 sentinel

        // Process head element
        if (term.head != null) {
          _generateStructureElementInBody(term.head!, varTable, ctx);
        }

        // Process tail element - may be another list or a variable
        if (term.tail != null) {
          _generateListTailInBody(term.tail!, varTable, ctx);
        }
      }

    } else if (term is StructTerm) {
      // Nested structure in BODY - need to handle both ground and non-ground
      // Start building the nested structure
      ctx.emit(bc.PutStructure(term.functor, term.arity, ctx.allocateTemp())); // nested: temp register (FCP-style), not the unencodable -1 sentinel

      // Process each argument using SetWriter/SetReader for variables
      for (final arg in term.args) {
        _generateStructureElementInBody(arg, varTable, ctx);
      }

    } else if (term is UnderscoreTerm) {
      ctx.emit(bc.UnifyVoid(count: 1));
    }
  }

  // Helper for building structure elements in BODY phase
  // This handles both ground and non-ground structures (with variables)
  void _generateStructureElementInBody(Term term, VariableTable varTable, CodeGenContext ctx) {
    term = _anonymousAsUnderscore(term);
    if (term is VarTerm) {
      // Variable in structure - emit as variable reference, not constant
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable in structure: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      // SetVariable with isReader flag
      ctx.emit(bc.SetVariable(regIndex, isReader: term.isReader));

    } else if (term is ConstTerm) {
      ctx.emit(bc.SetConstant(term.value));  // set_constant c

    } else if (term is ListTerm) {
      // Nested list in structure
      if (term.isNil) {
        ctx.emit(bc.SetConstant('nil'));
      } else {
        // Non-empty nested list: build as cons cell '.'(head, tail)
        ctx.emit(bc.PutStructure('.', 2, ctx.allocateTemp())); // nested: temp register (FCP-style), not the unencodable -1 sentinel

        // Process head element
        if (term.head != null) {
          _generateStructureElementInBody(term.head!, varTable, ctx);
        }

        // Process tail element
        if (term.tail != null) {
          _generateListTailInBody(term.tail!, varTable, ctx);
        }
      }

    } else if (term is StructTerm) {
      // Nested structure - build recursively
      ctx.emit(bc.PutStructure(term.functor, term.arity, ctx.allocateTemp())); // nested: temp register (FCP-style), not the unencodable -1 sentinel

      // Process each argument recursively
      for (final arg in term.args) {
        _generateStructureElementInBody(arg, varTable, ctx);
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable in structure
      final tempReg = ctx.allocateTemp();
      ctx.emit(bc.SetVariable(tempReg, isReader: false));  // Create fresh writer
    }
  }

  // Helper for building list tails in BODY phase
  // List tails can be: another list (recurse), a variable, or nil
  void _generateListTailInBody(Term term, VariableTable varTable, CodeGenContext ctx) {
    if (term is ListTerm) {
      if (term.isNil) {
        // Tail is nil: emit constant
        ctx.emit(bc.SetConstant('nil'));
      } else {
        // Tail is another list: build nested cons cell
        ctx.emit(bc.PutStructure('.', 2, ctx.allocateTemp())); // nested: temp register (FCP-style), not the unencodable -1 sentinel

        // Process head of nested list
        if (term.head != null) {
          _generateStructureElementInBody(term.head!, varTable, ctx);
        }

        // Recurse for tail
        if (term.tail != null) {
          _generateListTailInBody(term.tail!, varTable, ctx);
        }
      }
    } else {
      // Tail is a variable or other term - use standard handling
      _generateStructureElementInBody(term, varTable, ctx);
    }
  }

}
