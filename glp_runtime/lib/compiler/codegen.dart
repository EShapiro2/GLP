import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/bytecode/opcodes_v2.dart' as bcv2;
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart' show BytecodeProgram;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'ast.dart';
import 'analyzer.dart';
import 'error.dart';
import 'result.dart';

// ============================================================================
// Module System Support (Phase 2)
// ============================================================================

/// Import table: maps module names to 1-indexed positions
/// Following FCP convention where imports are indexed 1, 2, 3, ...
class ImportTable {
  final Map<String, int> _indices = {};
  int _nextIndex = 1;

  /// Add a module to the import table
  /// Returns the index assigned to this module
  int addImport(String moduleName) {
    if (!_indices.containsKey(moduleName)) {
      _indices[moduleName] = _nextIndex++;
    }
    return _indices[moduleName]!;
  }

  /// Get the index for a module name, or null if not in imports
  int? getIndex(String moduleName) => _indices[moduleName];

  /// Get the number of imports
  int get size => _indices.length;

  /// Get all import names in index order
  List<String> get orderedImports {
    final entries = _indices.entries.toList();
    entries.sort((a, b) => a.value.compareTo(b.value));
    return entries.map((e) => e.key).toList();
  }

  /// Check if a module is in the import table
  bool contains(String moduleName) => _indices.containsKey(moduleName);

  @override
  String toString() => 'ImportTable($_indices)';
}

/// Code generation context
class CodeGenContext {
  // Bytecode accumulator - can hold both v1 (Op) and v2 (OpV2) instructions
  final List<dynamic> instructions = [];

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

  // Module system: import table for RPC transformation
  final ImportTable importTable = ImportTable();

  int get currentPC => instructions.length;

  void emit(dynamic instruction) {
    instructions.add(instruction);
  }

  void emitLabel(String label) {
    // Record label position in map
    labels[label] = currentPC;
    // Emit the Label instruction
    instructions.add(bc.Label(label));
  }

  int allocateTemp() => nextTempVar++;

  void resetTemps(int variableCount) {
    // Start temps after all variable registers to avoid collision
    nextTempVar = variableCount > 10 ? variableCount : 10;
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

    // DEBUG: Print bytecode for this procedure
    if (proc.signature == 'foo/1') {
      print('\n=== BYTECODE FOR ${proc.signature} ===');
      for (int i = 0; i < ctx.instructions.length; i++) {
        final instr = ctx.instructions[i];
        String details = '';
        if (instr is bc.HeadStructure) {
          details = ' HeadStructure("${instr.functor}", ${instr.arity}, argSlot: ${instr.argSlot})';
        } else if (instr is bc.UnifyConstant) {
          details = ' UnifyConstant(${instr.value})';
        } else if (instr is bc.PutStructure) {
          details = ' PutStructure("${instr.functor}", ${instr.arity}, ${instr.argSlot})';
        } else if (instr is bcv2.PutVariable) {
          details = ' PutVariable(reg=${instr.varIndex}, slot=${instr.argSlot}, reader=${instr.isReader})';
        } else if (instr is bcv2.GetVariable) {
          details = ' GetVariable(reg=${instr.varIndex}, slot=${instr.argSlot}, reader=${instr.isReader})';
        } else if (instr is bcv2.UnifyVariable) {
          details = ' UnifyVariable(reg=${instr.varIndex}, reader=${instr.isReader})';
        } else if (instr is bc.Spawn) {
          details = ' Spawn("${instr.procedureLabel}", arity=${instr.arity})';
        }
        print('  $i: ${instr.runtimeType}$details');
      }
      print('=== END BYTECODE ===\n');
    }
  }

  void _generateClause(AnnotatedClause clause, CodeGenContext ctx, String nextLabel, bool isLastClause) {
    ctx.resetTemps(clause.varTable.getAllVars().length);
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

  void _generateHeadArgument(Term term, int argSlot, VariableTable varTable, CodeGenContext ctx) {
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
        // First occurrence: emit V2 GetVariable
        ctx.emit(bcv2.GetVariable(regIndex, argSlot, isReader: term.isReader));
        ctx.seenHeadVars.add(baseVarName);
      } else {
        // Subsequent occurrence: emit V2 GetValue
        ctx.emit(bcv2.GetValue(regIndex, argSlot, isReader: term.isReader));
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

      // Step 1: Extract the argument into a temp register
      final tempReg = ctx.allocateTemp();
      ctx.emit(bc.GetVariable(tempReg, argSlot));  // Load argument into temp

      // Step 2: Match the structure at the temp register (not argSlot!)
      ctx.emit(bc.HeadStructure(term.functor, term.arity, tempReg));

      // FCP AM: Process ALL arguments inline using Push/Pop for nested structures
      // _generateStructureElement already has correct Push/Pop logic (lines 335-361)
      for (final subArg in term.args) {
        _generateStructureElement(subArg, varTable, ctx, inHead: true);
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable as direct head argument: just ignore it
      // No instruction needed - the argument is simply not extracted
    }
  }

  void _generateStructureElement(Term term, VariableTable varTable, CodeGenContext ctx, {required bool inHead}) {
    // Called during structure traversal (S register in use)

    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      // Always emit v2 UnifyVariable based on syntactic mode
      // Occurrence tracking was causing mixed v1/v2 opcode generation
      ctx.emit(bcv2.UnifyVariable(regIndex, isReader: term.isReader));

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
          ctx.emit(bcv2.UnifyVariable(saveReg, isReader: false));
        } else {
          // WRITE mode (BODY): building nested structure within argument structure
          final tempReg = ctx.allocateTemp();
          ctx.emit(bc.PutStructure('.', 2, tempReg));
          if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: inHead);
          if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: inHead);
          ctx.emit(bcv2.UnifyVariable(tempReg, isReader: false));
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
        ctx.emit(bcv2.UnifyVariable(saveReg, isReader: false));
      } else {
        // WRITE mode
        final tempReg = ctx.allocateTemp();
        ctx.emit(bc.PutStructure(term.functor, term.arity, tempReg));
        for (final subArg in term.args) {
          _generateStructureElement(subArg, varTable, ctx, inHead: inHead);
        }
        ctx.emit(bcv2.UnifyVariable(tempReg, isReader: false));
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable in structure
      ctx.emit(bc.UnifyVoid(count: 1));
    }
  }

  void _generateGuard(Guard guard, VariableTable varTable, CodeGenContext ctx) {
    // Special built-in guards
    if (guard.predicate == 'ground' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        final varInfo = varTable.getVar(arg.name);
        if (varInfo != null) {
          ctx.emit(bc.Ground(varInfo.registerIndex!, negated: guard.negated));
          return;
        }
      }
    }

    if (guard.predicate == 'known' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        final varInfo = varTable.getVar(arg.name);
        if (varInfo != null) {
          ctx.emit(bc.Known(varInfo.registerIndex!, negated: guard.negated));
          return;
        }
      }
    }

    if (guard.predicate == 'no_readers' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        final varInfo = varTable.getVar(arg.name);
        if (varInfo != null) {
          ctx.emit(bc.NoReaders(varInfo.registerIndex!, negated: guard.negated));
          return;
        }
      }
    }

    if (guard.predicate == 'otherwise' && guard.args.isEmpty) {
      // 'otherwise' cannot be negated (enforced by analyzer)
      ctx.emit(bc.Otherwise());
      return;
    }

    // Ground equality guard: X =?= Y
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
            negated: guard.negated,
          ));
          return;
        }
      }
    }

    // Generic guard predicate call (runtime evaluation)
    // Setup arguments, then call guard
    for (int i = 0; i < guard.args.length; i++) {
      _generatePutArgument(guard.args[i], i, varTable, ctx);
    }

    ctx.emit(bc.Guard(guard.predicate, guard.args.length, negated: guard.negated));
  }

  void _generateBody(List<Goal> goals, VariableTable varTable, CodeGenContext ctx) {
    for (int i = 0; i < goals.length; i++) {
      final goal = goals[i];

      // Special handling for RemoteGoal (Module # Goal)
      if (goal is RemoteGoal) {
        _generateRemoteGoal(goal, varTable, ctx);
        continue;
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

  /// Generate code for a remote goal (Module # Goal)
  /// Following FCP RPC transformation (rpc.cp:164-175):
  /// - Static module: distribute # {Index, Goal}
  /// - Dynamic module: transmit # {ModuleVar, Goal}
  void _generateRemoteGoal(RemoteGoal remote, VariableTable varTable, CodeGenContext ctx) {
    final innerGoal = remote.goal;

    // First, set up arguments for the inner goal in A registers
    for (int j = 0; j < innerGoal.args.length; j++) {
      _generatePutArgument(innerGoal.args[j], j, varTable, ctx);
    }

    // Then emit the appropriate RPC opcode
    if (remote.isDynamic) {
      // Dynamic module: transmit # {ModuleVar, Goal}
      // The module is a variable - need to resolve at runtime
      final moduleTerm = remote.module as VarTerm;
      final varInfo = varTable.getVar(moduleTerm.name);
      if (varInfo == null || varInfo.registerIndex == null) {
        throw CompileError(
          'Unknown variable in dynamic RPC: ${moduleTerm.name}',
          remote.line,
          remote.column,
          phase: 'codegen'
        );
      }
      ctx.emit(bc.Transmit(varInfo.registerIndex!, innerGoal.functor, innerGoal.arity));
    } else {
      // Static module: distribute # {Index, Goal}
      final moduleName = remote.staticModuleName!;

      // Look up or add to import table
      final index = ctx.importTable.addImport(moduleName);

      ctx.emit(bc.Distribute(index, innerGoal.functor, innerGoal.arity));
    }
  }

  void _generatePutArgument(Term term, int argSlot, VariableTable varTable, CodeGenContext ctx) {
    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      // Use v2 unified instruction
      ctx.emit(bcv2.PutVariable(regIndex, argSlot, isReader: term.isReader));

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
      ctx.emit(bcv2.PutVariable(tempReg, argSlot, isReader: false));
    }
  }

  // Check if a term is fully ground (contains no variables)
  bool _isGroundTerm(Term term) {
    if (term is VarTerm) return false;
    if (term is ConstTerm) return true;
    if (term is ListTerm) {
      if (term.isNil) return true;
      return (term.head == null || _isGroundTerm(term.head!)) &&
             (term.tail == null || _isGroundTerm(term.tail!));
    }
    if (term is StructTerm) {
      return term.args.every((arg) => _isGroundTerm(arg));
    }
    return false;
  }

  // Convert a ground term to a Dart value (for constants)
  Object? _groundTermToValue(Term term) {
    if (term is ConstTerm) return term.value;
    if (term is ListTerm) {
      if (term.isNil) return 'nil';  // Empty list as 'nil'
      // Build list as nested cons cells: [H|T]
      final head = term.head != null ? _groundTermToValue(term.head!) : null;
      final tail = term.tail != null ? _groundTermToValue(term.tail!) : null;
      return [head, tail];  // Represent as 2-element list for cons cell
    }
    if (term is StructTerm) {
      // Return structure as map with functor and args
      return {
        'functor': term.functor,
        'args': term.args.map((arg) => _groundTermToValue(arg)).toList(),
      };
    }
    return null;
  }

  // Helper for building structure elements INSIDE argument structures
  // This is different from _generateStructureElement which is for HEAD/GUARD unification
  void _generateArgumentStructureElement(Term term, VariableTable varTable, CodeGenContext ctx) {
    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }
      final regIndex = varInfo.registerIndex!;
      // Emit unify instruction to add variable to structure
      ctx.emit(bcv2.UnifyVariable(regIndex, isReader: term.isReader));

    } else if (term is ConstTerm) {
      // Add constant to structure
      ctx.emit(bc.UnifyConstant(term.value));

    } else if (term is ListTerm) {
      if (term.isNil) {
        ctx.emit(bc.UnifyConstant('nil'));  // Empty list
      } else {
        // Non-empty list: check if ground or contains variables
        if (_isGroundTerm(term)) {
          // Ground list: convert to runtime StructTerm and emit as constant
          rt.Term convertListToStructTerm(ListTerm l) {
            if (l.isNil) return rt.ConstTerm('nil');

            rt.Term convertTerm(Term t) {
              if (t is ConstTerm) return rt.ConstTerm(t.value);
              if (t is ListTerm) return convertListToStructTerm(t);
              if (t is StructTerm) {
                final rtArgs = t.args.map(convertTerm).toList();
                return rt.StructTerm(t.functor, rtArgs);
              }
              return rt.ConstTerm(null);  // Fallback for unexpected cases
            }

            final head = l.head != null ? convertTerm(l.head!) : rt.ConstTerm('nil');
            final tail = l.tail != null ? convertTerm(l.tail!) : rt.ConstTerm('nil');
            return rt.StructTerm('.', [head, tail]);
          }
          final listStructTerm = convertListToStructTerm(term);
          ctx.emit(bc.UnifyConstant(listStructTerm));
        } else {
          // Non-ground list (contains variables): build as structure './2'
          // Pattern: [H|T] becomes '.'(H, T)
          ctx.emit(bc.PutStructure('.', 2, -1)); // -1 = building inside parent structure

          // Process head element
          if (term.head != null) {
            _generateStructureElementInBody(term.head!, varTable, ctx);
          }

          // Process tail element - may be another list or a variable
          if (term.tail != null) {
            _generateListTailInBody(term.tail!, varTable, ctx);
          }
        }
      }

    } else if (term is StructTerm) {
      // Nested structure in BODY - need to handle both ground and non-ground
      // Start building the nested structure
      ctx.emit(bc.PutStructure(term.functor, term.arity, -1)); // -1 = building inside parent structure

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
    if (term is VarTerm) {
      // Variable in structure - emit as variable reference, not constant
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable in structure: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      // V2 SetVariable with isReader flag
      ctx.emit(bcv2.SetVariable(regIndex, isReader: term.isReader));

    } else if (term is ConstTerm) {
      ctx.emit(bc.SetConstant(term.value));  // set_constant c

    } else if (term is ListTerm) {
      // Nested list in structure
      if (term.isNil) {
        ctx.emit(bc.SetConstant('nil'));
      } else {
        // Non-empty nested list: build as cons cell '.'(head, tail)
        ctx.emit(bc.PutStructure('.', 2, -1)); // -1 = building inside parent structure

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
      ctx.emit(bc.PutStructure(term.functor, term.arity, -1)); // -1 = building inside parent structure

      // Process each argument recursively
      for (final arg in term.args) {
        _generateStructureElementInBody(arg, varTable, ctx);
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable in structure
      final tempReg = ctx.allocateTemp();
      ctx.emit(bcv2.SetVariable(tempReg, isReader: false));  // Create fresh writer
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
        ctx.emit(bc.PutStructure('.', 2, -1)); // -1 = building inside parent structure

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
