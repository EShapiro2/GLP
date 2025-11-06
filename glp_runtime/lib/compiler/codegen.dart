import '../bytecode/opcodes.dart' as bc;
import '../bytecode/asm.dart';
import 'ast.dart';
import 'analyzer.dart';
import 'error.dart';

/// Code generation context
class CodeGenContext {
  // Bytecode accumulator
  final List<bc.Op> instructions = [];

  // Label management
  final Map<String, int> labels = {};
  final List<String> pendingLabels = [];  // Labels waiting to be placed

  // Temporary variable allocation
  int nextTempVar = 0;
  final Map<String, int> tempAllocation = {};

  // Current procedure context
  String? currentProcedure;
  int currentClauseIndex = 0;

  // Phase tracking
  bool inHead = false;
  bool inGuard = false;
  bool inBody = false;

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

  void resetTemps() {
    nextTempVar = 0;
    tempAllocation.clear();
  }
}

/// Code generator - transforms annotated AST to bytecode
class CodeGenerator {
  BytecodeProgram generate(AnnotatedProgram program) {
    final ctx = CodeGenContext();

    // Generate code for each procedure
    for (final proc in program.procedures) {
      _generateProcedure(proc, ctx);
    }

    // Build final bytecode program
    return BytecodeProgram(
      instructions: ctx.instructions,
      labels: ctx.labels,
    );
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
    ctx.resetTemps();

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

    // COMMIT or CLAUSE_NEXT
    if (isLastClause) {
      // Last clause: commit and execute body
      ctx.emit(bc.Commit());  // Apply σ̂w, enter BODY phase

      // BODY PHASE
      ctx.inHead = false;
      ctx.inGuard = false;
      ctx.inBody = true;

      if (clause.hasBody && clause.ast.body != null) {
        _generateBody(clause.ast.body!, clause.varTable, ctx);
      } else {
        // Empty body: just proceed
        ctx.emit(bc.Proceed());
      }

      // Label for clause end (before no_more_clauses)
      ctx.emitLabel('${ctx.currentProcedure}_c${ctx.currentClauseIndex}_end');

    } else {
      // Not last clause: clause_next to try next clause
      ctx.emitLabel('${ctx.currentProcedure}_c${ctx.currentClauseIndex}_end');
      ctx.emit(bc.ClauseNext(nextLabel));
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

      // Both writers and readers in head use GetVariable
      // This loads the argument into a register for later use
      ctx.emit(bc.GetVariable(regIndex, argSlot));

    } else if (term is ConstTerm) {
      // Constant in head: match with head_constant
      ctx.emit(bc.HeadConstant(term.value, argSlot));

    } else if (term is ListTerm) {
      if (term.isNil) {
        // Empty list: head_nil
        ctx.emit(bc.HeadNil(argSlot));
      } else {
        // Non-empty list [H|T]: head_list then structure traversal
        ctx.emit(bc.HeadList(argSlot));  // Equivalent to head_structure('[|]', 2, argSlot)

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
      // Structure in head: head_structure then traverse arguments
      ctx.emit(bc.HeadStructure(term.functor, term.arity, argSlot));

      for (final subArg in term.args) {
        _generateStructureElement(subArg, varTable, ctx, inHead: true);
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable: void/1
      ctx.emit(bc.UnifyVoid(count: 1));
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

      if (term.isReader) {
        // Reader at position S
        ctx.emit(bc.UnifyReader(regIndex));
      } else {
        // Writer at position S
        ctx.emit(bc.UnifyWriter(regIndex));
      }

    } else if (term is ConstTerm) {
      // Constant at position S
      ctx.emit(bc.UnifyConstant(term.value));

    } else if (term is ListTerm) {
      // Nested list: need to extract into temp, then match
      final tempReg = ctx.allocateTemp();

      if (inHead) {
        // READ mode: extract value at S into temp
        ctx.emit(bc.UnifyWriter(tempReg));  // Extract into temp (uses Writer instruction)

        // Then match temp against nested structure
        if (term.isNil) {
          ctx.emit(bc.HeadNil(tempReg));
        } else {
          ctx.emit(bc.HeadList(tempReg));
          if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: inHead);
          if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: inHead);
        }
      } else {
        // WRITE mode: build nested structure first
        if (term.isNil) {
          ctx.emit(bc.PutNil(tempReg));
        } else {
          ctx.emit(bc.PutList(tempReg));
          if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: inHead);
          if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: inHead);
        }

        // Then write temp to current S position
        ctx.emit(bc.UnifyWriter(tempReg));
      }

    } else if (term is StructTerm) {
      // Nested structure: extract into temp, then match
      final tempReg = ctx.allocateTemp();

      if (inHead) {
        // READ mode
        ctx.emit(bc.UnifyWriter(tempReg));  // Extract into temp
        ctx.emit(bc.HeadStructure(term.functor, term.arity, tempReg));
        for (final subArg in term.args) {
          _generateStructureElement(subArg, varTable, ctx, inHead: inHead);
        }
      } else {
        // WRITE mode
        ctx.emit(bc.PutStructure(term.functor, term.arity, tempReg));
        for (final subArg in term.args) {
          _generateStructureElement(subArg, varTable, ctx, inHead: inHead);
        }
        ctx.emit(bc.UnifyWriter(tempReg));
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

    if (guard.predicate == 'otherwise' && guard.args.isEmpty) {
      ctx.emit(bc.Otherwise());
      return;
    }

    // Generic guard predicate call
    // Setup arguments, then call guard
    for (int i = 0; i < guard.args.length; i++) {
      _generatePutArgument(guard.args[i], i, varTable, ctx);
    }

    ctx.emit(bc.Guard(guard.predicate, guard.args.length));
  }

  void _generateBody(List<Goal> goals, VariableTable varTable, CodeGenContext ctx) {
    for (int i = 0; i < goals.length; i++) {
      final goal = goals[i];
      final isTailPosition = (i == goals.length - 1);

      // Setup arguments in A registers
      for (int j = 0; j < goal.args.length; j++) {
        _generatePutArgument(goal.args[j], j, varTable, ctx);
      }

      // Spawn or Requeue
      if (isTailPosition) {
        // Tail call: requeue
        ctx.emit(bc.Requeue(goal.functor, goal.arity));
      } else {
        // Non-tail: spawn
        ctx.emit(bc.Spawn(goal.functor, goal.arity));
      }
    }

    // If no goals (empty body), emit proceed
    if (goals.isEmpty) {
      ctx.emit(bc.Proceed());
    }
  }

  void _generatePutArgument(Term term, int argSlot, VariableTable varTable, CodeGenContext ctx) {
    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      if (term.isReader) {
        // Reader: put_reader
        ctx.emit(bc.PutReader(regIndex, argSlot));
      } else {
        // Writer: put_writer
        ctx.emit(bc.PutWriter(regIndex, argSlot));
      }

    } else if (term is ConstTerm) {
      // Constant: put_constant
      ctx.emit(bc.PutConstant(term.value, argSlot));

    } else if (term is ListTerm) {
      if (term.isNil) {
        ctx.emit(bc.PutNil(argSlot));
      } else {
        // Build list structure
        ctx.emit(bc.PutList(argSlot));
        if (term.head != null) _generateStructureElement(term.head!, varTable, ctx, inHead: false);
        if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx, inHead: false);
      }

    } else if (term is StructTerm) {
      // Build structure
      ctx.emit(bc.PutStructure(term.functor, term.arity, argSlot));
      for (final arg in term.args) {
        _generateStructureElement(arg, varTable, ctx, inHead: false);
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable: create fresh unbound writer
      final tempReg = ctx.allocateTemp();
      ctx.emit(bc.PutWriter(tempReg, argSlot));
    }
  }
}

/// Bytecode Program result
class BytecodeProgram {
  final List<bc.Op> instructions;
  final Map<String, int> labels;

  BytecodeProgram({required this.instructions, required this.labels});

  int? getLabel(String name) => labels[name];

  @override
  String toString() => 'BytecodeProgram(${instructions.length} instructions, ${labels.length} labels)';
}
