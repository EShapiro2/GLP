// GLP Partial Evaluator
//
// Performs source-to-source transformation:
//   Stage 1: Unfold defined guards (unit clauses in guard position)
//   Stage 2: Unfold reduce/2 calls in body against reduce/2 facts

import 'ast.dart';
import 'error.dart';

// ============================================================================
// UNIFICATION RESULTS
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

// ============================================================================
// PARTIAL EVALUATOR
// ============================================================================

/// Partial evaluator for GLP programs
class PartialEvaluator {
  int _varCounter = 0;

  /// Stage 1: Transform all defined guards in a program.
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

  /// Stage 2: Unfold reduce/2 calls in clause bodies
  ///
  /// For each clause with body containing reduce(A?, B):
  ///   For each reduce/2 fact in the program:
  ///     Try to unify A with the fact's first argument
  ///     If success: create new clause with B bound and reduce call removed
  Program unfoldReduceCalls(Program program) {
    // 1. Collect all reduce/2 facts (unit clauses for reduce/2)
    final reduceFacts = _collectReduceFacts(program);

    if (reduceFacts.isEmpty) {
      return program; // No reduce facts, nothing to unfold
    }

    List<Procedure> transformedProcedures = [];

    for (final procedure in program.procedures) {
      List<Clause> transformedClauses = [];

      for (final clause in procedure.clauses) {
        final expanded = _unfoldReduceInClause(clause, reduceFacts);
        transformedClauses.addAll(expanded);
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

  /// Collect reduce/2 facts from the program
  /// A reduce fact is a unit clause: reduce(Pattern, Replacement).
  List<Clause> _collectReduceFacts(Program program) {
    final List<Clause> facts = [];

    for (final proc in program.procedures) {
      if (proc.name != 'reduce' || proc.arity != 2) continue;

      for (final clause in proc.clauses) {
        // Must have no guards
        if (clause.guards != null && clause.guards!.isNotEmpty) continue;

        // Must have no body, or body is just `true`
        if (clause.body != null && clause.body!.isNotEmpty) {
          if (clause.body!.length == 1 &&
              clause.body![0].functor == 'true' &&
              clause.body![0].args.isEmpty) {
            // Body is just `true`, this is a fact
          } else {
            continue; // Has real body goals
          }
        }

        facts.add(clause);
      }
    }

    return facts;
  }

  /// Unfold reduce/2 calls in a clause
  /// Returns a list of clauses (may be 1 if no unfolding, or multiple if expanded)
  List<Clause> _unfoldReduceInClause(Clause clause, List<Clause> reduceFacts) {
    if (clause.body == null || clause.body!.isEmpty) {
      return [clause]; // No body, nothing to unfold
    }

    // Find reduce/2 calls in the body
    int reduceIndex = -1;
    Goal? reduceCall;
    for (int i = 0; i < clause.body!.length; i++) {
      final goal = clause.body![i];
      if (goal.functor == 'reduce' && goal.args.length == 2) {
        reduceIndex = i;
        reduceCall = goal;
        break; // Process first reduce call found
      }
    }

    if (reduceCall == null) {
      return [clause]; // No reduce calls
    }

    // Try to unfold against each reduce fact
    List<Clause> expanded = [];

    for (final fact in reduceFacts) {
      // Rename variables in fact to fresh names
      final renamedFact = _renameClauseVars(fact);

      // Get the pattern and replacement from the fact
      final factPattern = renamedFact.head.args[0];
      final factReplacement = renamedFact.head.args[1];

      // Get the call's pattern and result variable
      final callPattern = reduceCall.args[0]; // A?
      final callResult = reduceCall.args[1];  // B

      // Try to unify callPattern with factPattern
      final result = _glpUnifyForPE([callPattern], [factPattern]);

      switch (result) {
        case UnifyFail():
          // This fact doesn't match, try next
          continue;

        case UnifySuspend():
          // Can't reduce at compile time, keep original
          // But still try other facts
          continue;

        case UnifySuccess(:final substitution):
          // Unification succeeded! Create expanded clause

          // Also unify callResult with factReplacement
          final resultUnify = _glpUnifyForPE([callResult], [factReplacement]);

          Map<String, Term> fullSubst = {...substitution};
          if (resultUnify is UnifySuccess) {
            fullSubst.addAll(resultUnify.substitution);
          }

          // Apply substitution to head
          var newHead = _applySubstitutionToAtom(clause.head, fullSubst);

          // Apply substitution to guards
          List<Guard>? newGuards;
          if (clause.guards != null && clause.guards!.isNotEmpty) {
            newGuards = clause.guards!
                .map((g) => _applySubstitutionToGuard(g, fullSubst))
                .toList();
          }

          // Build new body: replace reduce call with the bound result
          // The reduce(A?, B) call is removed; B is now bound via substitution
          List<Goal> newBody = [];
          for (int i = 0; i < clause.body!.length; i++) {
            if (i == reduceIndex) {
              // Skip the reduce call - it's been resolved
              // If factReplacement is a goal (not just `true`), we might need to add it
              // But typically reduce facts bind B to a goal that run/1 will execute
              continue;
            }
            newBody.add(_applySubstitutionToGoal(clause.body![i], fullSubst));
          }

          // If body is empty, make it null or [true]
          if (newBody.isEmpty) {
            newBody = [Goal('true', [], clause.line, clause.column)];
          }

          // Simplify guards - remove redundant ones
          final simplifiedGuards = _simplifyGuards(newGuards, newHead);

          expanded.add(Clause(
            newHead,
            guards: simplifiedGuards,
            body: newBody,
            line: clause.line,
            column: clause.column,
          ));
      }
    }

    // If no expansions succeeded, keep original clause
    if (expanded.isEmpty) {
      return [clause];
    }

    return expanded;
  }

  /// Rename all variables in a clause to fresh names
  Clause _renameClauseVars(Clause clause) {
    // Collect all variable names
    final varNames = <String>{};
    _collectVarNamesFromAtom(clause.head, varNames);
    if (clause.guards != null) {
      for (final guard in clause.guards!) {
        for (final arg in guard.args) {
          _collectVarNames(arg, varNames);
        }
      }
    }
    if (clause.body != null) {
      for (final goal in clause.body!) {
        for (final arg in goal.args) {
          _collectVarNames(arg, varNames);
        }
      }
    }

    // Build renaming map (skip underscores)
    final Map<String, String> renaming = {};
    for (final name in varNames) {
      if (name != '_') {
        renaming[name] = 'PE${_varCounter++}';
      }
    }

    // Apply renaming
    final newHead = _applyRenamingToAtom(clause.head, renaming);

    List<Guard>? newGuards;
    if (clause.guards != null) {
      newGuards = clause.guards!.map((g) => Guard(
        g.predicate,
        g.args.map((a) => _applyRenaming(a, renaming)).toList(),
        g.line,
        g.column,
        negated: g.negated,
      )).toList();
    }

    List<Goal>? newBody;
    if (clause.body != null) {
      newBody = clause.body!.map((g) => Goal(
        g.functor,
        g.args.map((a) => _applyRenaming(a, renaming)).toList(),
        g.line,
        g.column,
      )).toList();
    }

    return Clause(newHead, guards: newGuards, body: newBody, line: clause.line, column: clause.column);
  }

  void _collectVarNamesFromAtom(Atom atom, Set<String> names) {
    for (final arg in atom.args) {
      _collectVarNames(arg, names);
    }
  }

  Atom _applyRenamingToAtom(Atom atom, Map<String, String> renaming) {
    return Atom(
      atom.functor,
      atom.args.map((a) => _applyRenaming(a, renaming)).toList(),
      atom.line,
      atom.column,
    );
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
        renaming[name] = 'PE${_varCounter++}';
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

  /// Simplify guards by removing redundant ones after specialization.
  /// A guard is redundant if it always succeeds given the head pattern.
  List<Guard>? _simplifyGuards(List<Guard>? guards, Atom head) {
    if (guards == null || guards.isEmpty) return null;

    final simplified = <Guard>[];

    for (final guard in guards) {
      if (_isRedundantGuard(guard, head)) {
        // Skip this guard - it's always true
        continue;
      }
      simplified.add(guard);
    }

    return simplified.isEmpty ? null : simplified;
  }

  /// Check if a guard is redundant (always succeeds) given the head.
  bool _isRedundantGuard(Guard guard, Atom head) {
    // Type guards with concrete argument are redundant
    if (guard.args.length == 1) {
      final arg = guard.args[0];
      final concreteArg = _getConcreteArg(arg);

      if (concreteArg != null) {
        switch (guard.predicate) {
          case 'tuple':
          case 'compound':
            // tuple(structure) always succeeds
            return concreteArg is StructTerm;
          case 'list':
          case 'is_list':
            // list([...]) always succeeds
            return concreteArg is ListTerm;
          case 'integer':
            return concreteArg is ConstTerm && concreteArg.value is int;
          case 'number':
            return concreteArg is ConstTerm &&
                (concreteArg.value is int || concreteArg.value is double);
          case 'atom':
            return concreteArg is ConstTerm && concreteArg.value is String;
          case 'ground':
            // If argument is fully concrete (no variables), ground succeeds
            return _isGround(concreteArg);
        }
      }
    }

    return false;
  }

  /// Get the concrete (non-variable) form of a term.
  /// Returns null if the term contains unbound variables.
  Term? _getConcreteArg(Term term) {
    if (term is VarTerm) {
      // A reader reference - try to find what it refers to
      // For now, if it's a reader, we can't determine concreteness
      return null;
    }
    if (term is ConstTerm || term is StructTerm || term is ListTerm) {
      return term;
    }
    return null;
  }

  /// Check if a term is ground (contains no variables).
  bool _isGround(Term term) {
    if (term is VarTerm) return false;
    if (term is UnderscoreTerm) return true;
    if (term is ConstTerm) return true;
    if (term is StructTerm) {
      return term.args.every(_isGround);
    }
    if (term is ListTerm) {
      if (term.isNil) return true;
      final headGround = term.head == null || _isGround(term.head!);
      final tailGround = term.tail == null || _isGround(term.tail!);
      return headGround && tailGround;
    }
    return false;
  }
}
