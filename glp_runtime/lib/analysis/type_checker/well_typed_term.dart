// lib/analysis/type_checker/well_typed_term.dart
//
// Well-typed moded term checking for GLP type system.
// Specification: docs/type system/well-typed-term.md v0.7
// Paper Reference: Definition 5.4 (Well-Typed Moded Term)
//
// Determines when a moded term is well-typed by an automaton by checking path
// consistency via automaton traversal.

import 'mode.dart';
import 'moded_term.dart';
import 'program_dfa.dart';

// =============================================================================
// Result Types
// =============================================================================

/// Result of checking if a moded term is well-typed
class WellTypedResult {
  /// Whether the moded term is well-typed by the automaton
  final bool isWellTyped;

  /// Type assignments for each variable occurrence
  /// Key format: "X" for writers, "X?" for readers
  final Map<String, VariableTypeInfo> variableTypes;

  /// List of errors found during checking
  final List<WellTypedError> errors;

  WellTypedResult({
    required this.isWellTyped,
    required this.variableTypes,
    required this.errors,
  });

  factory WellTypedResult.success(Map<String, VariableTypeInfo> variableTypes) {
    return WellTypedResult(
      isWellTyped: true,
      variableTypes: variableTypes,
      errors: [],
    );
  }

  factory WellTypedResult.failure(List<WellTypedError> errors,
      [Map<String, VariableTypeInfo>? variableTypes]) {
    return WellTypedResult(
      isWellTyped: false,
      variableTypes: variableTypes ?? {},
      errors: errors,
    );
  }
}

/// Type information for a variable occurrence
class VariableTypeInfo {
  /// The DFA state where this variable appears (includes isComplement)
  final DFAState typeState;

  /// The mode at this position (consume for readers, produce for writers)
  final Mode mode;

  /// Whether this is a reader variable
  final bool isReader;

  VariableTypeInfo({
    required this.typeState,
    required this.mode,
    required this.isReader,
  });

  @override
  String toString() => '(${typeState.name}, ${mode == Mode.consume ? "↓" : "↑"})';

  @override
  bool operator ==(Object other) =>
      other is VariableTypeInfo &&
      typeState == other.typeState &&
      mode == other.mode &&
      isReader == other.isReader;

  @override
  int get hashCode => Object.hash(typeState, mode, isReader);
}

/// Base class for well-typing errors
abstract class WellTypedError {
  String get message;
}

/// Error: path has no matching automaton transition
class InconsistentPathError extends WellTypedError {
  final ModedPath path;
  final String reason;

  InconsistentPathError(this.path, this.reason);

  @override
  String get message => 'Inconsistent path: $reason\n  Path: $path';

  @override
  String toString() => message;
}

/// Error: same variable has different types at different occurrences
class InconsistentVariableError extends WellTypedError {
  final String variableName;
  final VariableTypeInfo firstOccurrence;
  final VariableTypeInfo secondOccurrence;

  InconsistentVariableError(
      this.variableName, this.firstOccurrence, this.secondOccurrence);

  @override
  String get message =>
      'Variable $variableName has inconsistent types: $firstOccurrence vs $secondOccurrence';

  @override
  String toString() => message;
}

/// Error: variable pair (X, X?) not dual
class NonDualError extends WellTypedError {
  final String baseName;
  final VariableTypeInfo? writerType;
  final VariableTypeInfo? readerType;
  final String? reason;

  NonDualError(this.baseName, this.writerType, this.readerType, [this.reason]);

  @override
  String get message {
    final reasonStr = reason != null ? ': $reason' : '';
    return 'Variable pair ($baseName, $baseName?) not dual$reasonStr: writer=$writerType, reader=$readerType';
  }

  @override
  String toString() => message;
}

/// Result of checking a single path against automaton
class PathCheckResult {
  final bool isConsistent;
  final String? reason;
  final VariableTypeInfo? variableAssignment;

  PathCheckResult({
    required this.isConsistent,
    this.reason,
    this.variableAssignment,
  });

  factory PathCheckResult.consistent([VariableTypeInfo? assignment]) {
    return PathCheckResult(
      isConsistent: true,
      variableAssignment: assignment,
    );
  }

  factory PathCheckResult.inconsistent(String reason) {
    return PathCheckResult(
      isConsistent: false,
      reason: reason,
    );
  }
}

// =============================================================================
// Public Functions
// =============================================================================

/// Check if a moded term is well-typed by an automaton.
///
/// Per Definition 4.5: A moded term T is well-typed by a GLP type D if:
/// 1. For each term path x ∈ paths(T) there is a consistent path in the automaton
/// 2. For every pair of variables in T, their types are complementary
///
/// Returns WellTypedResult with:
/// - isWellTyped: true iff all paths consistent and variable pairs complementary
/// - variableTypes: type assignments for each variable
/// - errors: list of all violations found
WellTypedResult checkModedTerm(ModedTerm term, Automaton automaton, ProgramDFA dfa) {
  final errors = <WellTypedError>[];
  final variableTypes = <String, VariableTypeInfo>{};

  // Step 1: Extract and check all paths
  final termPaths = paths(term);

  for (final path in termPaths) {
    final result = checkPathAgainstAutomaton(path, automaton, dfa);

    if (!result.isConsistent) {
      errors.add(InconsistentPathError(path, result.reason ?? 'Unknown'));
    } else if (result.variableAssignment != null) {
      // Record variable type
      final varKey = _variableKey(path.leaf);

      if (variableTypes.containsKey(varKey)) {
        // Same variable appears multiple times - types must match
        final existing = variableTypes[varKey]!;
        if (existing.typeState.name != result.variableAssignment!.typeState.name) {
          errors.add(InconsistentVariableError(
              varKey, existing, result.variableAssignment!));
        }
      } else {
        variableTypes[varKey] = result.variableAssignment!;
      }
    }
  }

  // Step 2: Check variable pair duality
  final dualityErrors = _checkDuality(variableTypes);
  errors.addAll(dualityErrors);

  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors,
  );
}

/// Check if a single moded path is consistent with the automaton.
///
/// Per Definition 4.3: A moded path is consistent with the automaton if:
/// 1. Structure matches: each non-leaf step corresponds to valid transition
/// 2. Leaf consistency: variable/constant at leaf matches DFA state
///
/// Fix 4.1: Switches automata at type boundaries when entering user-defined types
PathCheckResult checkPathAgainstAutomaton(
  ModedPath path,
  Automaton automaton,
  ProgramDFA dfa,
) {
  var state = automaton.startState;
  var currentAutomaton = automaton;  // Track current automaton for type switching

  // Handle single-step paths (just a variable or constant at root)
  if (path.length == 1) {
    return _checkLeafConsistencyForPath(path.leaf, state, dfa);
  }

  // Traverse path, following automaton transitions
  for (int i = 0; i < path.length - 1; i++) {
    final step = path.steps[i];
    final nextStep = path.steps[i + 1];

    // Build transition label from path step
    final label = _buildTransitionLabel(step, nextStep);

    // Try to follow transition
    final nextState = currentAutomaton.transition(state, label);

    if (nextState == null) {
      // Case 3 (Type path shorter - wildcard in type): Per Definition 4.5 v0.7
      // When at a wildcard state with no outgoing transition, check ONLY the
      // structural mode at this position. The remainder of the term path beyond
      // the wildcard is NOT examined - the wildcard accepts the entire subterm.
      if (state.isWildcard) {
        // The structural mode at position |y| is nextStep.mode (the mode at
        // the first position beyond where the type path can follow)
        final structuralModeAtWildcard = nextStep.mode;
        final expectedMode = state.isDual ? Mode.consume : Mode.produce;
        
        if (structuralModeAtWildcard == expectedMode) {
          // Mode matches - wildcard accepts entire subterm, path is consistent.
          // We don't record variable assignments here because the variables
          // inside the wildcard-accepted subterm are handled separately.
          return PathCheckResult.consistent();
        }
        
        // Mode mismatch at wildcard position
        return PathCheckResult.inconsistent(
            'Mode mismatch at wildcard ${state.name}: expected ${expectedMode == Mode.consume ? "↓" : "↑"}, got ${structuralModeAtWildcard == Mode.consume ? "↓" : "↑"}');
      }
      
      return PathCheckResult.inconsistent(
          'No transition for $label from state ${state.name}');
    }

    // Fix 4.1: Switch automata at type boundaries
    // When entering a user-defined type, switch to that type's automaton
    if (nextState.isUserDefinedType && nextState.baseName != state.baseName) {
      try {
        currentAutomaton = dfa.getAutomaton(nextState.name);
      } catch (e) {
        return PathCheckResult.inconsistent(
            'Cannot get automaton for type ${nextState.name}');
      }
    }

    state = nextState;
  }

  // Check leaf consistency
  return _checkLeafConsistencyForPath(path.leaf, state, dfa);
}

// =============================================================================
// Internal Functions
// =============================================================================

/// Build transition label from path steps
TransitionLabel _buildTransitionLabel(PathStep currentStep, PathStep nextStep) {
  // Parse functor/arity from current step symbol (e.g., "[|]/2" → "[|]", 2)
  final parts = currentStep.symbol.split('/');
  if (parts.length != 2) {
    // Leaf node (variable or constant) - shouldn't happen for non-leaf steps
    return TransitionLabel.functor(currentStep.symbol, 0, nextStep.argIndex, mode: nextStep.mode);
  }

  final functor = parts[0];
  final arity = int.tryParse(parts[1]) ?? 0;

  // Label encodes: functor(arity, argIndex) with mode from next step
  return TransitionLabel.functor(functor, arity, nextStep.argIndex, mode: nextStep.mode);
}

/// Check leaf consistency with DFA state using program_dfa.dart's checkLeafConsistency
PathCheckResult _checkLeafConsistencyForPath(
  PathStep leaf,
  DFAState state,
  ProgramDFA dfa,
) {
  // Convert PathStep to LeafTerm for checkLeafConsistency
  final leafTerm = _pathStepToLeafTerm(leaf);

  final result = checkLeafConsistency(leafTerm, state, dfa);

  if (result.isConsistent) {
    if (leaf.isVariable) {
      final isReader = leaf.isReader;
      final mode = isReader ? Mode.consume : Mode.produce;
      return PathCheckResult.consistent(VariableTypeInfo(
        typeState: result.type ?? state,
        mode: mode,
        isReader: isReader,
      ));
    }
    return PathCheckResult.consistent();
  } else {
    return PathCheckResult.inconsistent(result.reason ?? 'Leaf inconsistent');
  }
}

/// Convert PathStep to LeafTerm for checkLeafConsistency
/// Fix 4.2: Properly detects real literals (floating-point numbers)
/// Fix Bug 1: Atoms are strings per paper Section 4.1
LeafTerm _pathStepToLeafTerm(PathStep step) {
  if (step.isVariable) {
    // Use the actual structural mode from the path step, not a hardcoded mode
    if (step.isReader) {
      return LeafTerm.reader(step.symbol, mode: step.mode);
    } else {
      return LeafTerm.writer(step.symbol, mode: step.mode);
    }
  } else {
    // Constant - pass structural mode for wildcard checking (spec v0.6)
    final value = step.symbol;

    // Check for integer first (more specific - no decimal point)
    final intVal = int.tryParse(value);
    if (intVal != null) {
      return LeafTerm.integerConstant(intVal, mode: step.mode);
    }

    // Fix 4.2: Check for real (floating-point) - includes scientific notation
    final doubleVal = double.tryParse(value);
    if (doubleVal != null) {
      return LeafTerm.realConstant(doubleVal, mode: step.mode);
    }

    // Check for string (quoted)
    if ((value.startsWith("'") && value.endsWith("'")) ||
        (value.startsWith('"') && value.endsWith('"'))) {
      return LeafTerm.stringConstant(value.substring(1, value.length - 1), mode: step.mode);
    }

    // Fix Bug 1: Atoms (unquoted identifiers) are strings per paper Section 4.1
    // Paper line 49: "String ::= ... ; a ; b ; ... ; foo ; bar ; ..."
    // Therefore atoms like 'user', 'net', 'foo' etc. are strings
    return LeafTerm.stringConstant(value, mode: step.mode);
  }
}

/// Get variable key for the variable types map
String _variableKey(PathStep leaf) {
  // Use the symbol directly which includes ? for readers
  return leaf.symbol;
}

/// Check duality of variable pairs
/// Per spec v0.4: uses DFAState.baseName and isDual
List<NonDualError> _checkDuality(
    Map<String, VariableTypeInfo> variableTypes) {
  final errors = <NonDualError>[];

  // Group by base name (X and X? share base "X")
  final baseNames = <String, Map<String, VariableTypeInfo>>{};

  for (final entry in variableTypes.entries) {
    final varKey = entry.key;
    final info = entry.value;

    final baseName = varKey.endsWith('?')
        ? varKey.substring(0, varKey.length - 1)
        : varKey;

    baseNames.putIfAbsent(baseName, () => {});
    baseNames[baseName]![varKey] = info;
  }

  // Check each base name
  for (final entry in baseNames.entries) {
    final baseName = entry.key;
    final variants = entry.value;

    final writerKey = baseName;
    final readerKey = '$baseName?';

    if (variants.containsKey(writerKey) && variants.containsKey(readerKey)) {
      final writerInfo = variants[writerKey]!;
      final readerInfo = variants[readerKey]!;

      // Check modes
      if (writerInfo.mode != Mode.produce) {
        errors.add(NonDualError(baseName, writerInfo, readerInfo,
            'Writer must have produce mode'));
        continue;
      }
      if (readerInfo.mode != Mode.consume) {
        errors.add(NonDualError(baseName, writerInfo, readerInfo,
            'Reader must have consume mode'));
        continue;
      }

      // Special case: wildcards are universal
      final writerIsWildcard = writerInfo.typeState.baseName == '_';
      final readerIsWildcard = readerInfo.typeState.baseName == '_';

      if (writerIsWildcard || readerIsWildcard) {
        // Wildcards are dual to anything
        if (writerIsWildcard && writerInfo.typeState.isDual) {
          errors.add(NonDualError(baseName, writerInfo, readerInfo,
              'Writer wildcard must be _ (non-dual), not _?'));
          continue;
        }
        if (readerIsWildcard && !readerInfo.typeState.isDual) {
          errors.add(NonDualError(baseName, writerInfo, readerInfo,
              'Reader wildcard must be _? (dual), not _'));
          continue;
        }
        // Wildcards are OK - no error
        continue;
      }

      // Check states are duals (same baseName, opposite isDual)
      if (writerInfo.typeState.baseName != readerInfo.typeState.baseName) {
        errors.add(NonDualError(baseName, writerInfo, readerInfo,
            'Types must have same base: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}'));
        continue;
      }

      if (writerInfo.typeState.isDual == readerInfo.typeState.isDual) {
        errors.add(NonDualError(baseName, writerInfo, readerInfo,
            'One must be dual, other not: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}'));
      }
    }
  }

  return errors;
}
