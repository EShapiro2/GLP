// lib/analysis/type_checker/well_typed_term.dart
//
// Well-typed moded term checking for GLP type system.
// Specification: docs/modules/well-typed-term.md v0.3
// Paper Reference: Definition 4.3 (Consistent Paths), Definition 4.5 (Well-Typed Moded Term)
//
// Determines when a moded term is well-typed by a type DFA by checking path
// consistency via DFA traversal.

import 'mode.dart';
import 'moded_term.dart';
import 'type_dfa.dart';
import 'moded_label.dart';

// =============================================================================
// Result Types
// =============================================================================

/// Result of checking if a moded term is well-typed
class WellTypedResult {
  /// Whether the moded term is well-typed by the DFA
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
  /// The DFA state where this variable appears
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

/// Error: path has no matching DFA transition
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

/// Error: variable pair (X, X?) not complementary
class NonComplementaryError extends WellTypedError {
  final String baseName;
  final VariableTypeInfo? writerType;
  final VariableTypeInfo? readerType;

  NonComplementaryError(this.baseName, this.writerType, this.readerType);

  @override
  String get message =>
      'Variable pair ($baseName, $baseName?) not complementary: writer=$writerType, reader=$readerType';

  @override
  String toString() => message;
}

/// Result of checking a single path against DFA
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

/// Check if a moded term is well-typed by a type DFA.
///
/// Per Definition 4.5: A moded term T is well-typed by a GLP type D if:
/// 1. For each term path x ∈ paths(T) there is a consistent path in the type DFA
/// 2. For every pair of variables in T, their types are complementary
///
/// Returns WellTypedResult with:
/// - isWellTyped: true iff all paths consistent and variable pairs complementary
/// - variableTypes: type assignments for each variable
/// - errors: list of all violations found
WellTypedResult checkModedTerm(ModedTerm term, TypeDFA dfa) {
  final errors = <WellTypedError>[];
  final variableTypes = <String, VariableTypeInfo>{};

  // Step 1: Extract and check all paths
  final termPaths = paths(term);

  for (final path in termPaths) {
    final result = checkPathAgainstDFA(path, dfa);

    if (!result.isConsistent) {
      errors.add(InconsistentPathError(path, result.reason ?? 'Unknown'));
    } else if (result.variableAssignment != null) {
      // Record variable type
      final varKey = _variableKey(path.leaf);

      if (variableTypes.containsKey(varKey)) {
        // Same variable appears multiple times - types must match
        final existing = variableTypes[varKey]!;
        if (existing.typeState != result.variableAssignment!.typeState) {
          errors.add(InconsistentVariableError(
              varKey, existing, result.variableAssignment!));
        }
      } else {
        variableTypes[varKey] = result.variableAssignment!;
      }
    }
  }

  // Step 2: Check variable pair complementarity
  final complementErrors = _checkComplementarity(variableTypes);
  errors.addAll(complementErrors);

  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors,
  );
}

/// Check if a single moded path is consistent with the type DFA.
///
/// Per Definition 4.3: A moded path is consistent with the type DFA if:
/// 1. Structure matches: each non-leaf step corresponds to valid DFA transition
/// 2. Leaf consistency: variable/constant at leaf matches DFA state
PathCheckResult checkPathAgainstDFA(ModedPath path, TypeDFA dfa) {
  var state = dfa.startState;

  // Handle single-step paths (just a variable or constant at root)
  if (path.length == 1) {
    return _checkLeafConsistency(path.leaf, state, dfa);
  }

  // Traverse path, following DFA transitions
  for (int i = 0; i < path.length - 1; i++) {
    final step = path.steps[i];
    final nextStep = path.steps[i + 1];

    // Build label for the transition we need to follow
    final label = _buildTransitionLabel(step, nextStep);

    // Find transition (matching by pathElement, considering mode)
    final nextState = _findTransition(state, label, dfa);

    if (nextState == null) {
      return PathCheckResult.inconsistent(
          'No transition for ${label.pathElement} (mode: ${label.mode}) from state ${state.name}');
    }

    state = nextState;
  }

  // Check leaf consistency
  return _checkLeafConsistency(path.leaf, state, dfa);
}

// =============================================================================
// Internal Functions
// =============================================================================

/// Build transition label from path steps
ModedLabel _buildTransitionLabel(PathStep currentStep, PathStep nextStep) {
  // Parse functor/arity from current step symbol (e.g., "[|]/2" → "[|]", 2)
  final parts = currentStep.symbol.split('/');
  if (parts.length != 2) {
    // Leaf node (variable or constant) - shouldn't happen for non-leaf steps
    return ModedLabel(currentStep.symbol, mode: nextStep.mode);
  }

  final functor = parts[0];
  final arity = int.tryParse(parts[1]) ?? 0;

  // Label encodes: functor(arity, argIndex) with mode from next step
  return ModedLabel.functor(functor, arity, nextStep.argIndex, mode: nextStep.mode);
}

/// Find a DFA transition matching the label
///
/// First tries exact match (pathElement + mode), then falls back to structural
/// match (pathElement only) if DFA has unmoded transitions.
DFAState? _findTransition(DFAState from, ModedLabel label, TypeDFA dfa) {
  // Convert ModedLabel to PathElement for lookup (current TypeDFA uses PathElement)
  final pathElement = label.toPathElement();

  // Try exact match
  final exact = dfa.transitions[(from, pathElement)];
  if (exact != null) return exact;

  // Try structural match (same pathElement string, any mode)
  for (final entry in dfa.transitions.entries) {
    final (fromState, transPathElem) = entry.key;
    if (fromState == from && transPathElem.symbol == pathElement.symbol) {
      return entry.value;
    }
  }

  return null;
}

/// Check leaf consistency with DFA state
PathCheckResult _checkLeafConsistency(PathStep leaf, DFAState state, TypeDFA dfa) {
  if (leaf.isVariable) {
    // Variable leaf
    return _checkVariableLeaf(leaf, state, dfa);
  } else {
    // Constant leaf
    return _checkConstantLeaf(leaf, state, dfa);
  }
}

/// Check variable at leaf position
PathCheckResult _checkVariableLeaf(PathStep leaf, DFAState state, TypeDFA dfa) {
  final isReader = leaf.isReader;
  final requiredMode = isReader ? Mode.consume : Mode.produce;

  if (dfa.isPrimitiveState(state)) {
    // At primitive state - check if mode is accepted
    final acceptedModes = dfa.getModesAt(state);

    if (acceptedModes.contains(requiredMode)) {
      return PathCheckResult.consistent(VariableTypeInfo(
        typeState: state,
        mode: requiredMode,
        isReader: isReader,
      ));
    } else {
      final modeStr = isReader ? 'Reader (↓)' : 'Writer (↑)';
      return PathCheckResult.inconsistent(
          '$modeStr variable ${leaf.symbol} at state ${state.name} which accepts $acceptedModes');
    }
  } else {
    // Non-primitive state - variable can appear here if it represents
    // a subterm of the appropriate type
    // This is a valid position for a variable (accepts any value of this type)
    return PathCheckResult.consistent(VariableTypeInfo(
      typeState: state,
      mode: requiredMode,
      isReader: isReader,
    ));
  }
}

/// Check constant at leaf position
PathCheckResult _checkConstantLeaf(PathStep leaf, DFAState state, TypeDFA dfa) {
  // Constants must reach a leaf state (primitive or final)
  if (dfa.isPrimitiveState(state)) {
    // At primitive state - constant matches if modes allow
    // For constants, we check against the state's acceptance
    // Primitive states like Number or String accept appropriate constants
    return PathCheckResult.consistent();
  }

  if (dfa.finalStates.contains(state)) {
    // At final state - constant accepted
    return PathCheckResult.consistent();
  }

  // Check if there's a transition for this constant
  final constPathElem = PathElement.constant(leaf.symbol);
  final nextState = dfa.transitions[(state, constPathElem)];

  if (nextState != null && (dfa.finalStates.contains(nextState) ||
      dfa.isPrimitiveState(nextState))) {
    return PathCheckResult.consistent();
  }

  return PathCheckResult.inconsistent(
      'Constant ${leaf.symbol} not accepted at state ${state.name}');
}

/// Get variable key for the variable types map
String _variableKey(PathStep leaf) {
  // Use the symbol directly which includes ? for readers
  return leaf.symbol;
}

/// Check complementarity of variable pairs
List<NonComplementaryError> _checkComplementarity(
    Map<String, VariableTypeInfo> variableTypes) {
  final errors = <NonComplementaryError>[];

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

      // Must be at same type state
      if (writerInfo.typeState != readerInfo.typeState) {
        errors.add(NonComplementaryError(baseName, writerInfo, readerInfo));
      }
      // Mode complementarity is implicit: writer has produce, reader has consume
      // which are by definition complementary
    }
  }

  return errors;
}
