// lib/analysis/type_checker/well_typed_term.dart
//
// Well-typed moded term checking for GLP type system.
// Specification: TGLP (Moded-Types), sections/well-typing.tex Definition "Well-Typed Moded Term"
// Paper Reference: Definition 5.4 (Well-Typed Moded Term)
//
// Determines when a moded term is well-typed by an automaton by checking path
// consistency via automaton traversal.

import 'mode.dart';
import 'moded_term.dart';
import '../../runtime/terms.dart' show Nil;
import 'program_dfa.dart';
import 'subtyping.dart';

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

  /// The occurrences of a key after its first, [variableTypes] holding the
  /// first: each occurrence is typed by its own position (TGLP
  /// well-typing.tex, Definition "Type Assignment"), and each is a pair with
  /// every occurrence of its partner under condition 3 of Definition
  /// "Well-Typed Clause with Subtyping".  No rule makes the occurrences of one
  /// variable one type (GLP, 2026-10-14 08:34 UTC).
  final Map<String, List<VariableTypeInfo>> repeats;

  WellTypedResult({
    required this.isWellTyped,
    required this.variableTypes,
    required this.errors,
    this.repeats = const {},
  });

  /// Every occurrence of [key], its first and its [repeats].
  List<VariableTypeInfo> occurrencesOf(String key) => [
        if (variableTypes[key] != null) variableTypes[key]!,
        ...?repeats[key],
      ];

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
  final repeats = <String, List<VariableTypeInfo>>{};

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
        // The same variable again: typed by its own position, and paired as
        // every occurrence is ([WellTypedResult.repeats]).
        repeats.putIfAbsent(varKey, () => []).add(result.variableAssignment!);
      } else {
        variableTypes[varKey] = result.variableAssignment!;
      }
    }
  }

  // Step 2: Check variable pair duality, every occurrence of the writer
  // against every occurrence of the reader.
  errors.addAll(_checkDuality(variableTypes, dfa));
  for (final e in repeats.entries) {
    final key = e.key;
    final base = key.endsWith('?') ? key.substring(0, key.length - 1) : key;
    final partnerKey = key.endsWith('?') ? base : '$base?';
    final partners = [
      if (variableTypes[partnerKey] != null) variableTypes[partnerKey]!,
      ...?repeats[partnerKey],
    ];
    for (final info in e.value) {
      for (final p in partners) {
        errors.addAll(_checkDuality({key: info, partnerKey: p}, dfa));
      }
    }
  }

  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors,
    repeats: repeats,
  );
}

/// Check if a single moded path is consistent with the automaton.
///
/// Per Definition 4.3: A moded path is consistent with the automaton if:
/// 1. Structure matches: each non-leaf step corresponds to valid transition
/// 2. Leaf consistency: variable/constant at leaf matches DFA state
///
/// Fix 4.1: Switches automata at type boundaries when entering user-defined types
///
/// [open] names base types that stand for type parameters left open
/// (well_typed_clause.dart, a call checked with its callee's parameters open:
/// TGLP appendix-implementation-notes.tex, "The instantiation of a call").  A
/// path that reaches one is consistent whatever lies at or below it, since a
/// map could bind the parameter to a type admitting it, and a variable at one
/// is typed by it.  Empty by default, which is Definition "Consistent Paths"
/// itself.
PathCheckResult checkPathAgainstAutomaton(
  ModedPath path,
  Automaton automaton,
  ProgramDFA dfa, {
  Set<String> open = const {},
}) {
  var state = automaton.startState;
  var currentAutomaton = automaton;  // Track current automaton for type switching

  // At an open parameter, the subterm at step [i] holds whatever it is.
  PathCheckResult? atOpen(int i) {
    if (open.isEmpty || !open.contains(state.baseName)) return null;
    final leaf = path.leaf;
    if (i == path.length - 1 && leaf.isVariable) {
      return PathCheckResult.consistent(VariableTypeInfo(
        typeState: state,
        mode: leaf.isReader ? Mode.consume : Mode.produce,
        isReader: leaf.isReader,
      ));
    }
    return PathCheckResult.consistent();
  }

  // Handle single-step paths (just a variable or constant at root)
  if (path.length == 1) {
    return atOpen(0) ?? _checkLeafConsistencyForPath(path.leaf, state, dfa);
  }

  // Traverse path, following automaton transitions
  for (int i = 0; i < path.length - 1; i++) {
    final opened = atOpen(i);
    if (opened != null) return opened;
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
  return atOpen(path.length - 1) ??
      _checkLeafConsistencyForPath(path.leaf, state, dfa);
}

// =============================================================================
// Internal Functions
// =============================================================================

/// Build transition label from path steps
TransitionLabel _buildTransitionLabel(PathStep currentStep, PathStep nextStep) {
  // Parse functor/arity from current step symbol (e.g., "[|]/2" → "[|]", 2).
  // Split at the LAST '/': the functor itself may contain '/', as `/` and `//`
  // do, whose symbols are "//2" and "///2".
  final slash = currentStep.symbol.lastIndexOf('/');
  final arity = slash > 0 ? int.tryParse(currentStep.symbol.substring(slash + 1)) : null;
  if (arity == null) {
    // Leaf node (variable or constant) - shouldn't happen for non-leaf steps
    return TransitionLabel.functor(currentStep.symbol, 0, nextStep.argIndex, mode: nextStep.mode);
  }

  final functor = currentStep.symbol.substring(0, slash);

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

/// Convert PathStep to LeafTerm for checkLeafConsistency.
///
/// A constant leaf is classified by the kind of the value its step carries,
/// never by its text (GLP-Spec appendix-lp.tex, Definition "Logic Programs
/// Syntax"; TGLP well-typing.tex, the table of Definition "Consistent Paths",
/// rows 4 to 7; GLP #3 Cowork, 2026-10-09 18:38 UTC, "12:19", (ii)): an int an
/// integer, a double a real, a String a string, and the empty list a string
/// leaf that is the empty list.  Until 2026-10-10 the text was parsed, so the
/// string `'0000'` was the integer 0 and the string `'[]'` the empty list.
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
    final value = step.value;
    if (value is int) return LeafTerm.integerConstant(value, mode: step.mode);
    if (value is double) return LeafTerm.realConstant(value, mode: step.mode);
    // A string: a quoted name and a double-quoted string are two spellings of
    // one string, its characters as written, and `string/1` succeeds on it.
    if (value is String) return LeafTerm.stringConstant(value, mode: step.mode);
    // `[]` is a String (TGLP appendix-root-self.tex: "The empty list is a
    // String, hence a Constant"), so it is a string leaf; it still matches an
    // explicit `[]` alternative first, through the constant transition in case
    // 2d of checkLeafConsistency --- that is what keeps
    // `Stream(X) ::= [] ; [X|...]` enumerable for input coverage.
    if (value is Nil) return LeafTerm.emptyList(mode: step.mode);
    // A step built without its value (none of this library's paths is): the
    // constant of its symbol, of no primitive kind.
    return LeafTerm.constant(value ?? step.symbol, mode: step.mode);
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
    Map<String, VariableTypeInfo> variableTypes, ProgramDFA dfa) {
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

      // Check states are duals (structurally the same base type, opposite isDual)
      if (!sameBaseType(writerInfo.typeState.baseName, readerInfo.typeState.baseName, dfa)) {
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
