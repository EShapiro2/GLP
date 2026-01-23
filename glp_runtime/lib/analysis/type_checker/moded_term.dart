// lib/analysis/type_checker/moded_term.dart
//
// Moded terms for GLP type checking.
// Specification: docs/modules/moded-term.md v0.5
// Paper Reference: Definition 4.2 (Moded Term, Complement), lines 179-211
//
// A moded term is a GLP term with mode annotations (↓ consume, ↑ produce)
// on non-variable subterms. Variables have implicit mode based on
// reader/writer status.

import 'mode.dart';

// =============================================================================
// Moded Term Classes
// =============================================================================

/// A moded term: a GLP term with mode annotations.
///
/// Per Definition 4.2: A moded term T' corresponding to a GLP term T is the
/// result of adding a mode annotation (either ↓ consume or ↑ produce) to T
/// and to every non-variable subterm of T.
abstract class ModedTerm {
  /// The mode annotation of this term
  Mode get mode;

  /// Accept a visitor
  T accept<T>(ModedTermVisitor<T> visitor);
}

/// A compound term with mode annotation.
///
/// Example: ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
class ModedCompound extends ModedTerm {
  @override
  final Mode mode;

  final String functor;
  final int arity;
  final List<ModedTerm> args;

  ModedCompound(this.mode, this.functor, this.arity, this.args);

  /// Convenience constructor for list cons [H|T]
  factory ModedCompound.listCons(Mode mode, ModedTerm head, ModedTerm tail) {
    return ModedCompound(mode, '[|]', 2, [head, tail]);
  }

  /// Check if this is a list cons term
  bool get isListCons => functor == '[|]' && arity == 2;

  @override
  T accept<T>(ModedTermVisitor<T> visitor) => visitor.visitCompound(this);

  @override
  String toString() {
    final modeStr = mode == Mode.consume ? '↓' : '↑';
    if (isListCons) {
      return '$modeStr[${args[0]}|${args[1]}]';
    }
    if (arity == 0) {
      return '$modeStr$functor';
    }
    return '$modeStr$functor(${args.join(', ')})';
  }

  @override
  bool operator ==(Object other) =>
      other is ModedCompound &&
      mode == other.mode &&
      functor == other.functor &&
      arity == other.arity &&
      _listEquals(args, other.args);

  @override
  int get hashCode => Object.hash(mode, functor, arity, Object.hashAll(args));
}

/// A constant (integer, string, atom) with mode annotation.
///
/// Example: ↓42, ↑"hello", ↓[]
class ModedConstant extends ModedTerm {
  @override
  final Mode mode;

  /// The constant value: int, double, String, or atom name
  final Object value;

  ModedConstant(this.mode, this.value);

  /// Convenience constructor for nil []
  factory ModedConstant.nil(Mode mode) => ModedConstant(mode, '[]');

  /// Check if this is nil
  bool get isNil => value == '[]';

  /// True if value is an integer
  bool get isInteger => value is int;

  /// True if value is a real (floating-point) number
  bool get isReal => value is double;

  /// True if value is numeric (integer or real)
  bool get isNumeric => value is num;

  /// True if value is a quoted string
  bool get isString {
    if (value is! String) return false;
    final s = value as String;
    return (s.startsWith('"') && s.endsWith('"')) ||
           (s.startsWith("'") && s.endsWith("'"));
  }

  /// True if value is an unquoted atom (includes nil)
  bool get isAtom {
    if (value is! String) return false;
    return !isString;
  }

  @override
  T accept<T>(ModedTermVisitor<T> visitor) => visitor.visitConstant(this);

  @override
  String toString() {
    final modeStr = mode == Mode.consume ? '↓' : '↑';
    return '$modeStr$value';
  }

  @override
  bool operator ==(Object other) =>
      other is ModedConstant && mode == other.mode && value == other.value;

  @override
  int get hashCode => Object.hash(mode, value);
}

/// A variable (reader or writer) with structural mode annotation.
///
/// Per Remark 4.1 (Structural Mode vs. Variable Mode):
/// - Structural mode: inherited from enclosing type context (stored)
/// - Implicit mode: determined by reader/writer form (computed)
///
/// For well-typing, implicit mode must equal structural mode at variable positions.
///
/// Example: X? (reader), X (writer)
class ModedVariable extends ModedTerm {
  final String name;

  /// true for reader X?, false for writer X
  final bool isReader;

  /// Structural mode from parent context
  final Mode _structuralMode;

  ModedVariable(this.name, {required this.isReader, required Mode structuralMode})
      : _structuralMode = structuralMode;

  /// Convenience constructor for reader
  factory ModedVariable.reader(String name, {required Mode structuralMode}) =>
      ModedVariable(name, isReader: true, structuralMode: structuralMode);

  /// Convenience constructor for writer
  factory ModedVariable.writer(String name, {required Mode structuralMode}) =>
      ModedVariable(name, isReader: false, structuralMode: structuralMode);

  /// Structural mode (used in path traversal for automaton lookup)
  @override
  Mode get mode => _structuralMode;

  /// Implicit mode based on reader/writer form
  Mode get implicitMode => isReader ? Mode.consume : Mode.produce;

  /// Whether this variable is mode-consistent (for well-typing)
  /// Note: This is a convenience property. The authoritative mode consistency
  /// check is in Definition 4.3 (Consistent Paths) / well-typed-term module.
  bool get isModeConsistent => implicitMode == _structuralMode;

  /// Whether this is a writer
  bool get isWriter => !isReader;

  @override
  T accept<T>(ModedTermVisitor<T> visitor) => visitor.visitVariable(this);

  @override
  String toString() => isReader ? '$name?' : name;

  @override
  bool operator ==(Object other) =>
      other is ModedVariable &&
      name == other.name &&
      isReader == other.isReader &&
      _structuralMode == other._structuralMode;

  @override
  int get hashCode => Object.hash(name, isReader, _structuralMode);
}

/// Visitor for moded terms
abstract class ModedTermVisitor<T> {
  T visitCompound(ModedCompound term);
  T visitConstant(ModedConstant term);
  T visitVariable(ModedVariable term);
}

// =============================================================================
// Moded Path Classes
// =============================================================================

/// A path through a moded term.
///
/// A moded path is a sequence from root to leaf:
/// (0, m₀) --> f/n --(i₁, m₁)--> ... --(iₖ, mₖ)--> leaf
///
/// The leaf is either a constant or a variable.
class ModedPath {
  final List<PathStep> steps;

  ModedPath(this.steps);

  /// The root step (first step in path)
  PathStep get root => steps.first;

  /// The leaf step (last step in path)
  PathStep get leaf => steps.last;

  /// Whether this path starts with consume mode (input path)
  bool get isInputPath => root.mode == Mode.consume;

  /// Whether this path starts with produce mode (output path)
  bool get isOutputPath => root.mode == Mode.produce;

  /// Path length (number of steps)
  int get length => steps.length;

  @override
  String toString() {
    return steps.map((s) => '(${s.symbol}, ${s.argIndex}, ${s.mode})').join(' → ');
  }

  @override
  bool operator ==(Object other) =>
      other is ModedPath && _listEquals(steps, other.steps);

  @override
  int get hashCode => Object.hashAll(steps);
}

/// A single step in a moded path.
class PathStep {
  /// Symbol at this position:
  /// - functor/arity for compound (e.g., "merge/3", "[|]/2")
  /// - value for constant (e.g., "42", "[]")
  /// - variable name for variable (e.g., "X", "X?")
  final String symbol;

  /// Argument index: 0 for root, 1-based for arguments
  final int argIndex;

  /// Mode at this position
  final Mode mode;

  /// True if this step is a variable leaf
  final bool isVariable;

  /// If isVariable, whether it's a reader (X?)
  final bool isReader;

  PathStep({
    required this.symbol,
    required this.argIndex,
    required this.mode,
    this.isVariable = false,
    this.isReader = false,
  });

  /// Whether this is a writer variable
  bool get isWriter => isVariable && !isReader;

  @override
  String toString() {
    final modeStr = mode == Mode.consume ? '↓' : '↑';
    return '($symbol, $argIndex, $modeStr)';
  }

  @override
  bool operator ==(Object other) =>
      other is PathStep &&
      symbol == other.symbol &&
      argIndex == other.argIndex &&
      mode == other.mode &&
      isVariable == other.isVariable &&
      isReader == other.isReader;

  @override
  int get hashCode =>
      Object.hash(symbol, argIndex, mode, isVariable, isReader);
}

// =============================================================================
// Classification Methods (isConsumed, isProduced, isIO)
// Spec: moded-term.md v0.5
// =============================================================================

/// Returns true if all mode annotations in t are consume (↓).
///
/// Per spec: Returns true iff every non-variable subterm has mode = Mode.consume
bool isConsumed(ModedTerm t) {
  return t.accept(_IsConsumedVisitor());
}

class _IsConsumedVisitor implements ModedTermVisitor<bool> {
  @override
  bool visitCompound(ModedCompound term) {
    if (term.mode != Mode.consume) return false;
    return term.args.every((arg) => arg.accept(this));
  }

  @override
  bool visitConstant(ModedConstant term) {
    return term.mode == Mode.consume;
  }

  @override
  bool visitVariable(ModedVariable term) {
    // Variables have implicit mode based on reader/writer status
    return term.mode == Mode.consume;
  }
}

/// Returns true if all mode annotations in t are produce (↑).
///
/// Per spec: Returns true iff every non-variable subterm has mode = Mode.produce
bool isProduced(ModedTerm t) {
  return t.accept(_IsProducedVisitor());
}

class _IsProducedVisitor implements ModedTermVisitor<bool> {
  @override
  bool visitCompound(ModedCompound term) {
    if (term.mode != Mode.produce) return false;
    return term.args.every((arg) => arg.accept(this));
  }

  @override
  bool visitConstant(ModedConstant term) {
    return term.mode == Mode.produce;
  }

  @override
  bool visitVariable(ModedVariable term) {
    // Variables have implicit mode based on reader/writer status
    return term.mode == Mode.produce;
  }
}

/// Returns true if t is an I/O moded term.
///
/// An I/O moded term has:
/// - Root mode is consume (↓)
/// - On every path from root to leaf, mode transitions only in the
///   direction ↓ → ↑ (never ↑ → ↓)
///
/// Per spec algorithm:
/// ```
/// isIO(t):
///   if t.mode != Mode.consume:
///     return false
///   return allPathsValidIO(t, Mode.consume)
/// ```
bool isIO(ModedTerm t) {
  if (t.mode != Mode.consume) return false;
  return _allPathsValidIO(t, Mode.consume);
}

/// Helper: Check all paths have valid I/O mode transitions.
///
/// Valid transitions: ↓ → ↓, ↓ → ↑, ↑ → ↑
/// Invalid transitions: ↑ → ↓ (flipping back from produce to consume)
bool _allPathsValidIO(ModedTerm t, Mode parentMode) {
  final currentMode = t.mode;

  // Check valid transition: only ↓→↑ allowed, not ↑→↓
  if (parentMode == Mode.produce && currentMode == Mode.consume) {
    return false; // Invalid: flipped back from ↑ to ↓
  }

  if (t is ModedCompound) {
    return t.args.every((arg) => _allPathsValidIO(arg, currentMode));
  }

  // Leaf reached (constant or variable)
  return true;
}

// =============================================================================
// Dual Operation
// =============================================================================

/// Constructs the dual of a moded term per Definition 5.1.
///
/// The dual T? is obtained by:
/// 1. Flipping every mode annotation (↓ ↔ ↑)
/// 2. Replacing every variable by its paired variable (X ↔ X?)
///
/// The dual operation is an involution: dual(dual(t)) == t
ModedTerm dual(ModedTerm t) {
  return t.accept(_DualVisitor());
}

class _DualVisitor implements ModedTermVisitor<ModedTerm> {
  @override
  ModedTerm visitCompound(ModedCompound term) {
    return ModedCompound(
      term.mode.flip,
      term.functor,
      term.arity,
      term.args.map((arg) => dual(arg)).toList(),
    );
  }

  @override
  ModedTerm visitConstant(ModedConstant term) {
    return ModedConstant(term.mode.flip, term.value);
  }

  @override
  ModedTerm visitVariable(ModedVariable term) {
    // Flip both reader/writer and structural mode
    return ModedVariable(term.name, isReader: !term.isReader, structuralMode: term.mode.flip);
  }
}

// =============================================================================
// Path Extraction
// =============================================================================

/// Extracts all moded paths from a moded term.
///
/// Returns the set of all paths from root to leaves.
/// Each path is a sequence of PathSteps.
Set<ModedPath> paths(ModedTerm t) {
  final result = <ModedPath>{};

  // Create root step
  final rootStep = PathStep(
    symbol: _symbolOf(t),
    argIndex: 0,
    mode: t.mode,
    isVariable: t is ModedVariable,
    isReader: t is ModedVariable ? t.isReader : false,
  );

  _extractPaths(t, [rootStep], result);
  return result;
}

/// Recursively extract paths from a moded term
void _extractPaths(ModedTerm t, List<PathStep> prefix, Set<ModedPath> result) {
  if (t is ModedCompound) {
    // Compound: recurse into each argument
    for (int i = 0; i < t.arity; i++) {
      final child = t.args[i];
      final childStep = PathStep(
        symbol: _symbolOf(child),
        argIndex: i + 1, // 1-based
        mode: child.mode,
        isVariable: child is ModedVariable,
        isReader: child is ModedVariable ? child.isReader : false,
      );
      _extractPaths(child, [...prefix, childStep], result);
    }
  } else {
    // Leaf (constant or variable): add path to result
    result.add(ModedPath(prefix));
  }
}

/// Get the symbol representation of a moded term
String _symbolOf(ModedTerm t) {
  if (t is ModedCompound) {
    return '${t.functor}/${t.arity}';
  } else if (t is ModedConstant) {
    return t.value.toString();
  } else if (t is ModedVariable) {
    return t.isReader ? '${t.name}?' : t.name;
  }
  throw ArgumentError('Unknown moded term type: $t');
}

// =============================================================================
// Utility
// =============================================================================

/// Compare two lists for equality
bool _listEquals<T>(List<T> a, List<T> b) {
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
