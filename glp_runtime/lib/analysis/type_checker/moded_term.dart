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
import '../../runtime/terms.dart' show Nil, nil;

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

/// A constant (integer, real, string, or the empty list) with mode annotation.
///
/// Example: ↓42, ↑hello, ↓[]
class ModedConstant extends ModedTerm {
  @override
  final Mode mode;

  /// The constant's value, of its kind: an int, a double, a String, or the
  /// empty list, the runtime's [nil] (runtime/terms.dart).  GLP-Spec
  /// appendix-lp.tex, Definition "Logic Programs Syntax": "a constant
  /// (numbers, strings, or the empty list `[]`)", so the string `'[]'` is a
  /// String and not the empty list, and `'0'` a String and not a number.
  /// Until 2026-10-10 the empty list was held here as the string '[]', and
  /// the two were one value.
  final Object value;

  ModedConstant(this.mode, this.value);

  /// The empty list `[]`.
  factory ModedConstant.nil(Mode mode) => ModedConstant(mode, nil);

  /// Whether this is the empty list `[]`, and not the string `'[]'`.
  bool get isNil => value is Nil;

  /// True if value is an integer
  bool get isInteger => value is int;

  /// True if value is a real (floating-point) number
  bool get isReal => value is double;

  /// True if value is numeric (integer or real)
  bool get isNumeric => value is num;

  /// True if value is a String leaf.
  ///
  /// Row 7 of the consistency table (TGLP def:consistent-paths) makes a term
  /// constant `c` compatible with the type symbol `c`, with `String` and with
  /// `_`, and says nothing of quoting; `Constant ::= Number ; String ; Module`
  /// in the root carries the same reading, and `[]` is a `String` and hence a
  /// `Constant` (TGLP sec:root-self).  So every constant that is not a number
  /// is a String leaf, quoted or not, the empty list among them — as
  /// `well_typed_term.dart` builds the leaf and `subtyping.dart` reads a
  /// constant alternative, by the value's kind ([primitiveOfConstant]).  Until
  /// 2026-09-23 this getter counted only a quoted constant, a distinction the
  /// type system does not have; `isAtom`, which named the complementary half
  /// of that distinction, went with it.
  bool get isString => value is String || value is Nil;

  @override
  T accept<T>(ModedTermVisitor<T> visitor) => visitor.visitConstant(this);

  @override
  String toString() {
    final modeStr = mode == Mode.consume ? '↓' : '↑';
    return '$modeStr$value';
  }

  @override
  bool operator ==(Object other) =>
      other is ModedConstant &&
      mode == other.mode &&
      sameConstant(value, other.value);

  @override
  int get hashCode => Object.hash(mode, value);
}

/// Whether [a] and [b] are one constant: one value of one kind.  Dart's `==`
/// holds of the integer 1 and the real 1.0, which are two constants.
bool sameConstant(Object? a, Object? b) =>
    a == b && (a is double) == (b is double);

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
  List<PathStep>? _steps;
  final PathLink? _last;

  ModedPath(List<PathStep> steps)
      : _steps = steps,
        _last = null;

  /// The path whose last step is [last], its steps listed when first asked
  /// for: a walk of a term makes one of these for a path in constant time,
  /// where listing the steps of every path costs the sum of their lengths,
  /// quadratic in the length of a list ([forEachCheckedPath],
  /// well_typed_term.dart).
  ModedPath.ending(PathLink last) : _last = last;

  /// The steps from the root to the leaf.
  List<PathStep> get steps => _steps ??= _last!.steps();

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

/// A step of a path and the steps before it, shared by every path through
/// the step's vertex.
class PathLink {
  final PathStep step;
  final PathLink? parent;

  /// The number of steps from the root to this one.
  final int length;

  PathLink(this.step, this.parent) : length = (parent?.length ?? 0) + 1;

  /// The steps from the root to this one, listed by a loop up the parents.
  List<PathStep> steps() {
    final out = List<PathStep>.filled(length, step, growable: true);
    PathLink? link = this;
    for (var i = length - 1; i >= 0; i--) {
      out[i] = link!.step;
      link = link.parent;
    }
    return out;
  }
}

/// A single step in a moded path.
class PathStep {
  /// Symbol at this position:
  /// - functor/arity for compound (e.g., "merge/3", "[|]/2")
  /// - the constant's text for a constant (e.g., "42", "[]"), for display
  /// - variable name for variable (e.g., "X", "X?")
  final String symbol;

  /// The constant's value at a constant leaf, of its kind --- an int, a
  /// double, a String, or the empty list [nil] --- and null at a functor or a
  /// variable.  A constant is typed by its value's kind and never by its text
  /// (GLP #3 Cowork, 2026-10-09 18:38 UTC, "12:19", (ii)): the symbol of the
  /// string `'0000'` is the text of the integer 0000, and the symbol of the
  /// string `'[]'` the text of the empty list.  Until 2026-10-10 a step
  /// carried the symbol alone, and the leaf was classified by parsing it.
  final Object? value;

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
    this.value,
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
      isReader == other.isReader &&
      sameConstant(value, other.value);

  @override
  int get hashCode =>
      Object.hash(symbol, argIndex, mode, isVariable, isReader, value);
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
///
/// The term is walked with a stack of its subterms, so a term of any size is
/// walked in constant stack depth: TGLP well-typing.tex, Definition
/// "Well-Typed Clause", puts no bound on a term's size.  The paths are added
/// depth first and left to right, the order of the recursive walk this
/// replaces.  The checker walks a term's paths with [forEachCheckedPath]
/// (well_typed_term.dart), which builds no set.
Set<ModedPath> paths(ModedTerm t) {
  final result = <ModedPath>{};
  // The steps from the root to the subterm being visited.
  final prefix = <PathStep>[];
  // Each subterm still to visit, with its step and its depth.
  final pending = <(ModedTerm, PathStep, int)>[(t, pathStepOf(t, 0), 0)];
  while (pending.isNotEmpty) {
    final (term, step, depth) = pending.removeLast();
    prefix.length = depth;
    prefix.add(step);
    if (term is ModedCompound) {
      // Compound: each argument, the first visited first
      for (int i = term.arity - 1; i >= 0; i--) {
        final child = term.args[i];
        pending.add((child, pathStepOf(child, i + 1), depth + 1)); // 1-based
      }
    } else {
      // Leaf (constant or variable): add path to result
      result.add(ModedPath(List.of(prefix)));
    }
  }
  return result;
}

/// The step of a path at [t], the [argIndex]th argument of its parent (0 at
/// the root).
PathStep pathStepOf(ModedTerm t, int argIndex) => PathStep(
      symbol: _symbolOf(t),
      argIndex: argIndex,
      mode: t.mode,
      isVariable: t is ModedVariable,
      isReader: t is ModedVariable ? t.isReader : false,
      value: t is ModedConstant ? t.value : null,
    );

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
