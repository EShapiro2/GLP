// lib/analysis/type_checker/moded_head.dart
//
// Moded head construction for GLP type checking.
// Specification: docs/modules/moded-head.md v0.5
// Paper Reference: Definition 4.6 (lines 285-288)
//
// Constructs a moded head H' from a clause head H and a procedure declaration.
// The moded head is used for well-typing checks.

import 'mode.dart';
import 'moded_term.dart';
import 'type_ast.dart';
import '../../compiler/ast.dart' as ast;

// =============================================================================
// Public Functions
// =============================================================================

/// Constructs a moded head H' from clause head H per Definition 4.6.
///
/// Given a head H, a moded head H' is obtained by:
/// 1. Constructing an I/O-moded term corresponding to H, then
/// 2. Replacing each variable by its paired variable (X ↔ X?)
///
/// The variable flip captures inverted roles at the call boundary:
/// - Head writer X becomes reader X? (bound BY the goal)
/// - Head reader X? becomes writer X (will be bound BY the body)
///
/// **Preconditions:**
/// - `head` is a valid clause head (compound term)
/// - `decl` provides the procedure type declaration
/// - `head.functor == decl.name` and `head.arity == decl.arity`
///
/// **Postconditions:** Returns a ModedTerm where:
/// - Root mode is ↓ (consume)
/// - Each argument has mode based on declared type: Type? → ↓, Type → ↑
/// - Embedded modes within structures are combined via involution
/// - All variables are flipped (X ↔ X?)
///
/// Throws [ArityMismatchError] if head arity doesn't match declaration.
/// Throws [InvalidHeadError] if head is not a compound term.
ModedTerm modedHead(ast.Goal head, ProcDecl decl, {TypeEnvironment? typeEnv}) {
  // Validate arity
  if (head.arity != decl.arity) {
    throw ArityMismatchError(
      'Head arity ${head.arity} does not match declaration arity ${decl.arity}',
    );
  }

  // Step 1: Build I/O moded term (root mode = consume)
  final ioTerm = _buildIOModedTerm(head, decl, Mode.consume, typeEnv);

  // Step 2: Flip all variables
  return _flipAllVariables(ioTerm);
}

/// Constructs a produced moded term from a body atom.
///
/// Body atoms are goals being called, not definitions being matched.
/// Variables stay as-is (no flip) because the body atom represents
/// the caller's perspective.
///
/// **Preconditions:**
/// - `atom` is a valid body atom (compound term)
/// - `decl` provides the procedure type declaration
///
/// **Postconditions:** Returns a ModedTerm where:
/// - Root mode is ↑ (produce)
/// - Each argument has mode based on declared type: Type? → ↓, Type → ↑
/// - Embedded modes within structures are combined via involution
/// - Variables are NOT flipped
///
/// Throws [ArityMismatchError] if atom arity doesn't match declaration.
ModedTerm producedTerm(ast.Goal atom, ProcDecl decl, {TypeEnvironment? typeEnv}) {
  // Validate arity
  if (atom.arity != decl.arity) {
    throw ArityMismatchError(
      'Atom arity ${atom.arity} does not match declaration arity ${decl.arity}',
    );
  }

  // Build I/O moded term with root mode = produce
  // Note: NO variable flip for body atoms
  return _buildIOModedTerm(atom, decl, Mode.produce, typeEnv);
}

// =============================================================================
// Internal Functions
// =============================================================================

/// Build an I/O moded term from a goal/head.
///
/// - Root mode is [parentMode] (consume for heads, produce for body atoms)
/// - Input arguments (Type?) have mode ↓
/// - Output arguments (Type) have mode ↑
/// - Embedded modes within structures are combined via involution
ModedTerm _buildIOModedTerm(ast.Goal term, ProcDecl decl, Mode parentMode, TypeEnvironment? typeEnv) {
  final modedArgs = <ModedTerm>[];

  for (int i = 0; i < term.args.length; i++) {
    final argType = decl.argTypes[i];
    // Input (Type? or _?) → consume, Output (Type or _) → produce
    final argMode = decl.isInputArg(i) ? Mode.consume : Mode.produce;
    final modedArg = _buildModedSubterm(term.args[i], argMode, argType, typeEnv);
    modedArgs.add(modedArg);
  }

  return ModedCompound(parentMode, term.functor, term.arity, modedArgs);
}

/// Build a moded subterm from an AST term.
///
/// Recursively converts AST term to ModedTerm with the given mode.
/// For structures, looks up type definition to compute combined modes
/// for each subterm position using mode involution.
/// Variables preserve their reader/writer status (flip happens later if needed).
ModedTerm _buildModedSubterm(ast.Term term, Mode mode, TypeExpr? expectedType, TypeEnvironment? typeEnv) {
  if (term is ast.VarTerm) {
    // Variable: preserve reader/writer status
    return ModedVariable(term.name, isReader: term.isReader);
  }

  if (term is ast.StructTerm) {
    // Structure: look up type definition for embedded modes
    final subtermModes = _getSubtermModes(term.functor, term.arity, mode, expectedType, typeEnv);

    final modedArgs = <ModedTerm>[];
    for (int i = 0; i < term.args.length; i++) {
      final (subtermMode, subtermType) = subtermModes[i];
      modedArgs.add(_buildModedSubterm(term.args[i], subtermMode, subtermType, typeEnv));
    }
    return ModedCompound(mode, term.functor, term.arity, modedArgs);
  }

  if (term is ast.ListTerm) {
    if (term.isNil) {
      // Empty list []
      return ModedConstant.nil(mode);
    }
    // Non-empty list [H|T] - get embedded modes for head and tail
    final listModes = _getListSubtermModes(mode, expectedType, typeEnv);
    final (headMode, headType) = listModes.$1;
    final (tailMode, tailType) = listModes.$2;

    final head = _buildModedSubterm(term.head!, headMode, headType, typeEnv);
    final tail = _buildModedSubterm(term.tail!, tailMode, tailType, typeEnv);
    return ModedCompound.listCons(mode, head, tail);
  }

  if (term is ast.ConstTerm) {
    // Constant
    return ModedConstant(mode, term.value ?? 'null');
  }

  if (term is ast.UnderscoreTerm) {
    // Anonymous variable: treat as a unique writer variable
    // Use a synthetic name to avoid confusion
    return ModedVariable('_', isReader: false);
  }

  throw InvalidHeadError('Unknown term type: ${term.runtimeType}');
}

/// Get subterm modes for a structure by looking up type definition.
///
/// Uses mode involution: combinedMode = parentMode ⊕ embeddedMode
/// where ⊕ is XOR-like: same modes → produce, different → consume
List<(Mode, TypeExpr?)> _getSubtermModes(
  String functor,
  int arity,
  Mode parentMode,
  TypeExpr? expectedType,
  TypeEnvironment? typeEnv
) {
  // Default: propagate parent mode if no type info available
  final defaultModes = List.generate(arity, (_) => (parentMode, null as TypeExpr?));

  if (typeEnv == null || expectedType == null) {
    return defaultModes;
  }

  // Resolve type reference to get the type definition
  String? typeName;
  if (expectedType is TypeRef) {
    typeName = expectedType.name;
  }

  if (typeName == null) {
    return defaultModes;
  }

  final typeDef = typeEnv.getType(typeName);
  if (typeDef == null) {
    return defaultModes;
  }

  // Find matching structure alternative in type definition
  for (final alt in typeDef.alternatives) {
    if (alt is StructAlt && alt.functor == functor && alt.arity == arity) {
      // Found matching constructor - compute combined modes for each arg
      final result = <(Mode, TypeExpr?)>[];
      for (final argType in alt.args) {
        final embeddedMode = _getEmbeddedMode(argType);
        final combinedMode = combineMode(parentMode, embeddedMode);
        result.add((combinedMode, argType));
      }
      return result;
    }

    // Handle DiffList: \ operator
    if (alt is DiffListAlt && functor == r'\' && arity == 2) {
      final contentMode = _getEmbeddedMode(alt.content);
      final holeMode = _getEmbeddedMode(alt.hole);
      return [
        (combineMode(parentMode, contentMode), alt.content),
        (combineMode(parentMode, holeMode), alt.hole),
      ];
    }
  }

  return defaultModes;
}

/// Get subterm modes for list [H|T] by looking up type definition.
///
/// Returns a tuple of (headModeInfo, tailModeInfo) where each is (Mode, TypeExpr?).
((Mode, TypeExpr?), (Mode, TypeExpr?)) _getListSubtermModes(
  Mode parentMode,
  TypeExpr? expectedType,
  TypeEnvironment? typeEnv
) {
  // Default: propagate parent mode
  final defaultResult = ((parentMode, null as TypeExpr?), (parentMode, null as TypeExpr?));

  if (typeEnv == null || expectedType == null) {
    return defaultResult;
  }

  String? typeName;
  if (expectedType is TypeRef) {
    typeName = expectedType.name;
  }

  if (typeName == null) {
    return defaultResult;
  }

  final typeDef = typeEnv.getType(typeName);
  if (typeDef == null) {
    return defaultResult;
  }

  // Find ListConsAlt in type definition
  for (final alt in typeDef.alternatives) {
    if (alt is ListConsAlt) {
      final headEmbeddedMode = _getEmbeddedMode(alt.head);
      final tailEmbeddedMode = _getEmbeddedMode(alt.tail);
      return (
        (combineMode(parentMode, headEmbeddedMode), alt.head),
        (combineMode(parentMode, tailEmbeddedMode), alt.tail),
      );
    }
  }

  return defaultResult;
}

/// Get the embedded mode from a type expression.
///
/// TypeRef with isInput=true → consume (↓)
/// TypeRef with isInput=false → produce (↑)
/// PrimitiveModeAlt with isInput=true → consume (↓)
/// PrimitiveModeAlt with isInput=false → produce (↑)
/// Other expressions → produce (↑) by default
Mode _getEmbeddedMode(TypeExpr expr) {
  if (expr is TypeRef) {
    return expr.isInput ? Mode.consume : Mode.produce;
  }
  if (expr is PrimitiveModeAlt) {
    return expr.isInput ? Mode.consume : Mode.produce;
  }
  // Default to produce for other expressions
  return Mode.produce;
}

/// Flip all variables in a moded term.
///
/// X → X?, X? → X
/// This captures the inverted roles at the call boundary.
ModedTerm _flipAllVariables(ModedTerm term) {
  if (term is ModedCompound) {
    final flippedArgs = term.args.map(_flipAllVariables).toList();
    return ModedCompound(term.mode, term.functor, term.arity, flippedArgs);
  }

  if (term is ModedConstant) {
    // Constants are unchanged
    return term;
  }

  if (term is ModedVariable) {
    // Flip reader/writer
    return ModedVariable(term.name, isReader: !term.isReader);
  }

  throw InvalidHeadError('Unknown moded term type: ${term.runtimeType}');
}

// =============================================================================
// Errors
// =============================================================================

/// Error thrown when head/atom arity doesn't match declaration.
class ArityMismatchError implements Exception {
  final String message;
  ArityMismatchError(this.message);

  @override
  String toString() => 'ArityMismatchError: $message';
}

/// Error thrown when head is not a valid compound term.
class InvalidHeadError implements Exception {
  final String message;
  InvalidHeadError(this.message);

  @override
  String toString() => 'InvalidHeadError: $message';
}
