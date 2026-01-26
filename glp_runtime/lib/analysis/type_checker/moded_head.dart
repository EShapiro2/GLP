// lib/analysis/type_checker/moded_head.dart
//
// Moded head construction for GLP type checking.
// Specification: docs/type system/moded-head.md v0.8
// Paper Reference: Definition 5.5 (moded head)
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

// Counter for generating unique anonymous variable names
// Bug 4 fix: Each occurrence of _ must be treated as a fresh, independent variable
// Paper Remark 3.1: "Each occurrence [of _] denotes a fresh writer"
int _anonVarCounter = 0;

/// Reset the anonymous variable counter.
/// Should be called before processing each clause.
void resetAnonVarCounter() {
  _anonVarCounter = 0;
}

/// Generate a unique name for an anonymous variable.
/// Returns names like "_#1", "_#2", etc.
String _freshAnonVarName() {
  _anonVarCounter++;
  return '_#$_anonVarCounter';
}

/// Constructs a moded head H' from clause head H per Definition 5.5.
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
/// - All variables are unconditionally flipped (X ↔ X?)
///
/// Throws [ArityMismatchError] if head arity doesn't match declaration.
/// Throws [InvalidHeadError] if head is not a compound term.
ModedTerm modedHead(ast.Goal head, ProcDecl decl, {TypeEnvironment? typeEnv}) {
  // Reset counter for each clause to ensure unique names within the clause
  resetAnonVarCounter();
  // Validate arity
  if (head.arity != decl.arity) {
    throw ArityMismatchError(
      'Head arity ${head.arity} does not match declaration arity ${decl.arity}',
    );
  }

  // Step 1: Build I/O moded term (root mode = consume)
  final ioTerm = _buildIOModedTerm(head, decl, Mode.consume, typeEnv);

  // Step 2: Ensure each variable's form matches its position's mode
  return _ensureVariablesMatchModes(ioTerm);
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
  // Note: Do NOT reset counter here - body atoms share the same clause namespace
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
///
/// FIX: When expectedType is a wildcard (_ or _?), we don't recurse with
/// type-driven mode computation. Instead, all subterms inherit the wildcard's
/// mode uniformly. This implements Definition 5.5 (wildcard positions).
ModedTerm _buildModedSubterm(ast.Term term, Mode mode, TypeExpr? expectedType, TypeEnvironment? typeEnv) {
  // FIX: When type is wildcard (_ or _?) OR unknown (null), don't use type-driven mode computation.
  // Wildcards accept any term at this position - all subterms inherit the same mode.
  // Unknown types (null) occur when we can't resolve the type definition.
  // This handles Definition 5.5: type path ends at wildcard.
  if (expectedType == null || expectedType is PrimitiveModeAlt) {
    return _buildOpaqueModedTerm(term, mode);
  }

  if (term is ast.VarTerm) {
    // Variable: pass structural mode from context (per moded-term v0.6)
    return ModedVariable(term.name, isReader: term.isReader, structuralMode: mode);
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
    // Bug 4 fix: Each anonymous variable must be treated as a FRESH writer
    // Paper Remark 3.1: "Each occurrence denotes a fresh writer with no paired reader,
    // providing a controlled exception to the SRSW restriction."
    // Generate unique name to ensure each _ is independent
    final uniqueName = _freshAnonVarName();
    return ModedVariable(uniqueName, isReader: false, structuralMode: mode);
  }

  throw InvalidHeadError('Unknown term type: ${term.runtimeType}');
}

/// Get subterm modes for a structure by looking up type definition.
///
/// Uses mode involution: combinedMode = parentMode ⊕ embeddedMode
/// where ⊕ is XOR-like: same modes → produce, different → consume
///
/// For dual types (T?), uses algorithmic complementation:
/// modes are flipped and target types are dualized.
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
  bool isDual = false;
  if (expectedType is TypeRef) {
    typeName = expectedType.name;
    isDual = expectedType.isInput;
  }

  if (typeName == null) {
    return defaultModes;
  }

  // Get the base type definition
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
        // Combine with parent mode via involution
        final combinedMode = combineMode(parentMode, embeddedMode);
        // For dual types, flip the subterm type for DFA leaf checking
        final subtermType = isDual ? _dualType(argType) : argType;
        result.add((combinedMode, subtermType));
      }
      return result;
    }

    // Handle DiffList: \ operator
    if (alt is DiffListAlt && functor == r'\' && arity == 2) {
      final contentMode = _getEmbeddedMode(alt.content);
      final holeMode = _getEmbeddedMode(alt.hole);
      final contentCombined = combineMode(parentMode, contentMode);
      final holeCombined = combineMode(parentMode, holeMode);
      final contentType = isDual ? _dualType(alt.content) : alt.content;
      final holeType = isDual ? _dualType(alt.hole) : alt.hole;
      return [
        (contentCombined, contentType),
        (holeCombined, holeType),
      ];
    }
  }

  return defaultModes;
}

/// Get subterm modes for list [H|T] by looking up type definition.
///
/// Returns a tuple of (headModeInfo, tailModeInfo) where each is (Mode, TypeExpr?).
///
/// For dual types (T?), uses algorithmic complementation:
/// modes are flipped and target types are dualized.
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
  bool isDual = false;
  if (expectedType is TypeRef) {
    typeName = expectedType.name;
    isDual = expectedType.isInput;
  }

  if (typeName == null) {
    return defaultResult;
  }

  // Get the base type definition
  final typeDef = typeEnv.getType(typeName);
  if (typeDef == null) {
    return defaultResult;
  }

  // Find ListConsAlt in type definition
  for (final alt in typeDef.alternatives) {
    if (alt is ListConsAlt) {
      final headEmbeddedMode = _getEmbeddedMode(alt.head);
      final tailEmbeddedMode = _getEmbeddedMode(alt.tail);
      // Combine with parent mode via involution
      final headMode = combineMode(parentMode, headEmbeddedMode);
      final tailMode = combineMode(parentMode, tailEmbeddedMode);

      // For dual types, flip the subterm types for DFA leaf checking
      final headType = isDual ? _dualType(alt.head) : alt.head;
      final tailType = isDual ? _dualType(alt.tail) : alt.tail;

      return (
        (headMode, headType),
        (tailMode, tailType),
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

/// Get the dual of a type expression.
///
/// Flips the isInput flag for TypeRef and PrimitiveModeAlt.
/// Returns the expression unchanged for other types.
TypeExpr _dualType(TypeExpr expr) {
  if (expr is TypeRef) {
    // TypeRef has a dual() method
    return expr.dual();
  }
  if (expr is PrimitiveModeAlt) {
    return PrimitiveModeAlt(!expr.isInput, expr.line, expr.column);
  }
  return expr;
}

/// Build a moded term without type-driven mode computation.
///
/// Used when the expected type is a wildcard (_ or _?). The entire term
/// and all its subterms inherit the same mode from the wildcard position.
/// Variables are still collected so they can be type-checked for complementarity.
///
/// This implements Definition 5.5: when the type path ends at a wildcard,
/// the term at that position is accepted without examining its internal structure
/// against any type definition.
ModedTerm _buildOpaqueModedTerm(ast.Term term, Mode mode) {
  if (term is ast.VarTerm) {
    return ModedVariable(term.name, isReader: term.isReader, structuralMode: mode);
  }

  if (term is ast.ConstTerm) {
    return ModedConstant(mode, term.value ?? 'null');
  }

  if (term is ast.UnderscoreTerm) {
    // Each anonymous variable is a fresh writer (paper Remark 3.1)
    final uniqueName = _freshAnonVarName();
    return ModedVariable(uniqueName, isReader: false, structuralMode: mode);
  }

  if (term is ast.ListTerm) {
    if (term.isNil) {
      return ModedConstant.nil(mode);
    }
    // Build list structure - all children inherit the same mode
    final head = _buildOpaqueModedTerm(term.head!, mode);
    final tail = _buildOpaqueModedTerm(term.tail!, mode);
    return ModedCompound.listCons(mode, head, tail);
  }

  if (term is ast.StructTerm) {
    // Build structure - all children inherit the same mode (no type-driven computation)
    final modedArgs = term.args.map((arg) => _buildOpaqueModedTerm(arg, mode)).toList();
    return ModedCompound(mode, term.functor, term.arity, modedArgs);
  }

  throw InvalidHeadError('Unknown term type in opaque context: ${term.runtimeType}');
}

/// Complement all variables in the moded term.
///
/// Per Definition 5.5 step 2: Replace each variable with its paired variable.
/// This is unconditional - every variable is complemented.
///
/// This reflects the semantic inversion of head variables:
/// - Head writers become readers in the moded head (bound BY the goal)
/// - Head readers become writers in the moded head (will be bound BY the body)
ModedTerm _ensureVariablesMatchModes(ModedTerm term) {
  if (term is ModedCompound) {
    final adjustedArgs = term.args.map(_ensureVariablesMatchModes).toList();
    return ModedCompound(term.mode, term.functor, term.arity, adjustedArgs);
  }

  if (term is ModedConstant) {
    return term;
  }

  if (term is ModedVariable) {
    // Unconditional complementation: always flip the variable
    return ModedVariable(term.name, isReader: !term.isReader, structuralMode: term.mode);
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
