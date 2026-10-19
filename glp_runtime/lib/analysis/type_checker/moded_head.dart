// lib/analysis/type_checker/moded_head.dart
//
// Moded head construction for GLP type checking.
// Specification: TGLP (Moded-Types), sections/well-typing.tex Definition "Moded Head"
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
/// Returns names like "_#1", "_#2", etc.; a named anonymous variable gives its
/// name as [stem], "_R#3", so that a diagnostic names what was written.
String _freshAnonVarName([String stem = '_']) {
  _anonVarCounter++;
  return '$stem#$_anonVarCounter';
}

/// A clause variable as a moded variable.  A named anonymous variable, `_X` or
/// `_X?`, is an anonymous variable (TGLP "Anonymous variables": "any variable
/// whose name begins with `_` ... Each occurrence denotes a fresh writer with
/// no paired reader"), so it takes a fresh name at each occurrence, as `_`
/// does, and two occurrences of one name are two variables, as codegen
/// compiles them.  Until 2026-10-02 they were typed as one variable.
ModedVariable _modedVariable(ast.VarTerm term, Mode mode) => ModedVariable(
    term.name.startsWith('_') ? _freshAnonVarName(term.name) : term.name,
    isReader: term.isReader,
    structuralMode: mode);

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
///
/// A list is walked along its spine in a loop, each element built by a
/// recursive call, so the depth used is the nesting of the term's elements and
/// not the length of a list: TGLP well-typing.tex, Definition "Well-Typed
/// Clause", puts no bound on a term's size, and a clause whose argument was a
/// list of some 3100 elements overflowed the stack here.  The cells are built
/// in the order the recursion built them, each element before the rest of the
/// list, so the fresh names of anonymous variables are the same.
ModedTerm _buildModedSubterm(ast.Term term, Mode mode, TypeExpr? expectedType, TypeEnvironment? typeEnv) {
  // The cells of the list [term] begins, each with its mode and its element.
  final cells = <(Mode, ModedTerm)>[];
  while (expectedType != null &&
      expectedType is! PrimitiveModeAlt &&
      term is ast.ListTerm &&
      !term.isNil) {
    // Non-empty list [H|T] - get embedded modes for head and tail
    final listModes = _getListSubtermModes(mode, expectedType, typeEnv);
    final (headMode, headType) = listModes.$1;
    final (tailMode, tailType) = listModes.$2;
    cells.add(
        (mode, _buildModedSubterm(term.head!, headMode, headType, typeEnv)));
    term = term.tail!;
    mode = tailMode;
    expectedType = tailType;
  }
  var built = _buildModedNonCell(term, mode, expectedType, typeEnv);
  for (var i = cells.length - 1; i >= 0; i--) {
    final (cellMode, head) = cells[i];
    built = ModedCompound.listCons(cellMode, head, built);
  }
  return built;
}

/// [_buildModedSubterm] of a term that is not a list cell it walks: a
/// variable, a structure, the empty list, a constant, an anonymous variable,
/// or any term at a wildcard or unknown type.
ModedTerm _buildModedNonCell(ast.Term term, Mode mode, TypeExpr? expectedType, TypeEnvironment? typeEnv) {
  // FIX: When type is wildcard (_ or _?) OR unknown (null), don't use type-driven mode computation.
  // Wildcards accept any term at this position - all subterms inherit the same mode.
  // Unknown types (null) occur when we can't resolve the type definition.
  // This handles Definition 5.5: type path ends at wildcard.
  if (expectedType == null || expectedType is PrimitiveModeAlt) {
    return _buildOpaqueModedTerm(term, mode);
  }

  if (term is ast.VarTerm) {
    // Variable: pass structural mode from context (per moded-term v0.6)
    return _modedVariable(term, mode);
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
    // Empty list [] (a non-empty list is a cell [_buildModedSubterm] walks)
    return ModedConstant.nil(mode);
  }

  if (term is ast.ConstTerm) {
    // Constant
    return ModedConstant(mode, term.value ?? 'null');
  }

  if (term is ast.UnderscoreTerm) {
    // Each anonymous variable is fresh and independent, so it gets a unique
    // name (TGLP "Anonymous variables": "Each occurrence denotes a fresh
    // writer with no paired reader").  It keeps its source form: `_` is the
    // writer, and `_?`, the output placeholder a clause head's produced
    // position carries, is the reader that complementation (step 2) turns
    // into that fresh writer.
    final uniqueName = _freshAnonVarName();
    return ModedVariable(uniqueName,
        isReader: term.isReader, structuralMode: mode);
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

  // The matching structure alternative, among the type's own alternatives
  // and, through each alternative that is a type name S or S?, among S's
  // ([_alternativesOf]).
  for (final (alt, flip) in _alternativesOf(typeName, typeEnv)) {
    if (alt is StructAlt && alt.functor == functor && alt.arity == arity) {
      // Found matching constructor - compute combined modes for each arg
      final result = <(Mode, TypeExpr?)>[];
      for (final argType in alt.args) {
        var embeddedMode = _getEmbeddedMode(argType);
        if (flip) embeddedMode = embeddedMode.flip;
        // Combine with parent mode via involution
        final combinedMode = combineMode(parentMode, embeddedMode);
        // For dual types, flip the subterm type for DFA leaf checking
        final subtermType = (isDual != flip) ? _dualType(argType) : argType;
        result.add((combinedMode, subtermType));
      }
      return result;
    }

    // Handle DiffList: \ operator
    if (alt is DiffListAlt && functor == r'\' && arity == 2) {
      var contentMode = _getEmbeddedMode(alt.content);
      var holeMode = _getEmbeddedMode(alt.hole);
      if (flip) {
        contentMode = contentMode.flip;
        holeMode = holeMode.flip;
      }
      final contentCombined = combineMode(parentMode, contentMode);
      final holeCombined = combineMode(parentMode, holeMode);
      final contentType = (isDual != flip) ? _dualType(alt.content) : alt.content;
      final holeType = (isDual != flip) ? _dualType(alt.hole) : alt.hole;
      return [
        (contentCombined, contentType),
        (holeCombined, holeType),
      ];
    }
  }

  return defaultModes;
}

/// The alternatives of the type [typeName], each with whether it is reached
/// through an odd number of complemented type-name alternatives: its own
/// alternatives unflipped, and for each alternative that is a type name S or
/// S?, S's alternatives in their turn, flipped once more where it is S?.
/// TGLP appendix-type-automaton.tex, "Type definition transitions": "If A_j
/// references another type S or S?: transitions are inherited from S or S?
/// respectively"; typed-glp.tex: "An alternative in a type definition may also
/// be a type name S, providing type union for types with disjoint top-level
/// functors".  The automaton is built so (program_dfa.dart,
/// _addTypeTransitions); the moded term of a head or a body atom must be read
/// by the same alternatives, or a term of a type a union names is given its
/// parent's mode at every argument, and a mode-inverted argument of it
/// disagrees with the automaton's transition.  A definition cycle is cut.
List<(TypeExpr, bool)> _alternativesOf(String typeName, TypeEnvironment typeEnv) {
  final out = <(TypeExpr, bool)>[];
  void walk(String name, bool flip, Set<String> seen) {
    if (!seen.add(name)) return;
    final def = typeEnv.getType(name);
    if (def == null) return;
    for (final alt in def.alternatives) {
      if (alt is TypeRef && !alt.isParameterized &&
          !TypeRef.builtins.contains(alt.name)) {
        walk(alt.name, flip != alt.isInput, {...seen});
        continue;
      }
      out.add((alt, flip));
    }
  }
  walk(typeName, false, <String>{});
  return out;
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

  // Find ListConsAlt among the type's alternatives, through each that is a
  // type name ([_alternativesOf]).
  for (final (alt, flip) in _alternativesOf(typeName, typeEnv)) {
    if (alt is ListConsAlt) {
      var headEmbeddedMode = _getEmbeddedMode(alt.head);
      var tailEmbeddedMode = _getEmbeddedMode(alt.tail);
      if (flip) {
        headEmbeddedMode = headEmbeddedMode.flip;
        tailEmbeddedMode = tailEmbeddedMode.flip;
      }
      // Combine with parent mode via involution
      final headMode = combineMode(parentMode, headEmbeddedMode);
      final tailMode = combineMode(parentMode, tailEmbeddedMode);

      // For dual types, flip the subterm types for DFA leaf checking
      final headType = (isDual != flip) ? _dualType(alt.head) : alt.head;
      final tailType = (isDual != flip) ? _dualType(alt.tail) : alt.tail;

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
///
/// A list is walked along its spine in a loop, each element by a recursive
/// call, in the order the recursion took them ([_buildModedSubterm]).
ModedTerm _buildOpaqueModedTerm(ast.Term term, Mode mode) {
  final heads = <ModedTerm>[];
  while (term is ast.ListTerm && !term.isNil) {
    // Build list structure - all children inherit the same mode
    heads.add(_buildOpaqueModedTerm(term.head!, mode));
    term = term.tail!;
  }
  var built = _buildOpaqueNonCell(term, mode);
  for (var i = heads.length - 1; i >= 0; i--) {
    built = ModedCompound.listCons(mode, heads[i], built);
  }
  return built;
}

/// [_buildOpaqueModedTerm] of a term that is not a list cell.
ModedTerm _buildOpaqueNonCell(ast.Term term, Mode mode) {
  if (term is ast.VarTerm) {
    return _modedVariable(term, mode);
  }

  if (term is ast.ConstTerm) {
    return ModedConstant(mode, term.value ?? 'null');
  }

  if (term is ast.UnderscoreTerm) {
    // Each anonymous variable is fresh, and keeps its source form: `_` the
    // writer, `_?` the output placeholder (TGLP "Anonymous variables").
    final uniqueName = _freshAnonVarName();
    return ModedVariable(uniqueName,
        isReader: term.isReader, structuralMode: mode);
  }

  if (term is ast.ListTerm) {
    // The empty list (a non-empty one is a cell [_buildOpaqueModedTerm] walks)
    return ModedConstant.nil(mode);
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
///
/// A list is walked along its spine in a loop and its elements by recursive
/// calls ([_buildModedSubterm]).
ModedTerm _ensureVariablesMatchModes(ModedTerm term) {
  final cells = <ModedCompound>[];
  final heads = <ModedTerm>[];
  while (term is ModedCompound && term.isListCons) {
    cells.add(term);
    heads.add(_ensureVariablesMatchModes(term.args[0]));
    term = term.args[1];
  }
  var adjusted = _ensureNonCellMatchesModes(term);
  for (var i = cells.length - 1; i >= 0; i--) {
    final cell = cells[i];
    adjusted = ModedCompound(
        cell.mode, cell.functor, cell.arity, [heads[i], adjusted]);
  }
  return adjusted;
}

/// [_ensureVariablesMatchModes] of a term that is not a list cell.
ModedTerm _ensureNonCellMatchesModes(ModedTerm term) {
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
