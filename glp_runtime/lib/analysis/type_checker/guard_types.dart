// lib/analysis/type_checker/guard_types.dart
//
// Guard type checking: extract type constraints from guards and handle
// ground guards that satisfy mode coverage requirements.

import 'type_ast.dart';
import 'program_dfa.dart';
import '../../compiler/ast.dart' as ast;

/// Signature of a built-in guard for type checking purposes
class GuardSignature {
  /// Expected types of arguments
  final List<String> argTypeNames;

  /// If true, guard success implies arguments are ground
  final bool impliesGround;

  /// If true, guard success implies arguments are recursively ground
  /// (no nested unbound variables - enables multiple reader occurrences)
  final bool recursivelyGround;

  const GuardSignature({
    required this.argTypeNames,
    this.impliesGround = false,
    this.recursivelyGround = false,
  });
}

/// Registry of built-in guard type signatures
class GuardTypeRegistry {
  static const Map<String, GuardSignature> signatures = {
    // Type guards
    'number': GuardSignature(
      argTypeNames: ['Number'],
      impliesGround: true,
    ),
    'integer': GuardSignature(
      argTypeNames: ['Number'],
      impliesGround: true,
    ),
    'float': GuardSignature(
      argTypeNames: ['Number'],
      impliesGround: true,
    ),
    'string': GuardSignature(
      argTypeNames: ['String'],
      impliesGround: true,
    ),
    'atom': GuardSignature(
      argTypeNames: ['Any'],
      impliesGround: true,
    ),
    'ground': GuardSignature(
      argTypeNames: ['Any'],
      impliesGround: true,
      recursivelyGround: true,
    ),
    'known': GuardSignature(
      argTypeNames: ['Any'],
      impliesGround: false,  // known does NOT imply ground
    ),
    'unknown': GuardSignature(
      argTypeNames: ['Any'],
      impliesGround: false,
    ),
    'compound': GuardSignature(
      argTypeNames: ['Any'],
      impliesGround: false,
    ),
    'is_list': GuardSignature(
      argTypeNames: ['Any'],
      impliesGround: false,
    ),

    // Arithmetic comparisons - all imply ground
    '<': GuardSignature(
      argTypeNames: ['Number', 'Number'],
      impliesGround: true,
    ),
    '>': GuardSignature(
      argTypeNames: ['Number', 'Number'],
      impliesGround: true,
    ),
    '=<': GuardSignature(
      argTypeNames: ['Number', 'Number'],
      impliesGround: true,
    ),
    '>=': GuardSignature(
      argTypeNames: ['Number', 'Number'],
      impliesGround: true,
    ),
    '=:=': GuardSignature(
      argTypeNames: ['Number', 'Number'],
      impliesGround: true,
    ),
    '=\\=': GuardSignature(
      argTypeNames: ['Number', 'Number'],
      impliesGround: true,
    ),

    // Ground equality - implies ground
    '=?=': GuardSignature(
      argTypeNames: ['Any', 'Any'],
      impliesGround: true,
    ),
  };

  /// Get signature for a built-in guard, or null if not built-in
  static GuardSignature? getSignature(String functor) => signatures[functor];
}

/// Extract type constraints from clause guards
/// Returns map from variable name to constraining type state
Map<String, DFAState> extractGuardConstraints(
  List<ast.Guard>? guards,
  TypeEnvironment typeEnv,
  ProgramDFA dfa,
) {
  final constraints = <String, DFAState>{};
  if (guards == null || guards.isEmpty) return constraints;

  for (final guard in guards) {
    _processGuard(guard.predicate, guard.args, constraints, typeEnv, dfa);
  }

  return constraints;
}

void _processGuard(
  String functor,
  List<ast.Term> args,
  Map<String, DFAState> constraints,
  TypeEnvironment typeEnv,
  ProgramDFA dfa,
) {

  // Check built-in guards
  final signature = GuardTypeRegistry.getSignature(functor);
  if (signature != null) {
    for (int i = 0; i < args.length && i < signature.argTypeNames.length; i++) {
      final arg = args[i];
      if (arg is ast.VarTerm) {
        final typeName = signature.argTypeNames[i];
        final typeState = dfa.states[typeName];
        if (typeState == null) continue;
        final varName = arg.name;

        // Just keep the first constraint (full DFA intersection is complex)
        if (!constraints.containsKey(varName)) {
          constraints[varName] = typeState;
        }
      }
    }
    return;
  }

  // Check defined guards (user procedures used as guards)
  final procDecl = typeEnv.getProcedure(functor, args.length);
  if (procDecl != null) {
    for (int i = 0; i < args.length && i < procDecl.argTypes.length; i++) {
      final arg = args[i];
      if (arg is ast.VarTerm) {
        final argType = procDecl.argTypes[i];

        // Skip primitives - they don't add type constraints
        if (argType is PrimitiveModeAlt) continue;

        final typeRef = argType as TypeRef;
        final typeState = dfa.states[typeRef.name];
        if (typeState == null) continue;
        final varName = arg.name;

        // Just keep the first constraint
        if (!constraints.containsKey(varName)) {
          constraints[varName] = typeState;
        }
      }
    }
  }
}

/// Get variables that are recursively ground due to guards
/// These variables satisfy all mode coverage requirements
Set<String> getRecursivelyGroundVars(List<ast.Guard>? guards) {
  if (guards == null || guards.isEmpty) return {};

  final result = <String>{};

  for (final guard in guards) {
    final signature = GuardTypeRegistry.getSignature(guard.predicate);
    if (signature == null) continue;

    // Check if this guard implies groundness
    if (signature.recursivelyGround || signature.impliesGround) {
      for (final arg in guard.args) {
        if (arg is ast.VarTerm) {
          result.add(arg.name);
        }
      }
    }
  }

  return result;
}

/// Check if a guard implies its arguments are ground
bool guardImpliesGround(String functor) {
  final signature = GuardTypeRegistry.getSignature(functor);
  return signature?.impliesGround ?? false;
}
