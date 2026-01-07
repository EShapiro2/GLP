// lib/analysis/type_checker/guard_types.dart
//
// Guard type checking: extract type constraints from guards and handle
// ground guards that satisfy mode coverage requirements.

import 'type_ast.dart';
import 'type_dfa.dart';
import 'type_compiler.dart';
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
      argTypeNames: ['Output'],
      impliesGround: true,
    ),
    'ground': GuardSignature(
      argTypeNames: ['Output'],
      impliesGround: true,
      recursivelyGround: true,
    ),
    'known': GuardSignature(
      argTypeNames: ['Output'],
      impliesGround: false,  // known does NOT imply ground
    ),
    'unknown': GuardSignature(
      argTypeNames: ['Output'],
      impliesGround: false,
    ),
    'compound': GuardSignature(
      argTypeNames: ['Output'],
      impliesGround: false,
    ),
    'is_list': GuardSignature(
      argTypeNames: ['Output'],
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
      argTypeNames: ['Output', 'Output'],
      impliesGround: true,
    ),
  };

  /// Get signature for a built-in guard, or null if not built-in
  static GuardSignature? getSignature(String functor) => signatures[functor];
}

/// Extract type constraints from clause guards
/// Returns map from variable name to constraining type DFA
Map<String, TypeDFA> extractGuardConstraints(
  List<ast.Guard>? guards,
  TypeEnvironment typeEnv,
  TypeCompiler compiler,
) {
  final constraints = <String, TypeDFA>{};
  if (guards == null || guards.isEmpty) return constraints;

  for (final guard in guards) {
    _processGuard(guard.predicate, guard.args, constraints, typeEnv, compiler);
  }

  return constraints;
}

void _processGuard(
  String functor,
  List<ast.Term> args,
  Map<String, TypeDFA> constraints,
  TypeEnvironment typeEnv,
  TypeCompiler compiler,
) {

  // Check built-in guards
  final signature = GuardTypeRegistry.getSignature(functor);
  if (signature != null) {
    for (int i = 0; i < args.length && i < signature.argTypeNames.length; i++) {
      final arg = args[i];
      if (arg is ast.VarTerm) {
        final typeDFA = compiler.compile(signature.argTypeNames[i]);
        final varName = arg.name;

        if (constraints.containsKey(varName)) {
          // Intersect with existing constraint
          constraints[varName] = constraints[varName]!.intersect(typeDFA);
        } else {
          constraints[varName] = typeDFA;
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
        final typeRef = procDecl.argTypes[i];
        final typeDFA = compiler.compile(typeRef.name);
        final varName = arg.name;

        if (constraints.containsKey(varName)) {
          constraints[varName] = constraints[varName]!.intersect(typeDFA);
        } else {
          constraints[varName] = typeDFA;
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
