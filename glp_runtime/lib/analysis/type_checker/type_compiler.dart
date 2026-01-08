// lib/analysis/type_checker/type_compiler.dart
//
// Compiler from type definitions to DFAs.
// Implements Theorem 2.4: RUL programs ↔ Regular types ↔ DFAs
//
// Each type definition compiles to a DFA where:
// - States correspond to type names
// - Transitions correspond to functor/arg-position pairs
// - Final states mark complete terms

import 'mode.dart';
import 'type_ast.dart';
import 'type_dfa.dart';

/// Compiles TypeDef AST to TypeDFA
class TypeCompiler {
  final TypeEnvironment env;
  final Map<String, TypeDFA> _cache = {};
  
  TypeCompiler(this.env);
  
  /// Compile a type by name to DFA
  TypeDFA compile(String typeName) {
    // Check cache
    if (_cache.containsKey(typeName)) {
      return _cache[typeName]!;
    }
    
    // Handle built-in types
    if (typeName == 'Number') {
      final dfa = NumberTypeDFA();
      _cache[typeName] = dfa;
      return dfa;
    }
    
    if (typeName == 'String') {
      final dfa = StringTypeDFA();
      _cache[typeName] = dfa;
      return dfa;
    }
    
    // Look up type definition
    final typeDef = env.getType(typeName);
    if (typeDef == null) {
      throw TypeCompileError('Undefined type: $typeName');
    }
    
    // Compile the type definition
    final dfa = _compileTypeDef(typeDef);
    _cache[typeName] = dfa;
    return dfa;
  }
  
  /// Compile a type definition to DFA
  /// 
  /// Following the construction in Theorem 2.4:
  /// For a RUL program P defining types, we construct DFA M where:
  /// - States Q = type names + start + final
  /// - Transitions from structure of type alternatives
  TypeDFA _compileTypeDef(TypeDef typeDef) {
    // Collect all reachable type names (states)
    final reachableTypes = _collectReachableTypes(typeDef.name);
    
    // Build states
    final states = <DFAState>{};
    final stateMap = <String, DFAState>{};
    
    for (final typeName in reachableTypes) {
      final state = DFAState(typeName);
      states.add(state);
      stateMap[typeName] = state;
    }
    
    // Add final state for complete terms
    final finalState = DFAState('_FINAL_', isFinal: true);
    states.add(finalState);
    stateMap['_FINAL_'] = finalState;
    
    // Build transitions and track primitive state modes
    final transitions = <(DFAState, PathElement), DFAState>{};
    final primitiveStateModes = <DFAState, Set<Mode>>{};

    for (final typeName in reachableTypes) {
      final def = env.getType(typeName);
      if (def == null) continue;  // Built-in types handled separately

      final state = stateMap[typeName]!;

      for (final alt in def.alternatives) {
        _addTransitionsForAlt(state, alt, stateMap, transitions, finalState, primitiveStateModes);
      }
    }

    return TypeDFA(
      states: states,
      startState: stateMap[typeDef.name]!,
      finalStates: {finalState},
      transitions: transitions,
      primitiveStateModes: primitiveStateModes,
    );
  }
  
  /// Collect all type names reachable from a starting type
  Set<String> _collectReachableTypes(String startType) {
    final visited = <String>{};
    final queue = <String>[startType];
    
    while (queue.isNotEmpty) {
      final current = queue.removeLast();
      if (visited.contains(current)) continue;
      if (TypeRef.builtins.contains(current)) continue;  // Skip built-ins
      
      visited.add(current);
      
      final def = env.getType(current);
      if (def == null) continue;
      
      for (final alt in def.alternatives) {
        _collectTypesFromAlt(alt, queue);
      }
    }
    
    return visited;
  }
  
  /// Extract type references from a type alternative
  void _collectTypesFromAlt(TypeExpr alt, List<String> queue) {
    if (alt is TypeRef) {
      queue.add(alt.name);
    } else if (alt is StructAlt) {
      for (final arg in alt.args) {
        _collectTypesFromAlt(arg, queue);
      }
    } else if (alt is ListConsAlt) {
      _collectTypesFromAlt(alt.head, queue);
      _collectTypesFromAlt(alt.tail, queue);
    }
    // ConstantAlt, ListNilAlt, and PrimitiveModeAlt have no type references
  }
  
  /// Add DFA transitions for a type alternative
  void _addTransitionsForAlt(
    DFAState fromState,
    TypeExpr alt,
    Map<String, DFAState> stateMap,
    Map<(DFAState, PathElement), DFAState> transitions,
    DFAState finalState,
    Map<DFAState, Set<Mode>> primitiveStateModes,
  ) {
    if (alt is PrimitiveModeAlt) {
      // Primitive mode: mark state with its mode, make it accepting
      final mode = alt.isInput ? Mode.input : Mode.output;
      primitiveStateModes[fromState] =
          (primitiveStateModes[fromState] ?? <Mode>{})..add(mode);
      // Primitive states are final (accepting)
      // Note: finalState set management happens in caller
      return;

    } else if (alt is ConstantAlt) {
      // Constant: transition directly to final state
      final pathElem = PathElement.constant(alt.value);
      transitions[(fromState, pathElem)] = finalState;
      
    } else if (alt is ListNilAlt) {
      // Empty list: transition to final state
      final pathElem = PathElement.nil();
      transitions[(fromState, pathElem)] = finalState;
      
    } else if (alt is StructAlt) {
      // Structure: add transitions for each argument position
      for (int i = 0; i < alt.args.length; i++) {
        final pathElem = PathElement.functor(alt.functor, alt.arity, i + 1);
        final argType = alt.args[i];
        final targetState = _resolveTargetState(argType, stateMap, finalState);
        transitions[(fromState, pathElem)] = targetState;
      }
      
    } else if (alt is ListConsAlt) {
      // List cons: add head and tail transitions
      final headElem = PathElement.listHead();
      final tailElem = PathElement.listTail();

      final headTarget = _resolveTargetState(alt.head, stateMap, finalState);
      final tailTarget = _resolveTargetState(alt.tail, stateMap, finalState);

      transitions[(fromState, headElem)] = headTarget;
      transitions[(fromState, tailElem)] = tailTarget;

    } else if (alt is TypeRef) {
      // Type alias: MyStream ::= MyList
      // Add all transitions from the referenced type's alternatives
      final refDef = env.getType(alt.name);
      if (refDef != null) {
        for (final refAlt in refDef.alternatives) {
          _addTransitionsForAlt(fromState, refAlt, stateMap, transitions,
              finalState, primitiveStateModes);
        }
      }
    }
  }
  
  /// Resolve target state for a type expression in an argument position
  DFAState _resolveTargetState(
    TypeExpr expr,
    Map<String, DFAState> stateMap,
    DFAState finalState,
  ) {
    if (expr is TypeRef) {
      // Built-in types need special handling
      if (expr.name == 'Number' || expr.name == 'String') {
        // For built-ins in argument positions, we create a special state
        // that accepts the appropriate values
        return stateMap[expr.name] ?? _createBuiltinState(expr.name, stateMap);
      }
      return stateMap[expr.name] ?? finalState;
    }
    // For inline type expressions, we'd need to create intermediate states
    // For now, assume all type arguments are references
    return finalState;
  }
  
  DFAState _createBuiltinState(String name, Map<String, DFAState> stateMap) {
    final state = DFAState('_builtin_$name');
    stateMap[name] = state;
    return state;
  }
}

/// Extract all paths from a GLP term
/// This operates on the runtime term representation
Set<TermPath> extractPaths(dynamic term) {
  final paths = <TermPath>{};
  _extractPathsHelper(term, TermPath.empty(), paths);
  return paths;
}

void _extractPathsHelper(dynamic term, TermPath currentPath, Set<TermPath> paths) {
  if (term == null) {
    // Variable or unbound - no ground path
    return;
  }
  
  if (term is num) {
    // Number constant
    paths.add(currentPath.append(PathElement.constant(term)));
    return;
  }
  
  if (term is String) {
    // Atom or string constant
    paths.add(currentPath.append(PathElement.constant(term)));
    return;
  }
  
  if (term is List) {
    if (term.isEmpty) {
      // Empty list
      paths.add(currentPath.append(PathElement.nil()));
    } else {
      // Non-empty list [H|T]
      _extractPathsHelper(term.first, currentPath.append(PathElement.listHead()), paths);
      _extractPathsHelper(term.sublist(1), currentPath.append(PathElement.listTail()), paths);
    }
    return;
  }
  
  // For structures, we need the GLP term representation
  // This is a placeholder - actual implementation depends on runtime term format
  if (term is Map && term.containsKey('functor') && term.containsKey('args')) {
    final functor = term['functor'] as String;
    final args = term['args'] as List;
    
    for (int i = 0; i < args.length; i++) {
      final pathElem = PathElement.functor(functor, args.length, i + 1);
      _extractPathsHelper(args[i], currentPath.append(pathElem), paths);
    }
    return;
  }
}

/// Error during type compilation
class TypeCompileError implements Exception {
  final String message;
  
  TypeCompileError(this.message);
  
  @override
  String toString() => 'Type compile error: $message';
}
