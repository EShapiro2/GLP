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
      // Check if this alternative is mode-distinguished
      // (has at least one TypeRef with explicit ? marker)
      final isModedAlt = alt.args.any((arg) => arg is TypeRef && arg.isInput);

      // Structure: add transitions for each argument position
      for (int i = 0; i < alt.args.length; i++) {
        final argType = alt.args[i];
        final argIndex = i + 1;

        // Determine mode encoding based on argument syntax
        final Mode? pathMode;
        if (argType is TypeRef && argType.isInput) {
          // Explicit ? marker → Mode.input
          pathMode = Mode.input;
        } else if (argType is TypeRef && !argType.isInput && isModedAlt) {
          // TypeRef without ? in a moded alternative → Mode.output
          pathMode = Mode.output;
        } else {
          // Unmoded TypeRef or PrimitiveModeAlt → no mode in PathElement
          pathMode = null;
        }

        final pathElem = PathElement.functor(alt.functor, alt.arity, argIndex, mode: pathMode);
        final targetState = _resolveTargetState(argType, stateMap, finalState);

        // If arg is PrimitiveModeAlt, mark target state with its mode
        if (argType is PrimitiveModeAlt) {
          final mode = argType.isInput ? Mode.input : Mode.output;
          primitiveStateModes[targetState] =
              (primitiveStateModes[targetState] ?? <Mode>{})..add(mode);
        }

        transitions[(fromState, pathElem)] = targetState;
      }
      
    } else if (alt is ListConsAlt) {
      // Check if this alternative is mode-distinguished
      // (has at least one TypeRef with explicit ? marker)
      final isModedAlt = (alt.head is TypeRef && (alt.head as TypeRef).isInput) ||
                          (alt.tail is TypeRef && (alt.tail as TypeRef).isInput);

      // List cons: add head and tail transitions
      // Head element
      final headType = alt.head;
      final Mode? headMode;
      if (headType is TypeRef && headType.isInput) {
        // Explicit ? marker → Mode.input
        headMode = Mode.input;
      } else if (headType is TypeRef && !headType.isInput && isModedAlt) {
        // TypeRef without ? in a moded alternative → Mode.output
        headMode = Mode.output;
      } else {
        // Unmoded TypeRef or PrimitiveModeAlt → no mode in PathElement
        headMode = null;
      }
      final headElem = PathElement.listHead(mode: headMode);
      final headTarget = _resolveTargetState(headType, stateMap, finalState);

      // If head is PrimitiveModeAlt, mark target state with its mode
      if (headType is PrimitiveModeAlt) {
        final mode = headType.isInput ? Mode.input : Mode.output;
        primitiveStateModes[headTarget] =
            (primitiveStateModes[headTarget] ?? <Mode>{})..add(mode);
      }

      transitions[(fromState, headElem)] = headTarget;

      // Tail element
      final tailType = alt.tail;
      final Mode? tailMode;
      if (tailType is TypeRef && tailType.isInput) {
        // Explicit ? marker → Mode.input
        tailMode = Mode.input;
      } else if (tailType is TypeRef && !tailType.isInput && isModedAlt) {
        // TypeRef without ? in a moded alternative → Mode.output
        tailMode = Mode.output;
      } else {
        // Unmoded TypeRef or PrimitiveModeAlt → no mode in PathElement
        tailMode = null;
      }
      final tailElem = PathElement.listTail(mode: tailMode);
      final tailTarget = _resolveTargetState(tailType, stateMap, finalState);

      // If tail is PrimitiveModeAlt, mark target state with its mode
      if (tailType is PrimitiveModeAlt) {
        final mode = tailType.isInput ? Mode.input : Mode.output;
        primitiveStateModes[tailTarget] =
            (primitiveStateModes[tailTarget] ?? <Mode>{})..add(mode);
      }

      transitions[(fromState, tailElem)] = tailTarget;
      
    } else if (alt is TypeRef) {
      // Type reference at top level (e.g., Any ::< Every, Stream ::< List)
      // This is a subtype declaration - inherit ALL structure from supertype
      // Subtype values are a subset of supertype values, so same constructors apply
      try {
        final referencedDFA = compile(alt.name);

        // Inherit primitive modes from referenced type's start state
        if (referencedDFA.primitiveStateModes.containsKey(referencedDFA.startState)) {
          final modes = referencedDFA.getModesAt(referencedDFA.startState);
          primitiveStateModes[fromState] =
              (primitiveStateModes[fromState] ?? <Mode>{})..addAll(modes);
        }

        // Inherit ALL transitions from referenced type's start state
        // This enables patterns valid for supertype to work for subtype
        for (final entry in referencedDFA.transitions.entries) {
          final (srcState, pathElem) = entry.key;
          final dstState = entry.value;

          if (srcState == referencedDFA.startState) {
            transitions[(fromState, pathElem)] = dstState;
          }
        }
      } catch (e) {
        // If compilation fails, skip inheritance
        // This can happen for forward references or undefined types
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
