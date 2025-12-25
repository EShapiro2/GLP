// lib/analysis/type_checker/nfa_compiler.dart
//
// Compiles type definitions to NFAs.
// Each type alternative contributes transitions; union creates nondeterminism.

import 'mode.dart';
import 'moded_label.dart';
import 'type_nfa.dart';
import 'type_ast.dart';

/// Compiles type definitions to NFA
class TypeNFACompiler {
  final TypeEnvironment env;
  final Map<String, NFAState> _typeStates = {};
  final Set<NFAState> _allStates = {};
  final Map<(NFAState, ModedLabel), Set<NFAState>> _transitions = {};
  final Map<NFAState, Set<Mode>> _primitiveStateModes = {};
  final Map<NFAState, Set<NFAState>> _epsilonTransitions = {};
  final Set<NFAState> _finalStates = {};
  final Set<String> _compiledTypes = {};
  int _stateCounter = 0;

  TypeNFACompiler(this.env);

  /// Get or create state for a type name
  NFAState _getTypeState(String typeName) {
    return _typeStates.putIfAbsent(typeName, () {
      final state = NFAState(typeName);
      _allStates.add(state);
      return state;
    });
  }

  /// Create a unique anonymous state
  NFAState _createState(String prefix) {
    final state = NFAState('${prefix}_${_stateCounter++}');
    _allStates.add(state);
    return state;
  }

  /// Add a transition (nondeterministic: may add to existing set)
  void _addTransition(NFAState from, ModedLabel label, NFAState to) {
    final key = (from, label);
    _transitions[key] = (_transitions[key] ?? <NFAState>{})..add(to);
  }

  /// Add an ε-transition
  void _addEpsilonTransition(NFAState from, NFAState to) {
    _epsilonTransitions[from] = (_epsilonTransitions[from] ?? <NFAState>{})..add(to);
  }

  /// Compile a type by name to NFA
  TypeNFA compile(String typeName) {
    // Reset state for fresh compilation
    _typeStates.clear();
    _allStates.clear();
    _transitions.clear();
    _primitiveStateModes.clear();
    _epsilonTransitions.clear();
    _finalStates.clear();
    _compiledTypes.clear();
    _stateCounter = 0;

    // Handle built-in types specially
    if (typeName == 'Number' || typeName == 'String') {
      return _compileBuiltinType(typeName);
    }

    final typeDef = env.getType(typeName);
    if (typeDef == null) {
      throw TypeCompileError('Undefined type: $typeName');
    }

    final startState = _getTypeState(typeName);
    _compileTypeDef(typeName, typeDef);

    return TypeNFA(
      states: Set.from(_allStates),
      startState: startState,
      finalStates: Set.from(_finalStates),
      transitions: Map.from(_transitions),
      primitiveStateModes: Map.from(_primitiveStateModes),
      epsilonTransitions: Map.from(_epsilonTransitions),
    );
  }

  /// Compile an arbitrary type expression to NFA
  /// Used for clause contribution where patterns are converted to type expressions
  TypeNFA compileExpr(TypeExpr expr) {
    // Reset state for fresh compilation
    _typeStates.clear();
    _allStates.clear();
    _transitions.clear();
    _primitiveStateModes.clear();
    _epsilonTransitions.clear();
    _finalStates.clear();
    _compiledTypes.clear();
    _stateCounter = 0;

    final startState = _createState('start');
    _compileAlternative(startState, expr);

    return TypeNFA(
      states: Set.from(_allStates),
      startState: startState,
      finalStates: Set.from(_finalStates),
      transitions: Map.from(_transitions),
      primitiveStateModes: Map.from(_primitiveStateModes),
      epsilonTransitions: Map.from(_epsilonTransitions),
    );
  }

  /// Compile a type definition (recursively handles referenced types)
  void _compileTypeDef(String typeName, TypeDef typeDef) {
    if (_compiledTypes.contains(typeName)) return;
    _compiledTypes.add(typeName);

    final fromState = _getTypeState(typeName);

    // Each alternative contributes transitions from this state
    for (final alt in typeDef.alternatives) {
      _compileAlternative(fromState, alt);
    }
  }

  /// Compile a type alternative
  void _compileAlternative(NFAState fromState, TypeExpr alt) {
    if (alt is PrimitiveModeAlt) {
      // Primitive: mark state with mode
      final mode = alt.isInput ? Mode.input : Mode.output;
      _primitiveStateModes[fromState] =
          (_primitiveStateModes[fromState] ?? <Mode>{})..add(mode);
      return;
    }

    if (alt is ConstantAlt) {
      // Constant: transition to final state
      final label = ModedLabel.constant(alt.value);
      final target = _createState('final_${alt.value}');
      _finalStates.add(target);
      _addTransition(fromState, label, target);
      return;
    }

    if (alt is ListNilAlt) {
      // Empty list: transition to final state
      final label = ModedLabel.nil();
      final target = _createState('final_nil');
      _finalStates.add(target);
      _addTransition(fromState, label, target);
      return;
    }

    if (alt is StructAlt) {
      _compileStructAlt(fromState, alt);
      return;
    }

    if (alt is ListConsAlt) {
      _compileListConsAlt(fromState, alt);
      return;
    }

    if (alt is DiffListAlt) {
      _compileDiffListAlt(fromState, alt);
      return;
    }

    if (alt is TypeRef) {
      // Type reference: add ε-transition to referenced type's state
      final targetState = _getTypeState(alt.name);
      _addEpsilonTransition(fromState, targetState);

      // Recursively compile the referenced type
      if (!TypeRef.builtins.contains(alt.name)) {
        final refDef = env.getType(alt.name);
        if (refDef != null) {
          _compileTypeDef(alt.name, refDef);
        }
      }
      return;
    }
  }

  /// Compile a structure alternative
  void _compileStructAlt(NFAState fromState, StructAlt alt) {
    for (int i = 0; i < alt.args.length; i++) {
      final argType = alt.args[i];
      final argIndex = i + 1;

      // Determine mode for this position based on argument syntax
      final Mode? labelMode;
      if (argType is TypeRef) {
        // TypeRef: encode mode in the label
        labelMode = argType.isInput ? Mode.input : Mode.output;
      } else {
        // PrimitiveModeAlt or other: mode goes in primitiveStateModes
        labelMode = null;
      }

      final label = ModedLabel.functor(alt.functor, alt.arity, argIndex, mode: labelMode);
      final targetState = _resolveTargetState(argType);
      _addTransition(fromState, label, targetState);
    }
  }

  /// Compile a list cons alternative
  void _compileListConsAlt(NFAState fromState, ListConsAlt alt) {
    // Head
    final headType = alt.head;
    final Mode? headMode = (headType is TypeRef)
        ? (headType.isInput ? Mode.input : Mode.output)
        : null;
    final headLabel = ModedLabel.listHead(mode: headMode);
    final headTarget = _resolveTargetState(headType);
    _addTransition(fromState, headLabel, headTarget);

    // Tail
    final tailType = alt.tail;
    final Mode? tailMode = (tailType is TypeRef)
        ? (tailType.isInput ? Mode.input : Mode.output)
        : null;
    final tailLabel = ModedLabel.listTail(mode: tailMode);
    final tailTarget = _resolveTargetState(tailType);
    _addTransition(fromState, tailLabel, tailTarget);
  }

  /// Compile a difference list alternative
  void _compileDiffListAlt(NFAState fromState, DiffListAlt alt) {
    // Content (position 1)
    final contentType = alt.content;
    final Mode? contentMode = (contentType is TypeRef)
        ? (contentType.isInput ? Mode.input : Mode.output)
        : null;
    final contentLabel = ModedLabel.functor('\\', 2, 1, mode: contentMode);
    final contentTarget = _resolveTargetState(contentType);
    _addTransition(fromState, contentLabel, contentTarget);

    // Hole (position 2)
    final holeType = alt.hole;
    final Mode? holeMode = (holeType is TypeRef)
        ? (holeType.isInput ? Mode.input : Mode.output)
        : null;
    final holeLabel = ModedLabel.functor('\\', 2, 2, mode: holeMode);
    final holeTarget = _resolveTargetState(holeType);
    _addTransition(fromState, holeLabel, holeTarget);
  }

  /// Resolve target state for a type expression
  NFAState _resolveTargetState(TypeExpr expr) {
    if (expr is TypeRef) {
      final state = _getTypeState(expr.name);
      // Recursively compile if not built-in
      if (!TypeRef.builtins.contains(expr.name)) {
        final refDef = env.getType(expr.name);
        if (refDef != null) {
          _compileTypeDef(expr.name, refDef);
        }
      }
      return state;
    }
    if (expr is PrimitiveModeAlt) {
      // Create a unique state for this primitive position
      final state = _createState('prim');
      final mode = expr.isInput ? Mode.input : Mode.output;
      _primitiveStateModes[state] = {mode};
      return state;
    }
    // Nested structures would need more handling
    throw TypeCompileError('Unsupported nested type expression: $expr');
  }

  /// Compile a built-in type (Number, String)
  TypeNFA _compileBuiltinType(String typeName) {
    final state = NFAState(typeName);
    _allStates.add(state);
    // Built-in types are implicitly bi-moded (accept any mode)
    _primitiveStateModes[state] = {Mode.output, Mode.input};

    return TypeNFA(
      states: {state},
      startState: state,
      finalStates: {},
      transitions: {},
      primitiveStateModes: {state: {Mode.output, Mode.input}},
      epsilonTransitions: {},
    );
  }
}

/// Error during NFA compilation
class TypeCompileError implements Exception {
  final String message;

  TypeCompileError(this.message);

  @override
  String toString() => 'Type compile error: $message';
}
