// lib/analysis/type_checker/type_identity.dart
//
// Type identity for dynamic activation (TGLP, modules.tex §Dynamic Activation;
// plan step 1 of /Grassroots/docs/typed-dynamic-activation-plan.md).
//
// The type identity of a procedure declaration is the hash of its full type
// automaton in canonical minimised form, with every type the signature
// references expanded — not of the declaration's text.  By structural type
// compatibility (modules.tex §Design) two declarations are compatible exactly
// when their automata are equal, hence exactly when their identities are equal.
//
// The construction has four steps, in order:
//
//   1. Expansion.  From the declaration's state, follow the transition function
//      of appendix-type-automaton.tex Definition "Transition Function" to the
//      sub-automaton reachable from it, inlining a bare type-name alternative's
//      transitions ("transitions are inherited from S or S?") and expanding
//      every referenced type.  The result is a partial DFA over transition
//      labels (symbol, arity, argument position, mode).
//   2. Minimisation.  Moore partition refinement over that DFA.  The minimal
//      DFA of a regular language is unique up to isomorphism, so equal path
//      sets give isomorphic minimised automata.
//   3. Canonical serialisation.  A breadth-first walk from the start state,
//      taking each state's outgoing transitions in sorted label order and
//      numbering states in visit order.  Isomorphic minimised automata give
//      byte-identical prints.
//   4. Hash.  `hashOfPrint` — the same SHA-256-of-canonical-print construction
//      that builds the module source identity h(M) in `wire/flattening.dart`.
//      One construction serves both, which is what "the type identity refines
//      the source hash" (modules.tex §Dynamic Activation) means in code.
//
// Two implementation decisions this file makes, for TGLP's Implementation Notes
// appendix:
//
//   - The procedure name is NOT part of the automaton.  The appendix writes the
//     procedure transition as δ(p/n, (p, n, i, m_i)); this construction writes
//     the fixed symbol `#proc` in the functor slot and keeps the arity, the
//     argument position and the mode.  Static linking renames every procedure
//     to `M:p` (modules.tex §Static Linking), so a name-carrying identity could
//     never match between a goal's author and a shipped module; and the name is
//     already fixed on both sides by the table key `p/n`.  Abstracting it is
//     what structural compatibility asks for: two declarations with the same
//     argument types are compatible whatever they are called.
//   - A parameterised declaration has no type identity.  It is a template, not
//     a type (typed-glp-manual §17.7: it is checked once per instantiation);
//     `find_type` on one is an error.  Such keys are collected in
//     [TypeIdentityTables.parametric] and appear in neither table.

library;

import 'dart:typed_data';

import 'mode.dart';
import 'param_expansion.dart';
import 'program_dfa.dart' show UnknownTypeError;
import 'type_ast.dart';
import 'type_environment_builder.dart';
import '../../compiler/ast.dart' as ast;
import '../../wire/flattening.dart' show hashOfPrint;

/// Version tag of the canonical print, its first line.  A change to the
/// construction changes this tag, so an identity computed by one version is
/// never mistaken for one computed by another.
const String typeAutomatonPrintVersion = 'type-automaton/1';

// =============================================================================
// States
// =============================================================================

enum _Kind {
  /// The declaration's own state, `p/n`.
  proc,

  /// A defined type, `T` or `T?`.
  type,

  /// A primitive type: Integer, Real, String, Module (and their duals).
  prim,

  /// `_` (produced) or `_?` (consumed) — final, accepts any term at its mode.
  wildcard,

  /// The final state ✓, reached by a constant transition.
  check,
}

class _State {
  final _Kind kind;
  final String name; // type or primitive name; '' for wildcard and check
  final bool dual;

  const _State(this.kind, this.name, this.dual);

  static const check = _State(_Kind.check, '', false);
  static const proc = _State(_Kind.proc, '', false);
  static _State wildcard(bool dual) => _State(_Kind.wildcard, '', dual);
  static _State type(String name, bool dual) => _State(_Kind.type, name, dual);
  static _State prim(String name, bool dual) => _State(_Kind.prim, name, dual);

  @override
  bool operator ==(Object other) =>
      other is _State &&
      other.kind == kind &&
      other.name == name &&
      other.dual == dual;

  @override
  int get hashCode => Object.hash(kind, name, dual);

  @override
  String toString() => '${kind.name}:$name${dual ? '?' : ''}';
}

/// The partition class a state starts in.  Only the three final kinds are
/// distinguished up front; everything else is separated by its transitions.
/// `_` and `_?` are separate classes: one accepts any produced term, the other
/// any consumed term, and neither is the other.
int _initialClass(_State s) {
  switch (s.kind) {
    case _Kind.check:
      return 0;
    case _Kind.wildcard:
      return s.dual ? 1 : 2;
    case _Kind.proc:
    case _Kind.type:
    case _Kind.prim:
      return 3;
  }
}

/// The suffix the canonical print writes for a state's kind; empty for an
/// ordinary (non-final) state.
String _kindSuffix(_State s) {
  switch (s.kind) {
    case _Kind.check:
      return ' check';
    case _Kind.wildcard:
      return s.dual ? ' any:down' : ' any:up';
    case _Kind.proc:
    case _Kind.type:
    case _Kind.prim:
      return '';
  }
}

// =============================================================================
// Transition labels
// =============================================================================

String _modeTag(Mode m) => m == Mode.produce ? 'up' : 'down';

/// A functor or constant transition label: `"f"/n@i:mode`.  The symbol is
/// quoted and escaped, so no source symbol can be confused with the format's
/// own punctuation or with an unquoted reserved label.
String _label(String symbol, int arity, int argIndex, Mode mode) =>
    '${_quote(symbol)}/$arity@$argIndex:${_modeTag(mode)}';

/// A primitive-literal transition label: `#integer/0@0:mode`.  The appendix
/// gives a primitive type one transition per literal (δ(Integer, (k,0,↑)) = ✓
/// for any integer literal k); the whole infinite family is written once.
/// Reserved labels are unquoted, so they cannot collide with a source symbol.
String _primLabel(String primName, bool dual) =>
    '#${primName.toLowerCase()}/0@0:${_modeTag(dual ? Mode.consume : Mode.produce)}';

/// The procedure-argument transition label: `#proc/n@i:mode`.  See the library
/// note on why the procedure name is not part of it.
String _procLabel(int arity, int argIndex, Mode mode) =>
    '#proc/$arity@$argIndex:${_modeTag(mode)}';

String _quote(String s) {
  final b = StringBuffer('"');
  for (final r in s.runes) {
    switch (r) {
      case 0x5C: // backslash
        b.write(r'\\');
      case 0x22: // double quote
        b.write(r'\"');
      case 0x0A:
        b.write(r'\n');
      case 0x0D:
        b.write(r'\r');
      default:
        b.writeCharCode(r);
    }
  }
  b.write('"');
  return b.toString();
}

// =============================================================================
// Step 1 — expansion
// =============================================================================

/// Builds the sub-automaton reachable from a declaration's state, per
/// appendix-type-automaton.tex Definition "Transition Function".
class _Expansion {
  final Map<String, TypeDef> types;
  final Map<_State, Map<String, _State>> transitions = {};

  _Expansion(this.types);

  void expand(ProcDecl decl) {
    final work = <_State>[_State.proc];
    transitions[_State.proc] = _procTransitions(decl);
    work.addAll(transitions[_State.proc]!.values);

    while (work.isNotEmpty) {
      final s = work.removeLast();
      if (transitions.containsKey(s)) continue;
      final t = _transitionsOf(s);
      transitions[s] = t;
      work.addAll(t.values);
    }
  }

  Map<String, _State> _procTransitions(ProcDecl decl) {
    final out = <String, _State>{};
    for (var i = 0; i < decl.arity; i++) {
      final argType = decl.argTypes[i];
      final mode = argType.isInputMode ? Mode.consume : Mode.produce;
      out[_procLabel(decl.arity, i + 1, mode)] = _resolve(argType, false);
    }
    return out;
  }

  Map<String, _State> _transitionsOf(_State s) {
    switch (s.kind) {
      case _Kind.check:
      case _Kind.wildcard:
        return const {};
      case _Kind.prim:
        return {_primLabel(s.name, s.dual): _State.check};
      case _Kind.proc:
        throw StateError('procedure state reached as a transition target');
      case _Kind.type:
        final def = types[s.name];
        if (def == null) throw UnknownTypeError(s.name);
        final out = <String, _State>{};
        _addAlternatives(def, s.dual, out, {'${s.name}:${s.dual}'});
        return out;
    }
  }

  /// Add [def]'s alternatives as transitions out of the state viewed at [dual].
  /// [inlined] guards the bare-type-name inheritance against a definition cycle.
  void _addAlternatives(TypeDef def, bool dual, Map<String, _State> out,
      Set<String> inlined) {
    for (final alt in def.alternatives) {
      _addAlternative(alt, dual, out, inlined);
    }
  }

  void _addAlternative(
      TypeExpr alt, bool dual, Map<String, _State> out, Set<String> inlined) {
    final here = dual ? Mode.consume : Mode.produce;

    if (alt is ConstantAlt) {
      out.putIfAbsent(
          _label(alt.value.toString(), 0, 0, here), () => _State.check);
      return;
    }
    if (alt is ListNilAlt) {
      out.putIfAbsent(_label('[]', 0, 0, here), () => _State.check);
      return;
    }
    if (alt is ListConsAlt) {
      _addArg('[|]', 2, 1, alt.head, dual, out);
      _addArg('[|]', 2, 2, alt.tail, dual, out);
      return;
    }
    if (alt is StructAlt) {
      for (var i = 0; i < alt.args.length; i++) {
        _addArg(alt.functor, alt.args.length, i + 1, alt.args[i], dual, out);
      }
      return;
    }
    if (alt is DiffListAlt) {
      _addArg('\\', 2, 1, alt.content, dual, out);
      _addArg('\\', 2, 2, alt.hole, dual, out);
      return;
    }
    if (alt is TypeRef) {
      // A bare type-name alternative is not a functor and so is not a
      // transition: it stands for everything the named type accepts, and its
      // transitions are inherited (appendix-type-automaton.tex, Definition
      // "Transition Function", third bullet).
      if (alt.isParameterized) {
        throw StateError(
            'parameterised type reference survived expansion: ${alt.name}');
      }
      final d = alt.isInput != dual; // XOR
      if (TypeRef.builtins.contains(alt.name)) {
        out.putIfAbsent(_primLabel(alt.name, d), () => _State.check);
        return;
      }
      final target = types[alt.name];
      if (target == null) {
        throw UnknownTypeError(alt.name, alt.line, alt.column);
      }
      if (!inlined.add('${alt.name}:$d')) return; // definition cycle
      _addAlternatives(target, d, out, inlined);
      return;
    }
    if (alt is PrimitiveModeAlt) {
      // A type whose alternative is `_` or `_?` accepts any term at that mode.
      // Alias resolution replaces such a definition with the primitive itself
      // before the environment is built, so this is unreachable in a checked
      // program; it is handled rather than left to fail.
      out.putIfAbsent('#any/0@0:${_modeTag(alt.isInput != dual ? Mode.consume : Mode.produce)}',
          () => _State.wildcard(alt.isInput != dual));
      return;
    }
    throw StateError('unknown type alternative: $alt');
  }

  void _addArg(String functor, int arity, int argIndex, TypeExpr argType,
      bool dual, Map<String, _State> out) {
    var mode = argType.isInputMode ? Mode.consume : Mode.produce;
    if (dual) mode = mode.flip;
    out.putIfAbsent(
        _label(functor, arity, argIndex, mode), () => _resolve(argType, dual));
  }

  _State _resolve(TypeExpr e, bool dual) {
    if (e is PrimitiveModeAlt) return _State.wildcard(e.isInput != dual);
    if (e is TypeRef) {
      if (e.isParameterized) {
        throw StateError(
            'parameterised type reference survived expansion: ${e.name}');
      }
      final d = e.isInput != dual; // XOR
      if (TypeRef.builtins.contains(e.name)) return _State.prim(e.name, d);
      if (!types.containsKey(e.name)) {
        throw UnknownTypeError(e.name, e.line, e.column);
      }
      return _State.type(e.name, d);
    }
    throw StateError('cannot resolve type expression: $e');
  }
}

// =============================================================================
// Step 2 — minimisation
// =============================================================================

/// Moore partition refinement.  Returns each state's block number.
Map<_State, int> _minimise(Map<_State, Map<String, _State>> transitions) {
  var block = <_State, int>{
    for (final s in transitions.keys) s: _initialClass(s),
  };

  while (true) {
    final signatures = <_State, String>{};
    for (final entry in transitions.entries) {
      final labels = entry.value.keys.toList()..sort();
      final parts = [
        '${block[entry.key]}',
        for (final l in labels) '$l>${block[entry.value[l]!]}',
      ];
      signatures[entry.key] = parts.join('|');
    }

    final distinct = signatures.values.toSet().toList()..sort();
    final bySignature = {
      for (var i = 0; i < distinct.length; i++) distinct[i]: i,
    };
    final next = <_State, int>{
      for (final e in signatures.entries) e.key: bySignature[e.value]!,
    };

    final before = block.values.toSet().length;
    block = next;
    if (bySignature.length == before) return block;
  }
}

// =============================================================================
// Step 3 — canonical serialisation
// =============================================================================

/// The canonical print of the minimised type automaton of [decl], expanded
/// against [env]'s type definitions.
String canonicalTypeAutomatonPrint(ProcDecl decl, TypeEnvironment env) {
  final expansion = _Expansion(env.types)..expand(decl);
  final transitions = expansion.transitions;
  final block = _minimise(transitions);

  // Quotient: one representative's transitions per block, targets as blocks.
  final quotient = <int, Map<String, int>>{};
  for (final entry in transitions.entries) {
    final b = block[entry.key]!;
    quotient.putIfAbsent(b, () {
      return {
        for (final t in entry.value.entries) t.key: block[t.value]!,
      };
    });
  }
  final kindOf = <int, String>{};
  for (final s in transitions.keys) {
    kindOf.putIfAbsent(block[s]!, () => _kindSuffix(s));
  }

  // Breadth-first walk from the start block, transitions in sorted label order,
  // states numbered in visit order.
  final start = block[_State.proc]!;
  final number = <int, int>{start: 0};
  final order = <int>[start];
  for (var i = 0; i < order.length; i++) {
    final labels = quotient[order[i]]!.keys.toList()..sort();
    for (final l in labels) {
      final target = quotient[order[i]]![l]!;
      if (number.containsKey(target)) continue;
      number[target] = order.length;
      order.add(target);
    }
  }

  final lines = <String>[typeAutomatonPrintVersion];
  for (final b in order) {
    lines.add('s${number[b]}${kindOf[b]}');
    final labels = quotient[b]!.keys.toList()..sort();
    for (final l in labels) {
      lines.add('  $l -> s${number[quotient[b]![l]!]}');
    }
  }
  return '${lines.join('\n')}\n';
}

// =============================================================================
// Step 4 — hash
// =============================================================================

/// The type identity of [decl]: SHA-256 of its canonical automaton print.
Uint8List typeIdentity(ProcDecl decl, TypeEnvironment env) =>
    hashOfPrint(canonicalTypeAutomatonPrint(decl, env));

/// The type identity of [decl] as lowercase hex — the form the tables carry.
String typeIdentityHex(ProcDecl decl, TypeEnvironment env) =>
    hex(typeIdentity(decl, env));

/// Lowercase hex of a hash.
String hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

// =============================================================================
// The two tables
// =============================================================================

/// The type-identity tables of a compiled module (plan step 1, deliverable 4).
///
/// [declared] is what `find_type(P/N, Type)` reads: every procedure declared in
/// the module's scope, keyed `p/n`.  [exported] is the table a `Module` value
/// carries, against which `run(Goal, Type, Module)` compares: the exported
/// subset.  A key in [parametric] has no identity and is in neither table —
/// `find_type` on it is an error.  A key in [unresolved] names a declaration
/// whose signature references a type the scope does not define; it too is in
/// neither table.
class TypeIdentityTables {
  final Map<String, String> declared;
  final Map<String, String> exported;
  final Set<String> parametric;
  final Set<String> unresolved;

  const TypeIdentityTables({
    required this.declared,
    required this.exported,
    required this.parametric,
    required this.unresolved,
  });

  /// The identity `find_type(P/N, Type)` yields, or null when `P/N` has none —
  /// undeclared, parametric, or unresolved.
  String? identityOf(String nameArity) => declared[nameArity];
}

/// Build the two tables from a module's type environment.
///
/// An `imported procedure M#p(...)` declares a dependency on another module's
/// export rather than a procedure of this one, and is keyed `M#p/n`; such
/// declarations are not tabled.
TypeIdentityTables buildTypeIdentityTables(TypeEnvironment env) {
  final declared = <String, String>{};
  final exported = <String, String>{};
  final parametric = <String>{};
  final unresolved = <String>{};

  for (final decl in env.procedures.values) {
    if (decl.imported) continue;
    final key = decl.key;
    if (env.paramProcDecls.containsKey(key) || decl.isParameterized) {
      parametric.add(key);
      continue;
    }
    final String identity;
    try {
      identity = typeIdentityHex(decl, env);
    } on UnknownTypeError {
      unresolved.add(key);
      continue;
    }
    declared[key] = identity;
    if (decl.exported) exported[key] = identity;
  }

  return TypeIdentityTables(
    declared: declared,
    exported: exported,
    parametric: parametric,
    unresolved: unresolved,
  );
}

/// Build the two tables for a parsed module, against [ancestorScope] (the root
/// scope when none is given).  Parameterised types are expanded and aliases
/// resolved first, exactly as type checking does, so the automata are built
/// over the same monomorphic environment the checker uses.
TypeIdentityTables typeIdentityTablesForModule(ast.Module module,
    {TypeEnvironment? ancestorScope}) {
  final base = ancestorScope ?? buildRootScopeEnvironment();
  final expanded = expandParameterizedTypes(module,
      knownTypeNames: base.types.keys.toSet(),
      externalTemplates: base.typeTemplates);
  return buildTypeIdentityTables(
      buildTypeEnvironment(expanded, ancestorScope: base));
}
