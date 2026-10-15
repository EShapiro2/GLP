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
//   - A parameterised declaration has no type identity of its own.  It is a
//     template, not a type (TGLP "Parameterised Procedure Declarations": it is
//     checked once per instantiation); `find_type/2` on one is an error.  Such
//     keys are collected in [TypeIdentityTables.parametric] and appear in
//     neither table.  It has an identity at each opaque instance
//     ([opaqueInstanceIdentityHex]; TGLP modules.tex, Definition "Type Identity
//     of a Type, Opaque Instance").
//
// The same construction gives the type identity of a type
// ([typeIdentityOfTypeHex]) and of an opaque instance: only the start state of
// the walk, and the opaque states, differ (Definition "Type Identity of a Type,
// Opaque Instance": "as for a procedure declaration"; "its type identity is
// that of the resulting declaration").

library;

import 'dart:typed_data';

import 'mode.dart';
import 'param_expansion.dart';
import 'program_dfa.dart' show UnknownTypeError, constantLabel;
import 'type_ast.dart';
import 'type_environment_builder.dart';
import '../../compiler/ast.dart' as ast;
import '../../compiler/lexer.dart';
import '../../compiler/parser.dart';
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

  /// An opaque type labelled with a type identity, [_State.name] the label:
  /// a state with no outgoing transitions (TGLP parameterized-types.tex,
  /// Definition "Opaque Type"), two of which are one state exactly when their
  /// labels are equal (modules.tex, Definition "Type Identity of a Type,
  /// Opaque Instance").  It is one state at both modes: the mode of a position
  /// is in the label of the transition that reaches it.
  opaque,
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
  static _State opaque(String label) => _State(_Kind.opaque, label, false);

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
    case _Kind.opaque:
      // Each label is a class of its own ([_minimise] numbers them 4, 5, ...);
      // this is the first of them.
      return 4;
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
    case _Kind.opaque:
      // "An opaque type printed as its label" (Definition "Type Identity of a
      // Type, Opaque Instance").  A label is a lowercase hex hash, never one
      // of the words above.
      return ' ${s.name}';
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

  /// Abstract types standing for type parameters ([abstractTypePrefix]), which
  /// [importExportDisagreement] tracks where a type definition carries one as a
  /// bare alternative: there the parameter is no state of its own but adds its
  /// instance's transitions to the state that inlines it.  Empty for a type
  /// identity, which a parameterised declaration has none of.
  final Set<String> holes;

  /// For each state, the parameters in [holes] it inlines as bare
  /// alternatives, each with the mode it is viewed at.
  final Map<_State, Set<(String, bool)>> inlinedHoles = {};
  Set<(String, bool)>? _inlining;

  /// Abstract types standing for type parameters ([abstractTypePrefix]), each
  /// replaced by the opaque state labelled with its type identity: the opaque
  /// instance of [opaqueInstanceIdentityHex].  Empty everywhere else.
  final Map<String, String> opaque;

  _Expansion(this.types, {this.holes = const {}, this.opaque = const {}});

  /// The transitions of [s], computed on first use: a state the walk of
  /// [importExportDisagreement] reaches by instantiating a parameter need not
  /// be reachable from the declaration's own state.
  Map<String, _State> of(_State s) {
    final known = transitions[s];
    if (known != null) return known;
    final t = _transitionsOf(s);
    transitions[s] = t;
    return t;
  }

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

  /// The sub-automaton reachable from [start], a type's state: the expansion
  /// step of the type identity of a type.
  void expandFrom(_State start) {
    final work = <_State>[start];
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
      case _Kind.opaque:
        return const {};
      case _Kind.prim:
        return {_primLabel(s.name, s.dual): _State.check};
      case _Kind.proc:
        throw StateError('procedure state reached as a transition target');
      case _Kind.type:
        final def = types[s.name];
        if (def == null) throw UnknownTypeError(s.name);
        final out = <String, _State>{};
        final inlining = _inlining = <(String, bool)>{};
        _addAlternatives(def, s.dual, out, {'${s.name}:${s.dual}'});
        _inlining = null;
        if (inlining.isNotEmpty) inlinedHoles[s] = inlining;
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

    // A constant by its canonical print ([constantLabel]), which tells its
    // kind: the integer 0 and the string '0' are two labels, and the string
    // '[]' is not the empty list's.  A name, a number and `[]` print as they
    // did, so their labels, and the identities over them, are as they were.
    // Until 2026-10-10 the constant's text, in which the kinds were one.
    if (alt is ConstantAlt) {
      out.putIfAbsent(
          _label(constantLabel(alt.value), 0, 0, here), () => _State.check);
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
      if (opaque.containsKey(alt.name)) {
        // An opaque type has no transitions, so as a bare alternative it
        // would add none and leave no trace of its label: the opaque instance
        // of a declaration whose parameter occurs as an alternative of a type
        // definition is not one TGLP defines (Proposition "Opaque Activation"
        // excludes it), and has no identity here.
        throw OpaqueAlternativeError(
            alt.name.substring(abstractTypePrefix.length));
      }
      if (alt.name == TypeRef.opaqueUnion) {
        // `Opaque` has no transitions either, and as a bare alternative it
        // would leave no trace: a union with it is not one TGLP defines
        // (Definition "Opaque Type" defines it as a state, not as an
        // alternative), and has no identity here.
        throw StateError('Opaque, the union of all opaque types, occurs as '
            'an alternative of a type definition, which has no type identity');
      }
      if (TypeRef.builtins.contains(alt.name)) {
        out.putIfAbsent(_primLabel(alt.name, d), () => _State.check);
        return;
      }
      if (holes.contains(alt.name)) {
        // A parameter as a bare alternative: its abstract type has no
        // transitions, and what it adds is its instance's.
        _inlining?.add((alt.name, d));
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
      final label = opaque[e.name];
      if (label != null) return _State.opaque(label);
      // `Opaque`, the union of all opaque types: "a state with no outgoing
      // transitions, distinct from the wildcard and from every opaque type"
      // (TGLP parameterized-types.tex, Definition "Opaque Type").  It is the
      // opaque state labelled [opaqueUnionLabel], which no type identity is:
      // a label is a lowercase hex hash.  One state at both modes, as an
      // opaque type is.
      if (e.name == TypeRef.opaqueUnion) return _State.opaque(opaqueUnionLabel);
      if (TypeRef.builtins.contains(e.name)) return _State.prim(e.name, d);
      if (!types.containsKey(e.name)) {
        throw UnknownTypeError(e.name, e.line, e.column);
      }
      return _State.type(e.name, d);
    }
    throw StateError('cannot resolve type expression: $e');
  }
}

/// The parameters among [params] that the type [typeName], viewed at [dual],
/// inlines as bare alternatives (`Opt(X) ::= none ; X`), directly or through
/// a bare type-name alternative it inherits, each with the mode it is viewed
/// at there (true: consumed).  [params] name abstract types standing for type
/// parameters, defined in [types] with no alternatives.
///
/// This is the expansion [importExportDisagreement] binds such a parameter
/// by, shared with the instantiation of a call (well_typed_clause.dart,
/// `_supplied`): TGLP appendix-implementation-notes.tex, "The instantiation
/// of a call": "A parameter that occurs only as a bare alternative of a type
/// is bound as the check of an import binds it".  Empty where [typeName] is
/// not in [types] or names a type not in scope.
Set<(String, bool)> bareInlinedParameters(
    Map<String, TypeDef> types, Set<String> params, String typeName,
    {bool dual = false}) {
  if (params.isEmpty || !types.containsKey(typeName)) return const {};
  final e = _Expansion(types, holes: params);
  final s = _State.type(typeName, dual);
  try {
    e.of(s);
  } on UnknownTypeError {
    return const {};
  } on StateError {
    return const {};
  }
  return e.inlinedHoles[s] ?? const {};
}

// =============================================================================
// Step 2 — minimisation
// =============================================================================

/// Moore partition refinement.  Returns each state's block number.
Map<_State, int> _minimise(Map<_State, Map<String, _State>> transitions) {
  // An opaque state's class is its label's: 4, 5, ... in label order.
  final labels = {
    for (final s in transitions.keys)
      if (s.kind == _Kind.opaque) s.name
  }.toList()
    ..sort();
  final labelClass = {
    for (var i = 0; i < labels.length; i++) labels[i]: _initialClass(_State.opaque('')) + i,
  };
  var block = <_State, int>{
    for (final s in transitions.keys)
      s: s.kind == _Kind.opaque ? labelClass[s.name]! : _initialClass(s),
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
  return _canonicalPrint(expansion.transitions, _State.proc);
}

/// Steps 2 and 3 over an expanded automaton: minimise [transitions] and print
/// the quotient breadth-first from [startState]'s block.
String _canonicalPrint(
    Map<_State, Map<String, _State>> transitions, _State startState) {
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
  final start = block[startState]!;
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

// =============================================================================
// The type identity of a type, and of an opaque instance
// =============================================================================

/// The canonical print of the minimised type automaton of the type [type] —
/// a type name or a primitive, at the produced mode — expanded against
/// [types].  TGLP modules.tex, Definition "Type Identity of a Type, Opaque
/// Instance": "The type identity ι(T) of a type T is the hash of its type
/// automaton in canonical minimised form, as for a procedure declaration."
/// The construction is the declaration's, from T's state rather than from a
/// procedure's.
String canonicalTypePrint(TypeExpr type, Map<String, TypeDef> types) {
  final e = _Expansion(types);
  // T at the produced mode: `T?` names the same type, consumed.
  var start = e._resolve(type, false);
  if (start.dual) start = _State(start.kind, start.name, false);
  e.expandFrom(start);
  return _canonicalPrint(e.transitions, start);
}

/// The label the canonical print gives `Opaque`, the union of all opaque
/// types ([_Expansion]): never a type identity's, which is lowercase hex.
const String opaqueUnionLabel = 'Opaque';

/// ι([type]): the type identity of a type, as lowercase hex.
String typeIdentityOfTypeHex(TypeExpr type, Map<String, TypeDef> types) =>
    hex(hashOfPrint(canonicalTypePrint(type, types)));

/// The canonical print of the opaque instance D⟨h_1, ..., h_k⟩ of the
/// parameterised declaration [template] (D) at the type identities [labels],
/// expanded against [env], the scope D is declared in.
///
/// TGLP modules.tex, Definition "Type Identity of a Type, Opaque Instance":
/// "The opaque instance D⟨h_1, ..., h_k⟩ replaces each X_i by an opaque type
/// labelled h_i, two such opaque types being one state exactly when their
/// labels are equal; its type identity is that of the resulting declaration,
/// an opaque type printed as its label."  Each parameter stands as its
/// abstract type ([buildAbstractInstance]), whose state the expansion replaces
/// by the opaque state labelled h_i.  Throws [OpaqueAlternativeError] where a
/// parameter occurs as an alternative of a type definition.
String canonicalOpaqueInstancePrint(
    ProcDecl template, List<String> labels, TypeEnvironment env) {
  if (labels.length != template.typeParams.length) {
    throw ArgumentError('${template.key} has ${template.typeParams.length} '
        'type parameters, given ${labels.length} type identities');
  }
  final inst = buildAbstractInstance(
      template, template.typeParams, env.typeTemplates,
      knownMonoTypes: env.types.keys.toSet());
  final e = _Expansion({...env.types, ...inst.typeDefs}, opaque: {
    for (var i = 0; i < labels.length; i++)
      '$abstractTypePrefix${template.typeParams[i]}': labels[i],
  })
    ..expand(inst.decl);
  return _canonicalPrint(e.transitions, _State.proc);
}

/// The type identity of the opaque instance D⟨[labels]⟩ of [template], as
/// lowercase hex ([canonicalOpaqueInstancePrint]).
String opaqueInstanceIdentityHex(
        ProcDecl template, List<String> labels, TypeEnvironment env) =>
    hex(hashOfPrint(canonicalOpaqueInstancePrint(template, labels, env)));

/// Why `find_type(P/N, H, Type)` assigns no identity
/// ([TypeIdentityTables.opaqueInstanceIdentity]).
class OpaqueInstanceRefusal implements Exception {
  final String reason;
  OpaqueInstanceRefusal(this.reason);

  @override
  String toString() => reason;
}

/// A type parameter that occurs as an alternative of a type definition, where
/// an opaque instance replaces it by an opaque type.
class OpaqueAlternativeError implements Exception {
  final String parameter;
  OpaqueAlternativeError(this.parameter);

  @override
  String toString() => 'OpaqueAlternativeError: the type parameter '
      '$parameter occurs as an alternative of a type definition, so the '
      'opaque instance at it has no identity';
}

/// What a guest's exports record for an entry declared as the instance of a
/// declaration D with type parameters X_1, ..., X_k at its types T_1, ...,
/// T_k: [entry], the type identity of D⟨ι(T_1), ..., ι(T_k)⟩, and [types],
/// ι(T_1), ..., ι(T_k) in the order of D's parameters (TGLP modules.tex,
/// Dynamic Activation, "Opaque types": "A guest M whose entry is the template
/// D's instance at its type T records in its exports, for its entry, the type
/// identity of D⟨ι(T)⟩, and with it ι(T)").
///
/// PROVISIONAL (Udi, 2026-10-15), pending GLP-Spec's final form: the papers
/// define the record for a one-parameter template; a template of k
/// parameters records the list of k identities, one per parameter in
/// declaration order, a one-parameter template the list of one.
class OpaqueEntry {
  final String entry;
  final List<String> types;
  const OpaqueEntry({required this.entry, required this.types});

  @override
  bool operator ==(Object other) =>
      other is OpaqueEntry &&
      other.entry == entry &&
      other.types.length == types.length &&
      [for (var i = 0; i < types.length; i++) other.types[i] == types[i]]
          .every((b) => b);

  @override
  int get hashCode => Object.hash(entry, Object.hashAll(types));

  @override
  String toString() => 'OpaqueEntry(entry: $entry, types: $types)';
}

/// The record of [entry], a monomorphic declaration in [entryEnv], where it
/// is the instance of [template] (D, declared in [templateEnv], D's scope) at
/// some types T_1, ..., T_k; null where it is the instance of D at none.
///
/// It is the instance at T_1, ..., T_k when its automaton equals D's with D's
/// parameters instantiated to them: the agreement of an imported declaration with an
/// exported one ([importExportDisagreement]; modules.tex, "Cross-module type
/// checking": "their type automata are equal (structural type compatibility)
/// once the exported declaration's type parameters, if any, are
/// instantiated"), [entry] in the import's place and D in the export's, whose
/// walk finds T_i as the state of [entry] where D has its parameter X_i.
/// Each ι(T_i) is then taken in [entryEnv], where T_i is defined, and
/// D⟨ι(T_1), ..., ι(T_k)⟩ in [templateEnv], where it need not be.
///
/// PROVISIONAL (Udi, 2026-10-15), pending GLP-Spec's final form: "Opaque
/// types" defines the record for a template with one type parameter; a
/// template with k records the k identities ([OpaqueEntry]).  A parameter
/// that occurs in D only as a bare alternative of a type is bound to no type
/// of [entry]'s, and one that occurs as an alternative at all has no opaque
/// instance ([OpaqueAlternativeError]): neither has a record.
OpaqueEntry? opaqueEntryOf({
  required ProcDecl entry,
  required TypeEnvironment entryEnv,
  required ProcDecl template,
  required TypeEnvironment templateEnv,
}) {
  if (template.typeParams.isEmpty || entry.isParameterized) return null;
  final inst = buildAbstractInstance(
      template, template.typeParams, templateEnv.typeTemplates,
      knownMonoTypes: templateEnv.types.keys.toSet());
  final (found, theta) = _agreement(
    imported: entry,
    importedTypes: entryEnv.types,
    exported: inst.decl,
    exportedTypes: {...templateEnv.types, ...inst.typeDefs},
    exportedParams: inst.abstractTypeNames,
  );
  if (found != null) return null;
  final typeIds = <String>[];
  for (final p in template.typeParams) {
    final t = theta['$abstractTypePrefix$p'];
    if (t == null) return null;
    if (t.kind == _Kind.type && t.name.startsWith(_instancePrefix)) {
      return null;
    }
    if (t.kind == _Kind.check || t.kind == _Kind.proc) return null;
    final e = _Expansion(entryEnv.types)..expandFrom(t);
    typeIds.add(hex(hashOfPrint(_canonicalPrint(e.transitions, t))));
  }
  final String entryId;
  try {
    entryId = opaqueInstanceIdentityHex(template, typeIds, templateEnv);
  } on OpaqueAlternativeError {
    return null;
  }
  return OpaqueEntry(entry: entryId, types: typeIds);
}

/// Lowercase hex of a hash.
String hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

// =============================================================================
// Agreement of an imported declaration with the export it names
// =============================================================================

/// Where an imported declaration and the declaration its module exports
/// disagree: the [argument] (1-based) and what differs there.
class DeclarationDisagreement {
  final int argument;
  final String detail;
  const DeclarationDisagreement(this.argument, this.detail);

  @override
  String toString() => 'argument $argument: $detail';
}

/// Whether the imported declaration [imported] agrees with the declaration
/// [exported] its module exports, and if not, where they differ.
///
/// TGLP modules.tex, "Cross-module type checking": "The imported declaration
/// must agree with the declaration M exports: their type automata are equal
/// (structural type compatibility) once the exported declaration's type
/// parameters, if any, are instantiated (Definition Instantiation); subtyping
/// applies to the arguments of a call, not between the two declarations."
///
/// The automata are the ones a type identity is the hash of (steps 1 and 2
/// above): each declaration's, expanded against its own module's types
/// ([importedTypes], [exportedTypes]), the procedure name abstracted.  Equal
/// automata are bisimilar ones, which the walk below decides directly on the
/// pair, as Moore refinement (step 2) would on their union: a pair of states
/// agrees when the two start in one partition class and have the same
/// transition labels, the targets of each label agreeing in turn.  Two
/// automata built against two scopes are compared this way, rather than by
/// their prints, because the export's parameters must be instantiated first.
///
/// [exportedParams] are the abstract types (`$abstract_X`,
/// [buildAbstractInstance]) standing for the export's type parameters.  The
/// instantiation is found by matching: where the walk reaches a parameter `X`
/// of the export it binds `X` to the import's state there, and every later
/// occurrence of `X` must agree with that binding.  The instantiation is thus
/// "some instantiation" of the sentence, the one the import's own types
/// supply, and no subtyping enters: a state of the import that is a strict
/// subtype or supertype of the export's is a disagreement.
///
/// A parameter the walk never reaches as a state of its own occurs only as a
/// bare alternative of a type (`Opt(X) ::= none ; X`), where it adds its
/// instance's transitions to the state that inlines it.  It is bound as an
/// instantiation would bind it (GLP, 2026-10-12 22:31 UTC, item 2): to the
/// transitions and abstract types the import's states have at every place
/// it is inlined.  The export's own transitions are walked first, whatever
/// the parameter's instance, since an alternative adds transitions and
/// never takes one away; so a parameter still unbound after them occurs
/// nowhere but as a bare alternative, and its instance enters the walk only
/// through the labels it adds.  An instance with fewer transitions than those
/// common to every place could only leave a label of the import unmatched,
/// so if any instantiation makes the two automata equal, this one does.
///
/// [importedParams] are the abstract types standing for the import's own type
/// parameters, which it holds open: "a parameter the importing module holds
/// open stays open across the module boundary and is fixed at the call".  An
/// abstract type matches only itself (parameterized-types.tex, Definition
/// "Abstract Type"), so it agrees only with an export parameter bound to it.
DeclarationDisagreement? importExportDisagreement({
  required ProcDecl imported,
  required Map<String, TypeDef> importedTypes,
  Set<String> importedParams = const {},
  required ProcDecl exported,
  required Map<String, TypeDef> exportedTypes,
  Set<String> exportedParams = const {},
}) =>
    _agreement(
      imported: imported,
      importedTypes: importedTypes,
      importedParams: importedParams,
      exported: exported,
      exportedTypes: exportedTypes,
      exportedParams: exportedParams,
    ).$1;

/// The name of the state [importExportDisagreement] binds a parameter that
/// occurs only as a bare alternative to: no type name, so it collides with
/// none.
const _instancePrefix = '#instance:';

/// [importExportDisagreement]'s walk, returning with the disagreement the
/// instantiation it found: each export parameter bound to a state of the
/// import, at the produced view.
(DeclarationDisagreement?, Map<String, _State>) _agreement({
  required ProcDecl imported,
  required Map<String, TypeDef> importedTypes,
  Set<String> importedParams = const {},
  required ProcDecl exported,
  required Map<String, TypeDef> exportedTypes,
  Set<String> exportedParams = const {},
}) {
  final ie = _Expansion(importedTypes, holes: importedParams)..expand(imported);
  final ee = _Expansion(exportedTypes, holes: exportedParams)..expand(exported);

  // The export's parameters, each bound to a state of the import at the
  // produced view.
  final theta = <String, _State>{};

  _State? flip(_State s, bool d) {
    if (!d) return s;
    switch (s.kind) {
      case _Kind.type:
      case _Kind.prim:
      case _Kind.wildcard:
        return _State(s.kind, s.name, !s.dual);
      case _Kind.check:
      case _Kind.proc:
        return null;
      case _Kind.opaque:
        return s;
    }
  }

  bool isExportParam(_Side x) =>
      x.export &&
      x.state.kind == _Kind.type &&
      exportedParams.contains(x.state.name);

  String abstractMark(_State s) => 'abstract:${s.name}:${s.dual}';

  // A state's class in a view: its initial class, and for an opaque state its
  // label too, two opaque states being one exactly when their labels are
  // equal (`Opaque` among them, [opaqueUnionLabel]).
  String viewClass(_State s) => s.kind == _Kind.opaque
      ? '${_initialClass(s)}:${s.name}'
      : '${_initialClass(s)}';
  bool isImportAbstract(_State s) =>
      s.kind == _Kind.type && importedParams.contains(s.name);

  // The instance of a parameter that occurs only as a bare alternative, as a
  // state of the import of its own, `#instance:X`, viewed at the produced
  // mode.  The name is no type name, so it collides with none.
  const instancePrefix = _instancePrefix;
  final instances = <String, _View>{};

  _State flipTarget(_State s) => flip(s, true) ?? s;
  String flipLabel(String l) => l.endsWith(':up')
      ? '${l.substring(0, l.length - 3)}:down'
      : '${l.substring(0, l.length - 5)}:up';
  String flipMark(String m) => m.endsWith(':true')
      ? '${m.substring(0, m.length - 5)}:false'
      : '${m.substring(0, m.length - 6)}:true';
  // A view of the import's at the dual mode when [d].
  _View flipView(_View v, bool d) => !d
      ? v
      : _View(v.cls, {for (final m in v.marks) flipMark(m)}, {
          for (final e in v.out.entries)
            flipLabel(e.key): _Side(e.value.export, flipTarget(e.value.state)),
        });

  // A state of the import: its class, with its own abstract type or those it
  // inlines, and its transitions.
  _View importView(_State s) {
    if (s.kind == _Kind.type && s.name.startsWith(instancePrefix)) {
      return flipView(instances[s.name]!, s.dual);
    }
    final marks = <String>{
      for (final h in ie.inlinedHoles[s] ?? const <(String, bool)>{})
        abstractMark(_State.type(h.$1, h.$2)),
    };
    final base = isImportAbstract(s) ? abstractMark(s) : viewClass(s);
    return _View(base, marks, {
      for (final e in ie.of(s).entries) e.key: _Side(false, e.value),
    });
  }

  // A state of the export, with each parameter it inlines replaced by its
  // instance; null where one of them is not bound yet.
  _View? exportView(_State s) {
    final out = <String, _Side>{
      for (final e in ee.of(s).entries) e.key: _Side(true, e.value),
    };
    final marks = <String>{};
    for (final h in ee.inlinedHoles[s] ?? const <(String, bool)>{}) {
      final bound = theta[h.$1];
      if (bound == null) return null;
      final y = flip(bound, h.$2)!;
      switch (y.kind) {
        case _Kind.prim:
          out.putIfAbsent(_primLabel(y.name, y.dual), () => _Side(false, _State.check));
        case _Kind.wildcard:
          out.putIfAbsent(
              '#any/0@0:${_modeTag(y.dual ? Mode.consume : Mode.produce)}',
              () => _Side(false, y));
        case _Kind.type:
          final v = importView(y);
          if (isImportAbstract(y)) marks.add(abstractMark(y));
          marks.addAll(v.marks);
          for (final e in v.out.entries) {
            out.putIfAbsent(e.key, () => e.value);
          }
        case _Kind.check:
        case _Kind.proc:
        case _Kind.opaque:
          break;
      }
    }
    return _View(viewClass(s), marks, out);
  }

  _View? view(_Side x) => x.export ? exportView(x.state) : importView(x.state);

  String describe(_Side x) {
    final s = x.state;
    final q = s.dual ? '?' : '';
    switch (s.kind) {
      case _Kind.check:
        return 'a constant';
      case _Kind.proc:
        return 'the procedure';
      case _Kind.wildcard:
        return '_$q';
      case _Kind.prim:
        return '${s.name}$q';
      case _Kind.opaque:
        return 'the opaque type ${s.name}';
      case _Kind.type:
        if (s.name.startsWith(instancePrefix)) {
          final x = s.name.substring(instancePrefix.length);
          return 'the instance of ${x.substring(abstractTypePrefix.length)}$q';
        }
        final name = s.name.startsWith(abstractTypePrefix)
            ? s.name.substring(abstractTypePrefix.length)
            : s.name;
        return '$name$q';
    }
  }

  String at(List<String> path) =>
      path.isEmpty ? '' : ' at ${path.join(' ')}';

  int argOf(String procLabel) {
    // `#proc/n@i:mode`
    final at = procLabel.indexOf('@');
    final colon = procLabel.indexOf(':', at);
    return int.parse(procLabel.substring(at + 1, colon));
  }

  final queue = <_Pair>[
    _Pair(_Side(true, _State.proc), _Side(false, _State.proc), 0, const [])
  ];
  final seen = <(_Side, _Side)>{};
  var deferred = <_Pair>[];

  // Breadth first, the arguments in order, so the disagreement reported is at
  // the first argument and the shortest path where there is one.
  var next = 0;
  while (true) {
    while (next < queue.length) {
      final p = queue[next++];
      var a = p.export;
      final b = p.import;
      var exportDesc = describe(a);

      // The export's parameter here is instantiated to the import's state.
      if (isExportParam(a)) {
        final bound = theta[a.state.name];
        if (bound == null) {
          final instance = flip(b.state, a.state.dual);
          if (instance == null) {
            return (DeclarationDisagreement(p.argument,
                'the import has ${describe(b)}${at(p.path)} where the export '
                'has its parameter ${describe(a)}'), theta);
          }
          theta[a.state.name] = instance;
          continue;
        }
        a = _Side(false, flip(bound, a.state.dual)!);
        exportDesc = 'its parameter $exportDesc, instantiated to ${describe(a)}';
      }
      if (!seen.add((a, b))) continue;

      final va = view(a);
      final vb = view(b)!;
      if (va == null) {
        // A parameter the export inlines here is not bound yet.  Its
        // instance can only add transitions, so the export's own are walked
        // now; the state waits for the rest.
        final own = ee.of(a.state);
        for (final l in own.keys.toList()..sort()) {
          final t = vb.out[l];
          if (t == null) {
            return (DeclarationDisagreement(p.argument,
                'the import has ${describe(b)}${at(p.path)} where the export '
                'has $exportDesc, and they differ in their transitions --- '
                'only the export\'s has $l'), theta);
          }
          queue.add(_Pair(_Side(true, own[l]!), t, p.argument, [...p.path, l]));
        }
        deferred.add(_Pair(a, b, p.argument, p.path));
        continue;
      }

      if (p.argument == 0) {
        // The declaration's own state: the two must have the same arguments
        // at the same modes.
        final la = va.out.keys.toSet(), lb = vb.out.keys.toSet();
        if (!_sameSet(la, lb)) {
          final ia = {for (final l in la) argOf(l): l};
          final ib = {for (final l in lb) argOf(l): l};
          for (var i = 1; i <= ia.length; i++) {
            if (ia[i] != ib[i]) {
              final ma = ia[i]!.endsWith(':down') ? 'consumed (?)' : 'produced';
              final mb = ib[i]!.endsWith(':down') ? 'consumed (?)' : 'produced';
              return (DeclarationDisagreement(i,
                  'the import declares it $mb where the export declares it $ma'), theta);
            }
          }
        }
        for (final l in la.toList()..sort((x, y) => argOf(x) - argOf(y))) {
          queue.add(_Pair(va.out[l]!, vb.out[l]!, argOf(l), const []));
        }
        continue;
      }

      if (va.cls != vb.cls || !_sameSet(va.marks, vb.marks)) {
        return (DeclarationDisagreement(p.argument,
            'the import has ${describe(b)}${at(p.path)} where the export has '
            '$exportDesc'), theta);
      }
      final la = va.out.keys.toSet(), lb = vb.out.keys.toSet();
      if (!_sameSet(la, lb)) {
        final onlyI = (lb.difference(la).toList()..sort()).join(', ');
        final onlyE = (la.difference(lb).toList()..sort()).join(', ');
        return (DeclarationDisagreement(p.argument,
            'the import has ${describe(b)}${at(p.path)} where the export has '
            '$exportDesc, and they differ in their transitions'
            '${onlyI.isEmpty ? '' : ' --- only the import\'s has $onlyI'}'
            '${onlyE.isEmpty ? '' : ' --- only the export\'s has $onlyE'}'), theta);
      }
      for (final l in la.toList()..sort()) {
        queue.add(_Pair(va.out[l]!, vb.out[l]!, p.argument, [...p.path, l]));
      }
    }
    if (deferred.isEmpty) return (null, theta);
    // A parameter the export inlines as a bare alternative waits for its
    // binding at another position.
    var retry = deferred.where((p) => view(p.export) != null).toList();
    if (retry.isEmpty) {
      // It occurs nowhere else: bind it as an instantiation would, to what
      // the import's states have in common at every place it is inlined,
      // each viewed at the produced mode (see above).
      final places = <String, List<_View>>{};
      for (final p in deferred) {
        for (final h in ee.inlinedHoles[p.export.state] ?? const {}) {
          if (theta.containsKey(h.$1)) continue;
          (places[h.$1] ??= []).add(flipView(view(p.import)!, h.$2));
        }
      }
      for (final e in places.entries) {
        final first = e.value.first;
        final rest = e.value.skip(1);
        final name = '$instancePrefix${e.key}';
        instances[name] = _View(
          '${_initialClass(_State.type(name, false))}',
          {
            for (final m in first.marks)
              if (rest.every((v) => v.marks.contains(m))) m,
          },
          {
            for (final l in first.out.entries)
              if (rest.every((v) => v.out.containsKey(l.key))) l.key: l.value,
          },
        );
        theta[e.key] = _State.type(name, false);
      }
      retry = deferred;
    }
    deferred = deferred.where((p) => !retry.contains(p)).toList();
    for (final p in retry) {
      seen.remove((p.export, p.import));
      queue.add(p);
    }
  }
}

bool _sameSet<T>(Set<T> a, Set<T> b) =>
    a.length == b.length && a.containsAll(b);

/// A state of one of the two automata [importExportDisagreement] walks.
class _Side {
  final bool export;
  final _State state;
  const _Side(this.export, this.state);

  @override
  bool operator ==(Object other) =>
      other is _Side && other.export == export && other.state == state;

  @override
  int get hashCode => Object.hash(export, state);
}

/// A state as the walk sees it: its partition class, the abstract types it
/// is or inlines, and its transitions.
class _View {
  final String cls;
  final Set<String> marks;
  final Map<String, _Side> out;
  _View(this.cls, this.marks, this.out);
}

/// A pair of states the walk compares, the export's first, with the argument
/// and the path of labels below it that reached them.
class _Pair {
  final _Side export;
  final _Side import;
  final int argument;
  final List<String> path;
  _Pair(this.export, this.import, this.argument, this.path);
}

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
///
/// [opaque] is recorded beside [exported]: for each export that is a guest's
/// entry, declared as the instance of a parameterised declaration D of an
/// ancestor scope at its type T, the type identity of D⟨ι(T)⟩ and ι(T)
/// ([OpaqueEntry]; TGLP modules.tex, Dynamic Activation, "Opaque types").  Its
/// key is in [exported] too, at the entry's own identity, as before.
class TypeIdentityTables {
  final Map<String, String> declared;
  final Map<String, String> exported;
  final Set<String> parametric;
  final Set<String> unresolved;
  final Map<String, OpaqueEntry> opaque;

  /// The scope [declared] was built in, with its parameterised declarations
  /// and type templates, for the opaque instances of
  /// [opaqueInstanceIdentity]; built on first use, and absent from tables
  /// derived from an interface, which say nothing of a module's scope.
  final TypeEnvironment Function()? _scope;
  TypeEnvironment? _scopeBuilt;

  TypeIdentityTables({
    required this.declared,
    required this.exported,
    required this.parametric,
    required this.unresolved,
    this.opaque = const {},
    TypeEnvironment Function()? scope,
  }) : _scope = scope;

  /// These tables with [opaque] recorded beside them.
  TypeIdentityTables withOpaque(Map<String, OpaqueEntry> opaque) =>
      TypeIdentityTables(
        declared: declared,
        exported: exported,
        parametric: parametric,
        unresolved: unresolved,
        opaque: opaque,
        scope: _scope,
      );

  /// The type identity of the opaque instance at [labels] of the
  /// parameterised declaration of [nameArity] in this scope: what
  /// `find_type(P/N, Hs, Type)` yields (GLP-Spec appendix-guards, "Dynamic
  /// activation"; TGLP modules.tex, Definition "Type Identity of a Type,
  /// Opaque Instance").  Throws [OpaqueInstanceRefusal] where [nameArity] is
  /// not declared in this scope, or its declaration has not one type
  /// parameter for each label, or the instance has no identity
  /// ([OpaqueAlternativeError]).
  ///
  /// PROVISIONAL (Udi, 2026-10-15), pending GLP-Spec's final form: [labels]
  /// is the list of type identities, one per type parameter of the template
  /// in declaration order, a one-parameter template's the list of one.
  String opaqueInstanceIdentity(String nameArity, List<String> labels) {
    final build = _scope;
    if (build == null) {
      throw OpaqueInstanceRefusal(
          '$nameArity: the tables carry no scope to declare it in');
    }
    final scope = _scopeBuilt ??= build();
    final template = scope.paramProcDecls[nameArity];
    if (template == null) {
      if (declared.containsKey(nameArity) ||
          unresolved.contains(nameArity)) {
        throw OpaqueInstanceRefusal('$nameArity is declared in the caller\'s '
            'scope with no type parameter, not ${labels.length}');
      }
      throw OpaqueInstanceRefusal(
          '$nameArity is not declared in the caller\'s scope');
    }
    if (template.typeParams.length != labels.length) {
      throw OpaqueInstanceRefusal('the declaration of $nameArity has '
          '${template.typeParams.length} type parameters, and '
          '${labels.length} type identities are given');
    }
    try {
      return opaqueInstanceIdentityHex(template, labels, scope);
    } on OpaqueAlternativeError catch (e) {
      throw OpaqueInstanceRefusal('$nameArity: $e');
    }
  }

  /// The identity `find_type(P/N, Type)` yields, or null when `P/N` has none —
  /// undeclared, parametric, or unresolved.
  String? identityOf(String nameArity) => declared[nameArity];
}

/// Build the two tables from a module's type environment.
///
/// An `imported procedure M#p(...)` declares a dependency on another module's
/// export rather than a procedure of this one, and is keyed `M#p/n`; such
/// declarations are not tabled.
///
/// [decls] is the set of declarations to table, defaulting to every declaration
/// in scope — which is what a compiled module's tables are built over, the root
/// scope included.  A caller tabling a subset (the interface derivation below,
/// whose declarations are its exports and nothing else) passes that subset; the
/// identities are still taken against the whole of [env], since a declaration's
/// automaton expands whatever types its signature reaches.
TypeIdentityTables buildTypeIdentityTables(TypeEnvironment env,
    {Iterable<ProcDecl>? decls}) {
  final declared = <String, String>{};
  final exported = <String, String>{};
  final parametric = <String>{};
  final unresolved = <String>{};

  for (final decl in decls ?? env.procedures.values) {
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

/// Build the two tables for a parsed module, against [ancestorScope] (the
/// language primitives alone when none is given).  Parameterised types are
/// expanded and aliases resolved first, exactly as type checking does, so the
/// automata are built over the same monomorphic environment the checker uses.
TypeIdentityTables typeIdentityTablesForModule(ast.Module module,
    {TypeEnvironment? ancestorScope}) {
  final base = ancestorScope ?? TypeEnvironment.empty();
  final expanded = expandParameterizedTypes(module,
      knownTypeNames: base.types.keys.toSet(),
      externalTemplates: base.typeTemplates);
  final tables =
      buildTypeIdentityTables(buildTypeEnvironment(expanded, ancestorScope: base));
  // The same scope, carrying the module's parameterised type definitions as
  // templates (buildModuleTypeEnvironment), for find_type/3's opaque
  // instances; built only where one is asked for.
  return TypeIdentityTables(
    declared: tables.declared,
    exported: tables.exported,
    parametric: tables.parametric,
    unresolved: tables.unresolved,
    scope: () => buildTypeEnvironment(expanded, ancestorScope: base,
        typeTemplates: {
          for (final td in module.typeDefs)
            if (td.isParameterized) td.name: td,
        }),
  );
}

// =============================================================================
// The exported table, derived from an artefact's interface text
// =============================================================================

/// The interface text of an artefact: its reachable type definitions followed
/// by the declaration of each export, which is what [interfaceTypeIdentityTables]
/// parses.
///
/// [typeDefsText] is the artefact's `typeDefsText` and [exportDeclarationTexts]
/// the `declarationText` of each of its exports, in order.  An export with no
/// declaration carries empty text and contributes nothing.
String artefactInterfaceText({
  required String typeDefsText,
  required Iterable<String> exportDeclarationTexts,
}) {
  final b = StringBuffer();
  if (typeDefsText.isNotEmpty) {
    b.write(typeDefsText);
    if (!typeDefsText.endsWith('\n')) b.write('\n');
  }
  for (final d in exportDeclarationTexts) {
    if (d.trim().isEmpty) continue;
    b.write(d);
    if (!d.endsWith('\n')) b.write('\n');
  }
  return b.toString();
}

/// The exported type-identity table of a shipped module, derived from its
/// artefact's interface text rather than shipped beside it (modules.tex
/// §Dynamic Activation; the Implementation Notes appendix, "Deriving the
/// exported table at load").  A shipped table can disagree with the source it
/// describes; one recomputed from the attested source cannot.
///
/// The text is parsed by [Parser.parseInterface], the type environment is built
/// over it, and the identity of each parsed declaration is taken — the same
/// construction, over the same language primitives, that
/// [linkedTypeIdentityTables] applies to the compiled program, so the two agree
/// key for key.
///
/// Three things the text does not carry, each of which would otherwise give a
/// different identity for the same program:
///
///   - The primitive types are absent from it: they are the language's, built
///     into every runtime's checker, and the text is read over them alone.  The
///     root self.glp's types an export reaches are in it, the root being a
///     module of the program (TGLP modules.tex, Compilation, first step); until
///     2026-10-04 they were left out as ambient and supplied by a root scope
///     the engine set for the whole process.
///   - An export with no declaration carries empty declaration text and
///     contributes no types.  Its key is **absent** from every field, which is
///     what the compiled path does with a procedure that has no declaration:
///     [buildTypeIdentityTables] tables declarations, so an undeclared
///     procedure is in none of the four.  `unresolved` is the narrower case of a
///     declaration whose signature names a type the scope does not define.
///   - A parameterised declaration has no identity and lands in
///     [TypeIdentityTables.parametric], on this path as on the compiled one.
///
/// [TypeIdentityTables.declared] is empty: an interface section carries the
/// module's exports and says nothing about the rest of its scope, which is what
/// the declared table is.  `find_type/2` reads the running module's declared
/// table, built by [linkedTypeIdentityTables]; `run/3` compares against the
/// exported table this function derives.
///
/// An entry declared as the instance of a parameterised declaration D of an
/// ancestor scope carries D's declaration in its declaration text, after its
/// own and unexported ([entryTemplateDeclarationText] in wire/flattening.dart),
/// with the types D reaches in the type definitions: an interface carries
/// exports only, so an unexported parameterised declaration of an export's key
/// is that export's template.  It is taken out before the tables are built,
/// which are therefore what they are without it, and [TypeIdentityTables.opaque]
/// records D⟨ι(T)⟩ and ι(T) for the entry ([opaqueEntryOf]).
TypeIdentityTables interfaceTypeIdentityTables({
  required String typeDefsText,
  required Iterable<String> exportDeclarationTexts,
}) {
  final text = artefactInterfaceText(
      typeDefsText: typeDefsText,
      exportDeclarationTexts: exportDeclarationTexts);
  if (text.trim().isEmpty) {
    return TypeIdentityTables(
        declared: {}, exported: {}, parametric: {}, unresolved: {});
  }

  final parsed = Parser(Lexer(text).tokenize()).parseInterface();
  final exportedKeys = {
    for (final d in parsed.procDeclarations)
      if (d.exported && !d.imported) d.key
  };
  final templates = <String, ProcDecl>{
    for (final d in parsed.procDeclarations)
      if (!d.exported &&
          !d.imported &&
          d.isParameterized &&
          exportedKeys.contains(d.key))
        d.key: d
  };
  final iface = templates.isEmpty
      ? parsed
      : ast.Module(
          typeDefs: parsed.typeDefs,
          procDeclarations: [
            for (final d in parsed.procDeclarations)
              if (!identical(templates[d.key], d)) d
          ],
          line: parsed.line,
          column: parsed.column,
        );

  final base = TypeEnvironment.empty();
  final expanded = expandParameterizedTypes(iface,
      knownTypeNames: base.types.keys.toSet(),
      externalTemplates: base.typeTemplates);
  final env = buildTypeEnvironment(expanded, ancestorScope: base);

  // Table the interface's own declarations only.  They are taken from `env`,
  // not from `expanded`, so alias resolution has been applied to them exactly as
  // it is on the compiled path; `expanded` supplies which keys they are.
  final own = <ProcDecl>[];
  for (final d in expanded.procDeclarations) {
    if (d.imported) continue;
    final e = env.procedures[d.qualifiedKey];
    if (e != null) own.add(e);
  }

  final tables = buildTypeIdentityTables(env, decls: own);
  final opaque = <String, OpaqueEntry>{};
  // The scope the templates are expanded in: the interface's, with its
  // parameterised type definitions, which expansion removed from it, carried
  // as templates, as the compiled path carries them
  // (buildModuleTypeEnvironment).
  final templateEnv = templates.isEmpty
      ? env
      : buildTypeEnvironment(expanded, ancestorScope: base, typeTemplates: {
          for (final td in iface.typeDefs)
            if (td.isParameterized) td.name: td,
        });
  for (final t in templates.entries) {
    final entry = env.procedures[t.key];
    if (entry == null || !tables.exported.containsKey(t.key)) continue;
    final record = opaqueEntryOf(
        entry: entry,
        entryEnv: env,
        template: t.value,
        templateEnv: templateEnv);
    if (record != null) opaque[t.key] = record;
  }
  return TypeIdentityTables(
    declared: const {},
    exported: tables.exported,
    parametric: tables.parametric,
    unresolved: tables.unresolved,
    opaque: opaque,
  );
}
