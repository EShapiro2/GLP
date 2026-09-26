// lib/analysis/type_checker/meet.dart
//
// The MEET of two GLP types.
// Specification: TGLP (Moded-Types), sections/typed-glp.tex, \mypara{Type
// checking of guards}:
//
//   "A guard atom that tests the type of a head occurrence narrows it.  Let S be
//    the type of the occurrence and T the type declared for the position it
//    occupies in the guard.  The guard atom is well-typed if the MEET of S and
//    T --- the type whose paths are the paths of both, again a type, since
//    alternatives are distinguished by their top-level functor --- is non-empty,
//    and the occurrence has that meet as its type in the body, where condition 3
//    of Definition (Well-Typed Clause) is applied to it."
//
// The meet is a PATH INTERSECTION and nothing else: a path of the meet is a path
// of S and a path of T, at every position and in both modes.  Alternatives are
// distinguished by their top-level functor, so the intersection is taken
// alternative by alternative, and an alternative present in one type and absent
// from the other contributes nothing.

import 'program_dfa.dart';
import 'type_ast.dart';

/// A type reference in ABSOLUTE mode: the name of a type (null for the wildcard
/// `_`), and whether it is consumed.
///
/// An automaton's own dual flag and a reference's `?` compose by XOR
/// (`program_dfa.dart`, `_resolveTypeExpr`); carrying the composed flag here
/// keeps every comparison below a comparison of the mode a path actually has.
class _Ref {
  final String? name;
  final bool dual;
  const _Ref(this.name, this.dual);

  bool get isWildcard => name == null;
  bool get isPrimitive => name != null && TypeRef.builtins.contains(name);
  String get stateName => name == null ? (dual ? '_?' : '_') : (dual ? '$name?' : name!);

  @override
  bool operator ==(Object other) =>
      other is _Ref && other.name == name && other.dual == dual;
  @override
  int get hashCode => Object.hash(name, dual);
}

/// One alternative of a type, keyed by what distinguishes it: its top-level
/// functor (with arity), the constant it is, or the primitive type a bare
/// type-name alternative reaches.
class _Alt {
  final String key;
  final TypeExpr source; // the alternative as written, for rebuilding
  final List<_Ref> args; // the argument references, in absolute mode
  const _Alt(this.key, this.source, this.args);
}

/// The state of the meet of [a] and [b], or null where the meet is EMPTY --- no
/// path is a path of both.
///
/// Where the meet is one of the two types it is that type's own state and
/// nothing is created.  Otherwise the meet is a type this program does not name,
/// and it is added to [env] and [dfa] under a name built from the two ---
/// additive, and a no-op on a second call for the same pair
/// ([addTypeToProgramDFA]).
DFAState? meetOfTypes(
    DFAState a, DFAState b, ProgramDFA dfa, TypeEnvironment env) {
  if (a == b) return a;
  final ra = _Ref(a.isWildcard ? null : a.baseName, a.isDual);
  final rb = _Ref(b.isWildcard ? null : b.baseName, b.isDual);
  final m = _meetRef(ra, rb, env, <String>{});
  if (m == null) return null;
  if (m is _Ref) {
    return dfa.states[m.stateName] ??
        (m.isWildcard ? dfa.states[m.dual ? '_?' : '_'] : null);
  }
  final def = m as TypeDef;
  if (!env.types.containsKey(def.name)) {
    env.types[def.name] = def;
  }
  addTypeToProgramDFA(dfa, env.types[def.name]!, env.types);
  return dfa.states[ra.dual ? '${def.name}?' : def.name];
}

/// The meet of two type references: a [_Ref] where it is one of them or a type
/// already named, a [TypeDef] where it is a type this program does not name, and
/// null where it is empty.  [assumed] carries the pairs already on the walk, so a
/// pair of recursive types terminates on its own paths.
Object? _meetRef(_Ref a, _Ref b, TypeEnvironment env, Set<String> assumed) {
  if (a.dual != b.dual) return null; // no path of one carries the other's mode
  if (a == b) return a;
  // The wildcard's paths are every path of its mode, so it restricts nothing.
  if (a.isWildcard) return b;
  if (b.isWildcard) return a;

  final pairKey = '${a.stateName}&${b.stateName}';
  if (!assumed.add(pairKey)) return a; // coinductive: already being met

  final altsA = _flatten(a, env, <String>{});
  final altsB = _flatten(b, env, <String>{});
  if (altsA == null || altsB == null) return null;

  final byKeyB = {for (final alt in altsB) alt.key: alt};
  final met = <_Alt>[];
  for (final altA in altsA) {
    final altB = byKeyB[altA.key];
    if (altB != null) {
      final merged = _meetAlt(altA, altB, env, assumed);
      if (merged != null) met.add(merged);
      continue;
    }
    // A constant alternative is a value of a primitive type, so it survives
    // against that primitive: `red` is in the meet of `Colour ::= red ; green`
    // and `String`.
    final prim = _constantPrimitive(altA.key);
    if (prim != null && byKeyB.containsKey('prim:$prim')) {
      met.add(altA);
      continue;
    }
    // And the other way round, where THIS side carries the primitive.
    if (altA.key.startsWith('prim:')) {
      final p = altA.key.substring(5);
      for (final other in altsB) {
        if (_constantPrimitive(other.key) == p) met.add(other);
      }
    }
  }
  if (met.isEmpty) return null;

  if (_sameAlts(met, altsA)) return a;
  if (_sameAlts(met, altsB)) return b;

  final name = 'meet<${a.stateName},${b.stateName}>';
  return TypeDef(name, [for (final alt in met) alt.source], 0, 0);
}

/// The meet of two alternatives that share a key: argument by argument.  Empty
/// where any argument meet is empty.
_Alt? _meetAlt(_Alt a, _Alt b, TypeEnvironment env, Set<String> assumed) {
  if (a.args.isEmpty) return a; // a constant or a primitive: nothing below it
  if (a.args.length != b.args.length) return null;
  final argRefs = <_Ref>[];
  for (var i = 0; i < a.args.length; i++) {
    final m = _meetRef(a.args[i], b.args[i], env, assumed);
    if (m == null) return null;
    if (m is _Ref) {
      argRefs.add(m);
      continue;
    }
    final def = m as TypeDef;
    if (!env.types.containsKey(def.name)) env.types[def.name] = def;
    argRefs.add(_Ref(def.name, a.args[i].dual));
  }
  return _Alt(a.key, _rebuild(a.source, argRefs, a.args), argRefs);
}

/// [source] with each argument replaced by the meet reference at that position.
/// The `?` a rebuilt reference carries is the one that gives it its absolute
/// mode again under the enclosing type's own flag, which is `false` for a type
/// this file builds: every alternative of it is written from the producer's view.
TypeExpr _rebuild(TypeExpr source, List<_Ref> args, List<_Ref> was) {
  TypeExpr at(int i) => args[i].isWildcard
      ? PrimitiveModeAlt(args[i].dual, source.line, source.column)
      : TypeRef(args[i].name!, source.line, source.column,
          isInput: args[i].dual);
  if (source is StructAlt) {
    return StructAlt(source.functor, [for (var i = 0; i < args.length; i++) at(i)],
        source.line, source.column);
  }
  if (source is ListConsAlt) {
    return ListConsAlt(at(0), at(1), source.line, source.column);
  }
  if (source is DiffListAlt) {
    return DiffListAlt(at(0), at(1), source.line, source.column);
  }
  return source; // ConstantAlt, ListNilAlt, TypeRef to a primitive
}

/// The alternatives of [ref]'s type, with every bare type-name alternative
/// flattened into the alternatives of the type it names (TGLP typed-glp.tex: "An
/// alternative in a type definition may also be a type name S, providing type
/// union"), each argument resolved to its absolute mode.  Null where the name
/// has no definition.
List<_Alt>? _flatten(_Ref ref, TypeEnvironment env, Set<String> seen) {
  if (ref.isPrimitive) {
    return [_Alt('prim:${ref.name}', TypeRef(ref.name!, 0, 0, isInput: ref.dual), const [])];
  }
  final def = env.types[ref.name];
  if (def == null) return null;
  if (!seen.add(ref.stateName)) return const [];
  final out = <_Alt>[];
  for (final alt in def.alternatives) {
    if (alt is ConstantAlt) {
      out.add(_Alt('const:${alt.value}', alt, const []));
    } else if (alt is ListNilAlt) {
      out.add(_Alt('const:[]', alt, const []));
    } else if (alt is ListConsAlt) {
      out.add(_Alt('[|]/2', alt,
          [_resolve(alt.head, ref.dual), _resolve(alt.tail, ref.dual)]));
    } else if (alt is DiffListAlt) {
      out.add(_Alt('\\/2', alt,
          [_resolve(alt.content, ref.dual), _resolve(alt.hole, ref.dual)]));
    } else if (alt is StructAlt) {
      out.add(_Alt('${alt.functor}/${alt.args.length}', alt,
          [for (final arg in alt.args) _resolve(arg, ref.dual)]));
    } else if (alt is PrimitiveModeAlt) {
      // A top-level wildcard alternative: every path of its mode.
      out.add(_Alt('wild', alt, const []));
    } else if (alt is TypeRef) {
      final inner = _resolve(alt, ref.dual);
      final nested = _flatten(inner, env, seen);
      if (nested == null) return null;
      out.addAll(nested);
    }
  }
  return out;
}

/// [e], written inside a type whose absolute mode is [dual], as an absolute
/// reference.  The two flags compose by XOR, as `_resolveTypeExpr` composes them.
_Ref _resolve(TypeExpr e, bool dual) {
  if (e is PrimitiveModeAlt) return _Ref(null, e.isInput != dual);
  if (e is TypeRef) return _Ref(e.name, e.isInput != dual);
  return const _Ref(null, false);
}

/// The primitive type a constant alternative is a value of, or null where the
/// key is not a constant.  `[]` and every unquoted constant are strings --- GLP
/// has one representation for a quoted string and an unquoted constant.
String? _constantPrimitive(String key) {
  if (!key.startsWith('const:')) return null;
  final v = key.substring(6);
  if (int.tryParse(v) != null) return 'Integer';
  if (double.tryParse(v) != null) return 'Real';
  return 'String';
}

bool _sameAlts(List<_Alt> a, List<_Alt> b) {
  if (a.length != b.length) return false;
  final ka = {for (final x in a) x.key};
  final kb = {for (final x in b) x.key};
  if (ka.length != kb.length || !ka.containsAll(kb)) return false;
  for (final x in a) {
    final y = b.firstWhere((z) => z.key == x.key);
    if (!sameTypeExpr(x.source, y.source)) return false;
  }
  return true;
}
