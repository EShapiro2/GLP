// glp_runtime/lib/vglp/type_walk.dart
//
// The one structural walk over a type expression --- TypeRef, StructAlt,
// ListConsAlt, DiffListAlt --- that the canonical compilation's rewriters
// share (canonical.dart _renameTypeNames, dispatcher.dart _retype,
// constructs.dart _subst), each passing its own rule for a TypeRef.

import '../analysis/type_checker/type_ast.dart';

/// [t] with [onRef] applied at each type reference, a structure, a list cell
/// and a difference list rebuilt over their rewritten positions --- the same
/// object where every position is the same object, so that a rewriter that
/// changes nothing returns [t] itself --- and any other node [t] itself.  A
/// type reference is handed to [onRef] with its type arguments as they are,
/// so a rewriter that descends into them does so itself.
TypeExpr mapTypeRefs(TypeExpr t, TypeExpr Function(TypeRef ref) onRef) {
  if (t is TypeRef) return onRef(t);
  if (t is StructAlt) {
    final args = [for (final a in t.args) mapTypeRefs(a, onRef)];
    var same = true;
    for (var i = 0; same && i < args.length; i++) {
      same = identical(args[i], t.args[i]);
    }
    return same ? t : StructAlt(t.functor, args, t.line, t.column);
  }
  if (t is ListConsAlt) {
    final h = mapTypeRefs(t.head, onRef);
    final tl = mapTypeRefs(t.tail, onRef);
    return identical(h, t.head) && identical(tl, t.tail)
        ? t
        : ListConsAlt(h, tl, t.line, t.column);
  }
  if (t is DiffListAlt) {
    final c = mapTypeRefs(t.content, onRef);
    final h = mapTypeRefs(t.hole, onRef);
    return identical(c, t.content) && identical(h, t.hole)
        ? t
        : DiffListAlt(c, h, t.line, t.column);
  }
  return t;
}
