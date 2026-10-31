// glp_runtime/lib/vglp/type_source.dart
//
// What the two compilations of this directory share over type expressions:
// the walk that rewrites a type expression at its type references, and the
// printing of a type expression, a type definition and a procedure
// declaration as GLP source.

import '../compiler/glp_printer.dart';
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

/// A type expression as GLP source.  A type read off the checker's environment
/// may carry an EXPANDED name, `Stream<Coin>`, which is the environment's
/// internal name for an instantiation and is not source syntax; it is printed
/// as the instantiation it stands for, `Stream(Coin)`.  A constant alternative
/// and a functor are printed by GLP's printer (glp_printer.dart,
/// [constantSource] and [functorNameSource]), a string in its one canonical
/// print (GLP-Spec appendix-lp.tex, Definition "Logic Programs Syntax").
String typeSource(TypeExpr e) {
  if (e is TypeRef) {
    final name = _sourceName(e.name);
    final args = e.typeArgs.isEmpty
        ? ''
        : '(${e.typeArgs.map(typeSource).join(', ')})';
    return e.isInput ? '$name$args?' : '$name$args';
  }
  if (e is ConstantAlt) return constantSource(e.value);
  if (e is StructAlt) {
    return '${functorNameSource(e.functor)}'
        '(${e.args.map(typeSource).join(', ')})';
  }
  if (e is ListConsAlt) {
    return '[${typeSource(e.head)} | ${typeSource(e.tail)}]';
  }
  if (e is DiffListAlt) {
    return '${typeSource(e.content)} \\ ${typeSource(e.hole)}';
  }
  return e.toString();
}

/// `Stream<Coin>` to `Stream(Coin)`, nested instantiations included.
String _sourceName(String name) {
  final lt = name.indexOf('<');
  if (lt < 0 || !name.endsWith('>')) return name;
  final inner = name.substring(lt + 1, name.length - 1);
  final args = <String>[];
  var depth = 0;
  var start = 0;
  for (var i = 0; i < inner.length; i++) {
    final c = inner[i];
    if (c == '<' || c == '(') depth++;
    if (c == '>' || c == ')') depth--;
    if (c == ',' && depth == 0) {
      args.add(inner.substring(start, i).trim());
      start = i + 1;
    }
  }
  args.add(inner.substring(start).trim());
  return '${name.substring(0, lt)}(${args.map(_sourceName).join(', ')})';
}

// ---------------------------------------------------------------------------
// Printing
// ---------------------------------------------------------------------------

/// A procedure declaration as GLP source.  `exported` is kept: a compiled
/// program's entry point is what the root self.glp exports, and the mediator's
/// `med` is spliced into the initial goal rather than called across a module
/// boundary, but the flag costs nothing and keeps the emitted source the shape
/// the generic one had.
String printProcDecl(ProcDecl d) {
  final prefix = d.exported ? 'exported ' : (d.imported ? 'imported ' : '');
  final params = d.typeParams.isEmpty ? '' : '(${d.typeParams.join(', ')})';
  return '${prefix}procedure$params ${d.name}'
      '(${d.argTypes.map(typeSource).join(', ')}).';
}

/// A type definition as GLP source.
String printTypeDef(TypeDef d) {
  final params = d.typeParams.isEmpty ? '' : '(${d.typeParams.join(', ')})';
  return '${d.name}$params ::= ${d.alternatives.map(typeSource).join(' ; ')}.';
}
