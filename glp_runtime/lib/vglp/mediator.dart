// glp_runtime/lib/vglp/mediator.dart
//
// A type expression and a type definition as GLP source, as the canonical
// compilation prints them (canonical.dart, constructs.dart).
// Spec: vGLP, Section 6, "The Implementation: Compiling vGLP onto GLP".
//
// The file held the old compilation's mediator, the generic source
// programs/vglp/med.glp instantiated at a compiled program's answer, escrow
// and context types, which the paper replaced by the dispatcher and the
// construct processes on 2026-09-23; that half is deleted (vGLP #22's task,
// Code_inbox 2026-10-31 15:20 UTC), and git holds it.

import '../compiler/glp_printer.dart' show constantSource, functorNameSource;
import '../analysis/type_checker/type_ast.dart';

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

/// A type definition as GLP source.
String printTypeDef(TypeDef d) {
  final params = d.typeParams.isEmpty ? '' : '(${d.typeParams.join(', ')})';
  return '${d.name}$params ::= ${d.alternatives.map(typeSource).join(' ; ')}.';
}
