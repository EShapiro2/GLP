/// The type scope of a compiled program as bytes: the type definitions and
/// the procedure declarations its type-identity tables are built from
/// ([TypeIdentityTables.source]), so that the construction that built them
/// builds them again ([typeIdentityTablesForModule]).
///
/// The store keeps the compiled program the agent runs beside its key pair
/// (IGLP, Implementation Notes, "Persistence"), and a restart whose update is
/// refused runs that program again (IGLP, Termination and Restart: "the agent
/// restarts under the version it terminated under").  A compiled program's
/// goals read its tables --- `find_type/2` its declared table, `find_type/3`
/// the opaque instances of its parameterised declarations (TGLP modules.tex,
/// Dynamic Activation) --- and the artefact carries the exports' declarations
/// alone (IGLP, Code Format, "Interface table"), so the store keeps the
/// scope beside it.  The declarations are those of the linked program, under
/// the names the renaming gave them, which are no source text: they are
/// written as the syntax tree, not printed.
///
/// The format: a clen count and each type definition --- its name, its
/// parameters, its alternatives --- then a clen count and each procedure
/// declaration --- its name, its parameters, its argument types, its flags and
/// module path --- for the declarations and then the parameterised templates.
/// A type expression is a tag byte and its parts.
library;

import 'dart:typed_data';

import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/wire/codec.dart';

// Type expression tags.
const int _tRef = 0;
const int _tConst = 1;
const int _tStruct = 2;
const int _tNil = 3;
const int _tCons = 4;
const int _tPrimitive = 5;
const int _tDiffList = 6;

// Constant tags.
const int _cAtom = 0;
const int _cInt = 1;
const int _cReal = 2;

/// Write the type definitions and procedure declarations of [module].
void writeTypeScope(WireWriter w, ast.Module module) {
  w.clen(module.typeDefs.length);
  for (final td in module.typeDefs) {
    w.string(td.name);
    _strings(w, td.typeParams);
    w.clen(td.alternatives.length);
    for (final alt in td.alternatives) {
      _expr(w, alt);
    }
  }
  _decls(w, module.procDeclarations);
  _decls(w, module.paramProcDecls);
}

/// Read what [writeTypeScope] wrote, as a module of no clauses.
ast.Module readTypeScope(WireReader r) {
  final typeDefs = <TypeDef>[];
  for (var i = r.clen(); i > 0; i--) {
    final name = r.string();
    final params = _readStrings(r);
    final alts = [for (var j = r.clen(); j > 0; j--) _readExpr(r)];
    typeDefs.add(TypeDef(name, alts, 0, 0, typeParams: params));
  }
  final decls = _readDecls(r);
  final templates = _readDecls(r);
  return ast.Module(
    typeDefs: typeDefs,
    procDeclarations: decls,
    paramProcDecls: templates,
    line: 0,
    column: 0,
  );
}

/// [module]'s type scope as bytes ([writeTypeScope]).
Uint8List typeScopeBytes(ast.Module module) {
  final w = WireWriter();
  writeTypeScope(w, module);
  return w.toBytes();
}

/// The module of [bytes] ([readTypeScope]), which hold nothing more.
ast.Module typeScopeOf(Uint8List bytes) {
  final r = WireReader(bytes);
  final module = readTypeScope(r);
  if (!r.atEnd) {
    throw WireFormatException('the type scope has bytes past its end');
  }
  return module;
}

void _strings(WireWriter w, List<String> xs) {
  w.clen(xs.length);
  for (final x in xs) {
    w.string(x);
  }
}

List<String> _readStrings(WireReader r) =>
    [for (var i = r.clen(); i > 0; i--) r.string()];

void _decls(WireWriter w, List<ProcDecl> decls) {
  w.clen(decls.length);
  for (final d in decls) {
    w.string(d.name);
    _strings(w, d.typeParams);
    w.clen(d.argTypes.length);
    for (final t in d.argTypes) {
      _expr(w, t);
    }
    final path = d.modulePath;
    w.u8((d.isBuiltin ? 1 : 0) |
        (d.exported ? 2 : 0) |
        (d.imported ? 4 : 0) |
        (path != null ? 8 : 0));
    if (path != null) w.string(path);
  }
}

List<ProcDecl> _readDecls(WireReader r) {
  final out = <ProcDecl>[];
  for (var i = r.clen(); i > 0; i--) {
    final name = r.string();
    final params = _readStrings(r);
    final args = [for (var j = r.clen(); j > 0; j--) _readExpr(r)];
    final flags = r.u8();
    final path = (flags & 8) != 0 ? r.string() : null;
    out.add(ProcDecl(name, args, 0, 0,
        typeParams: params,
        isBuiltin: (flags & 1) != 0,
        exported: (flags & 2) != 0,
        imported: (flags & 4) != 0,
        modulePath: path));
  }
  return out;
}

void _expr(WireWriter w, TypeExpr e) {
  switch (e) {
    case TypeRef():
      w.u8(_tRef);
      w.string(e.name);
      w.u8(e.isInput ? 1 : 0);
      w.clen(e.typeArgs.length);
      for (final a in e.typeArgs) {
        _expr(w, a);
      }
    case ConstantAlt(:final value):
      w.u8(_tConst);
      if (value is String) {
        w.u8(_cAtom);
        w.string(value);
      } else if (value is int) {
        w.u8(_cInt);
        w.i64(value);
      } else if (value is double) {
        w.u8(_cReal);
        w.f64(value);
      } else {
        throw ArgumentError('a constant alternative of kind '
            '${value.runtimeType} has no written form');
      }
    case StructAlt():
      w.u8(_tStruct);
      w.string(e.functor);
      w.clen(e.args.length);
      for (final a in e.args) {
        _expr(w, a);
      }
    case ListNilAlt():
      w.u8(_tNil);
    case ListConsAlt():
      w.u8(_tCons);
      _expr(w, e.head);
      _expr(w, e.tail);
    case PrimitiveModeAlt():
      w.u8(_tPrimitive);
      w.u8(e.isInput ? 1 : 0);
    case DiffListAlt():
      w.u8(_tDiffList);
      _expr(w, e.content);
      _expr(w, e.hole);
    default:
      throw ArgumentError('a type expression of kind ${e.runtimeType} has no '
          'written form');
  }
}

TypeExpr _readExpr(WireReader r) {
  switch (r.u8()) {
    case _tRef:
      final name = r.string();
      final input = r.u8() == 1;
      final args = [for (var i = r.clen(); i > 0; i--) _readExpr(r)];
      return TypeRef(name, 0, 0, isInput: input, typeArgs: args);
    case _tConst:
      final Object value = switch (r.u8()) {
        _cAtom => r.string(),
        _cInt => r.i64(),
        _cReal => r.f64(),
        final t => throw WireFormatException('a constant alternative has the '
            'bad tag $t'),
      };
      return ConstantAlt(value, 0, 0);
    case _tStruct:
      final functor = r.string();
      return StructAlt(
          functor, [for (var i = r.clen(); i > 0; i--) _readExpr(r)], 0, 0);
    case _tNil:
      return ListNilAlt(0, 0);
    case _tCons:
      final head = _readExpr(r);
      return ListConsAlt(head, _readExpr(r), 0, 0);
    case _tPrimitive:
      return PrimitiveModeAlt(r.u8() == 1, 0, 0);
    case _tDiffList:
      final content = _readExpr(r);
      return DiffListAlt(content, _readExpr(r), 0, 0);
    case final t:
      throw WireFormatException('a type expression has the bad tag $t');
  }
}
