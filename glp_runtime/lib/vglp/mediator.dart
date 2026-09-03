// glp_runtime/lib/vglp/mediator.dart
//
// The mediator's emission.
// Spec: vGLP, sections/elicitation.tex, Definition "Canonical Compilation":
// ⌈M⌉ CONSISTS OF the compiled procedures "together with the mediator, between
// the person channel and the agent ... and the timer", the mediator "being
// generic in A and X and instantiated at the program's".
//
// So the mediator is part of the compiled program, not a library it imports.
// That is forced twice over: the Definition says ⌈M⌉ contains it, and a GLP
// module path resolves from the PROGRAM ROOT downward, so a program rooted at
// programs/cssn/ could not name programs/vglp/ even if it wanted to.
//
// programs/vglp/ is therefore the one generic source, and this is what
// instantiates it: the type parameters A and X of its declarations become the
// compiled program's own answer and context types, and its clauses are carried
// over unchanged — they mention no type at all.

import 'dart:io';

import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../compiler/ast.dart' as ast;
import '../compiler/glp_printer.dart';
import '../analysis/type_checker/type_ast.dart';
import 'types.dart';

/// The generic mediator source, read from [directory] — `programs/vglp/` in the
/// tree.  Its `self.glp` carries the vocabulary and its `med.glp` the clauses.
class MediatorSource {
  final ast.Module vocabulary;
  final ast.Module clauses;

  MediatorSource(this.vocabulary, this.clauses);

  factory MediatorSource.fromDirectory(String directory) {
    final self = File('$directory/self.glp');
    final med = File('$directory/med.glp');
    if (!self.existsSync() || !med.existsSync()) {
      throw StateError(
          'The generic mediator source is not at $directory: the canonical '
          'compilation emits the mediator into every compiled program and '
          'reads it from there (Definition "Canonical Compilation").');
    }
    return MediatorSource(
      Parser(Lexer(self.readAsStringSync()).tokenize()).parseModule(),
      Parser(Lexer(med.readAsStringSync()).tokenize()).parseModule(),
    );
  }
}

/// The mediator instantiated at one program's answer, escrow and context
/// types.
class InstantiatedMediator {
  /// The vocabulary's type definitions, with A, E and X replaced.  `Slot(R)`
  /// keeps its parameter: it is instantiated per slot, at the slot's own
  /// clause's reply type.
  final List<TypeDef> typeDefs;

  /// The mediator's procedure declarations, with A, E and X replaced and
  /// dropped from their type-parameter lists; `abort/3` keeps its R.
  final List<ProcDecl> procDecls;

  /// The mediator's clauses, unchanged: they mention no type.
  final List<ast.Procedure> procedures;

  InstantiatedMediator(this.typeDefs, this.procDecls, this.procedures);
}

/// Instantiate the generic mediator at the compiled program's types.
///
/// The generic source is parameterised in the answer type A, the escrow type
/// E and the context type X; the compiled program defines [answerTypeName],
/// [escrowTypeName] and [contextTypeName], and every occurrence of a parameter
/// becomes the corresponding one.  The vocabulary's types parameterised in
/// these alone — `AgentMsg`, `Card`, `UserAnswer`, `PendingEntry`,
/// `PendingList` — become monomorphic; `Slot(R)` is generic in a reply type
/// and stays so, instantiated per slot by the compiled declarations, as does
/// `abort/3`, which takes any slot.
InstantiatedMediator instantiate(MediatorSource source) {
  const binding = <String, String>{
    'A': answerTypeName,
    'E': escrowTypeName,
    'X': contextTypeName,
  };
  // A vocabulary type parameterised in A, E and X alone becomes monomorphic,
  // so a reference to it loses its arguments along with the definition's
  // parameter list; one with a parameter of its own keeps that parameter.
  final instantiated = <String>{
    for (final td in source.vocabulary.typeDefs)
      if (td.isParameterized &&
          td.typeParams.every(binding.containsKey))
        td.name
  };
  List<String> remaining(List<String> params) =>
      [for (final p in params) if (!binding.containsKey(p)) p];

  final typeDefs = [
    for (final td in source.vocabulary.typeDefs)
      TypeDef(td.name,
          [for (final a in td.alternatives) _bindExpr(a, binding, instantiated)],
          td.line, td.column,
          typeParams: remaining(td.typeParams))
  ];

  final procDecls = [
    for (final d in source.clauses.procDeclarations)
      ProcDecl(
        d.name,
        [for (final t in d.argTypes) _bindExpr(t, binding, instantiated)],
        d.line,
        d.column,
        typeParams: remaining(d.typeParams),
        isBuiltin: d.isBuiltin,
        exported: d.exported,
        imported: d.imported,
        modulePath: d.modulePath,
      )
  ];

  return InstantiatedMediator(typeDefs, procDecls, source.clauses.procedures);
}

/// Replace the type parameters throughout a type expression, and drop the
/// arguments of a reference to one of the vocabulary's own parameterised
/// types, which the instantiation makes monomorphic.
TypeExpr _bindExpr(
    TypeExpr e, Map<String, String> binding, Set<String> instantiated) {
  if (e is TypeRef) {
    final name = binding[e.name] ?? e.name;
    return TypeRef(name, e.line, e.column,
        isInput: e.isInput,
        typeArgs: instantiated.contains(e.name)
            ? const []
            : [for (final a in e.typeArgs) _bindExpr(a, binding, instantiated)]);
  }
  if (e is StructAlt) {
    return StructAlt(e.functor,
        [for (final a in e.args) _bindExpr(a, binding, instantiated)],
        e.line, e.column);
  }
  if (e is ListConsAlt) {
    return ListConsAlt(_bindExpr(e.head, binding, instantiated),
        _bindExpr(e.tail, binding, instantiated), e.line, e.column);
  }
  if (e is DiffListAlt) {
    return DiffListAlt(_bindExpr(e.content, binding, instantiated),
        _bindExpr(e.hole, binding, instantiated), e.line, e.column);
  }
  return e;
}

/// A type expression as GLP source.  A type read off the checker's environment
/// may carry an EXPANDED name, `Stream<Coin>`, which is the environment's
/// internal name for an instantiation and is not source syntax; it is printed
/// as the instantiation it stands for, `Stream(Coin)`.
String typeSource(TypeExpr e) {
  if (e is TypeRef) {
    final name = _sourceName(e.name);
    final args = e.typeArgs.isEmpty
        ? ''
        : '(${e.typeArgs.map(typeSource).join(', ')})';
    return e.isInput ? '$name$args?' : '$name$args';
  }
  if (e is StructAlt) {
    return '${e.functor}(${e.args.map(typeSource).join(', ')})';
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

/// A display declaration as GLP source, carried into the compiled program
/// unchanged (vGLP, Definition "Display Declaration, Default Display").  Atoms
/// are printed bare and string literals quoted, as the source had them.
String printDisplayDecl(ast.DisplayDecl d) {
  final items = d.items
      .map((i) => i.args.isEmpty
          ? i.name
          : '${i.name}(${i.args.map(_termSource).join(', ')})')
      .join(', ');
  if (d.isClauseForm) {
    final g = d.guard!;
    final positions = <String>[
      for (final q in g.question)
        q.writer == null
            ? _termSource(q.value!)
            : (q.value == null || q.value is ast.UnderscoreTerm
                ? q.writer!.name
                : '${q.writer!.name}=${_termSource(q.value!)}'),
      for (final c in g.context) '${c.name}?',
    ];
    return 'display ${d.predicate} *(${positions.join(', ')}) : $items.';
  }
  return 'display ${_termSource(d.pattern!)} : $items.';
}

String _termSource(ast.Term t) {
  if (t is ast.VarTerm) return t.isReader ? '${t.name}?' : t.name;
  if (t is ast.UnderscoreTerm) return '_';
  if (t is ast.ConstTerm) {
    final v = t.value;
    if (v is String) return _atomSource(v);
    return '$v';
  }
  if (t is ast.StructTerm) {
    return '${t.functor}(${t.args.map(_termSource).join(', ')})';
  }
  return GlpPrinter().printTerm(t);
}

/// An atom as source: bare where the lexer reads it back as one, else quoted
/// with single quotes --- `label('Send a message')`; a string literal keeps
/// the double quotes its value carries.
String _atomSource(String v) {
  if (v.startsWith('"')) return v;
  if (RegExp(r'^[a-z][A-Za-z0-9_]*$').hasMatch(v)) return v;
  final escaped = v
      .replaceAll('\\', '\\\\')
      .replaceAll("'", "\\'")
      .replaceAll('\n', '\\n')
      .replaceAll('\t', '\\t');
  return "'$escaped'";
}

/// The mediator's clauses as GLP source.
String printProcedures(List<ast.Procedure> procedures) {
  final printer = GlpPrinter();
  final buffer = StringBuffer();
  for (final p in procedures) {
    for (final c in p.clauses) {
      buffer.writeln(printer.printClause(c));
    }
    buffer.writeln();
  }
  return buffer.toString();
}
