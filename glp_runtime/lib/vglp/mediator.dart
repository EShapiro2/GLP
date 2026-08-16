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

/// The mediator instantiated at one program's answer and context types.
class InstantiatedMediator {
  /// The vocabulary's type definitions, with A and X replaced.
  final List<TypeDef> typeDefs;

  /// The mediator's procedure declarations, with A and X replaced and their
  /// type-parameter lists dropped: instantiated, they are monomorphic.
  final List<ProcDecl> procDecls;

  /// The mediator's clauses, unchanged: they mention no type.
  final List<ast.Procedure> procedures;

  InstantiatedMediator(this.typeDefs, this.procDecls, this.procedures);
}

/// Instantiate the generic mediator at the compiled program's types.
///
/// The generic source is parameterised in the answer type and the context
/// type; the compiled program defines [answerTypeName] and [contextTypeName],
/// and every occurrence of a parameter becomes the corresponding one.  The
/// mediator's own types — `Reply`, `Slot`, `AgentMsg`, `Card`, `UserAnswer`,
/// `Escrow`, `PendingEntry`, `PendingList` — are parameterised in the same way
/// and are instantiated with them.
InstantiatedMediator instantiate(MediatorSource source) {
  // The parameters, in the order the generic declarations name them: the
  // answer type first and the context type second, which is how
  // programs/vglp/self.glp writes every parameterised definition.
  const answerParam = 'A';
  const contextParam = 'X';
  final binding = <String, String>{
    answerParam: answerTypeName,
    contextParam: contextTypeName,
  };

  final typeDefs = [
    for (final td in source.vocabulary.typeDefs)
      TypeDef(td.name, [for (final a in td.alternatives) _bindExpr(a, binding)],
          td.line, td.column)
      // The parameter list goes: the instantiated definition is monomorphic.
  ];

  final procDecls = [
    for (final d in source.clauses.procDeclarations)
      ProcDecl(
        d.name,
        [for (final t in d.argTypes) _bindExpr(t, binding)],
        d.line,
        d.column,
        isBuiltin: d.isBuiltin,
        exported: d.exported,
        imported: d.imported,
        modulePath: d.modulePath,
      )
  ];

  return InstantiatedMediator(typeDefs, procDecls, source.clauses.procedures);
}

/// Replace the type parameters throughout a type expression.
TypeExpr _bindExpr(TypeExpr e, Map<String, String> binding) {
  if (e is TypeRef) {
    final name = binding[e.name] ?? e.name;
    return TypeRef(name, e.line, e.column,
        isInput: e.isInput,
        typeArgs: [for (final a in e.typeArgs) _bindExpr(a, binding)]);
  }
  if (e is StructAlt) {
    return StructAlt(e.functor,
        [for (final a in e.args) _bindExpr(a, binding)], e.line, e.column);
  }
  if (e is ListConsAlt) {
    return ListConsAlt(_bindExpr(e.head, binding), _bindExpr(e.tail, binding),
        e.line, e.column);
  }
  if (e is DiffListAlt) {
    return DiffListAlt(_bindExpr(e.content, binding),
        _bindExpr(e.hole, binding), e.line, e.column);
  }
  return e;
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
      '(${d.argTypes.map((t) => t.toString()).join(', ')}).';
}

/// A type definition as GLP source.
String printTypeDef(TypeDef d) {
  final params = d.typeParams.isEmpty ? '' : '(${d.typeParams.join(', ')})';
  return '${d.name}$params ::= ${d.alternatives.join(' ; ')}.';
}

/// A display declaration as GLP source, carried into the compiled program
/// unchanged (vGLP, Definition "Display Declaration, Default Display").
String printDisplayDecl(ast.DisplayDecl d) => d.toString();

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
