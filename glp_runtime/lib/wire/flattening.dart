/// Deterministic flattening + source identity h(M) (D3 wire format, §6).
///
/// Normative source: the IGLP paper appendix `app:wire-format`, §wf-flattening.
/// The flattened source of a project — the preimage of h(M) — is the canonical
/// print of the linked, pruned program: discovered, type-checked, renamed,
/// resolved, and pruned to the procedures reachable from the root's exported
/// entry points (the DCE step), then printed canonically. h(M) = SHA-256 of the
/// print bytes.
///
/// Canonical print: UTF-8, LF line ends, no comments, one declaration or clause
/// per line. Order: (i) the project's type definitions, lexicographically by
/// name; (ii) the exported procedure declarations, lexicographically by name
/// then arity; (iii) the procedures, lexicographically by renamed name then
/// arity — within a procedure, clauses keep source order (semantic). Procedure
/// order is not semantic and is fixed by sorting.
///
/// Note: root-scope (`self.glp`) types and procedures are shared runtime
/// infrastructure, not the contract's source, and are not part of h(M) — only
/// the project's own definitions are printed. Type-definition reachability
/// pruning is a refinement not yet applied: the project's type definitions are
/// all included (procedure reachability — the DCE — is applied).
library;

import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';
import 'package:crypto/crypto.dart' as crypto;
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/glp_printer.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';

/// The canonical print of a linked, pruned program (§6).
String canonicalPrint({
  required Program program,
  required List<ProcDecl> procDeclarations,
  required List<TypeDef> typeDefs,
}) {
  final lines = <String>[];

  // (i) Type definitions, lexicographically by name.
  final tds = [...typeDefs]..sort((a, b) => a.name.compareTo(b.name));
  for (final td in tds) {
    lines.add(_printTypeDef(td));
  }

  // (ii) Exported procedure declarations, lexicographically by name then arity.
  final exported = procDeclarations.where((d) => d.exported).toList()
    ..sort((a, b) {
      final c = a.name.compareTo(b.name);
      return c != 0 ? c : a.arity.compareTo(b.arity);
    });
  for (final d in exported) {
    lines.add(_printProcDecl(d));
  }

  // (iii) Procedures, lexicographically by renamed name then arity; clauses in
  // source order.
  final printer = GlpPrinter();
  final procs = [...program.procedures]
    ..sort((a, b) {
      final c = a.name.compareTo(b.name);
      return c != 0 ? c : a.arity.compareTo(b.arity);
    });
  for (final p in procs) {
    for (final clause in p.clauses) {
      lines.add(printer.printClause(clause));
    }
  }

  // LF line ends, one declaration/clause per line, trailing newline.
  return '${lines.join('\n')}\n';
}

/// SHA-256 of the canonical print bytes — the source identity h(M).
Uint8List hashOfPrint(String canonical) =>
    Uint8List.fromList(crypto.sha256.convert(utf8.encode(canonical)).bytes);

/// Flatten a project on disk: discover, type-check, link, prune (DCE), print
/// canonically, and hash. Returns the canonical print and h(M).
({String print, Uint8List hM}) flattenProject(
  String projectDir, {
  required String rootSelfGlpPath,
}) {
  // Install the root scope from the file we were given, as GlpEngine does at
  // construction. Without this, buildRootScopeEnvironment() falls back to the
  // bare Dart rootScopeTypes, which NAMES the root self.glp's derived types but
  // does not DEFINE them, so a module using one in a type definition — as
  // programs/system/mad_predicates.glp uses Constant in NetMsg — fails to check
  // with "Unresolved type". The partial evaluator needs the same source.
  final rootSelfFile = File(rootSelfGlpPath);
  if (rootSelfFile.existsSync()) {
    final rootSource = rootSelfFile.readAsStringSync();
    setRootScopeUnitClauseSource(rootSource);
    setRootScopeEnvironmentSource(rootSource);
  }

  final modules =
      discoverProgram(projectDir, rootSelfGlpPath: rootSelfGlpPath);
  // checkedLinkedProgram type-checks and applies the five linking steps,
  // including step-5 DCE, so program.procedures is the reachable set.
  final linked = checkedLinkedProgram(modules, rootDir: projectDir);
  // The project's own type definitions (root-scope types are ambient and not
  // part of h(M)).
  final typeDefs = <String, TypeDef>{};
  for (final mod in modules) {
    for (final td in mod.ast.typeDefs) {
      typeDefs.putIfAbsent(td.name, () => td);
    }
  }
  final print = canonicalPrint(
    program: linked.program,
    procDeclarations: linked.procDeclarations,
    typeDefs: typeDefs.values.toList(),
  );
  return (print: print, hM: hashOfPrint(print));
}

String _printTypeDef(TypeDef td) {
  final params =
      td.typeParams.isNotEmpty ? '(${td.typeParams.join(', ')})' : '';
  final alts = td.alternatives.map((a) => a.toString()).join(' ; ');
  return '${td.name}$params ::= $alts.';
}

String _printProcDecl(ProcDecl d) {
  final args = d.argTypes.map((t) => t.toString()).join(', ');
  final prefix = d.exported ? 'exported ' : '';
  return '${prefix}procedure ${d.name}($args).';
}

/// The declaration text an artefact's interface table carries for one export
/// (§5, interface table: "per export a string name, clen arity, and string
/// declaration text").
///
/// Printed as an `exported procedure` whatever the declaration's own visibility
/// flag says: every entry of the interface table is an export. The linker keeps
/// a single-module program's own declarations bare and unflagged, so the flag is
/// not a reliable witness of what the table already asserts.
String exportDeclarationText(ProcDecl d) {
  final args = d.argTypes.map((t) => t.toString()).join(', ');
  return 'exported procedure ${d.name}($args).';
}

/// The type-definition string an artefact's interface table carries: the type
/// definitions reachable from [exportDecls], in canonical print, one per line,
/// lexicographically by type name (§5, interface table; §6 order (i)).
///
/// Reachability is the transitive closure over the exported declarations'
/// argument types: a type named in an export, every type named in that type's
/// alternatives, and so on. A name with no definition in [typeDefs] contributes
/// nothing — the primitives (`Integer`, `Real`, `String`, `Module`) and the
/// root-scope types (`Stream`, `Channel`, …) are ambient at every runtime and
/// are not the program's own source, exactly as they are excluded from h(M).
/// A parameterized definition's own parameters are names of that kind too.
String interfaceTypeDefsText({
  required Iterable<ProcDecl> exportDecls,
  required Map<String, TypeDef> typeDefs,
}) {
  final pending = <String>[];
  for (final d in exportDecls) {
    for (final t in d.argTypes) {
      _collectTypeNames(t, pending);
    }
  }

  final reached = <String, TypeDef>{};
  while (pending.isNotEmpty) {
    final name = pending.removeLast();
    if (reached.containsKey(name)) continue;
    final td = typeDefs[name];
    if (td == null) continue; // ambient (primitive or root-scope) or a parameter
    reached[name] = td;
    for (final alt in td.alternatives) {
      _collectTypeNames(alt, pending);
    }
  }

  if (reached.isEmpty) return '';
  final names = reached.keys.toList()..sort();
  return '${names.map((n) => _printTypeDef(reached[n]!)).join('\n')}\n';
}

/// Append every named type referenced anywhere in [expr] to [out].
void _collectTypeNames(TypeExpr expr, List<String> out) {
  if (expr is TypeRef) {
    out.add(expr.name);
    for (final a in expr.typeArgs) {
      _collectTypeNames(a, out);
    }
  } else if (expr is StructAlt) {
    for (final a in expr.args) {
      _collectTypeNames(a, out);
    }
  } else if (expr is ListConsAlt) {
    _collectTypeNames(expr.head, out);
    _collectTypeNames(expr.tail, out);
  } else if (expr is DiffListAlt) {
    _collectTypeNames(expr.content, out);
    _collectTypeNames(expr.hole, out);
  }
  // ConstantAlt, ListNilAlt and PrimitiveModeAlt name no type.
}
