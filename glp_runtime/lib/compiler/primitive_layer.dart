import 'dart:io';

import 'ast.dart';
import 'error.dart';
import '../analysis/type_checker/root_scope.dart' show isReservedConstantName;

/// Load-time enforcement of "Admission to the Primitive Layer"
/// (TGLP appendix-root-self.tex, app:system-mode):
///
///   Rule A — a module that declares `-mode(system)` must be the root self.glp
///            or a module under `programs/system/`; any other location is
///            rejected.
///   Rule B — a module not in system mode may not name a reserved constant: a
///            constant that names a language primitive (kernel predicate /
///            reserved functor), as decided by [isReservedConstantName]. A
///            `_`-prefixed constant that names no primitive is not reserved.
///
/// [filePath] is the on-disk path of the module, or null/synthetic for
/// in-memory or engine-embedded sources; Rule A is skipped when no real file
/// backs the module (the embedded system predicates and the root self.glp load
/// without an application path).
void enforcePrimitiveLayer(
    String? filePath, Module module, String? rootSelfGlpPath) {
  if (module.compileMode == CompileMode.system) {
    _checkModeAdmission(filePath, module, rootSelfGlpPath);
  } else {
    _checkNoReservedNames(module);
  }
}

void _checkModeAdmission(
    String? filePath, Module module, String? rootSelfGlpPath) {
  // Only real on-disk files are location-constrained. In-memory sources and the
  // engine-embedded system predicates carry no application path and are part of
  // the primitive layer by construction. Without the root self.glp path the
  // location cannot be decided, so Rule A is not enforced.
  if (filePath == null || rootSelfGlpPath == null ||
      !File(filePath).existsSync()) {
    return;
  }

  final f = File(filePath).absolute.path;
  final root = File(rootSelfGlpPath).absolute.path;
  if (f == root) return; // the root self.glp

  final systemDir = File('${File(rootSelfGlpPath).parent.path}'
          '${Platform.pathSeparator}system')
      .absolute
      .path;
  if (f.startsWith('$systemDir${Platform.pathSeparator}')) return; // programs/system/**

  throw CompileError(
    "-mode(system) is confined to the primitive layer: only the root self.glp "
    "and modules under programs/system/ may declare it. '$filePath' is an "
    "application module — remove the directive and reach runtime functionality "
    "by calling a programs/system/ export.",
    module.line,
    module.column,
    phase: 'loader',
  );
}

void _checkNoReservedNames(Module module) {
  for (final proc in module.procedures) {
    for (final clause in proc.clauses) {
      _checkName(clause.head.functor, clause.head, module);
      for (final a in clause.head.args) {
        _checkTerm(a, module);
      }
      final guards = clause.guards;
      if (guards != null) {
        for (final g in guards) {
          _checkName(g.predicate, g, module);
          for (final a in g.args) {
            _checkTerm(a, module);
          }
        }
      }
      final body = clause.body;
      if (body != null) {
        for (final goal in body) {
          _checkName(goal.functor, goal, module);
          for (final a in goal.args) {
            _checkTerm(a, module);
          }
        }
      }
    }
  }
}

void _checkTerm(Term term, Module module) {
  if (term is StructTerm) {
    _checkName(term.functor, term, module);
    for (final a in term.args) {
      _checkTerm(a, module);
    }
  } else if (term is ListTerm) {
    if (term.head != null) _checkTerm(term.head!, module);
    if (term.tail != null) _checkTerm(term.tail!, module);
  } else if (term is ConstTerm) {
    final v = term.value;
    if (v is String) _checkName(v, term, module);
  }
}

void _checkName(String name, AstNode at, Module module) {
  if (isReservedConstantName(name)) {
    throw CompileError(
      "Constant '$name' names a language primitive and is reserved for system "
      "use; an application module may not name it. Call a programs/system/ "
      "export instead, or move the code into the primitive layer and declare "
      "-mode(system) (permitted only in the root self.glp and programs/system/).",
      at.line,
      at.column,
      phase: 'loader',
    );
  }
}
