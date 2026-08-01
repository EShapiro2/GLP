import 'dart:io';

import 'ast.dart';
import 'error.dart';

/// Load-time enforcement of "Admission to the Primitive Layer"
/// (TGLP appendix-root-self.tex, app:system-mode):
///
///   Rule A — a module that declares `-mode(system)` must be the root self.glp
///            or a module under `programs/system/`; any other location is
///            rejected.
///   Rule B — a module not in system mode may neither define nor call a
///            procedure whose name is a quoted underscore-prefixed constant.
///            The restriction is on names in CALL POSITION only: a constant
///            with that prefix is unrestricted as data — as a message tag, or
///            as a member of a type union.
///
/// Rule B tests the prefix and the position, not a list of names. It was a list
/// until 2026-07-31, and the list is what let twenty-nine of the thirty-eight
/// registered kernels fall outside the rule. The prefix is a sound test on its
/// own because an unquoted name beginning with an underscore lexes as an
/// anonymous variable (GLP-Spec appendix-guards.tex, "Naming and admission of
/// body kernels"), so a functor that begins with one was necessarily written
/// quoted.
///
/// Nothing reserves a functor in data position. A construction ban there is
/// bypassable through `=..` anyway, and the forgery it would address is a
/// runtime check on kernels taking a global name, which is IGLP's
/// (GLP-Spec, 2026-07-31).
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

/// Rule B: no definition and no call, anywhere in the module, of a procedure
/// whose name is a quoted underscore-prefixed constant.
///
/// The positions checked are exactly the ones the rule names. A procedure
/// declaration and a clause head are definition position; a guard predicate and
/// a body goal are call position. Term arguments are data and are not checked —
/// that is the whole of the 2026-07-31 narrowing.
///
/// The declaration is checked as well as the head because `builtinProcedures`
/// now lists every kernel, so the parser admits a clause-less declaration of one
/// in any module; without this a user module could declare `'_add'/3` and never
/// be caught.
void _checkNoReservedNames(Module module) {
  for (final decl in module.procDeclarations) {
    _checkName(decl.name, decl.line, decl.column, 'declares');
  }
  for (final proc in module.procedures) {
    for (final clause in proc.clauses) {
      _checkName(clause.head.functor, clause.head.line, clause.head.column,
          'defines');
      for (final g in clause.guards ?? const <Guard>[]) {
        _checkName(g.predicate, g.line, g.column, 'calls');
      }
      for (final goal in clause.body ?? const <Goal>[]) {
        _checkGoal(goal);
      }
    }
  }
}

/// A goal's called name, through the wrappers that hide it: `RemoteGoal`'s own
/// functor is `#` and `SpawnGoal`'s is `@`, so checking the wrapper alone would
/// miss `m # '_add'(...)` and `'_add'(...)@a`.
void _checkGoal(Goal goal) {
  if (goal is RemoteGoal) {
    _checkGoal(goal.goal);
    return;
  }
  if (goal is SpawnGoal) {
    _checkGoal(goal.innerGoal);
    return;
  }
  _checkName(goal.functor, goal.line, goal.column, 'calls');
}

/// True if [name] is admitted only to the primitive layer. An unquoted name
/// beginning with an underscore is an anonymous variable, so a procedure name
/// that begins with one was written as a quoted constant.
bool _isPrimitiveLayerName(String name) => name.startsWith('_');

void _checkName(String name, int line, int column, String what) {
  if (!_isPrimitiveLayerName(name)) return;
  throw CompileError(
    "'$name' is a primitive-layer procedure name: a module that does not "
    "declare -mode(system) neither defines nor calls a procedure whose name is "
    "a quoted underscore-prefixed constant, and this module $what one. Call a "
    "programs/system/ export instead, or move the code into the primitive layer "
    "and declare -mode(system) (permitted only in the root self.glp and "
    "programs/system/). The prefix is unrestricted as data — as a message tag, "
    "or as a member of a type union.",
    line,
    column,
    phase: 'loader',
  );
}
