/// The alias clauses of the fifth compilation step are type-checked as every
/// clause of the linked program is, each at the declaration of its entry
/// point.
///
/// TGLP 5c943e4, modules.tex, Compilation: "The flat program is the linked
/// program of def:program, and it is the object checked.  Every clause in it
/// satisfies SRSW, as def:glp-program requires of a GLP program, the alias
/// clauses of the fifth step included, and every clause in it is
/// type-checked, an alias clause at the declaration of its entry point."
/// GLP #10 Cowork, 2026-10-23 23:26 UTC (Code's case of 2026-10-04, the
/// transparency of the fifth step's alias clauses).
///
/// The fixture, programs/tests/alias_check, has two entry points of
/// different types, succ/2 over Integer and count/2 over String.  The tests
/// show that each alias clause is in the object the linked check is given,
/// declared there by its entry point's declaration, and that the check
/// refuses an alias clause ill-typed at that declaration: the alias of succ/2
/// retargeted at count's renamed procedure passes an Integer where count
/// consumes a String.
library;

import 'dart:io';

import 'package:glp_runtime/analysis/type_checker/type_ast.dart'
    show TypeRef;
import 'package:glp_runtime/analysis/type_checker/type_checker.dart'
    show TypeCheckResult, checkModule;
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart'
    show primitiveScope;
import 'package:test/test.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  final dir = Directory('../programs/tests/alias_check').absolute.path;
  const self = 'tests/alias_check';

  List<DiscoveredModule> discover() =>
      discoverProgram(dir, rootSelfGlpPath: rootSelf);

  test('loads, and each entry point runs through its alias clause', () async {
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    expect(engine.loadProgram(dir), isTrue);
    final s = await engine.runGoal('succ(1, M)');
    expect(s.succeeded, isTrue, reason: 'Error: ${s.error}');
    expect(s.bindings['M'].toString(), 'Const(2)');
    final c = await engine.runGoal('count("a", N)');
    expect(c.succeeded, isTrue, reason: 'Error: ${c.error}');
    expect(c.bindings['N'].toString(), 'Const(1)');
  });

  test('each alias clause is in the checked object, declared by its entry '
      'point', () {
    final modules = discover();
    final linked = checkedLinkedProgram(modules, rootDir: dir);
    final flat = linkedFlatModule(modules, linked);
    for (final (name, type) in [('succ', 'Integer'), ('count', 'String')]) {
      final alias = flat.procedures.singleWhere((p) => p.name == name);
      expect(alias.clauses, hasLength(1));
      final body = alias.clauses.single.body!;
      expect(body.single.functor, '$self:$name');
      // The alias's own declaration, the entry point's, under its plain name.
      final decls = flat.procDeclarations.where((d) => d.name == name);
      expect(decls, hasLength(1));
      final args = decls.single.argTypes.cast<TypeRef>();
      expect([for (final a in args) a.toString()], ['$type?', 'Integer']);
      // The environment the linked program was checked in declares it so.
      final checked = linked.checkedEnv!.procedures['$name/2']!;
      expect([for (final a in checked.argTypes) a.toString()],
          ['$type?', 'Integer']);
    }
  });

  test('the linked check refuses an alias clause ill-typed at its entry '
      "point's declaration", () {
    final modules = discover();
    final linked = checkedLinkedProgram(modules, rootDir: dir);
    final flat = linkedFlatModule(modules, linked);

    // The linked check of checkedLinkedProgram, over [procedures] in place
    // of the linked program's.
    TypeCheckResult check(List<Procedure> procedures) => checkModule(
          Module(
            typeDefs: flat.typeDefs,
            procDeclarations: flat.procDeclarations,
            procedures: procedures,
            line: 0,
            column: 0,
          ),
          transformedProcedures: PartialEvaluator()
              .transformDefinedGuards(Program(procedures, 0, 0),
                  scope: primitiveScope())
              .procedures,
          ancestorScope: primitiveScope(),
        );

    // The object as linked checks.
    final linkedVerdict = check(flat.procedures);
    expect(linkedVerdict.isWellTyped, isTrue,
        reason: linkedVerdict.errors.map((e) => e.message).join('\n'));

    // succ's alias, its head threaded at succ(Integer?, Integer) and its
    // body retargeted at count(String?, Integer): V0 is an Integer by succ's
    // declaration and is passed where count consumes a String.
    final retargeted = [
      for (final p in flat.procedures)
        if (p.name == 'succ')
          Procedure(
              'succ',
              2,
              [
                Clause(
                    Atom('succ', [VarTerm('V0', false, 0, 0),
                      VarTerm('V1', true, 0, 0)], 0, 0),
                    body: [
                      Goal('$self:count',
                          [VarTerm('V0', true, 0, 0),
                            VarTerm('V1', false, 0, 0)], 0, 0)
                    ],
                    line: 0,
                    column: 0)
              ],
              0,
              0)
        else
          p
    ];
    final r = check(retargeted);
    expect(r.isWellTyped, isFalse,
        reason: 'the alias clause of succ/2 is checked at succ(Integer?, '
            'Integer), where V0 is an Integer and not a String');
  });
}
