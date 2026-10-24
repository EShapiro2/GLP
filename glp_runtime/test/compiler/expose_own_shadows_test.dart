/// A procedure an exposing self.glp defines shadows one of the same name and
/// arity it exposes.
///
/// TGLP 91b4739, modules.tex, "The -expose directive": "Shadowing applies as
/// usual: a procedure the self.glp defines shadows one of the same name and
/// arity it exposes; two exposed modules contributing the same name and arity
/// at one level is an error."  GLP #10 Cowork, 2026-10-23 23:42 UTC, answering
/// Code's case of 2026-10-07 09:34 UTC.
///
/// The fixture, programs/tests/expose/own_shadows: its self.glp exposes
/// lib/rates.glp, whose rate(Level?, Integer) gives 10 and 20, and defines
/// rate(Integer?, Integer), its argument plus 100; user.glp, beside it, calls
/// rate(2, N).  Were the exposed rate/2 in scope, both calls would be refused,
/// an Integer standing where it consumes a Level.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  final dir = Directory('../programs/tests/expose/own_shadows').absolute.path;
  const self = 'tests/expose/own_shadows';

  test('loads, and rate/2 is the self.glp\'s in it and in user.glp', () async {
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    expect(engine.loadProgram(dir), isTrue);
    final run = await engine.runGoal('run(N)');
    expect(run.succeeded, isTrue, reason: 'Error: ${run.error}');
    expect(run.bindings['N'].toString(), 'Const(101)');
    final via = await engine.runGoal('via_user(N)');
    expect(via.succeeded, isTrue, reason: 'Error: ${via.error}');
    expect(via.bindings['N'].toString(), 'Const(102)');
  });

  test('both scopes declare rate/2 over Integer, the self.glp\'s', () {
    final modules = discoverProgram(dir, rootSelfGlpPath: rootSelf);
    final user = modules.singleWhere((m) => m.filePath.endsWith('/user.glp'));
    expect(user.ancestorScope.procedures['rate/2']!.argTypes.first.toString(),
        'Integer?');
    final selfGlp = modules.singleWhere(
        (m) => m.filePath.endsWith('/own_shadows/self.glp'));
    expect(selfGlp.ast.procDeclarations
        .singleWhere((d) => d.name == 'rate')
        .argTypes
        .first
        .toString(), 'Integer?');
  });

  test('every call of rate/2 resolves to the self.glp\'s, and the exposed '
      'one is not reached', () {
    final modules = discoverProgram(dir, rootSelfGlpPath: rootSelf);
    final linked = checkedLinkedProgram(modules, rootDir: dir);
    final procs = {for (final p in linked.program.procedures) p.name: p};
    expect(procs['$self:run']!.clauses.single.body!.single.functor,
        '$self:rate');
    expect(procs['$self/user:via']!.clauses.single.body!.single.functor,
        '$self:rate');
    expect(procs.keys, isNot(contains('$self/lib/rates:rate')));
  });
}
