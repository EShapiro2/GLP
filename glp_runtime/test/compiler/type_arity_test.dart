/// A type is named by its name and its number of parameters, as a procedure is
/// by its name and arity; a type reference resolves to the nearest scope
/// defining a type of that name and that number of parameters.
///
/// GLP #13 Cowork, Code_inbox 2026-11-07 23:40 UTC, item 1, Udi approving:
/// "So the root's Draw(T) is not hidden by sglp's 0-ary Draw.  Bring the
/// linker (_visibleTypeOwners, _renameTypeExpr) to it."  TGLP modules.tex,
/// Compilation, fourth step: "every type reference is resolved the same way,
/// to the renamed type of the nearest scope defining it".
///
/// The fixture, programs/tests/type_arity: its self.glp defines Box(X), box/1.
/// near/self.glp defines a 0-ary Box, box/2, nearer to near/m.glp, which names
/// both, Box(Integer) and Box: until 2026-11-08 the linker keyed the type's
/// owner by its name alone, so near/self.glp's Box took the reference
/// Box(Integer) and the linked program was refused.  hide/self.glp defines a
/// Box(X) of its own, two/2, which hides the enclosing one at hide/m.glp.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  final fixture = Directory('../programs/tests/type_arity').absolute.path;

  group('a nearer 0-ary T does not hide an outer T(X)', () {
    final dir = '$fixture/near';

    test('each reference is owned by the scope defining its name and number '
        'of parameters', () {
      final modules = discoverProgram(dir, rootSelfGlpPath: rootSelf);
      final m = modules.singleWhere((x) => x.filePath.endsWith('/near/m.glp'));
      final owners = typeOwnersByModule(modules)[m.filePath]!;
      expect(owners[typeKey('Box', 0)], 'tests/type_arity/near');
      expect(owners[typeKey('Box', 1)], 'tests/type_arity');
    });

    test('the program loads and runs, box/1 and box/2 each at its own Box',
        () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelf);
      expect(engine.loadProgram(dir), isTrue);
      final run = await engine.runGoal('run(N)');
      expect(run.succeeded, isTrue, reason: 'Error: ${run.error}');
      expect(run.bindings['N'].toString(), 'Const(10)');
    });
  });

  group('a nearer T(X) hides an outer T(X)', () {
    final dir = '$fixture/hide';

    test('the reference is owned by the nearer scope', () {
      final modules = discoverProgram(dir, rootSelfGlpPath: rootSelf);
      final m = modules.singleWhere((x) => x.filePath.endsWith('/hide/m.glp'));
      final owners = typeOwnersByModule(modules)[m.filePath]!;
      expect(owners[typeKey('Box', 1)], 'tests/type_arity/hide');
      expect(owners.containsKey(typeKey('Box', 0)), isFalse);
    });

    test('the program loads and runs at the nearer Box(X), two/2', () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelf);
      expect(engine.loadProgram(dir), isTrue);
      final run = await engine.runGoal('run(N)');
      expect(run.succeeded, isTrue, reason: 'Error: ${run.error}');
      expect(run.bindings['N'].toString(), 'Const(7)');
    });
  });
}
