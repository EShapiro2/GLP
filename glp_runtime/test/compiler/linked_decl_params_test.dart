/// A declaration's type-parameter list crosses the module boundary.
///
/// TGLP (modules.tex, "Cross-module type checking"): "Where the imported
/// declaration names type parameters, as an exported one may, the call
/// instantiates them as a local call does: a parameter the importing module
/// holds open stays open across the module boundary and is fixed at the call,
/// the clauses of the called procedure being those of the linked program."
///
/// The linked program is what the check is applied to, so a parameter is held
/// open across the boundary only if the list survives the rename to `M:p` and
/// the entry-point alias.  Until it did, a module whose own load was accepted
/// --- the list being parsed and seen there --- was refused inside a program,
/// its parameter reported as an undefined type of the renamed declaration.
///
/// Both fixtures are their own root: neither names a root-scope type, so the
/// test turns on the parameter list alone and on nothing the root `self.glp`
/// happens to define.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/program_linker.dart';

String _fixture(String name) =>
    Directory('../programs/tests/$name').absolute.path;

List<DiscoveredModule> _modules(String dir) =>
    discoverProgram(dir, rootSelfGlpPath: '$dir/self.glp');

void main() {
  group('a declaration carries its parameter list through the linker', () {
    late String dir;
    late LinkResult linked;

    setUp(() {
      dir = _fixture('param_import_linked');
      linked = linkProgram(_modules(dir), rootDir: dir);
    });

    List<String> paramsOf(List<ProcDecl> ds, String key) =>
        ds.singleWhere((d) => '${d.name}/${d.argTypes.length}' == key).typeParams;

    test('the renamed declaration of the exporting module keeps it', () {
      expect(paramsOf(linked.scopeDeclarations, 'worker:tie/2'), ['Y']);
      expect(paramsOf(linked.checkedDeclarations, 'worker:tie/2'), ['Y']);
    });

    test('the renamed declaration of the importing module keeps it', () {
      expect(
          paramsOf(linked.scopeDeclarations, 'param_import_linked:tie/2'), ['Y']);
      expect(paramsOf(linked.checkedDeclarations, 'param_import_linked:tie/2'),
          ['Y']);
    });

    test('the entry-point alias keeps it', () {
      expect(paramsOf(linked.scopeDeclarations, 'tie/2'), ['Y']);
      expect(paramsOf(linked.checkedDeclarations, 'tie/2'), ['Y']);
    });

    test('a declaration naming no parameter is unchanged', () {
      expect(paramsOf(linked.scopeDeclarations, 'go/2'), isEmpty);
    });

    test('the linked program checks', () {
      expect(() => typeCheckProgram(_modules(dir), rootDir: dir), returnsNormally);
    });
  });

  test('an undefined name outside the list is still refused, by name', () {
    final dir = _fixture('param_import_typo_neg');
    expect(
      () => typeCheckProgram(_modules(dir), rootDir: dir),
      throwsA(predicate((e) {
        final s = e.toString();
        return s.contains('undefined type "Strem"') &&
            s.contains('its type parameters are Y');
      }, 'names Strem and the parameter list it is not in')),
    );
  });
}
