/// A program checked once in a process is not checked again while its
/// sources are unchanged (GlpEngine._checked): a second engine loading it
/// runs the same, and a program whose module has changed since is checked
/// again --- a change that makes it ill-typed is refused, not served from
/// what was checked before.
///
/// The program is a copy of programs/tests/opaque_two (its guest under it),
/// made under programs/tests for the scope and removed after.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart' show TypeIdentityTables;
import 'package:glp_runtime/engine/glp_engine.dart';

const _root = '../programs/self.glp';

void main() {
  late Directory copy;

  setUp(() {
    copy = Directory('../programs/tests/checked_cache_$pid')..createSync();
    File('../programs/tests/opaque_two/self.glp').copySync('${copy.path}/self.glp');
    Directory('${copy.path}/guest').createSync();
    File('../programs/tests/opaque_two/guest/self.glp').copySync('${copy.path}/guest/self.glp');
  });
  tearDown(() => copy.deleteSync(recursive: true));

  test('loaded again unchanged, the same module; changed, checked again', () {
    final dir = Directory('${copy.path}/guest').absolute.path;
    final first = (GlpEngine(rootSelfGlpPath: _root)..loadProgram(dir)).appModule!;
    final second = (GlpEngine(rootSelfGlpPath: _root)..loadProgram(dir)).appModule!;
    expect(second.name, first.name);
    expect((second.declaredTypes! as TypeIdentityTables).exported.keys,
        (first.declaredTypes! as TypeIdentityTables).exported.keys);

    // An ill-typed clause added: the next load checks the program again and
    // refuses it.
    final f = File('$dir/self.glp');
    f.writeAsStringSync('${f.readAsStringSync()}t(1, 2) :- undefined_thing(3).\n');
    expect(() => GlpEngine(rootSelfGlpPath: _root).loadProgram(dir), throwsA(anything));
  });
}
