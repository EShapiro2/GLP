// glp_runtime/test/compiler/primitive_layer_test.dart
//
// Load-time enforcement of "Admission to the Primitive Layer"
// (TGLP appendix-root-self.tex, app:system-mode). Exercises the real loader
// paths — single-file `GlpEngine.loadFile` and hierarchical `discoverProgram` —
// not the in-memory analyzer check covered by reserved_constant_test.dart.
//
//   Rule A — `-mode(system)` is admitted only for the root self.glp and modules
//            under programs/system/.
//   Rule B — a module not in system mode may not NAME a reserved constant (a
//            kernel predicate / reserved functor), whether it appears as a goal
//            call, a struct functor, or a bare constant. A `_`-prefixed constant
//            that names no primitive (e.g. '_user') is not reserved.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/compiler/program_linker.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  final systemModule = File('../programs/system/mad_predicates.glp').absolute.path;

  late Directory tmp;
  late GlpEngine engine;

  setUp(() {
    tmp = Directory.systemTemp.createTempSync('pl_');
    engine = GlpEngine(rootSelfGlpPath: rootSelf);
  });
  tearDown(() => tmp.deleteSync(recursive: true));

  File fixture(String name, String src) =>
      File('${tmp.path}${Platform.pathSeparator}$name')..writeAsStringSync(src);

  Matcher throwsContaining(String needle) => throwsA(
      predicate((e) => e.toString().contains(needle), 'message contains "$needle"'));

  group('Rule A — -mode(system) location (single-file load)', () {
    test('rejects -mode(system) in an application module (outside system/)', () {
      final f = fixture('app.glp', '-mode(system).\np.\n');
      expect(() => engine.loadFile(f.path),
          throwsContaining('confined to the primitive layer'));
    });

    test('admits a real module under programs/system/', () {
      // mad_predicates.glp names the '_w'/'_send' kernels under -mode(system);
      // it lives under programs/system/, so Rule A admits it and it loads.
      expect(engine.loadFile(systemModule), isTrue);
    });
  });

  group('Rule B — reserved-constant naming in user mode (single-file load)', () {
    test('rejects a kernel call (goal functor)', () {
      final f = fixture('out.glp', "p :- '_output'(foo).\n");
      expect(() => engine.loadFile(f.path),
          throwsContaining('names a language primitive'));
    });

    test('rejects a reserved functor (struct term)', () {
      final f = fixture('w.glp', "p(t('_w'(a, b))).\n");
      expect(() => engine.loadFile(f.path),
          throwsContaining('names a language primitive'));
    });

    test('accepts a _-prefixed constant that names no primitive', () {
      // '_user' names no kernel; it is not reserved even with the prefix.
      final f = fixture('user.glp', "p('_user').\n");
      expect(engine.loadFile(f.path), isTrue);
    });
  });

  group('Enforcement on hierarchical program load (linker discovery)', () {
    test('rejects a program whose module strays into -mode(system)', () {
      final proj = Directory('${tmp.path}${Platform.pathSeparator}proj')
        ..createSync();
      File('${proj.path}${Platform.pathSeparator}bad.glp').writeAsStringSync(
          '-mode(system).\nexported procedure b.\nb.\n');
      expect(() => discoverProgram(proj.path, rootSelfGlpPath: rootSelf),
          throwsContaining('confined to the primitive layer'));
    });
  });
}
