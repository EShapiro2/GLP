// glp_runtime/test/compiler/primitive_layer_test.dart
//
// Load-time enforcement of "Admission to the Primitive Layer"
// (TGLP appendix-root-self.tex, app:system-mode). Exercises the real loader
// paths — single-file `GlpEngine.loadFile` and hierarchical `discoverProgram` —
// not the in-memory analyzer check covered by reserved_constant_test.dart.
//
//   Rule A — `-mode(system)` is admitted only for the root self.glp and modules
//            under programs/system/.
//   Rule B — a module not in system mode neither defines nor calls a procedure
//            whose name is a quoted underscore-prefixed constant. The
//            restriction is on names in CALL POSITION only; the prefix is
//            unrestricted as data — as a message tag, or as a member of a type
//            union.
//
// Rule B was a list of reserved names until 2026-07-31, and tested the whole
// term tree. GLP-Spec narrowed it to call position that day, with Udi's approval
// at each step, because the prefix-only-but-everywhere formulation forbids
// '_net' — 242 green code lines name it as data. Nothing reserves a functor in
// data position now: a construction ban there is bypassable through `=..`
// anyway, and the forgery it would address is a runtime check on kernels taking
// a global name, which is IGLP's.

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

  group('Rule B — primitive-layer names in user mode (single-file load)', () {
    test('rejects a kernel call (goal functor)', () {
      final f = fixture('out.glp', "p :- '_output'(foo).\n");
      expect(() => engine.loadFile(f.path),
          throwsContaining('is a primitive-layer procedure name'));
    });

    test('rejects a kernel call in guard position', () {
      final f = fixture('g.glp', "p(X) :- '_copy'(X?, Y) | q(Y?).\n");
      expect(() => engine.loadFile(f.path),
          throwsContaining('is a primitive-layer procedure name'));
    });

    test('rejects defining a procedure so named (clause head)', () {
      final f = fixture('def.glp', "'_test_kernel'(a).\n");
      expect(() => engine.loadFile(f.path),
          throwsContaining('is a primitive-layer procedure name'));
    });

    test('rejects declaring a procedure so named', () {
      // builtinProcedures now lists every kernel, so the parser admits a
      // clause-less declaration of one; Rule B is what stops a user module
      // writing it.
      final f = fixture('decl.glp', "procedure '_add'(Number?, Number?, Number).\n");
      expect(() => engine.loadFile(f.path),
          throwsContaining('is a primitive-layer procedure name'));
    });

    test('accepts a _-prefixed constant in argument position', () {
      // '_user' in data position — a message tag, not a call.
      final f = fixture('user.glp', "p('_user').\n");
      expect(engine.loadFile(f.path), isTrue);
    });

    test('accepts a _-prefixed functor in data position', () {
      // Was rejected as a "reserved functor" until 2026-07-31. Under the
      // call-position rule `'_w'(a, b)` here builds a term and calls nothing,
      // so it is data and unrestricted — the same licence that keeps the 242
      // '_net' lines green.
      final f = fixture('w.glp', "p(t('_w'(a, b))).\n");
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
