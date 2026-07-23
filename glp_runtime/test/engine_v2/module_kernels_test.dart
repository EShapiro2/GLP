/// Tests for module-as-value: `self_module`/1 (producer) and `run`/2 (consumer),
/// and the module value's payload — the compiled Artefact (h(M) + code).
///
/// Spec: IGLP sections/appendix-implementation-notes.tex §Self-Module — "a goal
/// carries the module value of the app it belongs to. `self_module` takes no
/// argument and returns that value", and "the Module constant carries the
/// artefact — h(M) and code — not code alone: the adopter checks h(M) against
/// the offer and runs the code". `run(Goal, Module)` is the inverse: it launches
/// Goal as a fresh initial goal on the Module value, PC into its code.
///
/// These are Dart-level tests: the root self.glp system predicates for
/// `self_module`/`run` are TGLP's and have not landed, so the kernels are
/// driven directly, as the arithmetic kernel tests do.
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/code_image.dart';
import 'package:glp_runtime/engine_v2/module_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/flattening.dart';

const _rootSelf = '../programs/self.glp';

/// A project whose exported `go/2` reaches `helper/2`.
const _projectSource = '''
exported procedure go(Integer?, Integer).
go(X, Y?) :- helper(X?, Y).

procedure helper(Integer?, Integer).
helper(X, Y?) :- Y := X? + 1.
''';

Directory _tempProject(String source) {
  final dir = Directory.systemTemp.createTempSync('glp_modvalue_');
  File('${dir.path}/main.glp').writeAsStringSync(source);
  return dir;
}

void main() {
  late BytecodeProgram prog;
  late Artefact artefact;

  setUpAll(() {
    prog = GlpCompiler().compile(File(_rootSelf).readAsStringSync());
    artefact = Artefact.fromCompiled(
      ops: prog.ops.cast<Object>(),
      hM: Uint8List.fromList(List<int>.generate(32, (i) => i)),
      moduleName: 'testmod',
      isaVersion: 'glp-isa-1',
    );
  });

  /// A signature the module exposes a compiled entry for, with its entry byte
  /// offset — chosen from the artefact rather than hard-coded.
  (String, int) firstCompiledEntry(Artefact a) {
    final image = CodeImage.fromArtefactBytes(a.toBytes());
    for (final s in image.symbols) {
      if (!s.compiled) continue;
      final off = image.entryOffsetOf(s.signature);
      if (off != null) return (s.signature, off);
    }
    fail('no compiled entry found in artefact');
  }

  ({Term goal, int arity}) bootGoalFor(GlpRuntime rt, String sig) {
    final arity = int.parse(sig.split('/').last);
    final name = sig.substring(0, sig.lastIndexOf('/'));
    final args = <Term>[];
    for (var i = 0; i < arity; i++) {
      final (w, _) = rt.heap.allocateVariable();
      args.add(VarRef(w));
    }
    return (
      goal: arity == 0 ? ConstTerm(name) : StructTerm(name, args),
      arity: arity
    );
  }

  group('self_module/1 — producer', () {
    test('binds its output to the module value the calling goal carries', () {
      final rt = GlpRuntime();
      final module = ModuleTerm(artefact, name: 'testmod');

      const goalId = 7777;
      rt.setGoalModule(goalId, module);
      rt.currentGoalId = goalId;

      final (outWriter, _) = rt.heap.allocateVariable();
      final kernel = rt.bodyKernels.lookup('self_module', 1);
      expect(kernel, isNotNull,
          reason: 'self_module/1 should be a registered body kernel');

      final result = kernel!(rt, [VarRef(outWriter)]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(outWriter);
      expect(value, isA<ModuleTerm>());
      expect(identical(value, module), isTrue,
          reason: 'returns the very module value the goal carries');
      expect((value as ModuleTerm).artefact, isA<Artefact>(),
          reason: 'the Module constant carries an artefact, not bare code');
    });

    test('aborts when the calling goal carries no module value', () {
      final rt = GlpRuntime();
      rt.currentGoalId = 4242; // no setGoalModule for this goal

      final (outWriter, _) = rt.heap.allocateVariable();
      final kernel = rt.bodyKernels.lookup('self_module', 1)!;
      expect(kernel(rt, [VarRef(outWriter)]), equals(BodyKernelResult.abort));
    });
  });

  group('run/2 — consumer (the start primitive)', () {
    test('launches a fresh goal on the module value, PC into its code', () {
      final rt = GlpRuntime();
      registerModuleKernels(rt); // normally done by the GlpEngine constructor

      final module = ModuleTerm(artefact, name: 'testmod');
      final (sig, entry) = firstCompiledEntry(artefact);
      final boot = bootGoalFor(rt, sig);

      final before = rt.gq.length;
      final kernel = rt.bodyKernels.lookup('run', 2);
      expect(kernel, isNotNull, reason: 'run/2 should be registered');

      final result = kernel!(rt, [boot.goal, module]);
      expect(result, equals(BodyKernelResult.success));
      expect(rt.gq.length, equals(before + 1),
          reason: 'run enqueues exactly one new goal');

      final launched = rt.gq.items.last;
      expect(launched.pc, equals(entry),
          reason: "PC is the boot goal's entry offset in the module's code");
      expect(identical(rt.getGoalModule(launched.id), module), isTrue,
          reason: 'the launched goal carries the module it was run on');

      final key = rt.getGoalProgram(launched.id);
      expect(key, isA<String>());
      expect((key as String).startsWith('module:'), isTrue);
      expect(rt.runners[key], isNotNull,
          reason: "a runner over the module's code is registered for the "
              'scheduler to route the goal to');
    });

    test('run then self_module round-trips the same module value', () {
      final rt = GlpRuntime();
      registerModuleKernels(rt);

      final module = ModuleTerm(artefact, name: 'testmod');
      final (sig, _) = firstCompiledEntry(artefact);
      final boot = bootGoalFor(rt, sig);

      expect(rt.bodyKernels.lookup('run', 2)!(rt, [boot.goal, module]),
          equals(BodyKernelResult.success));

      // Stand in the launched goal and ask for its module.
      final launched = rt.gq.items.last;
      rt.currentGoalId = launched.id;

      final (outWriter, _) = rt.heap.allocateVariable();
      expect(rt.bodyKernels.lookup('self_module', 1)!(rt, [VarRef(outWriter)]),
          equals(BodyKernelResult.success));
      expect(identical(rt.heap.getValue(outWriter), module), isTrue,
          reason: 'self_module is the inverse of run');
    });

    test('aborts when the second argument is not a module value', () {
      final rt = GlpRuntime();
      registerModuleKernels(rt);
      final kernel = rt.bodyKernels.lookup('run', 2)!;
      expect(kernel(rt, [ConstTerm('boot'), ConstTerm('not_a_module')]),
          equals(BodyKernelResult.abort));
    });

    test('aborts when the module exposes no such entry point', () {
      final rt = GlpRuntime();
      registerModuleKernels(rt);
      final module = ModuleTerm(artefact, name: 'testmod');
      final kernel = rt.bodyKernels.lookup('run', 2)!;
      final before = rt.gq.length;
      expect(kernel(rt, [ConstTerm('no_such_procedure_xyz'), module]),
          equals(BodyKernelResult.abort));
      expect(rt.gq.length, equals(before),
          reason: 'a failed resolution enqueues nothing');
    });
  });

  group('the module value carries the artefact — h(M) and code', () {
    test("h(M) is the unit's flattened-source hash, and the code is intact",
        () {
      final dir = _tempProject(_projectSource);
      try {
        final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
        engine.loadProgram(dir.path);

        final moduleValue = engine.appModule;
        expect(moduleValue, isNotNull,
            reason: 'loading a program gives the app a module value');
        final a = moduleValue!.artefact;
        expect(a, isA<Artefact>());
        final art = a as Artefact;

        // h(M): the source identity, independently recomputed from disk.
        final expected =
            flattenProject(dir.path, rootSelfGlpPath: _rootSelf).hM;
        expect(art.hM.length, equals(32));
        expect(art.hM, equals(expected),
            reason: "h(M) is the SHA-256 of the linked, pruned program's "
                'canonical print');

        // Code: the reachable procedures are present as compiled symbols.
        final compiledSigs =
            art.symbols.where((s) => s.compiled).map((s) => s.signature);
        expect(compiledSigs, isNotEmpty);
        expect(compiledSigs.any((s) => s.contains('go')), isTrue,
            reason: 'the exported entry point is compiled into the artefact');
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('the artefact round-trips through toBytes/fromBytes', () {
      final dir = _tempProject(_projectSource);
      try {
        final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
        engine.loadProgram(dir.path);
        final art = engine.appModule!.artefact as Artefact;

        final bytes = art.toBytes();
        final back = Artefact.fromBytes(bytes);

        expect(back.hM, equals(art.hM), reason: 'h(M) survives the wire');
        expect(back.moduleName, equals(art.moduleName));
        expect(back.isaVersion, equals(art.isaVersion));
        expect(back.symbols.length, equals(art.symbols.length));
        expect(back.toBytes(), equals(bytes),
            reason: 'byte-for-byte stable — the artefact is what gets shipped');
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('a module value built at load can be run', () {
      final dir = _tempProject(_projectSource);
      try {
        final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
        engine.loadProgram(dir.path);
        final moduleValue = engine.appModule!;
        final art = moduleValue.artefact as Artefact;

        final rt = GlpRuntime();
        registerModuleKernels(rt);
        final (sig, entry) = firstCompiledEntry(art);
        final boot = bootGoalFor(rt, sig);

        expect(rt.bodyKernels.lookup('run', 2)!(rt, [boot.goal, moduleValue]),
            equals(BodyKernelResult.success));
        final launched = rt.gq.items.last;
        expect(launched.pc, equals(entry));
        expect(identical(rt.getGoalModule(launched.id), moduleValue), isTrue);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });
  });
}
