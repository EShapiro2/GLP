/// Tests for module-as-value: `self_module`/1 (producer) and `run`/2 (consumer).
///
/// Spec: IGLP sections/appendix-implementation-notes.tex §Self-Module — "a goal
/// carries the module value of the app it belongs to. `self_module` takes no
/// argument and returns that value." `run(Goal, Module)` is the inverse: it
/// launches Goal as a fresh initial goal on the Module value, PC into its code.
///
/// These are Dart-level tests: the root self.glp system predicates for
/// `self_module`/`run` are TGLP's and have not landed, so the kernels are
/// driven directly, as the arithmetic kernel tests do.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/engine_v2/module_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';

void main() {
  late BytecodeProgram prog;

  setUpAll(() {
    final source = File('../programs/self.glp').readAsStringSync();
    prog = GlpCompiler().compile(source);
  });

  /// A signature the module actually exposes a compiled entry for, with its
  /// entry byte offset — chosen from the program rather than hard-coded.
  (String, int) firstCompiledEntry(BytecodeProgram p) {
    final image = codeImageFromProgram(p);
    for (final sig in p.labels.keys) {
      final off = image.entryOffsetOf(sig);
      if (off != null) return (sig, off);
    }
    fail('no compiled entry found in program');
  }

  group('self_module/1 — producer', () {
    test('binds its output to the module value the calling goal carries', () {
      final rt = GlpRuntime();
      final module = ModuleTerm(prog, name: 'testmod');

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

      final module = ModuleTerm(prog, name: 'testmod');
      final (sig, entry) = firstCompiledEntry(prog);
      final arity = int.parse(sig.split('/')[1]);
      final name = sig.split('/')[0];

      final argRefs = <Term>[];
      for (var i = 0; i < arity; i++) {
        final (w, _) = rt.heap.allocateVariable();
        argRefs.add(VarRef(w));
      }
      final Term bootGoal =
          arity == 0 ? ConstTerm(name) : StructTerm(name, argRefs);

      final before = rt.gq.length;
      final kernel = rt.bodyKernels.lookup('run', 2);
      expect(kernel, isNotNull, reason: 'run/2 should be registered');

      final result = kernel!(rt, [bootGoal, module]);
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
          reason: "a runner for the module's code is registered for the "
              'scheduler to route the goal to');
    });

    test('run then self_module round-trips the same module value', () {
      final rt = GlpRuntime();
      registerModuleKernels(rt);

      final module = ModuleTerm(prog, name: 'testmod');
      final (sig, _) = firstCompiledEntry(prog);
      final arity = int.parse(sig.split('/')[1]);
      final name = sig.split('/')[0];

      final argRefs = <Term>[];
      for (var i = 0; i < arity; i++) {
        final (w, _) = rt.heap.allocateVariable();
        argRefs.add(VarRef(w));
      }
      final Term bootGoal =
          arity == 0 ? ConstTerm(name) : StructTerm(name, argRefs);

      expect(rt.bodyKernels.lookup('run', 2)!(rt, [bootGoal, module]),
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
      final module = ModuleTerm(prog, name: 'testmod');
      final kernel = rt.bodyKernels.lookup('run', 2)!;
      final before = rt.gq.length;
      expect(
          kernel(rt, [ConstTerm('no_such_procedure_xyz'), module]),
          equals(BodyKernelResult.abort));
      expect(rt.gq.length, equals(before),
          reason: 'a failed resolution enqueues nothing');
    });
  });
}
