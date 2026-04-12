/// Dynamic Module Dispatch — Integration Tests
///
/// Tests the full dispatch chain:
///   caller → channel → serve → _activate → procedure
///
/// Spec: docs/type system/dynamic-module-dispatch.md

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart'
    show setPreludeUnitClauseSource;
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart'
    show setPreludeEnvironmentSource;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/glp_activation.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/bytecode/runner.dart';

void main() {
  // Set prelude sources (needed for compilation)
  final rootSelfGlp = File('../programs/self.glp');
  if (rootSelfGlp.existsSync()) {
    final source = rootSelfGlp.readAsStringSync();
    setPreludeUnitClauseSource(source);
    setPreludeEnvironmentSource(source);
  }

  final ddDir = '../programs/tests/dynamic_dispatch';

  group('serve/2', () {
    test('serve/2 compiles and has label', () {
      final engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      expect(engine.serveBytecode.labels.containsKey('serve/2'), isTrue);
    });
  });

  group('end-to-end dispatch', () {
    test('activate module and dispatch double(5, F) → F = 10', () {
      final compiler = GlpCompiler();
      final rt = GlpRuntime();

      // Compile root self.glp (needed for arithmetic at runtime)
      final rootSelfBytecode =
          compiler.compile(File('../programs/self.glp').readAsStringSync());

      // Compile serve/2
      final engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      final serveBytecode = engine.serveBytecode;

      // Compile math_service, merge with root self.glp
      final mathSource =
          File('$ddDir/math_service.glp').readAsStringSync();
      final mathBytecode = compiler.compile(mathSource).merge(rootSelfBytecode);

      // Activate the module
      final handle = activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: mathBytecode,
        moduleName: 'math_service',
      );
      expect(rt.glpChannels.containsKey('math_service'), isTrue);

      // Drain to let serve loop suspend waiting for input
      // (serve is an infrastructure goal, excluded from status — reports succeeded)
      final scheduler = Scheduler(rt: rt);
      var result = scheduler.drainWithStatus(maxCycles: 300);
      expect(result.status, equals(ExecutionStatus.succeeded));

      // Create goal: double(5, F)
      // F is a fresh writer — the result will be bound to it
      final (fWriter, _) = rt.heap.allocateVariable();
      final goal = StructTerm('double', [ConstTerm(5), VarRef(fWriter)]);

      // Send goal on channel
      final woken = handle.send(goal);
      for (final g in woken) {
        rt.gq.enqueue(g);
      }

      // Drain scheduler — serve reads goal, _activate dispatches to double,
      // double/2 computes 5 * 2 = 10
      result = scheduler.drainWithStatus(maxCycles: 10000);

      // Check result: F should be bound to 10
      final fValue = rt.heap.dereference(VarRef(fWriter));
      expect(fValue, isA<ConstTerm>(),
          reason: 'F should be bound to a constant (10)');
      expect((fValue as ConstTerm).value, equals(10));
    });

    test('activate module and dispatch triple(4, F) → F = 12', () {
      final compiler = GlpCompiler();
      final rt = GlpRuntime();

      final rootSelfBytecode =
          compiler.compile(File('../programs/self.glp').readAsStringSync());
      final engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      final serveBytecode = engine.serveBytecode;

      final mathSource =
          File('$ddDir/math_service.glp').readAsStringSync();
      final mathBytecode = compiler.compile(mathSource).merge(rootSelfBytecode);

      final handle = activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: mathBytecode,
        moduleName: 'math_service',
      );

      final scheduler = Scheduler(rt: rt);
      scheduler.drainWithStatus(maxCycles: 300);

      final (fWriter, _) = rt.heap.allocateVariable();
      final goal = StructTerm('triple', [ConstTerm(4), VarRef(fWriter)]);
      final woken = handle.send(goal);
      for (final g in woken) {
        rt.gq.enqueue(g);
      }

      final result = scheduler.drainWithStatus(maxCycles: 10000);
      final fValue = rt.heap.dereference(VarRef(fWriter));
      expect(fValue, isA<ConstTerm>());
      expect((fValue as ConstTerm).value, equals(12));
    });

    test('unknown goal does not crash (fallback)', () {
      final compiler = GlpCompiler();
      final rt = GlpRuntime();

      final rootSelfBytecode =
          compiler.compile(File('../programs/self.glp').readAsStringSync());
      final engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      final serveBytecode = engine.serveBytecode;

      final mathSource =
          File('$ddDir/math_service.glp').readAsStringSync();
      final mathBytecode = compiler.compile(mathSource).merge(rootSelfBytecode);

      final handle = activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: mathBytecode,
        moduleName: 'math_service',
      );

      final scheduler = Scheduler(rt: rt);
      scheduler.drainWithStatus(maxCycles: 300);

      // Send a goal for a non-existent procedure
      final goal = StructTerm('nonexistent', [ConstTerm(42)]);
      final woken = handle.send(goal);
      for (final g in woken) {
        rt.gq.enqueue(g);
      }

      // Should not crash — _activate fallback handles unknown procedures
      final result = scheduler.drainWithStatus(maxCycles: 5000);
      // The serve loop should continue running (infrastructure goal, excluded from status)
      expect(result.status, equals(ExecutionStatus.succeeded));
    });

    test('single_export module: dispatch inc(7, F) → F = 8', () {
      final compiler = GlpCompiler();
      final rt = GlpRuntime();

      final rootSelfBytecode =
          compiler.compile(File('../programs/self.glp').readAsStringSync());
      final engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      final serveBytecode = engine.serveBytecode;

      final source =
          File('$ddDir/single_export.glp').readAsStringSync();
      final moduleBytecode = compiler.compile(source).merge(rootSelfBytecode);

      final handle = activateModule(
        rt: rt,
        serveBytecode: serveBytecode,
        moduleBytecode: moduleBytecode,
        moduleName: 'single',
      );

      final scheduler = Scheduler(rt: rt);
      scheduler.drainWithStatus(maxCycles: 300);

      final (fWriter, _) = rt.heap.allocateVariable();
      final goal = StructTerm('inc', [ConstTerm(7), VarRef(fWriter)]);
      final woken = handle.send(goal);
      for (final g in woken) {
        rt.gq.enqueue(g);
      }

      scheduler.drainWithStatus(maxCycles: 10000);
      final fValue = rt.heap.dereference(VarRef(fWriter));
      expect(fValue, isA<ConstTerm>());
      expect((fValue as ConstTerm).value, equals(8));
    });
  });
}
