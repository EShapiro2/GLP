import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';

void main() {
  group('_activate/2 body kernel', () {
    test('dispatches valid goal through _select/1', () {
      // Compile a module with one exported procedure
      final compiler = GlpCompiler();
      final moduleBytecode = compiler.compile('''
exported procedure process(Any?).
process(X) :- otherwise | consume(X?).

procedure consume(Any?).
consume(_).
''');

      // Verify _select/1 was generated
      expect(moduleBytecode.labels.containsKey('_select/1'), isTrue);

      // Set up runtime
      final rt = GlpRuntime();

      // Create ModuleTerm wrapping the bytecode
      final moduleTerm = ModuleTerm(moduleBytecode, name: 'test_module');

      // Allocate a variable on the heap to serve as the Goal argument
      final goalTerm = StructTerm('process', [ConstTerm(42)]);

      // Call _activate kernel directly
      final result = activateKernel(rt, [moduleTerm, goalTerm]);

      expect(result, equals(BodyKernelResult.success));

      // Verify a goal was enqueued
      expect(rt.gq.length, equals(1));

      // Verify the enqueued goal points to _select/1
      final enqueued = rt.gq.dequeue()!;
      expect(enqueued.pc, equals(moduleBytecode.labels['_select/1']));

      // Verify a runner was registered for the module bytecode
      expect(rt.runners.containsKey(moduleBytecode), isTrue);
    });

    test('dispatches goal and procedure runs correctly', () {
      // Compile a module with an exported procedure
      final compiler = GlpCompiler();
      final moduleBytecode = compiler.compile('''
exported procedure greet(Any?).
greet(_) :- otherwise | true.
''');

      // Set up runtime with output capture
      final rt = GlpRuntime();
      final outputs = <String>[];
      rt.outputCallback = (s) => outputs.add(s);

      // Create module term and goal
      final moduleTerm = ModuleTerm(moduleBytecode, name: 'greeter');
      final goalTerm = StructTerm('greet', [ConstTerm('alice')]);

      // Call _activate
      final result = activateKernel(rt, [moduleTerm, goalTerm]);
      expect(result, equals(BodyKernelResult.success));

      // Now run the enqueued goal through a scheduler
      final runner = rt.runners[moduleBytecode]!;
      final scheduler = Scheduler(rt: rt, runners: {moduleBytecode: runner});
      final drainResult = scheduler.drainWithStatus(maxCycles: 100);

      // The goal should succeed (greet matches via _select, calls greet/1)
      expect(drainResult.status, isNot(equals(ExecutionStatus.failed)));
    });

    test('aborts when module has no _select/1', () {
      // Compile a module with NO exports — no _select/1 generated
      final compiler = GlpCompiler();
      final moduleBytecode = compiler.compile('''
procedure foo(Any?).
foo(X) :- ground(X?) | true.
''');

      expect(moduleBytecode.labels.containsKey('_select/1'), isFalse);

      final rt = GlpRuntime();
      final moduleTerm = ModuleTerm(moduleBytecode, name: 'no_exports');
      final goalTerm = StructTerm('foo', [ConstTerm(1)]);

      final result = activateKernel(rt, [moduleTerm, goalTerm]);
      expect(result, equals(BodyKernelResult.abort));

      // No goal should have been enqueued
      expect(rt.gq.length, equals(0));
    });

    test('aborts when first argument is not a ModuleTerm', () {
      final rt = GlpRuntime();
      final goalTerm = StructTerm('foo', [ConstTerm(1)]);

      // Pass a ConstTerm instead of ModuleTerm
      final result = activateKernel(rt, [ConstTerm('not_a_module'), goalTerm]);
      expect(result, equals(BodyKernelResult.abort));
    });

    test('unknown goal falls through to otherwise clause', () {
      // Compile a module exporting process/1
      final compiler = GlpCompiler();
      final moduleBytecode = compiler.compile('''
exported procedure process(Any?).
process(_) :- otherwise | true.
''');

      final rt = GlpRuntime();
      final moduleTerm = ModuleTerm(moduleBytecode, name: 'test_module');

      // Send a goal that doesn't match any export: unknown_proc(1)
      final goalTerm = StructTerm('unknown_proc', [ConstTerm(1)]);

      final result = activateKernel(rt, [moduleTerm, goalTerm]);
      expect(result, equals(BodyKernelResult.success));

      // Goal is enqueued — _select will try to match, fall through to otherwise
      expect(rt.gq.length, equals(1));

      // Run it — the otherwise clause calls true (succeeds silently)
      final runner = rt.runners[moduleBytecode]!;
      final scheduler = Scheduler(rt: rt, runners: {moduleBytecode: runner});
      final drainResult = scheduler.drainWithStatus(maxCycles: 100);
      expect(drainResult.status, isNot(equals(ExecutionStatus.failed)));
    });
  });
}
