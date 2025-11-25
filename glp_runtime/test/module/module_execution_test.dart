import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/bytecode/runner.dart';

void main() {
  group('Module execution - CallRemote', () {
    test('compiles remote goal and generates CallRemote opcode', () {
      final compiler = GlpCompiler();

      // Compile caller module that calls another module
      final callerSource = '-module(caller). test :- math # compute.';
      final callerModule = compiler.compileModule(callerSource);

      // Verify the module compiled and has the procedure
      expect(callerModule.name, equals('caller'));
      expect(callerModule.procedures, contains('test/0'));

      // Verify CallRemote opcode is in the instructions
      final hasCallRemote = callerModule.instructions.any(
        (op) => op.toString().contains('CallRemote') || op.runtimeType.toString() == 'CallRemote'
      );
      expect(hasCallRemote, isTrue, reason: 'Should have CallRemote opcode for math#compute');
    });

    test('fails on non-existent module at runtime', () {
      final compiler = GlpCompiler();

      // Compile caller module that calls non-existent module
      final callerSource = '-module(caller). test :- nonexistent # foo.';
      final callerModule = compiler.compileModule(callerSource);

      // Create runtime WITHOUT registering nonexistent module
      final rt = GlpRuntime();
      rt.serviceRegistry.registerOrReplace(callerModule);

      // Set up caller program
      final callerProgram = BytecodeProgram(callerModule.instructions);

      // Create goal environment
      final env = CallEnv();

      // Get entry point
      final entryPc = callerModule.getProcOffset('test', 0)!;

      // Create goal and runner
      final goalId = 1;
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, callerProgram);

      // Run the goal - should print error about missing module
      final runner = BytecodeRunner(callerProgram);
      final cx = RunnerContext(
        rt: rt,
        goalId: goalId,
        kappa: entryPc,
        env: env,
      );

      final result = runner.runWithStatus(cx);

      // Should terminate with error (missing module)
      expect(result, equals(RunResult.terminated));
    });

    test('fails on non-exported procedure at runtime', () {
      final compiler = GlpCompiler();

      // Compile math module with unexported procedure
      final mathSource = '-module(math). -export([public/1]). public(1). private(2).';
      final mathModule = compiler.compileModule(mathSource);

      // Compile caller module that calls unexported math#private
      final callerSource = '-module(caller). test :- math # private.';
      final callerModule = compiler.compileModule(callerSource);

      // Create runtime and register modules
      final rt = GlpRuntime();
      rt.serviceRegistry.registerOrReplace(mathModule);
      rt.serviceRegistry.registerOrReplace(callerModule);

      // Set up caller program
      final callerProgram = BytecodeProgram(callerModule.instructions);

      // Create goal environment
      final env = CallEnv();

      // Get entry point
      final entryPc = callerModule.getProcOffset('test', 0)!;

      // Create goal and runner
      final goalId = 1;
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, callerProgram);

      // Run the goal - should print error about non-exported procedure
      final runner = BytecodeRunner(callerProgram);
      final cx = RunnerContext(
        rt: rt,
        goalId: goalId,
        kappa: entryPc,
        env: env,
      );

      final result = runner.runWithStatus(cx);

      // Should terminate with error (non-exported procedure)
      expect(result, equals(RunResult.terminated));
    });

    test('successfully calls exported procedure from another module', () {
      final compiler = GlpCompiler();

      // Compile math module with exported procedure
      // compute/1 matches the call `math # compute(42)` (arity 1)
      final mathSource = '-module(math). -export([compute/1]). compute(42).';
      final mathModule = compiler.compileModule(mathSource);

      // Compile caller module that calls math#compute(42)
      final callerSource = '-module(caller). test :- math # compute(42).';
      final callerModule = compiler.compileModule(callerSource);

      // Create runtime and register modules
      final rt = GlpRuntime();
      rt.serviceRegistry.registerOrReplace(mathModule);
      rt.serviceRegistry.registerOrReplace(callerModule);

      // Set up caller program
      final callerProgram = BytecodeProgram(callerModule.instructions);

      // Create goal environment
      final env = CallEnv();

      // Get entry point
      final entryPc = callerModule.getProcOffset('test', 0)!;

      // Create goal and runner
      final goalId = 1;
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, callerProgram);

      // Run the goal
      final runner = BytecodeRunner(callerProgram);
      final cx = RunnerContext(
        rt: rt,
        goalId: goalId,
        kappa: entryPc,
        env: env,
      );

      runner.run(cx);

      // The CallRemote should spawn a goal in math module
      // Process any enqueued goals
      var iterations = 0;
      while (!rt.gq.isEmpty && iterations < 100) {
        final nextGoal = rt.gq.dequeue()!;
        final program = rt.getGoalProgram(nextGoal.id);
        if (program is BytecodeProgram) {
          final nextEnv = rt.getGoalEnv(nextGoal.id) ?? CallEnv();
          final nextRunner = BytecodeRunner(program);
          final nextCx = RunnerContext(
            rt: rt,
            goalId: nextGoal.id,
            kappa: nextGoal.pc,
            env: nextEnv,
          );
          nextRunner.run(nextCx);
        }
        iterations++;
      }

      // If we got here without errors, the remote call succeeded
      // The test passes if no exceptions were thrown
      expect(iterations, greaterThan(0), reason: 'Should have spawned at least one goal');
    });

    test('module with empty exports allows all procedures', () {
      final compiler = GlpCompiler();

      // Compile math module without -export (all public)
      // add/0 matches the call `math # add` (arity 0)
      final mathSource = '-module(math). add.';
      final mathModule = compiler.compileModule(mathSource);

      // Compile caller module
      final callerSource = '-module(caller). test :- math # add.';
      final callerModule = compiler.compileModule(callerSource);

      // Create runtime and register modules
      final rt = GlpRuntime();
      rt.serviceRegistry.registerOrReplace(mathModule);
      rt.serviceRegistry.registerOrReplace(callerModule);

      // Set up caller program
      final callerProgram = BytecodeProgram(callerModule.instructions);

      // Create goal environment
      final env = CallEnv();

      // Get entry point
      final entryPc = callerModule.getProcOffset('test', 0)!;

      // Create goal and runner
      final goalId = 1;
      rt.setGoalEnv(goalId, env);
      rt.setGoalProgram(goalId, callerProgram);

      // Run the goal
      final runner = BytecodeRunner(callerProgram);
      final cx = RunnerContext(
        rt: rt,
        goalId: goalId,
        kappa: entryPc,
        env: env,
      );

      runner.run(cx);

      // Process any enqueued goals
      var iterations = 0;
      while (!rt.gq.isEmpty && iterations < 100) {
        final nextGoal = rt.gq.dequeue()!;
        final program = rt.getGoalProgram(nextGoal.id);
        if (program is BytecodeProgram) {
          final nextEnv = rt.getGoalEnv(nextGoal.id) ?? CallEnv();
          final nextRunner = BytecodeRunner(program);
          final nextCx = RunnerContext(
            rt: rt,
            goalId: nextGoal.id,
            kappa: nextGoal.pc,
            env: nextEnv,
          );
          nextRunner.run(nextCx);
        }
        iterations++;
      }

      // Success if no errors occurred
      expect(iterations, greaterThan(0));
    });
  });

  group('ServiceRegistry', () {
    test('register and lookup module', () {
      final compiler = GlpCompiler();
      final source = '-module(mymodule). foo(1).';
      final module = compiler.compileModule(source);

      final rt = GlpRuntime();
      rt.serviceRegistry.register(module);

      final found = rt.serviceRegistry.lookup('mymodule');
      expect(found, isNotNull);
      expect(found!.name, equals('mymodule'));
    });

    test('hot reload replaces module', () {
      final compiler = GlpCompiler();
      final rt = GlpRuntime();

      // Register version 1
      final v1Source = '-module(math). double(2).';
      final v1Module = compiler.compileModule(v1Source);
      rt.serviceRegistry.registerOrReplace(v1Module);

      expect(rt.serviceRegistry.isLoaded('math'), isTrue);

      // Reload with version 2 (different implementation)
      final v2Source = '-module(math). double(4).';
      final v2Module = compiler.compileModule(v2Source);
      rt.serviceRegistry.reload(v2Module);

      // Module should still be loaded
      expect(rt.serviceRegistry.isLoaded('math'), isTrue);

      // New goals should use the new version
      final found = rt.serviceRegistry.lookup('math');
      expect(found, isNotNull);
    });

    test('unload removes module', () {
      final compiler = GlpCompiler();
      final rt = GlpRuntime();

      final source = '-module(temp). foo(1).';
      final module = compiler.compileModule(source);
      rt.serviceRegistry.register(module);

      expect(rt.serviceRegistry.isLoaded('temp'), isTrue);

      final removed = rt.serviceRegistry.unload('temp');
      expect(removed, isTrue);
      expect(rt.serviceRegistry.isLoaded('temp'), isFalse);
    });
  });
}
