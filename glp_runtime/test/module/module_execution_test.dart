import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/bytecode/runner.dart';

void main() {
  group('Module execution - CallRemote', () {
    test('calls remote procedure from another module', () {
      final compiler = GlpCompiler();

      // Compile math module with a simple binding procedure
      final mathSource = '''
        -module(math).
        -export([bind_value/2]).
        bind_value(X?, Y) :- Y := X?.
      ''';
      final mathModule = compiler.compileModule(mathSource);

      // Compile caller module that calls math#bind_value
      final callerSource = '''
        -module(caller).
        test(X) :- math # bind_value(42, X).
      ''';
      final callerModule = compiler.compileModule(callerSource);

      // Create runtime and register modules
      final rt = GlpRuntime();
      rt.serviceRegistry.registerOrReplace(mathModule);
      rt.serviceRegistry.registerOrReplace(callerModule);

      // Create writer for result variable
      final resultWriterId = rt.heap.newWriter();

      // Set up caller program
      final callerProgram = BytecodeProgram(callerModule.instructions);

      // Create goal environment with result writer
      final env = CallEnv(args: {
        0: VarRef(resultWriterId, isReader: false),
      });

      // Get entry point for test/1
      final entryPc = callerModule.getProcOffset('test', 1);
      expect(entryPc, isNotNull, reason: 'test/1 should exist in caller module');

      // Create goal and runner
      final goalId = 1;
      final goalRef = GoalRef(goalId, entryPc!);
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

      // Process any enqueued goals (the CallRemote spawns a goal in math module)
      var iterations = 0;
      while (rt.gq.isNotEmpty && iterations < 100) {
        final nextGoal = rt.gq.dequeue()!;
        final program = rt.getGoalProgram(nextGoal.goalId);
        if (program is BytecodeProgram) {
          final nextEnv = rt.getGoalEnv(nextGoal.goalId) ?? CallEnv();
          final nextRunner = BytecodeRunner(program);
          final nextCx = RunnerContext(
            rt: rt,
            goalId: nextGoal.goalId,
            kappa: nextGoal.pc,
            env: nextEnv,
          );
          nextRunner.run(nextCx);
        }
        iterations++;
      }

      // Verify result - writer should be bound to 42
      expect(rt.heap.isWriterBound(resultWriterId), isTrue,
          reason: 'Result writer should be bound');
      final value = rt.heap.valueOfWriter(resultWriterId);
      expect(value, equals(42), reason: 'Result should be 42 from math#bind_value');
    });

    test('fails on non-exported procedure', () {
      final compiler = GlpCompiler();

      // Compile math module with unexported procedure
      final mathSource = '''
        -module(math).
        -export([public/1]).
        public(X) :- X := 1.
        private(X) :- X := 2.
      ''';
      final mathModule = compiler.compileModule(mathSource);

      // Compile caller module that calls unexported math#private
      final callerSource = '''
        -module(caller).
        test(X) :- math # private(X).
      ''';
      final callerModule = compiler.compileModule(callerSource);

      // Create runtime and register modules
      final rt = GlpRuntime();
      rt.serviceRegistry.registerOrReplace(mathModule);
      rt.serviceRegistry.registerOrReplace(callerModule);

      // Set up caller program
      final callerProgram = BytecodeProgram(callerModule.instructions);

      // Create goal environment
      final resultWriterId = rt.heap.newWriter();
      final env = CallEnv(args: {
        0: VarRef(resultWriterId, isReader: false),
      });

      // Get entry point
      final entryPc = callerModule.getProcOffset('test', 1)!;

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

    test('fails on non-existent module', () {
      final compiler = GlpCompiler();

      // Compile caller module that calls non-existent module
      final callerSource = '''
        -module(caller).
        test(X) :- nonexistent # foo(X).
      ''';
      final callerModule = compiler.compileModule(callerSource);

      // Create runtime WITHOUT registering nonexistent module
      final rt = GlpRuntime();
      rt.serviceRegistry.registerOrReplace(callerModule);

      // Set up caller program
      final callerProgram = BytecodeProgram(callerModule.instructions);

      // Create goal environment
      final resultWriterId = rt.heap.newWriter();
      final env = CallEnv(args: {
        0: VarRef(resultWriterId, isReader: false),
      });

      // Get entry point
      final entryPc = callerModule.getProcOffset('test', 1)!;

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

    test('module with empty exports allows all procedures', () {
      final compiler = GlpCompiler();

      // Compile math module without -export (all public)
      final mathSource = '''
        -module(math).
        add(X?, Y?, Z) :- Z := X? + Y?.
      ''';
      final mathModule = compiler.compileModule(mathSource);

      // Compile caller module
      final callerSource = '''
        -module(caller).
        test(X) :- math # add(2, 3, X).
      ''';
      final callerModule = compiler.compileModule(callerSource);

      // Create runtime and register modules
      final rt = GlpRuntime();
      rt.serviceRegistry.registerOrReplace(mathModule);
      rt.serviceRegistry.registerOrReplace(callerModule);

      // Set up caller program
      final callerProgram = BytecodeProgram(callerModule.instructions);

      // Create goal environment
      final resultWriterId = rt.heap.newWriter();
      final env = CallEnv(args: {
        0: VarRef(resultWriterId, isReader: false),
      });

      // Get entry point
      final entryPc = callerModule.getProcOffset('test', 1)!;

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
      while (rt.gq.isNotEmpty && iterations < 100) {
        final nextGoal = rt.gq.dequeue()!;
        final program = rt.getGoalProgram(nextGoal.goalId);
        if (program is BytecodeProgram) {
          final nextEnv = rt.getGoalEnv(nextGoal.goalId) ?? CallEnv();
          final nextRunner = BytecodeRunner(program);
          final nextCx = RunnerContext(
            rt: rt,
            goalId: nextGoal.goalId,
            kappa: nextGoal.pc,
            env: nextEnv,
          );
          nextRunner.run(nextCx);
        }
        iterations++;
      }

      // Verify result - writer should be bound to 5
      expect(rt.heap.isWriterBound(resultWriterId), isTrue);
      final value = rt.heap.valueOfWriter(resultWriterId);
      expect(value, equals(5), reason: 'Result should be 2 + 3 = 5');
    });
  });

  group('ServiceRegistry', () {
    test('register and lookup module', () {
      final compiler = GlpCompiler();
      final source = '''
        -module(mymodule).
        foo(1).
      ''';
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
      final v1Source = '''
        -module(math).
        double(X?, Y) :- Y := X? * 2.
      ''';
      final v1Module = compiler.compileModule(v1Source);
      rt.serviceRegistry.registerOrReplace(v1Module);

      expect(rt.serviceRegistry.isLoaded('math'), isTrue);

      // Reload with version 2 (different implementation)
      final v2Source = '''
        -module(math).
        double(X?, Y) :- Y := X? + X?.
      ''';
      final v2Module = compiler.compileModule(v2Source);
      rt.serviceRegistry.reload(v2Module);

      // Module should still be loaded
      expect(rt.serviceRegistry.isLoaded('math'), isTrue);

      // New goals should use the new version
      final found = rt.serviceRegistry.lookup('math');
      expect(found, isNotNull);
      // (Can't easily verify the implementation changed without running it)
    });

    test('unload removes module', () {
      final compiler = GlpCompiler();
      final rt = GlpRuntime();

      final source = '''
        -module(temp).
        foo(1).
      ''';
      final module = compiler.compileModule(source);
      rt.serviceRegistry.register(module);

      expect(rt.serviceRegistry.isLoaded('temp'), isTrue);

      final removed = rt.serviceRegistry.unload('temp');
      expect(removed, isTrue);
      expect(rt.serviceRegistry.isLoaded('temp'), isFalse);
    });
  });
}
