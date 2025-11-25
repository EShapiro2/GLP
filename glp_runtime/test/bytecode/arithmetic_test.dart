import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'dart:io';

void main() {
  late BytecodeProgram stdlibProg;

  setUpAll(() {
    // Compile assign.glp with skipSRSW for system predicates
    final stdlibSource = File('/home/user/GLP/stdlib/assign.glp').readAsStringSync();
    final stdlibCompiler = GlpCompiler(skipSRSW: true);
    stdlibProg = stdlibCompiler.compile(stdlibSource);
    print('Stdlib compiled: ${stdlibProg.ops.length} instructions');
  });

  group('Arithmetic via := system predicate', () {
    test('add/3 body kernel executes directly', () {
      // Test that add/3 body kernel works when called directly
      final rt = GlpRuntime();

      // Allocate variables for testing
      final xId = rt.heap.allocateVariable();
      final yId = rt.heap.allocateVariable();
      final resultId = rt.heap.allocateVariable();

      // Bind X and Y to numbers
      rt.heap.bindVariableConst(xId, 5);
      rt.heap.bindVariableConst(yId, 3);

      // Call add kernel directly
      final xRef = VarRef(xId, isReader: true);
      final yRef = VarRef(yId, isReader: true);
      final resultRef = VarRef(resultId, isReader: false);  // writer

      final kernel = rt.bodyKernels.lookup('add', 3);
      expect(kernel, isNotNull, reason: 'add/3 kernel should be registered');

      final result = kernel!(rt, [xRef, yRef, resultRef]);
      expect(result, equals(BodyKernelResult.success));

      // Check that result is bound to 8
      final value = rt.heap.getValue(resultId);
      expect(value, isNotNull);
      expect(value, isA<ConstTerm>());
      expect((value as ConstTerm).value, equals(8));
    });

    test('sub/3 body kernel', () {
      final rt = GlpRuntime();

      final xId = rt.heap.allocateVariable();
      final yId = rt.heap.allocateVariable();
      final resultId = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xId, 10);
      rt.heap.bindVariableConst(yId, 4);

      final kernel = rt.bodyKernels.lookup('sub', 3);
      expect(kernel, isNotNull);

      final result = kernel!(rt, [
        VarRef(xId, isReader: true),
        VarRef(yId, isReader: true),
        VarRef(resultId, isReader: false),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultId);
      expect((value as ConstTerm).value, equals(6));
    });

    test('mul/3 body kernel', () {
      final rt = GlpRuntime();

      final xId = rt.heap.allocateVariable();
      final yId = rt.heap.allocateVariable();
      final resultId = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xId, 7);
      rt.heap.bindVariableConst(yId, 6);

      final kernel = rt.bodyKernels.lookup('mul', 3);
      final result = kernel!(rt, [
        VarRef(xId, isReader: true),
        VarRef(yId, isReader: true),
        VarRef(resultId, isReader: false),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultId);
      expect((value as ConstTerm).value, equals(42));
    });

    test('div/3 body kernel', () {
      final rt = GlpRuntime();

      final xId = rt.heap.allocateVariable();
      final yId = rt.heap.allocateVariable();
      final resultId = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xId, 15);
      rt.heap.bindVariableConst(yId, 4);

      final kernel = rt.bodyKernels.lookup('div', 3);
      final result = kernel!(rt, [
        VarRef(xId, isReader: true),
        VarRef(yId, isReader: true),
        VarRef(resultId, isReader: false),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultId);
      expect((value as ConstTerm).value, equals(3.75));
    });

    test('div/3 body kernel aborts on division by zero', () {
      final rt = GlpRuntime();

      final xId = rt.heap.allocateVariable();
      final yId = rt.heap.allocateVariable();
      final resultId = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xId, 10);
      rt.heap.bindVariableConst(yId, 0);

      final kernel = rt.bodyKernels.lookup('div', 3);
      final result = kernel!(rt, [
        VarRef(xId, isReader: true),
        VarRef(yId, isReader: true),
        VarRef(resultId, isReader: false),
      ]);
      expect(result, equals(BodyKernelResult.abort));
    });

    test('neg/2 body kernel', () {
      final rt = GlpRuntime();

      final xId = rt.heap.allocateVariable();
      final resultId = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xId, 42);

      final kernel = rt.bodyKernels.lookup('neg', 2);
      final result = kernel!(rt, [
        VarRef(xId, isReader: true),
        VarRef(resultId, isReader: false),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultId);
      expect((value as ConstTerm).value, equals(-42));
    });

    test('sqrt_kernel/2 body kernel', () {
      final rt = GlpRuntime();

      final xId = rt.heap.allocateVariable();
      final resultId = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xId, 16);

      final kernel = rt.bodyKernels.lookup('sqrt_kernel', 2);
      final result = kernel!(rt, [
        VarRef(xId, isReader: true),
        VarRef(resultId, isReader: false),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultId);
      expect((value as ConstTerm).value, equals(4.0));
    });

    test('all standard body kernels are registered', () {
      final rt = GlpRuntime();

      // Binary arithmetic
      expect(rt.bodyKernels.has('add', 3), isTrue);
      expect(rt.bodyKernels.has('sub', 3), isTrue);
      expect(rt.bodyKernels.has('mul', 3), isTrue);
      expect(rt.bodyKernels.has('div', 3), isTrue);
      expect(rt.bodyKernels.has('idiv', 3), isTrue);
      expect(rt.bodyKernels.has('mod', 3), isTrue);

      // Unary
      expect(rt.bodyKernels.has('neg', 2), isTrue);
      expect(rt.bodyKernels.has('abs_kernel', 2), isTrue);

      // Math functions
      expect(rt.bodyKernels.has('sqrt_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('sin_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('cos_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('tan_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('exp_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('ln_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('log10_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('pow_kernel', 3), isTrue);
      expect(rt.bodyKernels.has('asin_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('acos_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('atan_kernel', 2), isTrue);

      // Type conversions
      expect(rt.bodyKernels.has('integer_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('real_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('round_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('floor_kernel', 2), isTrue);
      expect(rt.bodyKernels.has('ceil_kernel', 2), isTrue);
    });
  });

  group('End-to-end := system predicate', () {
    test('assign.glp compiles and merges correctly', () {
      // Load stdlib (assign.glp)
      final stdlibSource = File('/home/user/GLP/stdlib/assign.glp').readAsStringSync();
      final stdlibCompiler = GlpCompiler(skipSRSW: true);
      final stdlibProg = stdlibCompiler.compile(stdlibSource);

      // A simple user program that just calls another predicate
      final userSource = '''
        hello.
      ''';
      final userCompiler = GlpCompiler();
      final userProg = userCompiler.compile(userSource);

      // Merge programs (stdlib first, then user)
      final mergedProg = userProg.merge(stdlibProg);

      print('Merged program has ${mergedProg.ops.length} instructions');
      print('Labels: ${mergedProg.labels.keys.toList()}');

      // Verify :=/2 label exists in merged program
      expect(mergedProg.labels.containsKey(':=/2'), isTrue,
          reason: 'Merged program should contain :=/2 from stdlib');
      expect(mergedProg.labels.containsKey('hello/0'), isTrue,
          reason: 'Merged program should contain hello/0 from user code');
    });

    test('user program with := compiles correctly with SRSW', () {
      // Correct SRSW pattern: Z? in head (reader), Z in body (writer)
      final userSource = '''
        compute_sum(Z?) :- Z := 5 + 3.
      ''';
      // No skipSRSW needed - this is valid SRSW
      final compiler = GlpCompiler();
      final prog = compiler.compile(userSource);

      expect(prog.ops.isNotEmpty, isTrue);
      expect(prog.labels.containsKey('compute_sum/1'), isTrue);

      print('compute_sum/1 compiled to ${prog.ops.length} instructions');
    });

    test('Z := 5 + 3 executes and binds Z to 8', () {
      print('\n=== END-TO-END ARITHMETIC TEST ===');

      // Load stdlib (assign.glp)
      final stdlibSource = File('/home/user/GLP/stdlib/assign.glp').readAsStringSync();
      final stdlibCompiler = GlpCompiler(skipSRSW: true);
      final stdlibProg = stdlibCompiler.compile(stdlibSource);

      // Compile user program
      final userSource = '''
        compute_sum(Z?) :- Z := 5 + 3.
      ''';
      final userCompiler = GlpCompiler();
      final userProg = userCompiler.compile(userSource);

      // Merge programs
      final mergedProg = userProg.merge(stdlibProg);
      print('Merged program: ${mergedProg.ops.length} instructions');

      // Create runtime
      final rt = GlpRuntime();

      // Allocate a variable for the result (Z)
      final resultVarId = rt.heap.allocateVariable();
      print('Allocated result variable: V$resultVarId');

      // Create runner and scheduler
      final runner = BytecodeRunner(mergedProg);
      final sched = Scheduler(rt: rt, runner: runner);

      // Create environment with the result variable as argument
      // The query is compute_sum(Result?) where Result is our allocated variable
      final env = CallEnv(args: {
        0: VarRef(resultVarId, isReader: true),  // Pass reader to head position Z?
      });

      // Set up goal
      final goalId = 1;
      rt.setGoalEnv(goalId, env);

      // Get entry point for compute_sum/1
      final entryPc = mergedProg.labels['compute_sum/1'];
      expect(entryPc, isNotNull, reason: 'compute_sum/1 should exist');
      print('compute_sum/1 entry at PC $entryPc');

      // Enqueue the initial goal
      rt.gq.enqueue(GoalRef(goalId, entryPc!));

      print('\nRunning scheduler to drain all goals...');
      final ran = sched.drain(maxCycles: 100, debug: true, debugOutput: true);
      print('Goals executed: ${ran.length}');

      // Debug: show what goals were spawned
      print('\nSpawned goals environments:');
      for (var id = 10000; id < rt.nextGoalId; id++) {
        final env = rt.getGoalEnv(id);
        if (env != null) {
          print('  Goal $id env: ${env.argBySlot}');
        }
      }

      // Check if the result variable is bound
      final isBound = rt.heap.isWriterBound(resultVarId);
      print('Result variable bound: $isBound');

      if (isBound) {
        final value = rt.heap.getValue(resultVarId);
        print('Result value: $value');

        expect(value, isA<ConstTerm>());
        if (value is ConstTerm) {
          print('Result = ${value.value}');
          expect(value.value, equals(8), reason: '5 + 3 should equal 8');
          print('✓ Z := 5 + 3 correctly evaluates to 8!');
        }
      } else {
        fail('Result variable should be bound after execution');
      }
    });
  });
}
