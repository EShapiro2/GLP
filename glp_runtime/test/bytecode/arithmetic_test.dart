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
    // Compile assign.glp - now SRSW compliant
    // Use relative path from glp_runtime directory
    final stdlibSource = File('../programs/self.glp').readAsStringSync();
    final stdlibCompiler = GlpCompiler();
    stdlibProg = stdlibCompiler.compile(stdlibSource);
    print('Stdlib compiled: ${stdlibProg.ops.length} instructions');
  });

  group('Arithmetic via := system predicate', () {
    test('add/3 body kernel executes directly', () {
      // Test that add/3 body kernel works when called directly
      final rt = GlpRuntime();

      // Allocate variables for testing (returns (writerAddr, readerAddr) tuple)
      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (yWriter, yReader) = rt.heap.allocateVariable();
      final (resultWriter, resultReader) = rt.heap.allocateVariable();

      // Bind X and Y to numbers
      rt.heap.bindVariableConst(xWriter, 5);
      rt.heap.bindVariableConst(yWriter, 3);

      // Call add kernel directly - readers for inputs, writer for output
      final xRef = VarRef(xReader);
      final yRef = VarRef(yReader);
      final resultRef = VarRef(resultWriter);  // writer

      final kernel = rt.bodyKernels.lookup('_add', 3);
      expect(kernel, isNotNull, reason: '_add/3 kernel should be registered');

      final result = kernel!(rt, [xRef, yRef, resultRef]);
      expect(result, equals(BodyKernelResult.success));

      // Check that result is bound to 8
      final value = rt.heap.getValue(resultWriter);
      expect(value, isNotNull);
      expect(value, isA<ConstTerm>());
      expect((value as ConstTerm).value, equals(8));
    });

    test('sub/3 body kernel', () {
      final rt = GlpRuntime();

      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (yWriter, yReader) = rt.heap.allocateVariable();
      final (resultWriter, _) = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xWriter, 10);
      rt.heap.bindVariableConst(yWriter, 4);

      final kernel = rt.bodyKernels.lookup('_sub', 3);
      expect(kernel, isNotNull);

      final result = kernel!(rt, [
        VarRef(xReader),
        VarRef(yReader),
        VarRef(resultWriter),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultWriter);
      expect((value as ConstTerm).value, equals(6));
    });

    test('mul/3 body kernel', () {
      final rt = GlpRuntime();

      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (yWriter, yReader) = rt.heap.allocateVariable();
      final (resultWriter, _) = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xWriter, 7);
      rt.heap.bindVariableConst(yWriter, 6);

      final kernel = rt.bodyKernels.lookup('_mul', 3);
      final result = kernel!(rt, [
        VarRef(xReader),
        VarRef(yReader),
        VarRef(resultWriter),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultWriter);
      expect((value as ConstTerm).value, equals(42));
    });

    test('div/3 body kernel', () {
      final rt = GlpRuntime();

      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (yWriter, yReader) = rt.heap.allocateVariable();
      final (resultWriter, _) = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xWriter, 15);
      rt.heap.bindVariableConst(yWriter, 4);

      final kernel = rt.bodyKernels.lookup('_div', 3);
      final result = kernel!(rt, [
        VarRef(xReader),
        VarRef(yReader),
        VarRef(resultWriter),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultWriter);
      expect((value as ConstTerm).value, equals(3.75));
    });

    test('div/3 body kernel aborts on division by zero', () {
      final rt = GlpRuntime();

      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (yWriter, yReader) = rt.heap.allocateVariable();
      final (resultWriter, _) = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xWriter, 10);
      rt.heap.bindVariableConst(yWriter, 0);

      final kernel = rt.bodyKernels.lookup('_div', 3);
      final result = kernel!(rt, [
        VarRef(xReader),
        VarRef(yReader),
        VarRef(resultWriter),
      ]);
      expect(result, equals(BodyKernelResult.abort));
    });

    test('neg/2 body kernel', () {
      final rt = GlpRuntime();

      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (resultWriter, _) = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xWriter, 42);

      final kernel = rt.bodyKernels.lookup('_neg', 2);
      final result = kernel!(rt, [
        VarRef(xReader),
        VarRef(resultWriter),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultWriter);
      expect((value as ConstTerm).value, equals(-42));
    });

    test('sqrt_kernel/2 body kernel', () {
      final rt = GlpRuntime();

      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (resultWriter, _) = rt.heap.allocateVariable();

      rt.heap.bindVariableConst(xWriter, 16);

      final kernel = rt.bodyKernels.lookup('_sqrt', 2);
      final result = kernel!(rt, [
        VarRef(xReader),
        VarRef(resultWriter),
      ]);
      expect(result, equals(BodyKernelResult.success));

      final value = rt.heap.getValue(resultWriter);
      expect((value as ConstTerm).value, equals(4.0));
    });

    test('all standard body kernels are registered', () {
      final rt = GlpRuntime();

      // Binary arithmetic
      expect(rt.bodyKernels.has('_add', 3), isTrue);
      expect(rt.bodyKernels.has('_sub', 3), isTrue);
      expect(rt.bodyKernels.has('_mul', 3), isTrue);
      expect(rt.bodyKernels.has('_div', 3), isTrue);
      expect(rt.bodyKernels.has('_idiv', 3), isTrue);
      expect(rt.bodyKernels.has('_mod', 3), isTrue);

      // Unary
      expect(rt.bodyKernels.has('_neg', 2), isTrue);
      expect(rt.bodyKernels.has('_abs', 2), isTrue);

      // Math functions
      expect(rt.bodyKernels.has('_sqrt', 2), isTrue);
      expect(rt.bodyKernels.has('_sin', 2), isTrue);
      expect(rt.bodyKernels.has('_cos', 2), isTrue);
      expect(rt.bodyKernels.has('_tan', 2), isTrue);
      expect(rt.bodyKernels.has('_exp', 2), isTrue);
      expect(rt.bodyKernels.has('_ln', 2), isTrue);
      expect(rt.bodyKernels.has('_log10', 2), isTrue);
      expect(rt.bodyKernels.has('_pow', 3), isTrue);
      expect(rt.bodyKernels.has('_asin', 2), isTrue);
      expect(rt.bodyKernels.has('_acos', 2), isTrue);
      expect(rt.bodyKernels.has('_atan', 2), isTrue);

      // Type conversions
      expect(rt.bodyKernels.has('_integer', 2), isTrue);
      expect(rt.bodyKernels.has('_real', 2), isTrue);
      expect(rt.bodyKernels.has('_round', 2), isTrue);
      expect(rt.bodyKernels.has('_floor', 2), isTrue);
      expect(rt.bodyKernels.has('_ceil', 2), isTrue);
    });
  });

  group('End-to-end := system predicate', () {
    test('assign.glp compiles and merges correctly', () {
      // Load stdlib (assign.glp) - now SRSW compliant
      // Use relative path from glp_runtime directory
      final stdlibSource = File('../programs/self.glp').readAsStringSync();
      final stdlibCompiler = GlpCompiler();
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

      // Load stdlib (assign.glp) - now SRSW compliant
      // Use relative path from glp_runtime directory
      final stdlibSource = File('../programs/self.glp').readAsStringSync();
      final stdlibCompiler = GlpCompiler();
      final stdlibProg = stdlibCompiler.compile(stdlibSource);

      // Compile user program
      // Z? is reader in head; Z (writer) used by := in body
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
      final (resultWriter, resultReader) = rt.heap.allocateVariable();
      print('Allocated result variable: W$resultWriter, R$resultReader');

      // Create runner and scheduler
      final runner = BytecodeRunner(mergedProg);
      final sched = Scheduler(rt: rt, runner: runner);

      // Create environment with the result variable as argument
      // Pass writer so callee can write to it via :=
      final env = CallEnv(args: {
        0: VarRef(resultWriter),  // Pass writer to head position Z
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
      final isBound = rt.heap.isWriterBound(resultWriter);
      print('Result variable bound: $isBound');

      if (isBound) {
        final value = rt.heap.getValue(resultWriter);
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
