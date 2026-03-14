/// Tests for arithmetic body kernels with Pointer Architecture Heap
///
/// Adapted from: test/bytecode/arithmetic_test.dart
/// For spec: docs/heap-pointer-architecture-spec.md v3.0
///
/// Tests that arithmetic operations work correctly with the new
/// pointer-based heap architecture.
library;

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
    // Compile assign.glp
    final stdlibSource = File('../programs/self.glp').readAsStringSync();
    final stdlibCompiler = GlpCompiler();
    stdlibProg = stdlibCompiler.compile(stdlibSource);
    print('Stdlib compiled: ${stdlibProg.ops.length} instructions');
  });

  group('Arithmetic via := system predicate - Pointer Architecture', () {
    test('add/3 body kernel executes directly', () {
      final rt = GlpRuntime();

      // Allocate variables - now returns (writerAddr, readerAddr) tuple
      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (yWriter, yReader) = rt.heap.allocateVariable();
      final (resultWriter, resultReader) = rt.heap.allocateVariable();

      // Bind X and Y to numbers
      rt.heap.bindWriter(xWriter, ConstTerm(5));
      rt.heap.bindWriter(yWriter, ConstTerm(3));

      // Call add kernel - VarRef now just has addr field
      // Readers for input, writer for output
      final xRef = VarRef(xReader);
      final yRef = VarRef(yReader);
      final resultRef = VarRef(resultWriter);

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

      rt.heap.bindWriter(xWriter, ConstTerm(10));
      rt.heap.bindWriter(yWriter, ConstTerm(4));

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

      rt.heap.bindWriter(xWriter, ConstTerm(7));
      rt.heap.bindWriter(yWriter, ConstTerm(6));

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

      rt.heap.bindWriter(xWriter, ConstTerm(15));
      rt.heap.bindWriter(yWriter, ConstTerm(4));

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

      rt.heap.bindWriter(xWriter, ConstTerm(10));
      rt.heap.bindWriter(yWriter, ConstTerm(0));

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

      rt.heap.bindWriter(xWriter, ConstTerm(42));

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

      rt.heap.bindWriter(xWriter, ConstTerm(16));

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

  group('End-to-end := system predicate - Pointer Architecture', () {
    test('assign.glp compiles and merges correctly', () {
      final stdlibSource = File('../programs/self.glp').readAsStringSync();
      final stdlibCompiler = GlpCompiler();
      final stdlibProg = stdlibCompiler.compile(stdlibSource);

      final userSource = '''
        hello.
      ''';
      final userCompiler = GlpCompiler();
      final userProg = userCompiler.compile(userSource);

      final mergedProg = userProg.merge(stdlibProg);

      print('Merged program has ${mergedProg.ops.length} instructions');
      print('Labels: ${mergedProg.labels.keys.toList()}');

      expect(mergedProg.labels.containsKey(':=/2'), isTrue,
          reason: 'Merged program should contain :=/2 from stdlib');
      expect(mergedProg.labels.containsKey('hello/0'), isTrue,
          reason: 'Merged program should contain hello/0 from user code');
    });

    test('user program with := compiles correctly with SRSW', () {
      final userSource = '''
        compute_sum(Z?) :- Z := 5 + 3.
      ''';
      final compiler = GlpCompiler();
      final prog = compiler.compile(userSource);

      expect(prog.ops.isNotEmpty, isTrue);
      expect(prog.labels.containsKey('compute_sum/1'), isTrue);

      print('compute_sum/1 compiled to ${prog.ops.length} instructions');
    });

    test('Z := 5 + 3 executes and binds Z to 8', () {
      print('\n=== END-TO-END ARITHMETIC TEST (Pointer Architecture) ===');

      final stdlibSource = File('../programs/self.glp').readAsStringSync();
      final stdlibCompiler = GlpCompiler();
      final stdlibProg = stdlibCompiler.compile(stdlibSource);

      final userSource = '''
        compute_sum(Z?) :- Z := 5 + 3.
      ''';
      final userCompiler = GlpCompiler();
      final userProg = userCompiler.compile(userSource);

      final mergedProg = userProg.merge(stdlibProg);
      print('Merged program: ${mergedProg.ops.length} instructions');

      final rt = GlpRuntime();

      // Allocate a variable for the result (Z) - now returns tuple
      final (resultWriter, resultReader) = rt.heap.allocateVariable();
      print('Allocated result variable: writer=$resultWriter, reader=$resultReader');

      final runner = BytecodeRunner(mergedProg);
      final sched = Scheduler(rt: rt, runner: runner);

      // Create environment with the result variable
      // Pass writer so callee can write via :=
      final env = CallEnv(args: {
        0: VarRef(resultWriter),
      });

      final goalId = 1;
      rt.setGoalEnv(goalId, env);

      final entryPc = mergedProg.labels['compute_sum/1'];
      expect(entryPc, isNotNull, reason: 'compute_sum/1 should exist');
      print('compute_sum/1 entry at PC $entryPc');

      rt.gq.enqueue(GoalRef(goalId, entryPc!));

      print('\nRunning scheduler to drain all goals...');
      final ran = sched.drain(maxCycles: 100, debug: true, debugOutput: true);
      print('Goals executed: ${ran.length}');

      // Check if the result variable is bound
      final isBound = rt.heap.isFullyBound(resultWriter);
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

  group('Variable Chain Dereferencing', () {
    test('arithmetic through variable chain', () {
      final rt = GlpRuntime();

      // Create chain: X -> Y -> 5
      final (xWriter, xReader) = rt.heap.allocateVariable();
      final (yWriter, yReader) = rt.heap.allocateVariable();
      final (zWriter, zReader) = rt.heap.allocateVariable();
      final (resultWriter, _) = rt.heap.allocateVariable();

      // Bind Y to 5
      rt.heap.bindWriter(yWriter, ConstTerm(5));

      // Bind X to Y (chain X -> Y)
      rt.heap.bindWriterToReader(xWriter, yReader);

      // Bind Z to 3
      rt.heap.bindWriter(zWriter, ConstTerm(3));

      // Add should dereference X through Y to get 5
      final kernel = rt.bodyKernels.lookup('_add', 3);
      final result = kernel!(rt, [
        VarRef(xReader),  // Will dereference to 5
        VarRef(zReader),  // Will dereference to 3
        VarRef(resultWriter),
      ]);

      expect(result, equals(BodyKernelResult.success));
      final value = rt.heap.getValue(resultWriter);
      expect((value as ConstTerm).value, equals(8));
    });
  });
}
