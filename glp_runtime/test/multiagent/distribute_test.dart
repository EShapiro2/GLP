/// Distribution Test (1 Producer, 2 Consumers)
///
/// Tests broadcast distribution:
///   @1 (producer): produces [1, 2, 3], distributes to both @2 and @3
///   @2 (consumer1): receives copy Y = [1, 2, 3]
///   @3 (consumer2): receives copy Z = [1, 2, 3]
///
/// Data flow: @1 → @2 AND @1 → @3 (broadcast)
///
/// Based on GLP distribute/3:
///   distribute([X|Xs], [X?|Ys?], [X?|Zs?]) :- ground(X?) | distribute(Xs?, Ys, Zs).
///   distribute([], [], []).

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/irma_context.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

void main() {
  test('Distribution: @1 broadcasts to @2 and @3', () {
    print('\n=== DISTRIBUTION TEST ===\n');
    print('Data flow: @1 produces and broadcasts to @2 and @3');

    // =========================================================
    // Step 1: Compile program
    // =========================================================
    final compiler = GlpCompiler();
    final program = compiler.compile('''
      produce([1, 2, 3]).
      % Simple distribute - produces complete output (no incremental streaming)
      distribute([1,2,3], [1,2,3], [1,2,3]).
      % Receive consumes the input
      receive(List, got(List?)) :- ground(List?) | true.
    ''');

    print('Compiled program: ${program.ops.length} ops');

    // =========================================================
    // Step 2: Create three agents
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'producer', runtime: runtime1);
    print('\n@1 (producer): Created');

    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'consumer1', runtime: runtime2);
    print('@2 (consumer1): Created');

    final runtime3 = GlpRuntime();
    final runner3 = BytecodeRunner(program);
    final scheduler3 = Scheduler(rt: runtime3, runners: {'main': runner3});
    final ctx3 = IrmaContext(agentId: 'consumer2', runtime: runtime3);
    print('@3 (consumer2): Created');

    // =========================================================
    // Step 3: Set up shared variables
    //   @1 owns: X (input), Y (output to consumer1), Z (output to consumer2)
    //   @2 imports Y? from @1
    //   @3 imports Z? from @1
    // =========================================================

    // @1 owns writer Y (copy for consumer1)
    final (yWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(yWriterAddr);
    print('@1: Allocated writer Y at addr=$yWriterAddr');

    // @1 owns writer Z (copy for consumer2)
    final (zWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(zWriterAddr);
    print('@1: Allocated writer Z at addr=$zWriterAddr');

    // @2 imports reader Y? from @1
    final y2ImportedAddr = runtime2.heap.allocateImportedReader();
    final y2Entry = VariableEntry(
      varId: y2ImportedAddr,
      isReader: true,
      creator: 'producer',
      role: VariableRole.importedReader,
      creatorLocalId: yWriterAddr,
    );
    ctx2.vp.add(VarKey(y2ImportedAddr, true), y2Entry);
    runtime2.heap.cells[y2ImportedAddr].content = y2Entry;
    print('@2: Imported reader Y? at addr=$y2ImportedAddr (from @1)');

    // @2 owns R2 (receive result)
    final (r2WriterAddr, _) = runtime2.heap.allocateVariable();
    ctx2.registerWriter(r2WriterAddr);
    print('@2: Allocated writer R2 at addr=$r2WriterAddr');

    // @3 imports reader Z? from @1
    final z3ImportedAddr = runtime3.heap.allocateImportedReader();
    final z3Entry = VariableEntry(
      varId: z3ImportedAddr,
      isReader: true,
      creator: 'producer',
      role: VariableRole.importedReader,
      creatorLocalId: zWriterAddr,
    );
    ctx3.vp.add(VarKey(z3ImportedAddr, true), z3Entry);
    runtime3.heap.cells[z3ImportedAddr].content = z3Entry;
    print('@3: Imported reader Z? at addr=$z3ImportedAddr (from @1)');

    // @3 owns R3 (receive result)
    final (r3WriterAddr, _) = runtime3.heap.allocateVariable();
    ctx3.registerWriter(r3WriterAddr);
    print('@3: Allocated writer R3 at addr=$r3WriterAddr');

    // =========================================================
    // Step 4: Set up message routing
    // =========================================================
    final messageLog = <String>[];

    ctx1.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('producer');
      IrmaContext? target;
      if (destination == 'consumer1') target = ctx2;
      if (destination == 'consumer2') target = ctx3;
      if (target != null) {
        final t = target!;
        if (message.type == MessageType.assignment) {
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? t.runtime.heap.allocateImportedReader()
              : t.runtime.heap.allocateImportedWriter(),
          );
          messageLog.add('[producer -> $destination] ASSIGNMENT');
          print('[producer -> $destination] ASSIGNMENT: $globalId := ${_shortValue(value)}');
          t.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[producer -> $destination] READ_REQUEST');
          print('[producer -> $destination] READ_REQUEST: varId=$varId');
          t.handleReadRequest(varId, 'producer');
        }
      }
    };

    ctx2.onMessageReady = (destination, message) {
      if (destination == 'producer') {
        final serializer = PayloadSerializer('consumer1');
        if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[consumer1 -> producer] READ_REQUEST');
          print('[consumer1 -> producer] READ_REQUEST: varId=$varId');
          ctx1.handleReadRequest(varId, 'consumer1');
        }
      }
    };

    ctx3.onMessageReady = (destination, message) {
      if (destination == 'producer') {
        final serializer = PayloadSerializer('consumer2');
        if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[consumer2 -> producer] READ_REQUEST');
          print('[consumer2 -> producer] READ_REQUEST: varId=$varId');
          ctx1.handleReadRequest(varId, 'consumer2');
        }
      }
    };

    print('\nMessage routing configured');

    // =========================================================
    // Step 5: Spawn goals
    // =========================================================

    // @1: distribute([1,2,3], Y, Z) - produces copies for both consumers
    final distributePC = program.labels['distribute/3']!;
    // Create input list directly as a term
    final inputList = StructTerm('.', [ConstTerm(1),
        StructTerm('.', [ConstTerm(2),
            StructTerm('.', [ConstTerm(3), ConstTerm('nil')])])]);
    final yWriterRef = VarRef(yWriterAddr);
    final zWriterRef = VarRef(zWriterAddr);
    runtime1.setGoalEnv(1, CallEnv(args: {0: inputList, 1: yWriterRef, 2: zWriterRef}));
    runtime1.setGoalProgram(1, 'main');
    runtime1.gq.enqueue(GoalRef(1, distributePC));
    print('\n@1: Spawned distribute([1,2,3], Y, Z)');

    // @2: receive(Y?, R2)
    final receivePC = program.labels['receive/2']!;
    final y2ReaderRef = VarRef(y2ImportedAddr);
    final r2WriterRef = VarRef(r2WriterAddr);
    runtime2.setGoalEnv(1, CallEnv(args: {0: y2ReaderRef, 1: r2WriterRef}));
    runtime2.setGoalProgram(1, 'main');
    runtime2.gq.enqueue(GoalRef(1, receivePC));
    print('@2: Spawned receive(Y?, R2)');

    // @3: receive(Z?, R3)
    final z3ReaderRef = VarRef(z3ImportedAddr);
    final r3WriterRef = VarRef(r3WriterAddr);
    runtime3.setGoalEnv(1, CallEnv(args: {0: z3ReaderRef, 1: r3WriterRef}));
    runtime3.setGoalProgram(1, 'main');
    runtime3.gq.enqueue(GoalRef(1, receivePC));
    print('@3: Spawned receive(Z?, R3)');

    // =========================================================
    // Step 6: Run execution
    // =========================================================
    print('\n=== EXECUTION ===\n');

    var iterations = 0;
    const maxIterations = 20;

    while (iterations < maxIterations) {
      iterations++;
      print('\n--- Iteration $iterations ---');

      // Run @1
      final result1 = scheduler1.drainWithStatus();
      print('@1: ${result1.status}');
      if (result1.status == ExecutionStatus.suspended) {
        ctx1.processSuspension(result1.blockingReaders);
      }
      ctx1.flushMessages();

      // Run @2
      final result2 = scheduler2.drainWithStatus(debug: true);
      print('@2: ${result2.status}, blockingReaders=${result2.blockingReaders}');
      if (result2.status == ExecutionStatus.suspended) {
        ctx2.processSuspension(result2.blockingReaders);
      }
      ctx2.flushMessages();

      // Run @3
      final result3 = scheduler3.drainWithStatus(debug: true);
      print('@3: ${result3.status}, blockingReaders=${result3.blockingReaders}');
      if (result3.status == ExecutionStatus.suspended) {
        ctx3.processSuspension(result3.blockingReaders);
      }
      ctx3.flushMessages();

      // Check if all completed
      if (result1.status == ExecutionStatus.succeeded &&
          result2.status == ExecutionStatus.succeeded &&
          result3.status == ExecutionStatus.succeeded &&
          runtime1.gq.isEmpty &&
          runtime2.gq.isEmpty &&
          runtime3.gq.isEmpty) {
        print('\nAll agents completed successfully');
        break;
      }
    }

    // =========================================================
    // Step 7: Display results
    // =========================================================
    print('\n=== RESULTS ===\n');

    print('Messages: ${messageLog.length}');
    print('Iterations: $iterations');

    print('@1 Y = ${_formatValue(runtime1.heap.derefAddr(yWriterAddr), runtime1.heap)}');
    print('@1 Z = ${_formatValue(runtime1.heap.derefAddr(zWriterAddr), runtime1.heap)}');
    print('@2 R2 = ${_formatValue(runtime2.heap.derefAddr(r2WriterAddr), runtime2.heap)}');
    print('@3 R3 = ${_formatValue(runtime3.heap.derefAddr(r3WriterAddr), runtime3.heap)}');

    // Assertions
    final r2Value = runtime2.heap.derefAddr(r2WriterAddr);
    expect(r2Value, isA<StructTerm>(), reason: '@2 R2 should be got(...)');

    final r3Value = runtime3.heap.derefAddr(r3WriterAddr);
    expect(r3Value, isA<StructTerm>(), reason: '@3 R3 should be got(...)');

    print('\n=== TEST PASSED ===');
  });
}

String _shortValue(Term value) {
  if (value is ConstTerm) return value.value.toString();
  if (value is StructTerm) {
    if (value.functor == '.') return '[...]';
    return '${value.functor}(...)';
  }
  return value.toString();
}

String _formatValue(Object value, dynamic heap) {
  if (value is ConstTerm) return value.value.toString();
  if (value is StructTerm) {
    if (value.functor == '.' && value.args.length == 2) {
      return _formatList(value, heap, 10);
    }
    return '${value.functor}(${value.args.map((a) => _formatValue(a, heap)).join(', ')})';
  }
  if (value is VarRef) {
    try {
      final deref = heap.derefAddr(value.addr);
      if (deref == value || deref is VariableEntry) return '_${value.addr}';
      return _formatValue(deref, heap);
    } catch (e) {
      return '_${value.addr}';
    }
  }
  if (value is VariableEntry) return '_entry(${value.varId})';
  return value.toString();
}

String _formatList(Object value, dynamic heap, int maxElements) {
  final elements = <String>[];
  Object current = value;
  var count = 0;
  while (count < maxElements) {
    if (current is ConstTerm && current.value == 'nil') break;
    if (current is StructTerm && current.functor == '.' && current.args.length == 2) {
      elements.add(_formatValue(current.args[0], heap));
      count++;
      final tail = current.args[1];
      if (tail is VarRef) {
        try { current = heap.derefAddr(tail.addr); }
        catch (e) { elements.add('|_${tail.addr}'); break; }
      } else {
        current = tail;
      }
    } else if (current is VarRef) {
      elements.add('|_${current.addr}');
      break;
    } else {
      elements.add('|$current');
      break;
    }
  }
  if (count >= maxElements) elements.add('...');
  return '[${elements.join(', ')}]';
}
