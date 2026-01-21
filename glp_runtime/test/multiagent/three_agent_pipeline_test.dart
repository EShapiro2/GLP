/// Three-Agent Pipeline Test
///
/// Tests data flow through three agents in a pipeline:
///   @1 (producer): produces [1, 2, 3]
///   @2 (transformer): transforms to [got(1), got(2), got(3)]
///   @3 (consumer): receives and wraps as done(List)
///
/// Data flow: @1 → @2 → @3
///
/// This tests multi-hop data flow with more than two agents.

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
  test('Three-agent pipeline: @1 → @2 → @3', () {
    print('\n=== THREE-AGENT PIPELINE TEST ===\n');
    print('Data flow: @1 produces → @2 transforms → @3 wraps');

    // =========================================================
    // Step 1: Compile program
    // =========================================================
    final compiler = GlpCompiler();
    // Use simple clauses that produce complete values (no incremental binding)
    final program = compiler.compile('''
      produce([1, 2, 3]).
      % Transform produces complete result
      transform([1, 2, 3], [got(1), got(2), got(3)]).
      % Wrap produces complete result
      wrap(List, done(List?)) :- ground(List?) | true.
    ''');

    print('Compiled program: ${program.ops.length} ops');

    // =========================================================
    // Step 2: Create three agents
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'agent1', runtime: runtime1);
    print('\n@1: Created (producer)');

    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'agent2', runtime: runtime2);
    print('@2: Created (transformer)');

    final runtime3 = GlpRuntime();
    final runner3 = BytecodeRunner(program);
    final scheduler3 = Scheduler(rt: runtime3, runners: {'main': runner3});
    final ctx3 = IrmaContext(agentId: 'agent3', runtime: runtime3);
    print('@3: Created (consumer)');

    // =========================================================
    // Step 3: Set up shared variables
    //   @1 owns A (raw numbers)
    //   @2 imports A? from @1, owns B (transformed)
    //   @3 imports B? from @2, owns C (wrapped)
    // =========================================================

    // @1 owns writer A
    final (aWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(aWriterAddr);
    print('\n@1: Allocated writer A at addr=$aWriterAddr');

    // @2 imports reader A? from @1
    final a2ImportedAddr = runtime2.heap.allocateImportedReader();
    final a2Entry = VariableEntry(
      varId: a2ImportedAddr,
      isReader: true,
      creator: 'agent1',
      role: VariableRole.importedReader,
      creatorLocalId: aWriterAddr,
    );
    ctx2.vp.add(VarKey(a2ImportedAddr, true), a2Entry);
    runtime2.heap.cells[a2ImportedAddr].content = a2Entry;
    print('@2: Imported reader A? at addr=$a2ImportedAddr (from @1)');

    // @2 owns writer B
    final (bWriterAddr, _) = runtime2.heap.allocateVariable();
    ctx2.registerWriter(bWriterAddr);
    print('@2: Allocated writer B at addr=$bWriterAddr');

    // @3 imports reader B? from @2
    final b3ImportedAddr = runtime3.heap.allocateImportedReader();
    final b3Entry = VariableEntry(
      varId: b3ImportedAddr,
      isReader: true,
      creator: 'agent2',
      role: VariableRole.importedReader,
      creatorLocalId: bWriterAddr,
    );
    ctx3.vp.add(VarKey(b3ImportedAddr, true), b3Entry);
    runtime3.heap.cells[b3ImportedAddr].content = b3Entry;
    print('@3: Imported reader B? at addr=$b3ImportedAddr (from @2)');

    // @3 owns writer C
    final (cWriterAddr, _) = runtime3.heap.allocateVariable();
    ctx3.registerWriter(cWriterAddr);
    print('@3: Allocated writer C at addr=$cWriterAddr');

    // =========================================================
    // Step 4: Set up message routing between all agents
    // =========================================================
    final messageLog = <String>[];

    // Set up routing for all agents - each agent needs ONE callback that routes to all destinations
    ctx1.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('agent1');
      if (destination == 'agent2') {
        if (message.type == MessageType.assignment) {
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx2.runtime.heap.allocateImportedReader()
              : ctx2.runtime.heap.allocateImportedWriter(),
          );
          messageLog.add('[agent1 -> agent2] ASSIGNMENT');
          print('[agent1 -> agent2] ASSIGNMENT: $globalId := ${_shortValue(value)}');
          ctx2.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[agent1 -> agent2] READ_REQUEST');
          print('[agent1 -> agent2] READ_REQUEST: varId=$varId');
          ctx2.handleReadRequest(varId, 'agent1');
        }
      }
    };

    ctx2.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('agent2');
      IrmaContext? target;
      String targetName = '';
      if (destination == 'agent1') { target = ctx1; targetName = 'agent1'; }
      if (destination == 'agent3') { target = ctx3; targetName = 'agent3'; }
      if (target != null) {
        if (message.type == MessageType.assignment) {
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? target.runtime.heap.allocateImportedReader()
              : target.runtime.heap.allocateImportedWriter(),
          );
          messageLog.add('[agent2 -> $targetName] ASSIGNMENT');
          print('[agent2 -> $targetName] ASSIGNMENT: $globalId := ${_shortValue(value)}');
          target.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[agent2 -> $targetName] READ_REQUEST');
          print('[agent2 -> $targetName] READ_REQUEST: varId=$varId');
          target.handleReadRequest(varId, 'agent2');
        }
      }
    };

    ctx3.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('agent3');
      if (destination == 'agent2') {
        if (message.type == MessageType.assignment) {
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx2.runtime.heap.allocateImportedReader()
              : ctx2.runtime.heap.allocateImportedWriter(),
          );
          messageLog.add('[agent3 -> agent2] ASSIGNMENT');
          print('[agent3 -> agent2] ASSIGNMENT: $globalId := ${_shortValue(value)}');
          ctx2.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[agent3 -> agent2] READ_REQUEST');
          print('[agent3 -> agent2] READ_REQUEST: varId=$varId');
          ctx2.handleReadRequest(varId, 'agent3');
        }
      }
    };

    print('\nMessage routing configured');

    // =========================================================
    // Step 5: Spawn goals
    // =========================================================

    // @1: produce(A)
    final producePC = program.labels['produce/1']!;
    final aWriterRef = VarRef(aWriterAddr);
    runtime1.setGoalEnv(1, CallEnv(args: {0: aWriterRef}));
    runtime1.setGoalProgram(1, 'main');
    runtime1.gq.enqueue(GoalRef(1, producePC));
    print('\n@1: Spawned produce(A)');

    // @2: transform(A?, B)
    final transformPC = program.labels['transform/2']!;
    final a2ReaderRef = VarRef(a2ImportedAddr);
    final bWriterRef = VarRef(bWriterAddr);
    runtime2.setGoalEnv(1, CallEnv(args: {0: a2ReaderRef, 1: bWriterRef}));
    runtime2.setGoalProgram(1, 'main');
    runtime2.gq.enqueue(GoalRef(1, transformPC));
    print('@2: Spawned transform(A?, B)');

    // @3: wrap(B?, C)
    final wrapPC = program.labels['wrap/2']!;
    final b3ReaderRef = VarRef(b3ImportedAddr);
    final cWriterRef = VarRef(cWriterAddr);
    runtime3.setGoalEnv(1, CallEnv(args: {0: b3ReaderRef, 1: cWriterRef}));
    runtime3.setGoalProgram(1, 'main');
    runtime3.gq.enqueue(GoalRef(1, wrapPC));
    print('@3: Spawned wrap(B?, C)');

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

    print('\n@1 A = ${_formatValue(runtime1.heap.derefAddr(aWriterAddr), runtime1.heap)}');
    print('@2 B = ${_formatValue(runtime2.heap.derefAddr(bWriterAddr), runtime2.heap)}');
    print('@3 C = ${_formatValue(runtime3.heap.derefAddr(cWriterAddr), runtime3.heap)}');

    // Assertions
    final cValue = runtime3.heap.derefAddr(cWriterAddr);
    expect(cValue, isA<StructTerm>(), reason: '@3 C should be done(...)');
    final cStruct = cValue as StructTerm;
    expect(cStruct.functor, equals('done'), reason: '@3 C should have functor done');

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
