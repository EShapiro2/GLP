/// Two-Hop Flow Test
///
/// Tests non-circular two-way data flow:
///   @1: produces Xs = [1,2,3]
///   @2: consumes Xs?, produces Ys = [got(1), got(2), got(3)]
///   @1: consumes Ys?
///
/// This is a stepping stone to circular merge - tests that data can
/// flow @1 -> @2 -> @1 without circularity.
///
/// Program:
///   produce([1,2,3]).
///   transform([X|Xs], [got(X?)|Ys?]) :- transform(Xs?, Ys).
///   transform([], []).

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
  test('Two-hop flow: @1 -> @2 -> @1', () {
    print('\n=== TWO-HOP FLOW TEST ===\n');
    print('Data flow: @1 produces Xs -> @2 transforms -> @1 consumes Ys');

    // =========================================================
    // Step 1: Compile program
    // =========================================================
    final compiler = GlpCompiler();
    final program = compiler.compile('''
      produce([1,2,3]).
      % Transform by prefixing each element with 'got'
      transform([X|Xs], [got(X?)|Ys?]) :- transform(Xs?, Ys).
      transform([], []).
    ''');

    print('Compiled program: ${program.ops.length} ops');
    print('Labels: ${program.labels.keys.toList()}');

    // =========================================================
    // Step 2: Create isolate 1 (producer and final consumer)
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'agent1', runtime: runtime1);
    print('\n@1: Created runtime and context (producer + final consumer)');

    // =========================================================
    // Step 3: Create isolate 2 (transformer)
    // =========================================================
    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'agent2', runtime: runtime2);
    print('@2: Created runtime and context (transformer)');

    // =========================================================
    // Step 4: Set up shared variables
    //   @1 owns Xs (producer writes)
    //   @2 imports Xs? (transformer reads)
    //   @2 owns Ys (transformer writes)
    //   @1 imports Ys? (consumer reads)
    // =========================================================

    // @1 owns writer Xs
    final (xsWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(xsWriterAddr);
    print('@1: Allocated writer Xs at addr=$xsWriterAddr');

    // @2 imports reader Xs? from @1
    final xs2ImportedAddr = runtime2.heap.allocateImportedReader();
    final xs2Entry = VariableEntry(
      varId: xs2ImportedAddr,
      isReader: true,
      creator: 'agent1',
      role: VariableRole.importedReader,
      creatorLocalId: xsWriterAddr,
    );
    ctx2.vp.add(VarKey(xs2ImportedAddr, true), xs2Entry);
    runtime2.heap.cells[xs2ImportedAddr].content = xs2Entry;
    print('@2: Imported reader Xs? at addr=$xs2ImportedAddr (from @1 writer $xsWriterAddr)');

    // @2 owns writer Ys
    final (ysWriterAddr, _) = runtime2.heap.allocateVariable();
    ctx2.registerWriter(ysWriterAddr);
    print('@2: Allocated writer Ys at addr=$ysWriterAddr');

    // @1 imports reader Ys? from @2
    final ys1ImportedAddr = runtime1.heap.allocateImportedReader();
    final ys1Entry = VariableEntry(
      varId: ys1ImportedAddr,
      isReader: true,
      creator: 'agent2',
      role: VariableRole.importedReader,
      creatorLocalId: ysWriterAddr,
    );
    ctx1.vp.add(VarKey(ys1ImportedAddr, true), ys1Entry);
    runtime1.heap.cells[ys1ImportedAddr].content = ys1Entry;
    print('@1: Imported reader Ys? at addr=$ys1ImportedAddr (from @2 writer $ysWriterAddr)');

    // =========================================================
    // Step 5: Set up message routing
    // =========================================================
    final messageLog = <String>[];

    void logMessage(String from, String to, String type, String details) {
      final msg = '[$from -> $to] $type: $details';
      messageLog.add(msg);
      print(msg);
    }

    // @1 -> @2 routing
    ctx1.onMessageReady = (destination, message) {
      if (destination == 'agent2') {
        if (message.type == MessageType.assignment) {
          final serializer = PayloadSerializer('agent1');
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx2.runtime.heap.allocateImportedReader()
              : ctx2.runtime.heap.allocateImportedWriter(),
          );
          logMessage('agent1', 'agent2', 'ASSIGNMENT', '$globalId := ${_shortValue(value)}');
          ctx2.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final serializer = PayloadSerializer('agent1');
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          logMessage('agent1', 'agent2', 'READ_REQUEST', 'varId=$varId');
          ctx2.handleReadRequest(varId, 'agent1');
        }
      }
    };

    // @2 -> @1 routing
    ctx2.onMessageReady = (destination, message) {
      if (destination == 'agent1') {
        if (message.type == MessageType.assignment) {
          final serializer = PayloadSerializer('agent2');
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx1.runtime.heap.allocateImportedReader()
              : ctx1.runtime.heap.allocateImportedWriter(),
          );
          logMessage('agent2', 'agent1', 'ASSIGNMENT', '$globalId := ${_shortValue(value)}');
          ctx1.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final serializer = PayloadSerializer('agent2');
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          logMessage('agent2', 'agent1', 'READ_REQUEST', 'varId=$varId');
          ctx1.handleReadRequest(varId, 'agent2');
        }
      }
    };

    print('\nMessage routing configured');

    // =========================================================
    // Step 6: Spawn goals
    // =========================================================

    // @1: produce(Xs)
    final producePC = program.labels['produce/1']!;
    final xsWriterRef = VarRef(xsWriterAddr);
    final goalId1a = 1;
    final env1a = CallEnv(args: {0: xsWriterRef});
    runtime1.setGoalEnv(goalId1a, env1a);
    runtime1.setGoalProgram(goalId1a, 'main');
    runtime1.gq.enqueue(GoalRef(goalId1a, producePC));
    print('\n@1: Spawned goal produce(Xs) at PC=$producePC');

    // @2: transform(Xs?, Ys)
    final transformPC = program.labels['transform/2']!;
    final xs2ReaderRef = VarRef(xs2ImportedAddr);
    final ysWriterRef = VarRef(ysWriterAddr);
    final goalId2 = 1;
    final env2 = CallEnv(args: {0: xs2ReaderRef, 1: ysWriterRef});
    runtime2.setGoalEnv(goalId2, env2);
    runtime2.setGoalProgram(goalId2, 'main');
    runtime2.gq.enqueue(GoalRef(goalId2, transformPC));
    print('@2: Spawned goal transform(Xs?, Ys) at PC=$transformPC');

    // =========================================================
    // Step 7: Run execution
    // =========================================================
    print('\n=== EXECUTION ===\n');

    var iterations = 0;
    const maxIterations = 20;

    while (iterations < maxIterations) {
      iterations++;
      print('\n--- Iteration $iterations ---');

      // Run @1 first (producer)
      print('\n@1 running...');
      final result1 = scheduler1.drainWithStatus();
      print('@1: status=${result1.status}, GQ=${runtime1.gq.length}');
      if (result1.status == ExecutionStatus.suspended) {
        ctx1.processSuspension(result1.blockingReaders);
      }
      ctx1.flushMessages();

      // Run @2 (transformer)
      print('\n@2 running...');
      final result2 = scheduler2.drainWithStatus(debug: true);
      print('@2: status=${result2.status}, GQ=${runtime2.gq.length}');
      if (result2.status == ExecutionStatus.suspended) {
        ctx2.processSuspension(result2.blockingReaders);
      }
      ctx2.flushMessages();

      // Check if both completed
      if (result1.status == ExecutionStatus.succeeded &&
          result2.status == ExecutionStatus.succeeded &&
          runtime1.gq.isEmpty &&
          runtime2.gq.isEmpty) {
        print('\nBoth agents completed successfully');
        break;
      }

      // Check for deadlock
      if (result1.status == ExecutionStatus.suspended &&
          result2.status == ExecutionStatus.suspended &&
          runtime1.gq.isEmpty &&
          runtime2.gq.isEmpty) {
        print('\nDEADLOCK: Both agents suspended');
        break;
      }
    }

    // =========================================================
    // Step 8: Request Ys? from @1 and display results
    // =========================================================
    print('\n=== RESULTS ===');

    // @1 requests Ys?
    print('\n@1 requesting Ys?...');
    ctx1.processSuspension({ys1ImportedAddr});
    ctx1.flushMessages();
    ctx2.flushMessages();

    print('\nIterations: $iterations');
    print('Messages: ${messageLog.map((m) => m.split('] ')[1].split(':')[0]).join(', ')}');

    // Check @1's Xs value
    print('\n@1 Xs = ${_formatValue(runtime1.heap.derefAddr(xsWriterAddr), runtime1.heap)}');

    // Check @2's Ys value
    print('@2 Ys = ${_formatValue(runtime2.heap.derefAddr(ysWriterAddr), runtime2.heap)}');

    // Check @1's Ys? value
    print('@1 Ys? = ${_formatValue(runtime1.heap.derefAddr(ys1ImportedAddr), runtime1.heap)}');

    // Assertions
    // @1 Xs should be [1,2,3]
    final xsValue = runtime1.heap.derefAddr(xsWriterAddr);
    expect(xsValue, isA<StructTerm>(), reason: '@1 Xs should be a cons cell');

    // @2 Ys should be [got(1),got(2),got(3)]
    final ysValue = runtime2.heap.derefAddr(ysWriterAddr);
    expect(ysValue, isA<StructTerm>(), reason: '@2 Ys should be a cons cell');

    print('\n=== TEST PASSED ===');
  });
}

String _shortValue(Term value) {
  if (value is ConstTerm) {
    return value.value.toString();
  } else if (value is StructTerm) {
    if (value.functor == '.' && value.args.length == 2) {
      return '[...]';
    }
    return '${value.functor}/...';
  }
  return value.toString();
}

String _formatValue(Object value, dynamic heap) {
  if (value is ConstTerm) {
    return value.value.toString();
  } else if (value is StructTerm) {
    if (value.functor == '.' && value.args.length == 2) {
      return _formatList(value, heap, 10);
    } else {
      return '${value.functor}(${value.args.map((a) => _formatValue(a, heap)).join(', ')})';
    }
  } else if (value is VarRef) {
    try {
      final deref = heap.derefAddr(value.addr);
      if (deref == value || deref is VariableEntry) {
        return '_${value.addr}';
      }
      return _formatValue(deref, heap);
    } catch (e) {
      return '_${value.addr}';
    }
  } else if (value is VariableEntry) {
    return '_entry(${value.varId})';
  } else {
    return value.toString();
  }
}

String _formatList(Object value, dynamic heap, int maxElements) {
  final elements = <String>[];
  Object current = value;
  var count = 0;

  while (count < maxElements) {
    if (current is ConstTerm && current.value == 'nil') {
      break;
    } else if (current is StructTerm && current.functor == '.' && current.args.length == 2) {
      elements.add(_formatValue(current.args[0], heap));
      count++;
      final tail = current.args[1];
      if (tail is VarRef) {
        try {
          current = heap.derefAddr(tail.addr);
        } catch (e) {
          elements.add('|_${tail.addr}');
          break;
        }
      } else {
        current = tail;
      }
    } else if (current is VarRef) {
      elements.add('|_${current.addr}');
      break;
    } else if (current is VariableEntry) {
      elements.add('|_entry');
      break;
    } else {
      elements.add('|$current');
      break;
    }
  }

  if (count >= maxElements) {
    elements.add('...');
  }

  return '[${elements.join(', ')}]';
}
