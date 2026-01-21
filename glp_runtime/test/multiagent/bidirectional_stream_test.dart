/// Bidirectional Stream Test
///
/// Two isolates running circular merge:
///   Isolate 1: merge(Xs?, [a], Ys)  -- reads Xs from isolate 2, writes Ys
///   Isolate 2: merge(Ys?, [b], Xs)  -- reads Ys from isolate 1, writes Xs
///
/// Expected infinite pattern:
///   From clause 2 firing when first arg is still unbound:
///   Ys = [a, b, a, b, ...]
///   Xs = [b, a, b, a, ...]
///
/// This test stops after 20 elements are produced.

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
  test('Bidirectional Stream - circular merge between two isolates', () {
    print('\n=== BIDIRECTIONAL STREAM TEST ===\n');

    // =========================================================
    // Step 1: Compile program
    // =========================================================
    final compiler = GlpCompiler();
    final program = compiler.compile('''
      merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
      merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
      merge([], [], []).
    ''');

    print('Compiled merge program: ${program.ops.length} ops');
    print('Labels: ${program.labels.keys.toList()}');

    // =========================================================
    // Step 2: Create isolate 1
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'isolate1', runtime: runtime1);
    print('\n@1: Created runtime and context');

    // =========================================================
    // Step 3: Create isolate 2
    // =========================================================
    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'isolate2', runtime: runtime2);
    print('@2: Created runtime and context');

    // =========================================================
    // Step 4: Allocate shared variables
    // =========================================================

    // Isolate 1 has writer Ys (result of its merge)
    final (ysWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(ysWriterAddr);
    print('@1: Allocated writer Ys at addr=$ysWriterAddr');

    // Isolate 2 has writer Xs (result of its merge)
    final (xsWriterAddr, _) = runtime2.heap.allocateVariable();
    ctx2.registerWriter(xsWriterAddr);
    print('@2: Allocated writer Xs at addr=$xsWriterAddr');

    // Isolate 1 imports reader Xs? from isolate 2
    final xs1ImportedAddr = runtime1.heap.allocateImportedReader();
    final xs1Entry = VariableEntry(
      varId: xs1ImportedAddr,
      isReader: true,
      creator: 'isolate2',
      role: VariableRole.importedReader,
      creatorLocalId: xsWriterAddr,
    );
    ctx1.vp.add(VarKey(xs1ImportedAddr, true), xs1Entry);
    runtime1.heap.cells[xs1ImportedAddr].content = xs1Entry;
    print('@1: Imported reader Xs? at addr=$xs1ImportedAddr (from @2 writer $xsWriterAddr)');

    // Isolate 2 imports reader Ys? from isolate 1
    final ys2ImportedAddr = runtime2.heap.allocateImportedReader();
    final ys2Entry = VariableEntry(
      varId: ys2ImportedAddr,
      isReader: true,
      creator: 'isolate1',
      role: VariableRole.importedReader,
      creatorLocalId: ysWriterAddr,
    );
    ctx2.vp.add(VarKey(ys2ImportedAddr, true), ys2Entry);
    runtime2.heap.cells[ys2ImportedAddr].content = ys2Entry;
    print('@2: Imported reader Ys? at addr=$ys2ImportedAddr (from @1 writer $ysWriterAddr)');

    // =========================================================
    // Step 5: Set up message routing with tracing
    // =========================================================
    final messageLog = <String>[];
    var elementCount = 0;
    const maxElements = 20;

    void logMessage(String from, String to, String type, String details) {
      final msg = '[$from -> $to] $type: $details';
      messageLog.add(msg);
      print(msg);
    }

    // @1 -> @2 routing
    ctx1.onMessageReady = (destination, message) {
      if (destination == 'isolate2') {
        if (message.type == MessageType.assignment) {
          final serializer = PayloadSerializer('isolate1');
          // Use receiver's (ctx2) heap for variable allocation
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx2.runtime.heap.allocateImportedReader()
              : ctx2.runtime.heap.allocateImportedWriter(),
          );
          logMessage('isolate1', 'isolate2', 'ASSIGNMENT', '$globalId := $value');
          ctx2.handleAssignment(globalId.creator, globalId.localId, value);
          elementCount++;
        } else if (message.type == MessageType.readRequest) {
          final serializer = PayloadSerializer('isolate1');
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          // Requester is the sender of this message: isolate1
          logMessage('isolate1', 'isolate2', 'READ_REQUEST', 'varId=$varId, requester=isolate1');
          ctx2.handleReadRequest(varId, 'isolate1');
        }
      }
    };

    // @2 -> @1 routing
    ctx2.onMessageReady = (destination, message) {
      if (destination == 'isolate1') {
        if (message.type == MessageType.assignment) {
          final serializer = PayloadSerializer('isolate2');
          // Use receiver's (ctx1) heap for variable allocation
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx1.runtime.heap.allocateImportedReader()
              : ctx1.runtime.heap.allocateImportedWriter(),
          );
          logMessage('isolate2', 'isolate1', 'ASSIGNMENT', '$globalId := $value');
          ctx1.handleAssignment(globalId.creator, globalId.localId, value);
          elementCount++;
        } else if (message.type == MessageType.readRequest) {
          final serializer = PayloadSerializer('isolate2');
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          // Requester is the sender of this message: isolate2
          logMessage('isolate2', 'isolate1', 'READ_REQUEST', 'varId=$varId, requester=isolate2');
          ctx1.handleReadRequest(varId, 'isolate2');
        }
      }
    };

    print('\nMessage routing configured');

    // =========================================================
    // Step 6: Spawn goals
    // =========================================================

    // @1: merge(Xs?, [a], Ys)
    final entryPC = program.labels['merge/3']!;

    // Build [a] list as cons cell: '.'(a, nil) and store on heap
    final aList1 = StructTerm('.', [ConstTerm('a'), ConstTerm('nil')]);
    final aList1Addr = runtime1.heap.storeTermOnHeap(aList1);

    // Create reader VarRef for imported Xs?
    final xsReaderRef1 = VarRef(xs1ImportedAddr);  // Reader address (imported)
    // Create writer VarRef for local Ys
    final ysWriterRef1 = VarRef(ysWriterAddr);  // Writer address

    final goalId1 = 1;
    final env1 = CallEnv(args: {0: xsReaderRef1, 1: VarRef(aList1Addr), 2: ysWriterRef1});
    runtime1.setGoalEnv(goalId1, env1);
    runtime1.setGoalProgram(goalId1, 'main');
    runtime1.gq.enqueue(GoalRef(goalId1, entryPC));
    print('\n@1: Spawned goal merge(Xs?, [a], Ys) at PC=$entryPC');

    // @2: merge(Ys?, [b], Xs)
    // Build [b] list as cons cell: '.'(b, nil) and store on heap
    final bList2 = StructTerm('.', [ConstTerm('b'), ConstTerm('nil')]);
    final bList2Addr = runtime2.heap.storeTermOnHeap(bList2);

    // Create reader VarRef for imported Ys?
    final ysReaderRef2 = VarRef(ys2ImportedAddr);  // Reader address (imported)
    // Create writer VarRef for local Xs
    final xsWriterRef2 = VarRef(xsWriterAddr);  // Writer address

    final goalId2 = 1;
    final env2 = CallEnv(args: {0: ysReaderRef2, 1: VarRef(bList2Addr), 2: xsWriterRef2});
    runtime2.setGoalEnv(goalId2, env2);
    runtime2.setGoalProgram(goalId2, 'main');
    runtime2.gq.enqueue(GoalRef(goalId2, entryPC));
    print('@2: Spawned goal merge(Ys?, [b], Xs) at PC=$entryPC');

    // =========================================================
    // Step 7: Run interleaved execution
    // =========================================================
    print('\n=== EXECUTION ===\n');

    var iterations = 0;
    const maxIterations = 100;

    while (elementCount < maxElements && iterations < maxIterations) {
      iterations++;
      print('\n--- Iteration $iterations (elements: $elementCount) ---');

      // Run @1
      print('\n@1 running...');
      final result1 = scheduler1.drainWithStatus();
      print('@1: status=${result1.status}, GQ=${runtime1.gq.length}');

      // Per spec Section 5.2 Case 2: Send read requests for blocking readers
      if (result1.status == ExecutionStatus.suspended && result1.blockingReaders.isNotEmpty) {
        print('@1: Processing ${result1.blockingReaders.length} blocking readers');
        ctx1.processSuspension(result1.blockingReaders);
      }

      // Flush @1 messages
      ctx1.flushMessages();

      // Run @2
      print('\n@2 running...');
      final result2 = scheduler2.drainWithStatus();
      print('@2: status=${result2.status}, GQ=${runtime2.gq.length}');

      // Per spec Section 5.2 Case 2: Send read requests for blocking readers
      if (result2.status == ExecutionStatus.suspended && result2.blockingReaders.isNotEmpty) {
        print('@2: Processing ${result2.blockingReaders.length} blocking readers');
        ctx2.processSuspension(result2.blockingReaders);
      }

      // Flush @2 messages
      ctx2.flushMessages();

      // Check if both are stuck
      if (result1.status == ExecutionStatus.suspended &&
          result2.status == ExecutionStatus.suspended &&
          runtime1.gq.isEmpty &&
          runtime2.gq.isEmpty) {
        print('\nBoth isolates suspended with no pending goals - checking messages...');
        // Give one more chance for messages to be processed
        ctx1.flushMessages();
        ctx2.flushMessages();
        if (runtime1.gq.isEmpty && runtime2.gq.isEmpty) {
          print('DEADLOCK: Both isolates waiting on each other');
          break;
        }
      }

      // Check if both succeeded
      if (result1.status == ExecutionStatus.succeeded &&
          result2.status == ExecutionStatus.succeeded) {
        print('\nBoth isolates completed');
        break;
      }
    }

    // =========================================================
    // Step 8: Display results
    // =========================================================
    print('\n=== RESULTS ===\n');
    print('Iterations: $iterations');
    print('Elements produced: $elementCount');
    print('\nMessage log:');
    for (final msg in messageLog) {
      print('  $msg');
    }

    // Try to display the streams
    print('\n@1 Ys stream head:');
    try {
      final ysValue = runtime1.heap.derefAddr(ysWriterAddr);
      print('  Ys = ${_formatStream(ysValue, runtime1.heap, 10)}');
    } catch (e) {
      print('  Error: $e');
    }

    print('\n@2 Xs stream head:');
    try {
      final xsValue = runtime2.heap.derefAddr(xsWriterAddr);
      print('  Xs = ${_formatStream(xsValue, runtime2.heap, 10)}');
    } catch (e) {
      print('  Error: $e');
    }

    // Assertions
    expect(elementCount, greaterThan(0), reason: 'Should produce at least some elements');
    print('\n=== TEST COMPLETE ===');
  });
}

/// Format a stream (list) up to maxElements
String _formatStream(Object value, dynamic heap, int maxElements) {
  final elements = <String>[];
  var current = value;
  var count = 0;

  while (count < maxElements) {
    // Check for empty list constant
    if (current is ConstTerm && current.value == '[]') {
      elements.add('[]');
      break;
    } else if (current is StructTerm && current.functor == '.' && current.args.length == 2) {
      // Cons cell [H|T]
      elements.add(_formatElement(current.args[0], heap));
      count++;
      // Dereference tail
      final tail = current.args[1];
      if (tail is VarRef) {
        try {
          current = heap.derefAddr(tail.addr);
        } catch (e) {
          elements.add('...(unbound: $e)');
          break;
        }
      } else {
        current = tail;
      }
    } else if (current is VarRef) {
      elements.add('_${current.addr}');
      break;
    } else if (current is VariableEntry) {
      elements.add('_entry');
      break;
    } else {
      elements.add(current.toString());
      break;
    }
  }

  if (count >= maxElements) {
    elements.add('...');
  }

  return '[${elements.join(', ')}]';
}

String _formatElement(Object elem, [dynamic heap]) {
  if (elem is ConstTerm) {
    return elem.value.toString();
  } else if (elem is VarRef && heap != null) {
    // Dereference VarRef to see if it's bound
    try {
      final derefed = heap.derefAddr(elem.addr);
      if (derefed is ConstTerm) {
        return derefed.value.toString();
      } else if (derefed is VarRef) {
        return '_${derefed.addr}';  // Still unbound
      } else {
        return derefed.toString();
      }
    } catch (e) {
      return '_${elem.addr}';
    }
  } else if (elem is VarRef) {
    return '_${elem.addr}';
  } else {
    return elem.toString();
  }
}
