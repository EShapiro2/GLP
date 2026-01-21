/// Reversed Flow Test
///
/// Tests one-way data flow with REVERSED direction:
///   @1: q(X?)     -- receives X? as imported reader, processes list
///   @2: p(X)      -- binds X = [a,b]
///
/// This is the opposite direction of simple_imported_reader_test.dart
/// to verify data can flow @2 -> @1 as well as @1 -> @2.
///
/// Program:
///   p([a,b]).
///   q([X|Xs]) :- q(Xs?).
///   q([]).

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
  test('Reversed flow - q(X?)@1, p(X)@2', () {
    print('\n=== REVERSED FLOW TEST ===\n');
    print('Direction: @2 (writer) -> @1 (reader)');

    // =========================================================
    // Step 1: Compile program
    // =========================================================
    final compiler = GlpCompiler();
    final program = compiler.compile('''
      p([a,b]).
      q([_|Xs]) :- q(Xs?).
      q([]).
    ''');

    print('Compiled program: ${program.ops.length} ops');
    print('Labels: ${program.labels.keys.toList()}');

    // =========================================================
    // Step 2: Create isolate 1 (@1) - will have the READER
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'isolate1', runtime: runtime1);
    print('\n@1: Created runtime and context (READER side)');

    // =========================================================
    // Step 3: Create isolate 2 (@2) - will have the WRITER
    // =========================================================
    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'isolate2', runtime: runtime2);
    print('@2: Created runtime and context (WRITER side)');

    // =========================================================
    // Step 4: Allocate shared variable X
    // REVERSED: @2 has writer, @1 has imported reader
    // =========================================================

    // @2 has writer X (result of p(X))
    final (xWriterAddr, _) = runtime2.heap.allocateVariable();
    ctx2.registerWriter(xWriterAddr);
    print('@2: Allocated writer X at addr=$xWriterAddr');

    // @1 imports reader X? from @2
    final x1ImportedAddr = runtime1.heap.allocateImportedReader();
    final x1Entry = VariableEntry(
      varId: x1ImportedAddr,
      isReader: true,
      creator: 'isolate2',  // Creator is @2, not @1
      role: VariableRole.importedReader,
      creatorLocalId: xWriterAddr,
    );
    ctx1.vp.add(VarKey(x1ImportedAddr, true), x1Entry);
    runtime1.heap.cells[x1ImportedAddr].content = x1Entry;
    print('@1: Imported reader X? at addr=$x1ImportedAddr (from @2 writer $xWriterAddr)');

    // =========================================================
    // Step 5: Set up message routing
    // =========================================================
    final messageLog = <String>[];

    void logMessage(String from, String to, String type, String details) {
      final msg = '[$from -> $to] $type: $details';
      messageLog.add(msg);
      print(msg);
    }

    // @1 -> @2 routing (read requests go this way)
    ctx1.onMessageReady = (destination, message) {
      print('[MSG TRACE] @1 sending to $destination: type=${message.type}');
      if (destination == 'isolate2') {
        if (message.type == MessageType.assignment) {
          final serializer = PayloadSerializer('isolate1');
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx2.runtime.heap.allocateImportedReader()
              : ctx2.runtime.heap.allocateImportedWriter(),
          );
          logMessage('isolate1', 'isolate2', 'ASSIGNMENT', '$globalId := $value');
          ctx2.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final serializer = PayloadSerializer('isolate1');
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          logMessage('isolate1', 'isolate2', 'READ_REQUEST', 'varId=$varId, requester=isolate1');
          ctx2.handleReadRequest(varId, 'isolate1');
        }
      }
    };

    // @2 -> @1 routing (assignments go this way)
    ctx2.onMessageReady = (destination, message) {
      print('[MSG TRACE] @2 sending to $destination: type=${message.type}');
      if (destination == 'isolate1') {
        if (message.type == MessageType.assignment) {
          final serializer = PayloadSerializer('isolate2');
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx1.runtime.heap.allocateImportedReader()
              : ctx1.runtime.heap.allocateImportedWriter(),
          );
          logMessage('isolate2', 'isolate1', 'ASSIGNMENT', '$globalId := $value');
          ctx1.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final serializer = PayloadSerializer('isolate2');
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          logMessage('isolate2', 'isolate1', 'READ_REQUEST', 'varId=$varId, requester=isolate2');
          ctx1.handleReadRequest(varId, 'isolate2');
        }
      }
    };

    print('\nMessage routing configured');

    // =========================================================
    // Step 6: Spawn goals (REVERSED from original test)
    // =========================================================

    // @1: q(X?) - processes the imported reader
    final qEntryPC = program.labels['q/1']!;
    final xReaderRef1 = VarRef(x1ImportedAddr);
    final goalId1 = 1;
    final env1 = CallEnv(args: {0: xReaderRef1});
    runtime1.setGoalEnv(goalId1, env1);
    runtime1.setGoalProgram(goalId1, 'main');
    runtime1.gq.enqueue(GoalRef(goalId1, qEntryPC));
    print('\n@1: Spawned goal q(X?) at PC=$qEntryPC');

    // @2: p(X) - binds X = [a,b]
    final pEntryPC = program.labels['p/1']!;
    final xWriterRef2 = VarRef(xWriterAddr);
    final goalId2 = 1;
    final env2 = CallEnv(args: {0: xWriterRef2});
    runtime2.setGoalEnv(goalId2, env2);
    runtime2.setGoalProgram(goalId2, 'main');
    runtime2.gq.enqueue(GoalRef(goalId2, pEntryPC));
    print('@2: Spawned goal p(X) at PC=$pEntryPC');

    // Check what's at the imported reader address before execution
    print('@1: Before execution, cell[$x1ImportedAddr] = ${runtime1.heap.cells[x1ImportedAddr].content}');

    // =========================================================
    // Step 7: Run execution
    // =========================================================
    print('\n=== EXECUTION ===\n');

    var iterations = 0;
    const maxIterations = 20;

    while (iterations < maxIterations) {
      iterations++;
      print('\n--- Iteration $iterations ---');

      // Run @1 FIRST (the reader side - should suspend)
      print('\n@1 running first (reader)...');
      print('@1: Before running, cell[$x1ImportedAddr] = ${runtime1.heap.cells[x1ImportedAddr].content}');
      final result1 = scheduler1.drainWithStatus(debug: true);
      print('@1: status=${result1.status}, GQ=${runtime1.gq.length}');
      print('@1: blockingReaders=${result1.blockingReaders}');
      print('@1: rt.suspended.keys=${runtime1.suspended.keys.toList()}');
      print('@1: After running, cell[$x1ImportedAddr] = ${runtime1.heap.cells[x1ImportedAddr].content}');

      // Process suspension if suspended
      if (result1.status == ExecutionStatus.suspended) {
        print('@1: Processing suspension, sending read request');
        ctx1.processSuspension(result1.blockingReaders);
      }
      ctx1.flushMessages();

      // Run @2 second (the writer side)
      print('\n@2 running (writer)...');
      final result2 = scheduler2.drainWithStatus();
      print('@2: status=${result2.status}, GQ=${runtime2.gq.length}');
      print('@2: After execution, X at addr $xWriterAddr = ${runtime2.heap.derefAddr(xWriterAddr)}');
      ctx2.flushMessages();

      // Check if both completed
      if (result1.status == ExecutionStatus.succeeded &&
          result2.status == ExecutionStatus.succeeded &&
          runtime1.gq.isEmpty &&
          runtime2.gq.isEmpty) {
        print('\nBoth isolates completed successfully');
        break;
      }

      // Check for deadlock
      if (result1.status == ExecutionStatus.suspended &&
          result2.status == ExecutionStatus.suspended &&
          runtime1.gq.isEmpty &&
          runtime2.gq.isEmpty) {
        print('\nDEADLOCK: Both isolates suspended');
        break;
      }
    }

    // =========================================================
    // Step 8: Display results
    // =========================================================
    print('\n=== RESULTS ===\n');
    print('Iterations: $iterations');
    print('\nMessage log:');
    for (final msg in messageLog) {
      print('  $msg');
    }

    // Check X value at @2 (the writer)
    print('\n@2 X value (writer):');
    try {
      final xValue = runtime2.heap.derefAddr(xWriterAddr);
      print('  X = ${_formatValue(xValue, runtime2.heap)}');
    } catch (e) {
      print('  Error: $e');
    }

    // Check X? value at @1 (the reader)
    print('\n@1 X? value (reader):');
    try {
      final xValue1 = runtime1.heap.derefAddr(x1ImportedAddr);
      print('  X? = ${_formatValue(xValue1, runtime1.heap)}');
    } catch (e) {
      print('  Error: $e');
    }

    // Assertions
    // @2 should have X = [a,b]
    final xValue = runtime2.heap.derefAddr(xWriterAddr);
    expect(xValue, isA<StructTerm>(), reason: '@2 X should be a cons cell');

    // Message log should show:
    // 1. READ_REQUEST from @1 to @2
    // 2. ASSIGNMENT from @2 to @1
    expect(messageLog.length, equals(2), reason: 'Should have exactly 2 messages');
    expect(messageLog[0], contains('READ_REQUEST'), reason: 'First message should be READ_REQUEST');
    expect(messageLog[1], contains('ASSIGNMENT'), reason: 'Second message should be ASSIGNMENT');

    print('\n=== TEST PASSED ===');
  });
}

String _formatValue(Object value, dynamic heap) {
  if (value is ConstTerm) {
    return value.value.toString();
  } else if (value is StructTerm) {
    if (value.functor == '.' && value.args.length == 2) {
      // List
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
  var current = value;
  var count = 0;

  while (count < maxElements) {
    if (current is ConstTerm && current.value == '[]') {
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
