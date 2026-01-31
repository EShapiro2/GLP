/// Simple Imported Reader Test
///
/// Tests one-way data flow with imported reader:
///   @1: p(X)      -- binds X = [a,b]
///   @2: q(X?)     -- receives X? as imported reader, processes list
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
  test('Simple imported reader - one-way flow', () {
    print('\n=== SIMPLE IMPORTED READER TEST ===\n');

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

    // Dump q/1 bytecode (including NoMoreClauses)
    print('\n=== q/1 BYTECODE (including end) ===');
    final qStart = program.labels['q/1']!;
    final qEnd = (program.labels['q/1_end'] ?? program.ops.length) + 3; // Include past end
    for (var i = qStart; i < qEnd && i < program.ops.length; i++) {
      final marker = (i == program.labels['q/1_end']) ? ' <-- q/1_end' : '';
      print('  $i: ${program.ops[i]}$marker');
    }

    // =========================================================
    // Step 2: Create isolate 1 (@1)
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'isolate1', runtime: runtime1);
    print('\n@1: Created runtime and context');

    // =========================================================
    // Step 3: Create isolate 2 (@2)
    // =========================================================
    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'isolate2', runtime: runtime2);
    print('@2: Created runtime and context');

    // =========================================================
    // Step 4: Allocate shared variable X
    // =========================================================

    // @1 has writer X (result of p(X))
    final (xWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(xWriterAddr);
    print('@1: Allocated writer X at addr=$xWriterAddr');

    // @2 imports reader X? from @1
    final x2ImportedAddr = runtime2.heap.allocateImportedReader();
    final x2Entry = VariableEntry(
      varId: x2ImportedAddr,
      isReader: true,
      creator: 'isolate1',
      role: VariableRole.importedReader,
      creatorLocalId: xWriterAddr,
    );
    ctx2.vp.add(VarKey(x2ImportedAddr, true), x2Entry);
    runtime2.heap.cells[x2ImportedAddr].content = x2Entry;
    print('@2: Imported reader X? at addr=$x2ImportedAddr (from @1 writer $xWriterAddr)');

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

    // @2 -> @1 routing
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
    // Step 6: Spawn goals
    // =========================================================

    // @1: p(X) - binds X = [a,b]
    final pEntryPC = program.labels['p/1']!;
    final xWriterRef1 = VarRef(xWriterAddr);
    final goalId1 = 1;
    final env1 = CallEnv(args: {0: xWriterRef1});
    runtime1.setGoalEnv(goalId1, env1);
    runtime1.setGoalProgram(goalId1, 'main');
    runtime1.gq.enqueue(GoalRef(goalId1, pEntryPC));
    print('\n@1: Spawned goal p(X) at PC=$pEntryPC');

    // @2: q(X?) - processes the imported reader
    final qEntryPC = program.labels['q/1']!;
    final xReaderRef2 = VarRef(x2ImportedAddr);
    final goalId2 = 1;
    final env2 = CallEnv(args: {0: xReaderRef2});
    runtime2.setGoalEnv(goalId2, env2);
    runtime2.setGoalProgram(goalId2, 'main');
    runtime2.gq.enqueue(GoalRef(goalId2, qEntryPC));
    print('@2: Spawned goal q(X?) at PC=$qEntryPC');

    // Check what's at the imported reader address before execution
    print('@2: Before execution, cell[$x2ImportedAddr] = ${runtime2.heap.cells[x2ImportedAddr].content}');

    // =========================================================
    // Step 7: Run execution
    // =========================================================
    print('\n=== EXECUTION ===\n');

    var iterations = 0;
    const maxIterations = 20;

    while (iterations < maxIterations) {
      iterations++;
      print('\n--- Iteration $iterations ---');

      // Run @2 FIRST (like the working shared_variable_test)
      print('\n@2 running first...');
      print('@2: Before running, cell[$x2ImportedAddr] = ${runtime2.heap.cells[x2ImportedAddr].content}');
      final result2 = scheduler2.drainWithStatus(debug: true);
      print('@2: status=${result2.status}, GQ=${runtime2.gq.length}');
      print('@2: blockingReaders=${result2.blockingReaders}');
      print('@2: rt.suspended.keys=${runtime2.suspended.keys.toList()}');
      print('@2: After running, cell[$x2ImportedAddr] = ${runtime2.heap.cells[x2ImportedAddr].content}');

      // Process suspension if suspended
      if (result2.status == ExecutionStatus.suspended) {
        print('@2: Processing suspension, sending read request');
        ctx2.processSuspension(result2.blockingReaders);
      }
      ctx2.flushMessages();

      // Run @1 second
      print('\n@1 running...');
      final result1 = scheduler1.drainWithStatus();
      print('@1: status=${result1.status}, GQ=${runtime1.gq.length}');
      print('@1: After execution, X at addr $xWriterAddr = ${runtime1.heap.derefAddr(xWriterAddr)}');
      ctx1.flushMessages();

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

    // Check X value at @1
    print('\n@1 X value:');
    try {
      final xValue = runtime1.heap.derefAddr(xWriterAddr);
      print('  X = ${_formatValue(xValue, runtime1.heap)}');
    } catch (e) {
      print('  Error: $e');
    }

    // Check X? value at @2
    print('\n@2 X? value:');
    try {
      final xValue2 = runtime2.heap.derefAddr(x2ImportedAddr);
      print('  X? = ${_formatValue(xValue2, runtime2.heap)}');
    } catch (e) {
      print('  Error: $e');
    }

    // Assertions
    // @1 should have X = [a,b]
    final xValue = runtime1.heap.derefAddr(xWriterAddr);
    expect(xValue, isA<StructTerm>(), reason: '@1 X should be a cons cell');

    print('\n=== TEST COMPLETE ===');
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
