/// Bidirectional Exchange Test
///
/// Tests bidirectional communication without circular dependency:
///   @1: produces A = [1,2,3], wants to receive B
///   @2: produces B = [a,b,c], wants to receive A
///
/// Both agents produce independently, then exchange results.
/// This tests that data can flow both directions: @1 <-> @2
///
/// Program:
///   produce_numbers([1,2,3]).
///   produce_letters([a,b,c]).

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
  test('Bidirectional exchange: @1 <-> @2', () {
    print('\n=== BIDIRECTIONAL EXCHANGE TEST ===\n');
    print('Data flow: @1 produces A, @2 produces B, then exchange');

    // =========================================================
    // Step 1: Compile program
    // =========================================================
    final compiler = GlpCompiler();
    final program = compiler.compile('''
      produce_numbers([1,2,3]).
      produce_letters([a,b,c]).
    ''');

    print('Compiled program: ${program.ops.length} ops');

    // =========================================================
    // Step 2: Create agents
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'agent1', runtime: runtime1);
    print('\n@1: Created runtime and context');

    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'agent2', runtime: runtime2);
    print('@2: Created runtime and context');

    // =========================================================
    // Step 3: Set up shared variables
    //   @1 owns A (numbers)
    //   @2 owns B (letters)
    //   @1 imports B? from @2
    //   @2 imports A? from @1
    // =========================================================

    // @1 owns writer A
    final (aWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(aWriterAddr);
    print('@1: Allocated writer A at addr=$aWriterAddr');

    // @2 owns writer B
    final (bWriterAddr, _) = runtime2.heap.allocateVariable();
    ctx2.registerWriter(bWriterAddr);
    print('@2: Allocated writer B at addr=$bWriterAddr');

    // @1 imports reader B? from @2
    final b1ImportedAddr = runtime1.heap.allocateImportedReader();
    final b1Entry = VariableEntry(
      varId: b1ImportedAddr,
      isReader: true,
      creator: 'agent2',
      role: VariableRole.importedReader,
      creatorLocalId: bWriterAddr,
    );
    ctx1.vp.add(VarKey(b1ImportedAddr, true), b1Entry);
    runtime1.heap.cells[b1ImportedAddr].content = b1Entry;
    print('@1: Imported reader B? at addr=$b1ImportedAddr (from @2 writer $bWriterAddr)');

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
    print('@2: Imported reader A? at addr=$a2ImportedAddr (from @1 writer $aWriterAddr)');

    // =========================================================
    // Step 4: Set up message routing
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
    // Step 5: Spawn production goals
    // =========================================================

    // @1: produce_numbers(A)
    final numbersPC = program.labels['produce_numbers/1']!;
    final aWriterRef = VarRef(aWriterAddr);
    final goalId1 = 1;
    final env1 = CallEnv(args: {0: aWriterRef});
    runtime1.setGoalEnv(goalId1, env1);
    runtime1.setGoalProgram(goalId1, 'main');
    runtime1.gq.enqueue(GoalRef(goalId1, numbersPC));
    print('\n@1: Spawned goal produce_numbers(A) at PC=$numbersPC');

    // @2: produce_letters(B)
    final lettersPC = program.labels['produce_letters/1']!;
    final bWriterRef = VarRef(bWriterAddr);
    final goalId2 = 1;
    final env2 = CallEnv(args: {0: bWriterRef});
    runtime2.setGoalEnv(goalId2, env2);
    runtime2.setGoalProgram(goalId2, 'main');
    runtime2.gq.enqueue(GoalRef(goalId2, lettersPC));
    print('@2: Spawned goal produce_letters(B) at PC=$lettersPC');

    // =========================================================
    // Step 6: Run production phase
    // =========================================================
    print('\n=== PRODUCTION PHASE ===\n');

    // Run @1 to produce numbers
    print('@1 producing...');
    final result1 = scheduler1.drainWithStatus();
    print('@1: status=${result1.status}');
    ctx1.flushMessages();

    // Run @2 to produce letters
    print('@2 producing...');
    final result2 = scheduler2.drainWithStatus();
    print('@2: status=${result2.status}');
    ctx2.flushMessages();

    // =========================================================
    // Step 7: Exchange phase - each requests the other's value
    // =========================================================
    print('\n=== EXCHANGE PHASE ===\n');

    // @1 requests B?
    print('@1 requesting B?...');
    ctx1.processSuspension({b1ImportedAddr});
    ctx1.flushMessages();
    ctx2.flushMessages();

    // @2 requests A?
    print('@2 requesting A?...');
    ctx2.processSuspension({a2ImportedAddr});
    ctx2.flushMessages();
    ctx1.flushMessages();

    // =========================================================
    // Step 8: Display results
    // =========================================================
    print('\n=== RESULTS ===\n');

    print('Messages: ${messageLog.length}');
    for (final msg in messageLog) {
      print('  $msg');
    }

    // Check @1's values
    print('\n@1 A = ${_formatValue(runtime1.heap.derefAddr(aWriterAddr), runtime1.heap)}');
    print('@1 B? = ${_formatValue(runtime1.heap.derefAddr(b1ImportedAddr), runtime1.heap)}');

    // Check @2's values
    print('@2 B = ${_formatValue(runtime2.heap.derefAddr(bWriterAddr), runtime2.heap)}');
    print('@2 A? = ${_formatValue(runtime2.heap.derefAddr(a2ImportedAddr), runtime2.heap)}');

    // Assertions
    final aValue = runtime1.heap.derefAddr(aWriterAddr);
    expect(aValue, isA<StructTerm>(), reason: '@1 A should be [1,2,3]');

    final bValue = runtime2.heap.derefAddr(bWriterAddr);
    expect(bValue, isA<StructTerm>(), reason: '@2 B should be [a,b,c]');

    final b1Value = runtime1.heap.derefAddr(b1ImportedAddr);
    expect(b1Value, isA<StructTerm>(), reason: '@1 B? should receive [a,b,c]');

    final a2Value = runtime2.heap.derefAddr(a2ImportedAddr);
    expect(a2Value, isA<StructTerm>(), reason: '@2 A? should receive [1,2,3]');

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
