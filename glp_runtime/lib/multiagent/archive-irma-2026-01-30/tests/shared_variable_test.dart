/// Minimal test for shared logic variables between two isolates
/// 
/// Setup:
///   @1 has writer X, runs p(X)
///   @2 has imported reader X?, runs q(X?)
///   
/// Program:
///   p(a).
///   q(a).
///
/// Expected flow:
///   1. @1 runs p(X), matches p(a), binds X = a
///   2. @1 sends assignment X? := a to @2
///   3. @2 receives assignment, binds X? = a
///   4. @2 runs q(X?), X? is now bound to a, matches q(a)
///
/// Design doc: /docs/ma/shared-variable-test-design.md
import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/multiagent/irma_context.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'dart:convert';

/// Parse read request payload to extract varId and requester
(int, String) _parseReadRequestPayload(List<int> payload) {
  int offset = 0;

  // Decode length prefix
  int decodeLength(int start) {
    if (payload[start] < 128) {
      return payload[start];
    }
    // Multi-byte length not handled for this simple test
    throw UnimplementedError('Multi-byte length encoding');
  }
  int lengthSize(int start) => payload[start] < 128 ? 1 : 2;

  // Parse global variable ID
  final idLength = decodeLength(offset);
  offset += lengthSize(0);
  final idBytes = payload.sublist(offset, offset + idLength);
  final globalId = GlobalVarId.decode(utf8.decode(idBytes));
  offset += idLength;

  // Parse requester
  final reqLength = decodeLength(offset);
  offset += lengthSize(offset);
  final reqBytes = payload.sublist(offset, offset + reqLength);
  final requester = utf8.decode(reqBytes);

  return (globalId.localId, requester);
}

void main() {
  group('Shared Logic Variable - Two Isolates', () {
    test('p(X)@1, q(X?)@2 - writer binds, reader receives', () {
      print('\n=== SHARED VARIABLE TEST ===\n');
      
      // =========================================================
      // Step 1: Compile program
      // =========================================================
      const programSource = '''
p(a).
q(a).
''';
      
      final compiler = GlpCompiler();
      final program = compiler.compile(programSource);
      print('Compiled program: ${program.ops.length} ops');
      print('Labels: ${program.labels.keys.toList()}');
      
      // =========================================================
      // Step 2: Create isolate @1
      // =========================================================
      final runtime1 = GlpRuntime();
      final runner1 = BytecodeRunner(program);
      final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
      final ctx1 = IrmaContext(agentId: 'isolate1', runtime: runtime1);
      print('@1: Created runtime and context');
      
      // =========================================================
      // Step 3: Create isolate @2
      // =========================================================
      final runtime2 = GlpRuntime();
      final runner2 = BytecodeRunner(program);
      final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
      final ctx2 = IrmaContext(agentId: 'isolate2', runtime: runtime2);
      print('@2: Created runtime and context');
      
      // =========================================================
      // Step 4: Allocate shared variable X
      // =========================================================
      
      // @1: Allocate writer X (two-cell: writer at N, reader at N+1)
      final (writerVarId, _) = runtime1.heap.allocateVariable();
      print('@1: Allocated writer X with varId=$writerVarId');
      
      // Register writer in @1's V_p (sets up heap callback)
      ctx1.registerWriter(writerVarId);
      print('@1: Registered writer in V_p');
      
      // @2: Allocate single cell for imported reader (no local writer)
      final importedReaderId = runtime2.heap.allocateImportedReader();
      print('@2: Allocated imported reader with localId=$importedReaderId');
      
      // Create VariableEntry for imported reader
      final readerEntry = VariableEntry(
        varId: importedReaderId,
        isReader: true,
        creator: 'isolate1',
        role: VariableRole.importedReader,
        creatorLocalId: writerVarId,  // Creator's original ID for message routing
      );
      
      // Add to @2's V_p
      ctx2.vp.add(VarKey(importedReaderId, true), readerEntry);
      
      // Attach entry to heap cell (for deref and suspension storage)
      runtime2.heap.cells[importedReaderId].content = readerEntry;
      print('@2: Registered imported reader in V_p, attached to heap cell');
      
      // =========================================================
      // Step 5: Set up message routing (bidirectional)
      // =========================================================
      
      // @1 -> @2: Assignments
      ctx1.onMessageReady = (destination, message) {
        print('[@1 -> $destination] ${message.type}');
        if (destination == 'isolate2' && message.type == MessageType.assignment) {
          final (creator, creatorLocalId, value) = _parseAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx2.runtime.heap.allocateImportedReader()
              : ctx2.runtime.heap.allocateImportedWriter(),
          );
          print('[@2] Received assignment: creator=$creator, creatorLocalId=$creatorLocalId, value=$value');
          ctx2.handleAssignment(creator, creatorLocalId, value);
        }
      };
      
      // @2 -> @1: Read requests
      ctx2.onMessageReady = (destination, message) {
        print('[@2 -> $destination] ${message.type}');
        if (destination == 'isolate1' && message.type == MessageType.readRequest) {
          final (varId, requester) = _parseReadRequestPayload(message.payload);
          print('[@1] Received read request: varId=$varId, requester=$requester');
          ctx1.handleReadRequest(varId, requester);
        }
      };
      
      print('Message routing configured (bidirectional)');
      
      // =========================================================
      // Step 6: Spawn goal p(X) in @1
      // =========================================================
      final goalId1 = 1;
      final entryPC1 = program.labels['p/1'];
      if (entryPC1 == null) {
        fail('Label p/1 not found. Available: ${program.labels.keys}');
      }
      
      final writerRef = VarRef(writerVarId);  // Writer addr (even)
      final env1 = CallEnv(args: {0: writerRef});
      
      runtime1.setGoalEnv(goalId1, env1);
      runtime1.setGoalProgram(goalId1, 'main');
      runtime1.gq.enqueue(GoalRef(goalId1, entryPC1));
      print('@1: Spawned goal p(X) at PC=$entryPC1');
      
      // =========================================================
      // Step 7: Spawn goal q(X?) in @2
      // =========================================================
      final goalId2 = 1;
      final entryPC2 = program.labels['q/1'];
      if (entryPC2 == null) {
        fail('Label q/1 not found. Available: ${program.labels.keys}');
      }
      
      final readerRef = VarRef(importedReaderId);  // Imported reader addr
      final env2 = CallEnv(args: {0: readerRef});
      
      runtime2.setGoalEnv(goalId2, env2);
      runtime2.setGoalProgram(goalId2, 'main');
      runtime2.gq.enqueue(GoalRef(goalId2, entryPC2));
      print('@2: Spawned goal q(X?) at PC=$entryPC2');
      
      // =========================================================
      // Step 8: Run @2 first - should suspend and send read request
      // =========================================================
      print('\n--- Running @2 (should suspend) ---');
      final result2a = scheduler2.drainWithStatus(debug: true);
      print('@2: drain status = ${result2a.status}');
      print('@2: blocking readers = ${result2a.blockingReaders}');
      
      // Process suspension to send read request
      if (result2a.status == ExecutionStatus.suspended) {
        print('@2: Processing suspension, sending read request');
        ctx2.processSuspension(result2a.blockingReaders);
      }
      
      // =========================================================
      // Step 9: Flush read request from @2 to @1
      // =========================================================
      print('\n--- Flushing messages from @2 ---');
      final msgCount2 = ctx2.flushMessages();
      print('@2: Flushed $msgCount2 messages');
      
      // =========================================================
      // Step 10: Run @1 - should bind X and send assignment
      // =========================================================
      print('\n--- Running @1 ---');
      final result1 = scheduler1.drainWithStatus(debug: true);
      print('@1: drain status = ${result1.status}');
      print('@1: GQ length = ${runtime1.gq.length}');
      
      // Check X is bound
      final value1 = runtime1.heap.getValue(writerVarId);
      print('@1: X = $value1');
      
      // =========================================================
      // Step 9: Flush messages from @1 to @2
      // =========================================================
      print('\n--- Flushing messages from @1 ---');
      final msgCount = ctx1.flushMessages();
      print('@1: Flushed $msgCount messages');
      
      // =========================================================
      // Step 10: Check @2's state before running
      // =========================================================
      print('\n--- @2 state after receiving assignment ---');
      print('@2: GQ length = ${runtime2.gq.length}');
      final derefResult = runtime2.heap.derefAddr(importedReaderId);
      print('@2: X? deref = $derefResult (${derefResult.runtimeType})');
      
      // =========================================================
      // Step 11: Run @2
      // =========================================================
      print('\n--- Running @2 ---');
      final result2 = scheduler2.drainWithStatus(debug: true);
      print('@2: drain status = ${result2.status}');
      print('@2: GQ length = ${runtime2.gq.length}');
      
      // =========================================================
      // Step 12: Verify results
      // =========================================================
      print('\n=== VERIFICATION ===');
      
      // @1: X should be bound to 'a'
      expect(value1, isA<ConstTerm>());
      expect((value1 as ConstTerm).value, equals('a'));
      print('@1: X = a ✓');
      
      // @1: should have succeeded
      expect(result1.status, equals(ExecutionStatus.succeeded));
      print('@1: succeeded ✓');
      
      // @2: X? should be bound to 'a'
      final value2 = runtime2.heap.getValue(importedReaderId);
      print('@2: X? = $value2');
      expect(value2, isA<ConstTerm>());
      expect((value2 as ConstTerm).value, equals('a'));
      print('@2: X? = a ✓');
      
      // @2: should have succeeded
      expect(result2.status, equals(ExecutionStatus.succeeded));
      print('@2: succeeded ✓');
      
      print('\n=== TEST PASSED ===');
    });
  });
}

/// Parse assignment payload to extract creator, creatorLocalId, and value
(String, int, Term) _parseAssignmentPayload(List<int> payload, int Function(bool isReader) allocator) {
  // Use PayloadSerializer to deserialize properly
  final serializer = PayloadSerializer('test');
  final (globalId, term) = serializer.deserializeAssignmentPayload(payload, allocator);
  return (globalId.creator, globalId.localId, term);
}
