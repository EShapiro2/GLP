/// Friend-Mediated Introduction Test
///
/// Tests the friend introduction protocol:
///   @1 (Alice): Knows both Bob and Carol, introduces them
///   @2 (Bob): Receives introduction containing Carol's info
///   @3 (Carol): Receives introduction containing Bob's info
///
/// Protocol:
/// 1. Alice creates intro(bob, carol) for Bob
/// 2. Alice creates intro(carol, bob) for Carol
/// 3. Bob receives and wraps as got(intro(bob, carol))
/// 4. Carol receives and wraps as got(intro(carol, bob))
///
/// This tests 3-agent message distribution via a central coordinator.

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
  test('Friend introduction: Alice introduces Bob and Carol', () {
    print('\n=== FRIEND-MEDIATED INTRODUCTION TEST ===\n');
    print('Alice knows Bob and Carol, introduces them to each other');

    // =========================================================
    // Step 1: Compile program
    // =========================================================
    final compiler = GlpCompiler();
    // Simpler SRSW-compliant program for friend introduction
    // Alice creates introduction info, Bob and Carol each receive their part
    final program = compiler.compile('''
      % Alice creates introductions for both Bob and Carol
      create_intros(intro(bob, carol), intro(carol, bob)).

      % Bob processes his intro - wraps it with "met"
      process_intro(intro(Who, Whom), met(Who?, Whom?)).

      % Simple receive pattern
      receive_intro(Intro, got(Intro?)) :- ground(Intro?) | true.
    ''');

    print('Compiled program: ${program.ops.length} ops');

    // =========================================================
    // Step 2: Create three agents
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'alice', runtime: runtime1);
    print('\n@1 (Alice): Created');

    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'bob', runtime: runtime2);
    print('@2 (Bob): Created');

    final runtime3 = GlpRuntime();
    final runner3 = BytecodeRunner(program);
    final scheduler3 = Scheduler(rt: runtime3, runners: {'main': runner3});
    final ctx3 = IrmaContext(agentId: 'carol', runtime: runtime3);
    print('@3 (Carol): Created');

    // =========================================================
    // Step 3: Set up shared variables
    //   Alice owns: IntroB (intro for Bob), IntroC (intro for Carol)
    //   Bob imports IntroB? from Alice
    //   Carol imports IntroC? from Alice
    // =========================================================

    // @1 (Alice) owns writer IntroB (intro message for Bob)
    final (introBWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(introBWriterAddr);
    print('\n@1: Allocated writer IntroB at addr=$introBWriterAddr');

    // @1 (Alice) owns writer IntroC (intro message for Carol)
    final (introCWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(introCWriterAddr);
    print('@1: Allocated writer IntroC at addr=$introCWriterAddr');

    // @2 (Bob) imports reader IntroB? from Alice
    final introB2ImportedAddr = runtime2.heap.allocateImportedReader();
    final introB2Entry = VariableEntry(
      varId: introB2ImportedAddr,
      isReader: true,
      creator: 'alice',
      role: VariableRole.importedReader,
      creatorLocalId: introBWriterAddr,
    );
    ctx2.vp.add(VarKey(introB2ImportedAddr, true), introB2Entry);
    runtime2.heap.cells[introB2ImportedAddr].content = introB2Entry;
    print('@2: Imported reader IntroB? at addr=$introB2ImportedAddr (from Alice)');

    // @2 (Bob) owns writer ResultB (result of processing intro)
    final (resultBWriterAddr, _) = runtime2.heap.allocateVariable();
    ctx2.registerWriter(resultBWriterAddr);
    print('@2: Allocated writer ResultB at addr=$resultBWriterAddr');

    // @3 (Carol) imports reader IntroC? from Alice
    final introC3ImportedAddr = runtime3.heap.allocateImportedReader();
    final introC3Entry = VariableEntry(
      varId: introC3ImportedAddr,
      isReader: true,
      creator: 'alice',
      role: VariableRole.importedReader,
      creatorLocalId: introCWriterAddr,
    );
    ctx3.vp.add(VarKey(introC3ImportedAddr, true), introC3Entry);
    runtime3.heap.cells[introC3ImportedAddr].content = introC3Entry;
    print('@3: Imported reader IntroC? at addr=$introC3ImportedAddr (from Alice)');

    // @3 (Carol) owns writer ResultC (result of processing intro)
    final (resultCWriterAddr, _) = runtime3.heap.allocateVariable();
    ctx3.registerWriter(resultCWriterAddr);
    print('@3: Allocated writer ResultC at addr=$resultCWriterAddr');

    // =========================================================
    // Step 4: Set up message routing
    // =========================================================
    final messageLog = <String>[];

    ctx1.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('alice');
      IrmaContext? target;
      if (destination == 'bob') target = ctx2;
      if (destination == 'carol') target = ctx3;
      if (target != null) {
        final t = target!;
        if (message.type == MessageType.assignment) {
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? t.runtime.heap.allocateImportedReader()
              : t.runtime.heap.allocateImportedWriter(),
          );
          messageLog.add('[alice -> $destination] ASSIGNMENT');
          print('[alice -> $destination] ASSIGNMENT: $globalId := ${_shortValue(value)}');
          t.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[alice -> $destination] READ_REQUEST');
          print('[alice -> $destination] READ_REQUEST: varId=$varId');
          t.handleReadRequest(varId, 'alice');
        }
      }
    };

    ctx2.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('bob');
      if (destination == 'alice') {
        if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[bob -> alice] READ_REQUEST');
          print('[bob -> alice] READ_REQUEST: varId=$varId');
          ctx1.handleReadRequest(varId, 'bob');
        }
      }
    };

    ctx3.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('carol');
      if (destination == 'alice') {
        if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[carol -> alice] READ_REQUEST');
          print('[carol -> alice] READ_REQUEST: varId=$varId');
          ctx1.handleReadRequest(varId, 'carol');
        }
      }
    };

    print('\nMessage routing configured');

    // =========================================================
    // Step 5: Spawn goals
    // =========================================================

    // @1 (Alice): create_intros(IntroB, IntroC)
    final createPC = program.labels['create_intros/2']!;
    final introBWriterRef = VarRef(introBWriterAddr);
    final introCWriterRef = VarRef(introCWriterAddr);
    runtime1.setGoalEnv(1, CallEnv(args: {0: introBWriterRef, 1: introCWriterRef}));
    runtime1.setGoalProgram(1, 'main');
    runtime1.gq.enqueue(GoalRef(1, createPC));
    print('\n@1: Spawned create_intros(IntroB, IntroC)');

    // @2 (Bob): receive_intro(IntroB?, ResultB)
    final receivePC = program.labels['receive_intro/2']!;
    final introB2ReaderRef = VarRef(introB2ImportedAddr);
    final resultBWriterRef = VarRef(resultBWriterAddr);
    runtime2.setGoalEnv(1, CallEnv(args: {0: introB2ReaderRef, 1: resultBWriterRef}));
    runtime2.setGoalProgram(1, 'main');
    runtime2.gq.enqueue(GoalRef(1, receivePC));
    print('@2: Spawned receive_intro(IntroB?, ResultB)');

    // @3 (Carol): receive_intro(IntroC?, ResultC)
    final introC3ReaderRef = VarRef(introC3ImportedAddr);
    final resultCWriterRef = VarRef(resultCWriterAddr);
    runtime3.setGoalEnv(1, CallEnv(args: {0: introC3ReaderRef, 1: resultCWriterRef}));
    runtime3.setGoalProgram(1, 'main');
    runtime3.gq.enqueue(GoalRef(1, receivePC));
    print('@3: Spawned receive_intro(IntroC?, ResultC)');

    // =========================================================
    // Step 6: Run execution
    // =========================================================
    print('\n=== EXECUTION ===\n');

    var iterations = 0;
    const maxIterations = 20;

    while (iterations < maxIterations) {
      iterations++;
      print('\n--- Iteration $iterations ---');

      // Run @1 (Alice)
      final result1 = scheduler1.drainWithStatus();
      print('@1 (Alice): ${result1.status}');
      if (result1.status == ExecutionStatus.suspended) {
        ctx1.processSuspension(result1.blockingReaders);
      }
      ctx1.flushMessages();

      // Run @2 (Bob)
      final result2 = scheduler2.drainWithStatus(debug: true);
      print('@2 (Bob): ${result2.status}, blockingReaders=${result2.blockingReaders}');
      if (result2.status == ExecutionStatus.suspended) {
        ctx2.processSuspension(result2.blockingReaders);
      }
      ctx2.flushMessages();

      // Run @3 (Carol)
      final result3 = scheduler3.drainWithStatus(debug: true);
      print('@3 (Carol): ${result3.status}, blockingReaders=${result3.blockingReaders}');
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

    print('\n@1 (Alice):');
    print('  IntroB = ${_formatValue(runtime1.heap.derefAddr(introBWriterAddr), runtime1.heap)}');
    print('  IntroC = ${_formatValue(runtime1.heap.derefAddr(introCWriterAddr), runtime1.heap)}');

    print('@2 (Bob):');
    print('  ResultB = ${_formatValue(runtime2.heap.derefAddr(resultBWriterAddr), runtime2.heap)}');

    print('@3 (Carol):');
    print('  ResultC = ${_formatValue(runtime3.heap.derefAddr(resultCWriterAddr), runtime3.heap)}');

    // Assertions
    final resultB = runtime2.heap.derefAddr(resultBWriterAddr);
    expect(resultB, isA<StructTerm>(), reason: '@2 ResultB should be got(intro(...))');

    final resultC = runtime3.heap.derefAddr(resultCWriterAddr);
    expect(resultC, isA<StructTerm>(), reason: '@3 ResultC should be got(intro(...))');

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
