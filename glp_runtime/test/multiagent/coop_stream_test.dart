/// Cooperative Stream Multi-Agent Tests
///
/// Tests different allocations of producers and merger across agents.
/// All patterns use one-way data flow (no circular dependencies).
///
/// Pattern A: @1 produces Xs, @2 produces Ys and merges
///   @1: producer1(Xs)           -- writes Xs = [1,2,3]
///   @2: producer2(Ys), merge(Xs?, Ys?, Zs)  -- reads Xs?, writes Ys and Zs
///
/// Pattern B: @1 produces Xs and merges, @2 produces Ys
///   @1: producer1(Xs), merge(Xs?, Ys?, Zs)  -- writes Xs and Zs, reads Ys?
///   @2: producer2(Ys)           -- writes Ys = [a,b,c]
///
/// Program:
///   producer1([1,2,3]).
///   producer2([a,b,c]).
///   merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
///   merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
///   merge([], [], []).

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
  late BytecodeProgram program;

  setUpAll(() {
    final compiler = GlpCompiler();
    program = compiler.compile('''
      producer1([1,2,3]).
      producer2([a,b,c]).

      merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
      merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
      merge([], [], []).
    ''');
    print('Compiled program: ${program.ops.length} ops');
    print('Labels: ${program.labels.keys.toList()}');
  });

  group('Cooperative Stream - Two Agents', () {

    test('Pattern A: @1 produces, @2 merges', () {
      print('\n=== PATTERN A: @1 produces Xs, @2 produces Ys and merges ===\n');

      // Create agents
      final runtime1 = GlpRuntime();
      final runner1 = BytecodeRunner(program);
      final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
      final ctx1 = IrmaContext(agentId: 'agent1', runtime: runtime1);

      final runtime2 = GlpRuntime();
      final runner2 = BytecodeRunner(program);
      final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
      final ctx2 = IrmaContext(agentId: 'agent2', runtime: runtime2);

      // @1 has writer Xs
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
      print('@2: Imported reader Xs? at addr=$xs2ImportedAddr');

      // @2 has local Ys and Zs (no cross-agent sharing needed)
      final (ysWriterAddr, _) = runtime2.heap.allocateVariable();
      final (zsWriterAddr, zsReaderAddr) = runtime2.heap.allocateVariable();
      print('@2: Allocated local Ys at addr=$ysWriterAddr, Zs at writer=$zsWriterAddr reader=$zsReaderAddr');

      // Set up message routing
      final messageLog = <String>[];
      _setupRouting(ctx1, ctx2, messageLog);

      // Spawn goals
      // @1: producer1(Xs)
      final p1PC = program.labels['producer1/1']!;
      runtime1.setGoalEnv(1, CallEnv(args: {0: VarRef(xsWriterAddr)}));
      runtime1.setGoalProgram(1, 'main');
      runtime1.gq.enqueue(GoalRef(1, p1PC));
      print('@1: Spawned producer1(Xs)');

      // @2: producer2(Ys)
      final p2PC = program.labels['producer2/1']!;
      runtime2.setGoalEnv(1, CallEnv(args: {0: VarRef(ysWriterAddr)}));
      runtime2.setGoalProgram(1, 'main');
      runtime2.gq.enqueue(GoalRef(1, p2PC));
      print('@2: Spawned producer2(Ys)');

      // @2: merge(Xs?, Ys?, Zs) - note Ys? is local reader
      final (_, ysReaderAddr) = (ysWriterAddr, ysWriterAddr + 1); // reader is writer+1
      final mergePC = program.labels['merge/3']!;
      runtime2.setGoalEnv(2, CallEnv(args: {
        0: VarRef(xs2ImportedAddr),  // Xs? imported
        1: VarRef(ysReaderAddr),      // Ys? local
        2: VarRef(zsWriterAddr),      // Zs writer
      }));
      runtime2.setGoalProgram(2, 'main');
      runtime2.gq.enqueue(GoalRef(2, mergePC));
      print('@2: Spawned merge(Xs?, Ys?, Zs)');

      // Run execution loop
      final result = _runExecution(scheduler1, scheduler2, ctx1, ctx2, runtime1, runtime2);

      // Display results
      print('\n=== RESULTS ===');
      print('Iterations: ${result.iterations}');
      print('Messages: ${messageLog.join(", ")}');

      // Check Zs value at @2
      final zsValue = runtime2.heap.derefAddr(zsReaderAddr);
      print('@2 Zs = ${_formatList(zsValue, runtime2.heap)}');

      // Should be interleaved: [1, a, 2, b, 3, c] or similar
      expect(zsValue, isA<StructTerm>(), reason: 'Zs should be a list');
      expect(result.success, isTrue, reason: 'Both agents should succeed');
    });

    test('Pattern B: @2 produces, @1 merges', () {
      print('\n=== PATTERN B: @2 produces Ys, @1 produces Xs and merges ===\n');

      // Create agents
      final runtime1 = GlpRuntime();
      final runner1 = BytecodeRunner(program);
      final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
      final ctx1 = IrmaContext(agentId: 'agent1', runtime: runtime1);

      final runtime2 = GlpRuntime();
      final runner2 = BytecodeRunner(program);
      final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
      final ctx2 = IrmaContext(agentId: 'agent2', runtime: runtime2);

      // @2 has writer Ys
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
      print('@1: Imported reader Ys? at addr=$ys1ImportedAddr');

      // @1 has local Xs and Zs
      final (xsWriterAddr, xsReaderAddr) = runtime1.heap.allocateVariable();
      final (zsWriterAddr, zsReaderAddr) = runtime1.heap.allocateVariable();
      print('@1: Allocated local Xs at writer=$xsWriterAddr, Zs at writer=$zsWriterAddr');

      // Set up message routing
      final messageLog = <String>[];
      _setupRouting(ctx1, ctx2, messageLog);

      // Spawn goals
      // @1: producer1(Xs)
      final p1PC = program.labels['producer1/1']!;
      runtime1.setGoalEnv(1, CallEnv(args: {0: VarRef(xsWriterAddr)}));
      runtime1.setGoalProgram(1, 'main');
      runtime1.gq.enqueue(GoalRef(1, p1PC));
      print('@1: Spawned producer1(Xs)');

      // @1: merge(Xs?, Ys?, Zs)
      final mergePC = program.labels['merge/3']!;
      runtime1.setGoalEnv(2, CallEnv(args: {
        0: VarRef(xsReaderAddr),      // Xs? local
        1: VarRef(ys1ImportedAddr),   // Ys? imported
        2: VarRef(zsWriterAddr),      // Zs writer
      }));
      runtime1.setGoalProgram(2, 'main');
      runtime1.gq.enqueue(GoalRef(2, mergePC));
      print('@1: Spawned merge(Xs?, Ys?, Zs)');

      // @2: producer2(Ys)
      final p2PC = program.labels['producer2/1']!;
      runtime2.setGoalEnv(1, CallEnv(args: {0: VarRef(ysWriterAddr)}));
      runtime2.setGoalProgram(1, 'main');
      runtime2.gq.enqueue(GoalRef(1, p2PC));
      print('@2: Spawned producer2(Ys)');

      // Run execution loop
      final result = _runExecution(scheduler1, scheduler2, ctx1, ctx2, runtime1, runtime2);

      // Display results
      print('\n=== RESULTS ===');
      print('Iterations: ${result.iterations}');
      print('Messages: ${messageLog.join(", ")}');

      // Check Zs value at @1
      final zsValue = runtime1.heap.derefAddr(zsReaderAddr);
      print('@1 Zs = ${_formatList(zsValue, runtime1.heap)}');

      expect(zsValue, isA<StructTerm>(), reason: 'Zs should be a list');
      expect(result.success, isTrue, reason: 'Both agents should succeed');
    });

    test('Pattern C: Both produce locally, @1 imports result', () {
      print('\n=== PATTERN C: @2 produces both and merges, @1 imports result ===\n');

      // This tests: producer and merger on @2, result Zs exported to @1
      // @2: producer1(Xs), producer2(Ys), merge(Xs?, Ys?, Zs)
      // @1: just reads Zs?

      // Create agents
      final runtime1 = GlpRuntime();
      final runner1 = BytecodeRunner(program);
      final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
      final ctx1 = IrmaContext(agentId: 'agent1', runtime: runtime1);

      final runtime2 = GlpRuntime();
      final runner2 = BytecodeRunner(program);
      final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
      final ctx2 = IrmaContext(agentId: 'agent2', runtime: runtime2);

      // @2 has writer Zs (the merged result)
      final (zsWriterAddr2, zsReaderAddr2) = runtime2.heap.allocateVariable();
      ctx2.registerWriter(zsWriterAddr2);
      print('@2: Allocated writer Zs at addr=$zsWriterAddr2');

      // @2 has local Xs and Ys
      final (xsWriterAddr, xsReaderAddr) = runtime2.heap.allocateVariable();
      final (ysWriterAddr, ysReaderAddr) = runtime2.heap.allocateVariable();
      print('@2: Allocated local Xs at $xsWriterAddr, Ys at $ysWriterAddr');

      // @1 imports reader Zs? from @2
      final zs1ImportedAddr = runtime1.heap.allocateImportedReader();
      final zs1Entry = VariableEntry(
        varId: zs1ImportedAddr,
        isReader: true,
        creator: 'agent2',
        role: VariableRole.importedReader,
        creatorLocalId: zsWriterAddr2,
      );
      ctx1.vp.add(VarKey(zs1ImportedAddr, true), zs1Entry);
      runtime1.heap.cells[zs1ImportedAddr].content = zs1Entry;
      print('@1: Imported reader Zs? at addr=$zs1ImportedAddr');

      // Set up message routing
      final messageLog = <String>[];
      _setupRouting(ctx1, ctx2, messageLog);

      // Spawn goals on @2
      // @2: producer1(Xs)
      final p1PC = program.labels['producer1/1']!;
      runtime2.setGoalEnv(1, CallEnv(args: {0: VarRef(xsWriterAddr)}));
      runtime2.setGoalProgram(1, 'main');
      runtime2.gq.enqueue(GoalRef(1, p1PC));

      // @2: producer2(Ys)
      final p2PC = program.labels['producer2/1']!;
      runtime2.setGoalEnv(2, CallEnv(args: {0: VarRef(ysWriterAddr)}));
      runtime2.setGoalProgram(2, 'main');
      runtime2.gq.enqueue(GoalRef(2, p2PC));

      // @2: merge(Xs?, Ys?, Zs)
      final mergePC = program.labels['merge/3']!;
      runtime2.setGoalEnv(3, CallEnv(args: {
        0: VarRef(xsReaderAddr),
        1: VarRef(ysReaderAddr),
        2: VarRef(zsWriterAddr2),
      }));
      runtime2.setGoalProgram(3, 'main');
      runtime2.gq.enqueue(GoalRef(3, mergePC));
      print('@2: Spawned producer1, producer2, merge');

      // @1: just a simple goal that reads Zs? - use a dummy wrapper
      // We'll manually check the value after @2 completes
      print('@1: Will read Zs? after @2 completes');

      // Run @2 first (it does all the work)
      print('\n--- Running @2 (producer + merger) ---');
      var result2 = scheduler2.drainWithStatus(debug: true);
      ctx2.flushMessages();
      print('@2: status=${result2.status}');

      // Now @1 should request Zs?
      print('\n--- @1 requesting Zs? ---');
      ctx1.processSuspension({zs1ImportedAddr});
      ctx1.flushMessages();

      // Run @2 again to send assignment
      result2 = scheduler2.drainWithStatus();
      ctx2.flushMessages();

      // Check Zs? value at @1
      final zsValue = runtime1.heap.derefAddr(zs1ImportedAddr);
      print('\n=== RESULTS ===');
      print('@1 Zs? = ${_formatList(zsValue, runtime1.heap)}');
      print('Messages: ${messageLog.join(", ")}');

      expect(zsValue, isA<StructTerm>(), reason: 'Zs should be a list');
    });
  });
}

void _setupRouting(IrmaContext ctx1, IrmaContext ctx2, List<String> messageLog) {
  ctx1.onMessageReady = (destination, message) {
    if (destination == 'agent2') {
      messageLog.add('@1->@2:${message.type.name}');
      if (message.type == MessageType.assignment) {
        final serializer = PayloadSerializer('agent1');
        final (globalId, value) = serializer.deserializeAssignmentPayload(
          message.payload,
          (bool isReader) => isReader
            ? ctx2.runtime.heap.allocateImportedReader()
            : ctx2.runtime.heap.allocateImportedWriter(),
        );
        ctx2.handleAssignment(globalId.creator, globalId.localId, value);
      } else if (message.type == MessageType.readRequest) {
        final serializer = PayloadSerializer('agent1');
        final varId = serializer.deserializeReadRequestPayload(message.payload);
        ctx2.handleReadRequest(varId, 'agent1');
      }
    }
  };

  ctx2.onMessageReady = (destination, message) {
    if (destination == 'agent1') {
      messageLog.add('@2->@1:${message.type.name}');
      if (message.type == MessageType.assignment) {
        final serializer = PayloadSerializer('agent2');
        final (globalId, value) = serializer.deserializeAssignmentPayload(
          message.payload,
          (bool isReader) => isReader
            ? ctx1.runtime.heap.allocateImportedReader()
            : ctx1.runtime.heap.allocateImportedWriter(),
        );
        ctx1.handleAssignment(globalId.creator, globalId.localId, value);
      } else if (message.type == MessageType.readRequest) {
        final serializer = PayloadSerializer('agent2');
        final varId = serializer.deserializeReadRequestPayload(message.payload);
        ctx1.handleReadRequest(varId, 'agent2');
      }
    }
  };
}

class _ExecutionResult {
  final int iterations;
  final bool success;
  _ExecutionResult(this.iterations, this.success);
}

_ExecutionResult _runExecution(
  Scheduler scheduler1,
  Scheduler scheduler2,
  IrmaContext ctx1,
  IrmaContext ctx2,
  GlpRuntime runtime1,
  GlpRuntime runtime2,
) {
  var iterations = 0;
  const maxIterations = 30;

  while (iterations < maxIterations) {
    iterations++;
    print('\n--- Iteration $iterations ---');

    // Run @1
    final result1 = scheduler1.drainWithStatus(debug: iterations <= 3);
    print('@1: status=${result1.status}, GQ=${runtime1.gq.length}');
    if (result1.status == ExecutionStatus.suspended && result1.blockingReaders.isNotEmpty) {
      print('@1: Processing suspension on ${result1.blockingReaders}');
      ctx1.processSuspension(result1.blockingReaders);
    }
    ctx1.flushMessages();

    // Run @2
    final result2 = scheduler2.drainWithStatus(debug: iterations <= 3);
    print('@2: status=${result2.status}, GQ=${runtime2.gq.length}');
    if (result2.status == ExecutionStatus.suspended && result2.blockingReaders.isNotEmpty) {
      print('@2: Processing suspension on ${result2.blockingReaders}');
      ctx2.processSuspension(result2.blockingReaders);
    }
    ctx2.flushMessages();

    // Check completion
    if (result1.status == ExecutionStatus.succeeded &&
        result2.status == ExecutionStatus.succeeded &&
        runtime1.gq.isEmpty &&
        runtime2.gq.isEmpty) {
      print('\nBoth agents completed successfully');
      return _ExecutionResult(iterations, true);
    }

    // Check deadlock
    if (result1.status != ExecutionStatus.succeeded &&
        result2.status != ExecutionStatus.succeeded &&
        runtime1.gq.isEmpty &&
        runtime2.gq.isEmpty) {
      print('\nPossible deadlock detected');
      return _ExecutionResult(iterations, false);
    }
  }

  print('\nMax iterations reached');
  return _ExecutionResult(iterations, false);
}

String _formatList(Object value, dynamic heap) {
  if (value is ConstTerm) {
    if (value.value == 'nil' || value.value == '[]') return '[]';
    return value.value.toString();
  } else if (value is StructTerm && value.functor == '.' && value.args.length == 2) {
    final elements = <String>[];
    Object current = value;
    var count = 0;
    while (count < 20) {
      if (current is ConstTerm) {
        break;
      } else if (current is StructTerm && current.functor == '.' && current.args.length == 2) {
        final head = current.args[0];
        if (head is ConstTerm) {
          elements.add(head.value.toString());
        } else if (head is VarRef) {
          final deref = heap.derefAddr(head.addr);
          elements.add(_formatList(deref, heap));
        } else {
          elements.add(head.toString());
        }
        final tail = current.args[1];
        if (tail is VarRef) {
          current = heap.derefAddr(tail.addr);
        } else {
          current = tail;
        }
        count++;
      } else {
        elements.add('|$current');
        break;
      }
    }
    return '[${elements.join(', ')}]';
  } else if (value is VarRef) {
    return '_${value.addr}';
  } else if (value is VariableEntry) {
    return '_entry';
  }
  return value.toString();
}
