/// Three-Agent Merge Test
///
/// Tests merge where all three components are on separate agents:
///   @1 (producer1): produces Xs = [1,2,3]
///   @2 (producer2): produces Ys = [a,b,c]
///   @3 (merger): imports Xs? and Ys?, runs merge(Xs?, Ys?, Zs)
///
/// Data flow: @1 → @3 ← @2 (both feed into @3)
///
/// This tests a pure processor pattern where @3 has no local production.

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
  test('Three-agent merge: @1 produces, @2 produces, @3 merges', () {
    print('\n=== THREE-AGENT MERGE TEST ===\n');
    print('Data flow: @1 (Xs) → @3 (merge) ← @2 (Ys)');

    // =========================================================
    // Step 1: Compile program
    // =========================================================
    final compiler = GlpCompiler();
    final program = compiler.compile('''
      produce_xs([1,2,3]).
      produce_ys([a,b,c]).

      merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
      merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
      merge([], [], []).
    ''');

    print('Compiled program: ${program.ops.length} ops');

    // =========================================================
    // Step 2: Create three agents
    // =========================================================
    final runtime1 = GlpRuntime();
    final runner1 = BytecodeRunner(program);
    final scheduler1 = Scheduler(rt: runtime1, runners: {'main': runner1});
    final ctx1 = IrmaContext(agentId: 'producer1', runtime: runtime1);
    print('\n@1 (producer1): Created');

    final runtime2 = GlpRuntime();
    final runner2 = BytecodeRunner(program);
    final scheduler2 = Scheduler(rt: runtime2, runners: {'main': runner2});
    final ctx2 = IrmaContext(agentId: 'producer2', runtime: runtime2);
    print('@2 (producer2): Created');

    final runtime3 = GlpRuntime();
    final runner3 = BytecodeRunner(program);
    final scheduler3 = Scheduler(rt: runtime3, runners: {'main': runner3});
    final ctx3 = IrmaContext(agentId: 'merger', runtime: runtime3);
    print('@3 (merger): Created');

    // =========================================================
    // Step 3: Set up shared variables
    //   @1 owns Xs
    //   @2 owns Ys
    //   @3 imports Xs? from @1, Ys? from @2, owns Zs
    // =========================================================

    // @1 owns writer Xs
    final (xsWriterAddr, _) = runtime1.heap.allocateVariable();
    ctx1.registerWriter(xsWriterAddr);
    print('\n@1: Allocated writer Xs at addr=$xsWriterAddr');

    // @2 owns writer Ys
    final (ysWriterAddr, _) = runtime2.heap.allocateVariable();
    ctx2.registerWriter(ysWriterAddr);
    print('@2: Allocated writer Ys at addr=$ysWriterAddr');

    // @3 imports reader Xs? from @1
    final xs3ImportedAddr = runtime3.heap.allocateImportedReader();
    final xs3Entry = VariableEntry(
      varId: xs3ImportedAddr,
      isReader: true,
      creator: 'producer1',
      role: VariableRole.importedReader,
      creatorLocalId: xsWriterAddr,
    );
    ctx3.vp.add(VarKey(xs3ImportedAddr, true), xs3Entry);
    runtime3.heap.cells[xs3ImportedAddr].content = xs3Entry;
    print('@3: Imported reader Xs? at addr=$xs3ImportedAddr (from @1)');

    // @3 imports reader Ys? from @2
    final ys3ImportedAddr = runtime3.heap.allocateImportedReader();
    final ys3Entry = VariableEntry(
      varId: ys3ImportedAddr,
      isReader: true,
      creator: 'producer2',
      role: VariableRole.importedReader,
      creatorLocalId: ysWriterAddr,
    );
    ctx3.vp.add(VarKey(ys3ImportedAddr, true), ys3Entry);
    runtime3.heap.cells[ys3ImportedAddr].content = ys3Entry;
    print('@3: Imported reader Ys? at addr=$ys3ImportedAddr (from @2)');

    // @3 owns writer Zs
    final (zsWriterAddr, _) = runtime3.heap.allocateVariable();
    ctx3.registerWriter(zsWriterAddr);
    print('@3: Allocated writer Zs at addr=$zsWriterAddr');

    // =========================================================
    // Step 4: Set up message routing
    // =========================================================
    final messageLog = <String>[];

    // Route between all agents
    ctx1.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('producer1');
      if (destination == 'merger') {
        if (message.type == MessageType.assignment) {
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx3.runtime.heap.allocateImportedReader()
              : ctx3.runtime.heap.allocateImportedWriter(),
          );
          messageLog.add('[producer1 -> merger] ASSIGNMENT');
          print('[producer1 -> merger] ASSIGNMENT: $globalId := ${_shortValue(value)}');
          ctx3.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[producer1 -> merger] READ_REQUEST');
          print('[producer1 -> merger] READ_REQUEST: varId=$varId');
          ctx3.handleReadRequest(varId, 'producer1');
        }
      }
    };

    ctx2.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('producer2');
      if (destination == 'merger') {
        if (message.type == MessageType.assignment) {
          final (globalId, value) = serializer.deserializeAssignmentPayload(
            message.payload,
            (bool isReader) => isReader
              ? ctx3.runtime.heap.allocateImportedReader()
              : ctx3.runtime.heap.allocateImportedWriter(),
          );
          messageLog.add('[producer2 -> merger] ASSIGNMENT');
          print('[producer2 -> merger] ASSIGNMENT: $globalId := ${_shortValue(value)}');
          ctx3.handleAssignment(globalId.creator, globalId.localId, value);
        } else if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[producer2 -> merger] READ_REQUEST');
          print('[producer2 -> merger] READ_REQUEST: varId=$varId');
          ctx3.handleReadRequest(varId, 'producer2');
        }
      }
    };

    ctx3.onMessageReady = (destination, message) {
      final serializer = PayloadSerializer('merger');
      IrmaContext? target;
      if (destination == 'producer1') target = ctx1;
      if (destination == 'producer2') target = ctx2;
      if (target != null) {
        if (message.type == MessageType.readRequest) {
          final varId = serializer.deserializeReadRequestPayload(message.payload);
          messageLog.add('[merger -> $destination] READ_REQUEST');
          print('[merger -> $destination] READ_REQUEST: varId=$varId');
          target.handleReadRequest(varId, 'merger');
        }
      }
    };

    print('\nMessage routing configured');

    // =========================================================
    // Step 5: Spawn goals
    // =========================================================

    // @1: produce_xs(Xs)
    final xsPC = program.labels['produce_xs/1']!;
    final xsWriterRef = VarRef(xsWriterAddr);
    runtime1.setGoalEnv(1, CallEnv(args: {0: xsWriterRef}));
    runtime1.setGoalProgram(1, 'main');
    runtime1.gq.enqueue(GoalRef(1, xsPC));
    print('\n@1: Spawned produce_xs(Xs)');

    // @2: produce_ys(Ys)
    final ysPC = program.labels['produce_ys/1']!;
    final ysWriterRef = VarRef(ysWriterAddr);
    runtime2.setGoalEnv(1, CallEnv(args: {0: ysWriterRef}));
    runtime2.setGoalProgram(1, 'main');
    runtime2.gq.enqueue(GoalRef(1, ysPC));
    print('@2: Spawned produce_ys(Ys)');

    // @3: merge(Xs?, Ys?, Zs)
    final mergePC = program.labels['merge/3']!;
    final xs3ReaderRef = VarRef(xs3ImportedAddr);
    final ys3ReaderRef = VarRef(ys3ImportedAddr);
    final zsWriterRef = VarRef(zsWriterAddr);
    runtime3.setGoalEnv(1, CallEnv(args: {0: xs3ReaderRef, 1: ys3ReaderRef, 2: zsWriterRef}));
    runtime3.setGoalProgram(1, 'main');
    runtime3.gq.enqueue(GoalRef(1, mergePC));
    print('@3: Spawned merge(Xs?, Ys?, Zs)');

    // =========================================================
    // Step 6: Run execution
    // =========================================================
    print('\n=== EXECUTION ===\n');

    var iterations = 0;
    const maxIterations = 20;

    while (iterations < maxIterations) {
      iterations++;
      print('\n--- Iteration $iterations ---');

      // Run @1 (producer1)
      final result1 = scheduler1.drainWithStatus();
      print('@1 (producer1): ${result1.status}');
      if (result1.status == ExecutionStatus.suspended) {
        ctx1.processSuspension(result1.blockingReaders);
      }
      ctx1.flushMessages();

      // Run @2 (producer2)
      final result2 = scheduler2.drainWithStatus();
      print('@2 (producer2): ${result2.status}');
      if (result2.status == ExecutionStatus.suspended) {
        ctx2.processSuspension(result2.blockingReaders);
      }
      ctx2.flushMessages();

      // Run @3 (merger)
      final result3 = scheduler3.drainWithStatus(debug: true);
      print('@3 (merger): ${result3.status}, blockingReaders=${result3.blockingReaders}');
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

    print('\n@1 Xs = ${_formatValue(runtime1.heap.derefAddr(xsWriterAddr), runtime1.heap)}');
    print('@2 Ys = ${_formatValue(runtime2.heap.derefAddr(ysWriterAddr), runtime2.heap)}');
    print('@3 Zs = ${_formatValue(runtime3.heap.derefAddr(zsWriterAddr), runtime3.heap)}');

    // Assertions
    final zsValue = runtime3.heap.derefAddr(zsWriterAddr);
    expect(zsValue, isA<StructTerm>(), reason: '@3 Zs should be a cons cell');

    // Check Zs contains elements from both Xs and Ys
    final zsList = _collectList(zsValue, runtime3.heap);
    expect(zsList.contains(1) || zsList.contains(2) || zsList.contains(3), isTrue,
        reason: 'Zs should contain elements from Xs');
    expect(zsList.contains('a') || zsList.contains('b') || zsList.contains('c'), isTrue,
        reason: 'Zs should contain elements from Ys');

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

List<Object> _collectList(Object value, dynamic heap) {
  final elements = <Object>[];
  Object current = value;
  // First dereference if it's a VarRef
  if (current is VarRef) {
    try { current = heap.derefAddr(current.addr); }
    catch (e) { return elements; }
  }
  while (true) {
    if (current is ConstTerm && current.value == 'nil') break;
    if (current is StructTerm && current.functor == '.' && current.args.length == 2) {
      var head = current.args[0];
      // Dereference head if needed
      if (head is VarRef) {
        try { head = heap.derefAddr(head.addr); }
        catch (e) { /* skip */ }
      }
      if (head is ConstTerm && head.value != null) {
        elements.add(head.value!);
      }
      var tail = current.args[1];
      if (tail is VarRef) {
        try { current = heap.derefAddr(tail.addr); }
        catch (e) { break; }
      } else {
        current = tail;
      }
    } else if (current is VarRef) {
      try { current = heap.derefAddr(current.addr); }
      catch (e) { break; }
    } else {
      break;
    }
  }
  return elements;
}
