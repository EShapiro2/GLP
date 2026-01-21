/// Test: Can we pass a StructTerm directly as a CallEnv argument?
/// This mimics what bidirectional_stream_test does.

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';

void main() {
  test('Direct StructTerm in CallEnv - copy pattern', () {
    print('\n=== DIRECT STRUCTTERM TEST ===\n');

    final compiler = GlpCompiler();
    final program = compiler.compile('''
      copy([], []).
      copy([X|Xs], [X?|Ys?]) :- copy(Xs?, Ys).
    ''');

    print('Compiled program: ${program.ops.length} ops');

    final runtime = GlpRuntime();
    final runner = BytecodeRunner(program);
    final scheduler = Scheduler(rt: runtime, runners: {'main': runner});

    // Allocate output variable
    final (resultWriterAddr, _) = runtime.heap.allocateVariable();
    print('Allocated writer R at addr=$resultWriterAddr');

    // Build [a, b, c] as nested cons cells with 'nil' terminator
    final inputList = StructTerm('.', [
      ConstTerm('a'),
      StructTerm('.', [
        ConstTerm('b'),
        StructTerm('.', [
          ConstTerm('c'),
          ConstTerm('nil')
        ])
      ])
    ]);

    // Store the input list on heap (per spec v2.16.3 Heap-Only Requirement)
    final inputAddr = runtime.heap.storeTermOnHeap(inputList);
    print('Stored input list on heap at addr=$inputAddr');

    // Spawn copy([a,b,c], R)
    final copyPC = program.labels['copy/2']!;
    final resultRef = VarRef(resultWriterAddr);
    runtime.setGoalEnv(1, CallEnv(args: {0: VarRef(inputAddr), 1: resultRef}));
    runtime.setGoalProgram(1, 'main');
    runtime.gq.enqueue(GoalRef(1, copyPC));
    print('Spawned copy([a,b,c], R)');

    // Run
    final result = scheduler.drainWithStatus(debug: true);
    print('\nStatus: ${result.status}');

    // Check result
    final resultValue = runtime.heap.derefAddr(resultWriterAddr);
    print('R = ${_formatValue(resultValue, runtime.heap)}');

    expect(result.status, equals(ExecutionStatus.succeeded));
    expect(resultValue, isA<StructTerm>());

    // Verify it's [a, b, c]
    final list = _collectList(resultValue, runtime.heap);
    print('Collected: $list');
    expect(list, equals(['a', 'b', 'c']));
  });
}

String _formatValue(Object value, dynamic heap) {
  if (value is ConstTerm) {
    if (value.value == 'nil') return '[]';
    return value.value.toString();
  }
  if (value is StructTerm) {
    if (value.functor == '.' && value.args.length == 2) {
      return _formatList(value, heap, 10);
    }
    return '${value.functor}(${value.args.map((a) => _formatValue(a, heap)).join(', ')})';
  }
  if (value is VarRef) {
    try {
      final deref = heap.derefAddr(value.addr);
      if (deref == value) return '_${value.addr}';
      return _formatValue(deref, heap);
    } catch (e) {
      return '_${value.addr}';
    }
  }
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

List<String> _collectList(Object value, dynamic heap) {
  final elements = <String>[];
  Object current = value;
  if (current is VarRef) {
    try { current = heap.derefAddr(current.addr); }
    catch (e) { return elements; }
  }
  while (true) {
    if (current is ConstTerm && current.value == 'nil') break;
    if (current is StructTerm && current.functor == '.' && current.args.length == 2) {
      var head = current.args[0];
      if (head is VarRef) {
        try { head = heap.derefAddr(head.addr); }
        catch (e) { /* skip */ }
      }
      if (head is ConstTerm && head.value != null) {
        elements.add(head.value.toString());
      }
      var tail = current.args[1];
      if (tail is VarRef) {
        try { current = heap.derefAddr(tail.addr); }
        catch (e) { break; }
      } else {
        current = tail;
      }
    } else {
      break;
    }
  }
  return elements;
}
