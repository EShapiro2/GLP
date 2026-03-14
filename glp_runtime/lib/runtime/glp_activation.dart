/// GLP-level module activation.
///
/// Creates a GLP channel, spawns serve(Module, ChannelReader?), and
/// returns a handle for sending goals on the channel.
///
/// Phase 4 of dynamic module dispatch (docs/modules/dynamic-dispatch-implementation-plan.md).
library;

import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';

/// Handle for a GLP module channel.
///
/// Holds the writer end of the channel for sending goal terms.
/// Each [send] call extends the stream: [goal | newTail].
class GlpChannelHandle {
  final HeapFCP _heap;
  int _writerAddr;

  GlpChannelHandle(this._heap, this._writerAddr);

  /// Current writer address (for debugging/testing).
  int get writerAddr => _writerAddr;

  /// Send a goal term on the channel.
  ///
  /// Binds current writer to [goal | newTail], advances writer to newTail.
  /// Returns goals woken up by the injection (must be enqueued by caller).
  List<GoalRef> send(Term goal) {
    final (tailWriterAddr, _) = _heap.allocateVariable();
    final consCell = StructTerm('.', [goal, VarRef(tailWriterAddr)]);
    final activations = _heap.bindVariable(_writerAddr, consCell);
    _writerAddr = tailWriterAddr;
    return activations;
  }

  /// Close the channel (bind writer to nil / empty list).
  ///
  /// Returns goals woken up by the closure (must be enqueued by caller).
  List<GoalRef> close() {
    return _heap.bindVariable(_writerAddr, ConstTerm('nil'));
  }
}

/// Activate a module at the GLP level.
///
/// Creates a GLP channel, constructs a ModuleTerm from compiled bytecode,
/// spawns serve(ModuleTerm, ChannelReader?), and returns the channel handle.
///
/// The serve runner is registered in rt.runners so the scheduler can find it.
/// The caller must drain the scheduler to execute the spawned serve goal.
GlpChannelHandle activateModule({
  required GlpRuntime rt,
  required BytecodeProgram serveBytecode,
  required BytecodeProgram moduleBytecode,
  required String moduleName,
}) {
  // 1. Create GLP channel (writer/reader pair)
  final (writerAddr, readerAddr) = rt.heap.allocateVariable();

  // 2. Construct ModuleTerm and store on heap
  final moduleTerm = ModuleTerm(moduleBytecode, name: moduleName);
  final moduleAddr = rt.heap.storeTermOnHeap(moduleTerm);

  // 3. Spawn serve(Module, ChannelReader?)
  final goalId = rt.nextGoalId++;
  final env = CallEnv(args: {0: VarRef(moduleAddr), 1: VarRef(readerAddr)});
  rt.setGoalEnv(goalId, env);
  rt.setGoalProgram(goalId, serveBytecode);

  final servePc = serveBytecode.labels['serve/2']!;
  rt.gq.enqueue(GoalRef(goalId, servePc));

  // 4. Tag as infrastructure goal (spec §3.4, §3.5)
  rt.infrastructureGoalIds.add(goalId);

  // 5. Register serve runner if not already present
  if (!rt.runners.containsKey(serveBytecode)) {
    rt.runners[serveBytecode] = BytecodeRunner(serveBytecode);
  }

  // 6. Register channel handle in rt.glpChannels for RPC routing (Phase 5)
  final channel = GlpChannelHandle(rt.heap, writerAddr);
  rt.glpChannels[moduleName] = channel;

  // 7. Return channel handle (writer end)
  return channel;
}
