/// GLP channel handle.
///
/// Writer end of a GLP channel: sending a goal extends the stream
/// [goal | newTail]. Part of the channel-routing surface used by the
/// Distribute/Transmit opcodes (currently inert for statically linked
/// programs). The serve/_activate dynamic-dispatch path that constructed
/// these handles has been retired.
library;

import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/machine_state.dart' show GoalRef;

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

