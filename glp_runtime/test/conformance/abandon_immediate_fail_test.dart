import 'package:test/test.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/heap.dart';
import 'package:glp_runtime/runtime/hanger.dart';
import 'package:glp_runtime/runtime/roq.dart';
import 'package:glp_runtime/runtime/suspend.dart';
import 'package:glp_runtime/runtime/abandon.dart';

void main() {
  test('Abandon(X!) immediately wakes readers FIFO; writer marked abandoned', () {
    final heap = Heap();
    final roq = ROQueues();

    const varId = 77;
    

    heap.addWriter(WriterCell(varId, varId));
    heap.addReader(ReaderCell(varId));

    final h1 = Hanger(goalId: 100, kappa: 9, armed: true);
    final h2 = Hanger(goalId: 200, kappa: 9, armed: true);
    roq.enqueue(varId, SuspensionNote(varId, h1));
    roq.enqueue(varId, SuspensionNote(varId, h2));

    final acts = AbandonOps.abandonWriter(
      heap: heap,
      roq: roq,
      writerId: varId,
    );

    expect(acts.map((a) => a.id).toList(), [100, 200]); // FIFO wake
    expect(heap.writer(varId)!.abandoned, isTrue);
    expect(roq.isEmpty(varId), isTrue);

    // Subsequent abandon calls produce no extra activations (hanger single-reactivation)
    final acts2 = AbandonOps.abandonWriter(
      heap: heap,
      roq: roq,
      writerId: varId,
    );
    expect(acts2, isEmpty);
  });
}
