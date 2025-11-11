import 'package:test/test.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/hanger.dart';
import 'package:glp_runtime/runtime/heap.dart';
import 'package:glp_runtime/runtime/roq.dart';
import 'package:glp_runtime/runtime/suspend.dart';
import 'package:glp_runtime/runtime/commit.dart';

void main() {
  test('commit applies σ̂w effect: wake FIFO and single-reactivation', () {
    final heap = Heap();
    final roq = ROQueues();

    final writerId = 1;
    final readerId = 1001;

    heap.addWriter(WriterCell(writerId, readerId));
    heap.addReader(ReaderCell(readerId));

    final h1 = Hanger(goalId: 10, kappa: 7, armed: true);
    final h2 = Hanger(goalId: 20, kappa: 7, armed: true);
    roq.enqueue(readerId, SuspensionNote(readerId, h1));
    roq.enqueue(readerId, SuspensionNote(readerId, h2));

    final acts = CommitOps.applySigmaHat(
      heap: heap,
      roq: roq,
      writerIds: [writerId],
    );

    expect(acts.map((a) => a.id).toList(), [10, 20]);
    expect(acts.map((a) => a.pc).toSet(), {7});
    expect(roq.isEmpty(readerId), isTrue);

    final h = Hanger(goalId: 30, kappa: 7, armed: true);
    roq.enqueue(readerId, SuspensionNote(readerId, h));
    final acts2 = CommitOps.applySigmaHat(
      heap: heap,
      roq: roq,
      writerIds: [writerId],
    );
    expect(acts2.map((a) => a.id).toList(), [30]);
    expect(h.armed, isFalse);
  });
}
