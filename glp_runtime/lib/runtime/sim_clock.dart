/// A machine's simulated clock and its queue of pending tokens (GLP-Spec
/// appendix-guards.tex at 5fd9a15, "Simulated time (start_clock/1, rated/2,
/// step_clock/2)"): "A machine has a simulated clock, a real number of
/// seconds, and a queue of pending tokens, each with an activation time".
/// One is held by each [GlpRuntime], as when_idle's waits are: "The clock is of
/// one machine, as when_idle is."  The kernels '_start_clock', '_rated' and
/// '_step_clock' (body_kernels.dart) are its only users.
library;

import 'package:collection/collection.dart' show PriorityQueue;

import 'heap_fcp.dart' show HeapCell;

/// A pending token: the writer of the token, its activation time, and the
/// order in which it was made pending, which breaks a tie ("two tokens of one
/// activation time are released in the order in which they were made
/// pending").
class PendingToken {
  final double time;
  final int seq;
  final HeapCell writer;
  PendingToken(this.time, this.seq, this.writer);
}

int _byTimeThenSeq(PendingToken a, PendingToken b) {
  final c = a.time.compareTo(b.time);
  return c != 0 ? c : a.seq.compareTo(b.seq);
}

class SimClock {
  /// Whether start_clock has run on this machine: "rated and step_clock abort
  /// before the first start_clock".
  bool started = false;

  /// The clock, in seconds.
  double clock = 0.0;

  /// The state of the clock's generator, a seed of random/4's generator.
  int state = 0;

  int _seq = 0;

  PriorityQueue<PendingToken> _queue = PriorityQueue<PendingToken>(_byTimeThenSeq);

  /// The number of tokens pending.
  int get pendingCount => _queue.length;

  /// start_clock(Seed), but for the binding of the pending tokens, which the
  /// caller makes from what [start] returns: every pending token, in the
  /// order made pending; the clock set to 0; the generator seeded.
  List<PendingToken> start(int seed) {
    final pending = takeAll();
    clock = 0.0;
    state = seed;
    started = true;
    return pending;
  }

  /// Make the token [writer] pending at the clock plus [delay].
  void add(double delay, HeapCell writer) {
    _queue.add(PendingToken(clock + delay, _seq++, writer));
  }

  /// The pending token of least activation time, ties to the earliest made
  /// pending, or null where none is pending.
  PendingToken? get earliest => _queue.isEmpty ? null : _queue.first;

  /// Remove the earliest pending token and set the clock to its time.
  PendingToken release() {
    final t = _queue.removeFirst();
    clock = t.time;
    return t;
  }

  /// Every pending token, in the order made pending, the queue left empty.
  List<PendingToken> takeAll() {
    final all = _queue.toList()..sort((a, b) => a.seq.compareTo(b.seq));
    _queue = PriorityQueue<PendingToken>(_byTimeThenSeq);
    return all;
  }
}
