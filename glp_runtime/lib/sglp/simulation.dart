// glp_runtime/lib/sglp/simulation.dart
//
// The state an sGLP run adds to a configuration of the machine (svGLP,
// sections/sglp.tex, Definition "Configuration, Pending, Quiescent"): the
// simulated time t and the activation times alpha of the pending goals.
//
//   Machine: a Reduce of a goal that is not pending (or a Communicate or a
//            Cold-call) leaves t alone and gives each rated goal B @ r of the
//            body it spawns the activation time t + tau, tau exponential with
//            rate r.  A pending goal is not in the machine's queue, so it is
//            not reduced.
//   Release: where the machine is quiescent --- its queue empty --- the
//            pending goal of least activation time is taken, t advances to
//            that time, and the goal joins the queue as an ordinary goal.
//
// alpha is kept as a binary heap on (activation time, goal identifier).  The
// scheduler (runtime/scheduler.dart) takes the Release; the byte interpreter
// (engine_v2/interp.dart, `spawn_rated`) makes goals pending.
//
// Agents (Definition "Simulation Program": the compilation of M runs at each
// agent a).  A goal of the run is at an agent or at none: the run declaration
// creates the agents 1..N, the caller places each agent's initial goal at it
// (GlpEngine.runGoal, `agents:`), and every goal a Reduce spawns is at its
// parent's agent --- rated or not, and through its Release, which keeps the
// goal.  A goal at no agent is the harness's: the network of the lifted
// system, say, whose reductions are no agent's Reduce and are not logged.

import 'draws.dart' as draws;
import 'log.dart';
import 'population.dart';

/// One pending goal: a member of the domain of alpha.
class PendingGoal {
  /// Its activation time alpha(A), in simulated seconds.
  final double time;

  /// Its identifier (lineage), which breaks ties in activation time without
  /// reference to the schedule.
  final int lineage;

  /// The engine's goal id, for the release record.
  final int goalId;

  /// The rated goal's procedure, name/arity, for the release record.
  final String signature;

  /// Makes the goal an ordinary goal: puts it in the machine's queue.
  final void Function() release;

  PendingGoal(this.time, this.lineage, this.goalId, this.signature, this.release);

  bool before(PendingGoal o) =>
      time < o.time || (time == o.time && lineage < o.lineage);
}

/// A person goal as person/2 spawned it (lib/sglp/person.dart): at [agent],
/// of the person procedure [procedure] that the agent's kind [kind] declares
/// for the interactive type [type], handed [seed], for the asked goal whose
/// identifier is [askedLineage].
class PersonSpawn {
  final int agent;
  final String type;
  final String kind;
  final String procedure;
  final int seed;
  final int askedLineage;
  final int goalId;
  PersonSpawn(this.agent, this.type, this.kind, this.procedure, this.seed,
      this.askedLineage, this.goalId);

  @override
  String toString() =>
      'person($agent, $type, $kind:$procedure, seed $seed, goal $goalId)';
}

/// A Release as it was taken: the time the clock advanced to, and the goal.
class ReleaseRecord {
  final double time;
  final int goalId;
  final String signature;
  ReleaseRecord(this.time, this.goalId, this.signature);

  @override
  String toString() => 'release($time, $signature#$goalId)';
}

/// The simulated clock, the pending goals and the seed of one sGLP run.
class SimState {
  /// The seed every draw of the run derives from: the run declaration's, or
  /// the engine's default where the program declares no run.
  final int seed;

  /// The run declaration's `until`, in simulated seconds: a Release whose
  /// activation time exceeds it is not taken.  Null where there is no run
  /// declaration, and every pending goal is released in its time.
  final double? horizon;

  /// The run declaration, where the program has one.
  final Population? population;

  /// The simulated time t.  It advances at a Release and nowhere else.
  double _clock = 0;
  double get clock => _clock;

  /// Releases are taken only while this holds.  The engine clears it while
  /// the conjuncts of a posted conjunction are being put to the machine one at
  /// a time, since the configuration is not the run's until all are in.
  bool releaseEnabled = true;

  /// A bound on the Releases this run takes, for a harness that wants a run
  /// of so many Releases; null for none.
  int? maxReleases;

  /// Releases taken so far.
  int releases = 0;

  /// Called at each Release, after the clock has advanced and before the
  /// goal joins the queue.
  void Function(ReleaseRecord)? onRelease;

  /// Called at each reduction the machine makes in this run, with the
  /// reduced goal's procedure and the clock.  Null unless a harness sets it.
  void Function(String signature, int goalId, double clock)? onReduce;

  /// Called at each person goal person/2 spawns.  Null unless a harness sets
  /// it.
  void Function(PersonSpawn)? onPerson;

  /// The run's log (Definition "Interface Variable, Log"), where the engine
  /// was given somewhere to write it; null otherwise, and then nothing tracks
  /// the interface variables.
  SimLog? log;

  /// The goal the machine is reducing now, set by the scheduler around each
  /// reduction and by a Release that reduces a kernel goal: the Reduce whose
  /// assignments the log attributes to that goal's agent.  Null between
  /// reductions.
  int? reducing;

  /// The identifier of each goal of the run that has one: its lineage.
  final Map<int, int> _lineage = {};

  /// The agent of each goal of the run that is at one.
  final Map<int, int> _agent = {};

  final List<PendingGoal> _heap = [];

  SimState({required this.seed, this.horizon, this.population});

  // ---------------------------------------------------------------- lineage

  /// The identifier of goal [goalId].  A goal the run did not name --- one
  /// put to the machine by a kernel such as `run/2` --- is named by its
  /// engine id.
  int lineageOf(int goalId) =>
      _lineage[goalId] ?? draws.combine(draws.tagRoot, -goalId);

  void setLineage(int goalId, int lineage) => _lineage[goalId] = lineage;

  /// Name the [ordinal]-th goal [childId] spawned by the reduction of
  /// [parentId], and return its identifier.  The child is at its parent's
  /// agent.
  int nameChild(int parentId, int childId, int ordinal) {
    final l = draws.childLineage(lineageOf(parentId), ordinal);
    _lineage[childId] = l;
    inherit(parentId, childId);
    return l;
  }

  /// Forget a goal that has been reduced: nothing asks its identifier or its
  /// agent again.
  void forget(int goalId) {
    _lineage.remove(goalId);
    _agent.remove(goalId);
  }

  // ----------------------------------------------------------------- agents

  /// The agent goal [goalId] is at, or null if it is at none.
  int? agentOf(int? goalId) => goalId == null ? null : _agent[goalId];

  /// Place goal [goalId] at [agent]: an initial goal, or a person goal.
  void placeAt(int goalId, int agent) => _agent[goalId] = agent;

  /// Goal [childId], put to the machine by the reduction of [parentId], is at
  /// the parent's agent.
  void inherit(int? parentId, int childId) {
    final a = parentId == null ? null : _agent[parentId];
    if (a != null) {
      _agent[childId] = a;
    } else {
      _agent.remove(childId);
    }
  }

  /// The agent of the Reduce the machine is making now, or null.
  int? get reducingAgent => agentOf(reducing);

  /// The machine begins a Reduce of goal [goalId].
  void beginReduce(int goalId) {
    reducing = goalId;
    log?.beginReduce();
  }

  /// The Reduce begun by [beginReduce] is done: the log takes its
  /// assignments, at its goal's agent.
  void endReduce() {
    log?.endReduce(reducingAgent);
    reducing = null;
  }

  // ---------------------------------------------------------------- pending

  /// Make a goal pending: its activation time is the clock plus an
  /// exponential delay with rate [ratePerSecond], drawn from the seed and the
  /// goal's identifier.  Returns the activation time.
  double addPending({
    required int goalId,
    required int lineage,
    required double ratePerSecond,
    required String signature,
    required void Function() release,
  }) {
    final t = _clock + draws.exponentialDelay(seed, lineage, ratePerSecond);
    _push(PendingGoal(t, lineage, goalId, signature, release));
    return t;
  }

  int get pendingCount => _heap.length;
  bool get hasPending => _heap.isNotEmpty;

  /// The least activation time, or null if nothing is pending.
  double? get nextActivation => _heap.isEmpty ? null : _heap.first.time;

  /// True where a Release may be taken now that the machine is quiescent.
  bool get canRelease {
    if (!releaseEnabled || _heap.isEmpty) return false;
    if (maxReleases != null && releases >= maxReleases!) return false;
    final h = horizon;
    return h == null || _heap.first.time <= h;
  }

  /// Take a Release: remove the pending goal of least activation time,
  /// advance the clock to it, and make it an ordinary goal.  The caller has
  /// established quiescence and [canRelease].
  ReleaseRecord release() {
    final g = _pop();
    // alpha(A) >= t holds of every pending goal (Definition "Configuration,
    // Pending, Quiescent"): the clock never moves back.
    if (g.time > _clock) _clock = g.time;
    releases++;
    final rec = ReleaseRecord(_clock, g.goalId, g.signature);
    onRelease?.call(rec);
    g.release();
    return rec;
  }

  // ------------------------------------------------------------- the heap

  void _push(PendingGoal g) {
    _heap.add(g);
    var i = _heap.length - 1;
    while (i > 0) {
      final p = (i - 1) >> 1;
      if (!_heap[i].before(_heap[p])) break;
      final t = _heap[i];
      _heap[i] = _heap[p];
      _heap[p] = t;
      i = p;
    }
  }

  PendingGoal _pop() {
    final top = _heap.first;
    final last = _heap.removeLast();
    if (_heap.isNotEmpty) {
      _heap[0] = last;
      var i = 0;
      final n = _heap.length;
      while (true) {
        final l = 2 * i + 1;
        final r = l + 1;
        var m = i;
        if (l < n && _heap[l].before(_heap[m])) m = l;
        if (r < n && _heap[r].before(_heap[m])) m = r;
        if (m == i) break;
        final t = _heap[i];
        _heap[i] = _heap[m];
        _heap[m] = t;
        i = m;
      }
    }
    return top;
  }
}
