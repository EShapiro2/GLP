import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspend_ops.dart';
import 'body_kernels.dart';
import 'place_streams.dart';
import 'sim_clock.dart';
import 'saved_state.dart';
import 'terms.dart' show Term, ModuleTerm;
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/multiagent/mad_context.dart' show MadContext;
import 'package:glp_runtime/bytecode/runner.dart'
    show CallEnv, GoalRunner;

/// What the runtime holds for one goal ([GlpRuntime._goals]): its argument
/// registers, its program key and whether one was set, its module value and
/// whether one was set (null is a value), its tail budget and the reader of a
/// wait it began, the last two with the tick of the clock they were first set
/// at, for the saved state's order.
class GoalRecord {
  final GoalId id;
  CallEnv? env;
  Object? program;
  bool hasProgram = false;
  Object? module;
  bool hasModule = false;
  int? budget;
  int budgetSeq = 0;
  HeapCell? waitReader;
  int waitSeq = 0;

  GoalRecord(this.id);

  void setProgram(Object? p) {
    program = p;
    hasProgram = true;
  }

  void setModule(Object? m) {
    module = m;
    hasModule = true;
  }
}

class GlpRuntime {
  final HeapFCP heap;
  final GoalQueue gq;
  final BodyKernelRegistry bodyKernels;

  /// The runtime-registered runners, by program key: the Scheduler falls back
  /// to this map for a goal whose program key its own map does not hold, which
  /// is how `run/2` routes a launched goal to its module's ByteRunner.
  final Map<Object?, GoalRunner> runners = {};

  /// The state held for each goal in the queue or suspended, by its id: its
  /// argument registers, its program key, its module value (the ModuleTerm
  /// whose code the goal's PC indexes into, the `self_module`/`run`
  /// substrate: every goal carries its module, spawned goals inherit it), its
  /// tail budget and the reader of a wait it began.  One record per goal
  /// since 2026-11-01, in place of five maps keyed by the goal's id, each
  /// written at every spawn and read at every drain.  A record is made by the
  /// first setter called for its goal ([_goal]), which is the one for its
  /// registers, and goes with [goalEnded].
  final Map<GoalId, GoalRecord> _goals = <GoalId, GoalRecord>{};

  /// A clock of the setters, for the saved state: the lists it writes keep
  /// the order the old maps kept, first set first ([saveState]).
  int _goalSeq = 0;

  GoalRecord _goal(GoalId g) => _goals[g] ??= GoalRecord(g);

  /// The record of goal [g], or null where the runtime holds none for it.
  GoalRecord? goalRecordOf(GoalId g) => _goals[g];

  /// The goal-id counter.  Every goal takes its id from it --- a spawned goal,
  /// a goal run/2 or run/3 activates, a REPL goal, the goal an agent's host
  /// posts at boot --- so no two goals share one.
  int nextGoalId = 1;

  /// The goal currently being reduced. The interpreter sets this immediately
  /// before invoking a body kernel, so a kernel (e.g. `self_module`) can reach
  /// its own goal's per-goal state through `rt` without the (rt, args) kernel
  /// signature carrying a goal handle.
  GoalId? currentGoalId;

  // Timer tracking for wait() guards
  int _pendingTimers = 0;
  int get pendingTimers => _pendingTimers;
  void incrementPendingTimers() => _pendingTimers++;
  void decrementPendingTimers() => _pendingTimers--;

  // Wait state tracking for wait() guards: the reader the timer will signal
  // is the goal's record's ([GoalRecord.waitReader]); when the goal resumes,
  // it checks whether that reader is bound (the timer fired).

  // Suspension tracking: the goals blocked on each reader, the suspended set
  // S by reader (IGLP dglp.tex, Definition "dGLP Transition System")
  // Updated by suspendGoalFCP, cleared when goals reactivate
  final Map<HeapCell, Set<GoalRef>> suspended = <HeapCell, Set<GoalRef>>{};

  // The readers each goal in [suspended] waits on, its suspension set W: the
  // index by which a goal reactivated leaves [suspended] at the cost of its
  // own readers (dGLP Reduce, S' = S \ {(G, W) : G ∈ R}; IGLP dglp.tex,
  // Definition "dGLP Transition System").  Until 2026-10-02 it left by a
  // visit to every entry of [suspended], so a wakeup cost in the number of
  // goals suspended.
  final Map<GoalRef, Set<HeapCell>> _suspendedOn = <GoalRef, Set<HeapCell>>{};

  // F — the failed goals of the dGLP and madGLP Reduce transactions. A reduction
  // has three outcomes; a goal that fails joins F and the agent goes on reducing
  // the rest of its queue, since no transaction ends a computation. The
  // scheduler folds this into the run's status without stopping the drain.
  // Recorded as the text of the failed goal, which is what a diagnostic needs.
  final List<String> failedGoals = [];

  // madGLP context (set when running in multiagent mode)
  // Used by '_cold_send' kernel to access globalization infrastructure
  MadContext? madContext;

  /// The declared places and their event streams (place_streams.dart): the
  /// runtime's bookkeeping behind declare(place(P, R, E)) and remove(place(P)), kept
  /// whether a networking layer is bound or none is.
  late final PlaceStreams placeStreams = PlaceStreams(this);

  /// The machine's simulated clock and its queue of pending tokens (GLP-Spec
  /// appendix-guards.tex at 5fd9a15, "Simulated time": "The clock is of one
  /// machine, as when_idle is"), behind '_start_clock', '_rated' and
  /// '_step_clock'.
  final SimClock simClock = SimClock();

  /// The person's identity this runtime holds: the key `self_key/1` answers,
  /// `sign/3` signs under, and the compiler certifies modules with. Set by the
  /// engine at construction (multiagent/identity.dart); for an agent, the key
  /// pair the runtime makes when it spawns the agent, before anything is
  /// compiled or sent (GLP-Spec appendix-guards.tex, "Identity and
  /// signature"; IGLP, Implementation Notes, "Boot").
  PersonIdentity? identity;

  /// The person send_to_user/1 emits to, the '_output'/1 kernel's two ends
  /// (GLP-Spec appendix-guards.tex, "Output to the person": send_to_user(
  /// Stream) "emits each element of Stream to the person operating the
  /// agent").  Where [outputTermCallback] is set, a person's bridge is the
  /// person: it is handed each element whole, the ground term, and nothing is
  /// printed or read back.  Where it is not, a terminal is the person:
  /// [outputCallback], or stdout where that is not set either, is handed the
  /// element as a terminal shows it (body_kernels.dart, [formatGroundTerm]).
  /// Until 2026-10-11 there was the terminal alone, and a screen parsed what
  /// it printed.
  void Function(Term term)? outputTermCallback;
  void Function(String)? outputCallback;

  /// Clear wait state for a goal (after timer completes)
  void clearWaitState(int goalId) {
    _goals[goalId]?.waitReader = null;
  }

  /// Set wait state for a goal
  void setWaitReader(int goalId, HeapCell readerId) {
    final rec = _goal(goalId);
    if (rec.waitReader == null) rec.waitSeq = ++_goalSeq;
    rec.waitReader = readerId;
  }

  /// Get the wait reader for a goal (if any)
  HeapCell? getWaitReader(int goalId) => _goals[goalId]?.waitReader;

  // when_idle (GLP-Spec appendix-guards.tex at e3a8d52, the time guards):
  // "when_idle suspends while the machine has a Reduce or a Communicate to
  // make, and succeeds when it has none."  A goal suspends on it as on wait/1:
  // on the reader of a fresh variable, whose writer the scheduler binds when
  // the machine is idle (Scheduler.drainWithStatus), which re-tries the goal.
  // Keyed by goal, in the order the goals first suspended on it.
  final Map<int, ({HeapCell writer, HeapCell reader})> _idleWaits = {};

  /// The machine has no Reduce and no Communicate to make (IGLP eadadcd,
  /// Implementation Notes, "The when_idle Guard": "The guard succeeds when
  /// the agent's run queue is empty and, in madGLP, its outbox too: a queued
  /// outbound message is a Communicate still to make").  The goal whose guard
  /// asks has been taken from the queue, so its own reduction is not counted.
  /// A message in a madGLP agent's outbox counts while it is one the agent's
  /// Send is enabled for, unsent and not held (Definition madGLP Send); a
  /// held message waits on authorise_link/2, not on the machine.
  bool get isIdle {
    if (gq.length != 0) return false;
    final ctx = madContext;
    return ctx is! MadContext || !ctx.mp.hasSendable;
  }

  /// The reader goal [goalId] suspends on while it waits on when_idle: the
  /// one it already waits on, or a fresh one, the goal then joining the end
  /// of the goals that wait.
  HeapCell idleReader(int goalId) {
    final w = _idleWaits[goalId];
    if (w != null) return w.reader;
    final (writer, reader) = heap.allocateVariable();
    _idleWaits[goalId] = (writer: writer, reader: reader);
    return reader;
  }

  /// Goal [goalId] has passed when_idle: it no longer waits on it.
  void clearIdleWait(int goalId) => _idleWaits.remove(goalId);

  /// The number of goals whose state the runtime holds: those in the queue
  /// or suspended, once every goal that ended has left ([goalEnded]).
  int get goalsHeld => _goals.length;

  /// Some goal may be waiting on when_idle.  An entry whose goal has since
  /// been re-tried by another reader and gone on is counted until
  /// [wakeIdle] passes over it.
  bool get hasIdleWaits => _idleWaits.isNotEmpty;

  /// Re-try one goal that waits on when_idle, the one that has waited
  /// longest: bind the writer of the reader it suspended on, which puts it
  /// back in the queue.  One at a time, since the goal re-tried may make work
  /// for the machine, and then the next is not idle.  An entry whose goal was
  /// re-tried by another of its readers wakes nothing (its suspension record
  /// is disarmed) and is passed over.  True if a goal was re-tried.
  bool wakeIdle() {
    while (_idleWaits.isNotEmpty) {
      final goalId = _idleWaits.keys.first;
      final w = _idleWaits.remove(goalId)!;
      final reactivated = heap.bindVariableConst(w.writer, 0);
      for (final goalRef in reactivated) {
        enqueueReactivatedGoal(goalRef);
      }
      if (reactivated.isNotEmpty) return true;
    }
    return false;
  }

  GlpRuntime({HeapFCP? heap, GoalQueue? gq, BodyKernelRegistry? bodyKernels})
      : heap = heap ?? HeapFCP(),
        gq = gq ?? GoalQueue(),
        bodyKernels = bodyKernels ?? _createDefaultBodyKernels();

  /// Create body kernel registry with standard kernels registered
  static BodyKernelRegistry _createDefaultBodyKernels() {
    final registry = BodyKernelRegistry();
    registerStandardBodyKernels(registry);
    return registry;
  }

  /// Suspend goal using FCP-exact shared suspension records
  void suspendGoalFCP({
    required int goalId,
    required int kappa,
    required Set<HeapCell> readerVarIds,
  }) {
    // Track which readers have this goal suspended
    final goalRef = GoalRef(goalId, kappa);
    for (final readerId in readerVarIds) {
      suspended.putIfAbsent(readerId, () => <GoalRef>{}).add(goalRef);
    }
    if (readerVarIds.isNotEmpty) {
      _suspendedOn.putIfAbsent(goalRef, () => <HeapCell>{}).addAll(readerVarIds);
    }

    SuspendOps.suspendGoalFCP(
      heap: heap,
      goalId: goalId,
      kappa: kappa,
      readerVarIds: readerVarIds,
    );
  }

  /// One tail reduction of goal [g] spends one of its tail budget: true at
  /// zero, when the scheduler yields and the budget is reset to
  /// [tailRecursionBudgetInit].
  bool tailReduce(GoalId g) {
    final rec = _goal(g);
    final current = rec.budget ?? tailRecursionBudgetInit;
    final next = (current <= 0) ? 0 : current - 1;
    if (rec.budget == null) rec.budgetSeq = ++_goalSeq;
    if (next == 0) {
      rec.budget = tailRecursionBudgetInit;
      return true;
    } else {
      rec.budget = next;
      return false;
    }
  }

  int budgetOf(GoalId g) => _goals[g]?.budget ?? tailRecursionBudgetInit;

  void setGoalEnv(GoalId g, CallEnv env) {
    _goal(g).env = env;
  }

  CallEnv? getGoalEnv(GoalId g) => _goals[g]?.env;

  /// A new goal's record, its registers set: what a spawn makes in one step,
  /// setting its program and module on the record it returns.
  GoalRecord addGoal(GoalId g, CallEnv env) => _goal(g)..env = env;

  /// Goal [g] has ended: it reduced with no goal of its body continuing it, or
  /// it failed.  It is no longer in the queue or the suspended set (dGLP
  /// Transition System: a Reduce replaces the goal by its body, a Fail moves
  /// it to F, which keeps its text), so its argument registers, its program
  /// and module, its tail budget and any wait it began are dropped.  Until
  /// 2026-10-02 they were kept for the whole run, some 500 bytes a goal: 1.6
  /// of the 5 GB live at the end of a year of sGLP's 100-agent social graph.
  void goalEnded(GoalId g) {
    _goals.remove(g);
  }

  void setGoalProgram(GoalId g, Object? program) {
    _goal(g).setProgram(program);
  }

  Object? getGoalProgram(GoalId g) => _goals[g]?.program;

  /// Set the module VALUE a goal runs (its ModuleTerm) — read back by
  /// `self_module`, inherited by spawned children.
  void setGoalModule(GoalId g, Object? module) {
    _goal(g).setModule(module);
  }

  /// Get the module VALUE a goal runs (its ModuleTerm), or null if unset.
  Object? getGoalModule(GoalId g) => _goals[g]?.module;

  /// The root self.glp's declared type-identity table (TGLP, Implementation
  /// Notes, "The tables"), keyed as the root's compiled code carries each
  /// procedure, `:p/n`: the table of the caller's scope for a goal that runs
  /// no module, one posted at the root with no program loaded, whose scope is
  /// the root's.  `find_type` reads it there (module_kernels.dart).  Built on
  /// first use by the engine that set it; null where none did.  Untyped, as
  /// [ModuleTerm.declaredTypes] is.
  Object? Function()? rootDeclaredTypes;

  /// A suspended goal re-enqueued through the activation its binding returns
  /// (IGLP, Implementation Notes, "Event-driven execution"): it goes back into
  /// the queue and leaves [suspended] as it is enqueued (dGLP Reduce, S' = S \
  /// {(G, W) : G ∈ R}).  This is the one path by which a woken goal is
  /// re-enqueued: every binding's activations, the runner's commit and
  /// body-structure bindings among them, go through it (GLP #11 Cowork,
  /// Code_inbox 2026-11-03 16:40 UTC, item 8; until 2026-11-05 the runner put
  /// them in the queue itself and the scheduler took them out of [suspended]
  /// when it took them, and until 2026-10-02 such a goal stayed in [suspended]
  /// for the rest of the run: after 30 days of sGLP's social graph the map
  /// held 464,267 goals, 631 of them suspended).  The state is stored between
  /// cycles (IGLP, "Persistence"), so the moment within a cycle at which the
  /// goal leaves [suspended] is not observed.
  void enqueueReactivatedGoal(GoalRef goal) {
    gq.enqueue(goal);
    _removeFromSuspended(goal);
  }

  // ===========================================================================
  // The agent's saved state (IGLP, Definition "Restart"; Implementation Notes,
  // "Persistence": "the resolvent with its goal identities and suspension
  // sets")
  // ===========================================================================

  /// Write the resolvent and what the runtime holds of each goal: the goal-id
  /// counter and the heap's serial counter; F, the failed goals, as text; the
  /// queue A in order; each goal held, queued or suspended, with its argument
  /// registers, its program and module and its tail budget; the suspended
  /// goals S, by reader and by goal, in the order they suspended (the
  /// suspension lists themselves are on the writer cells the state writes);
  /// the goals waiting on a timer or on when_idle; and the declared places.
  /// Written at the end of an event cycle, when no goal is being reduced.
  void saveState(StateWriter w) {
    w.uint(nextGoalId);
    w.uint(heap.HP);
    w.uint(failedGoals.length);
    for (final f in failedGoals) {
      w.str(f);
    }
    final queued = gq.items.toList();
    w.uint(queued.length);
    for (final g in queued) {
      w.goalRef(g);
    }
    // The goals' registers, programs, modules, budgets and wait readers, each
    // list as the map it was until 2026-11-01 wrote it: the goals that have
    // the thing, first set first.  Registers, programs and modules are set
    // together, in the record's order; a budget and a wait reader begin
    // later, in the order of their clock ([_goalSeq]).
    final withEnv = [for (final g in _goals.values) if (g.env != null) g];
    w.uint(withEnv.length);
    for (final g in withEnv) {
      final env = g.env!;
      w.uint(g.id);
      w.uint(env.length);
      for (final MapEntry(key: slot, value: arg) in env.entries) {
        w.uint(slot);
        w.term(arg);
      }
    }
    final withProgram = [for (final g in _goals.values) if (g.hasProgram) g];
    w.uint(withProgram.length);
    for (final g in withProgram) {
      final program = g.program;
      if (program != null && program is! String) {
        throw StateError('goal ${g.id} runs a program keyed by '
            '${program.runtimeType}, which has no saved form');
      }
      w.uint(g.id);
      w.optStr(program as String?);
    }
    final withModule = [for (final g in _goals.values) if (g.hasModule) g];
    w.uint(withModule.length);
    for (final g in withModule) {
      final module = g.module;
      if (module != null && module is! ModuleTerm) {
        throw StateError('goal ${g.id} carries a module of kind '
            '${module.runtimeType}, which has no saved form');
      }
      w.uint(g.id);
      w.module(module as ModuleTerm?);
    }
    final withBudget = [for (final g in _goals.values) if (g.budget != null) g]
      ..sort((a, b) => a.budgetSeq.compareTo(b.budgetSeq));
    w.uint(withBudget.length);
    for (final g in withBudget) {
      w.uint(g.id);
      w.uint(g.budget!);
    }
    w.uint(suspended.length);
    for (final MapEntry(key: reader, value: goals) in suspended.entries) {
      w.cell(reader);
      w.uint(goals.length);
      for (final g in goals) {
        w.goalRef(g);
      }
    }
    w.uint(_suspendedOn.length);
    for (final MapEntry(key: goal, value: readers) in _suspendedOn.entries) {
      w.goalRef(goal);
      w.uint(readers.length);
      for (final r in readers) {
        w.cell(r);
      }
    }
    final withWait = [for (final g in _goals.values) if (g.waitReader != null) g]
      ..sort((a, b) => a.waitSeq.compareTo(b.waitSeq));
    w.uint(withWait.length);
    for (final g in withWait) {
      w.uint(g.id);
      w.cell(g.waitReader!);
    }
    w.uint(_idleWaits.length);
    for (final MapEntry(key: id, value: pair) in _idleWaits.entries) {
      w.uint(id);
      w.cell(pair.writer);
      w.cell(pair.reader);
    }
    placeStreams.saveState(w);
  }

  /// Read back what [saveState] wrote, into this runtime, fresh from its
  /// engine's construction: nothing queued, held or suspended.  A goal that
  /// waits on a timer (wait/1, wait_until/1) is restored suspended on its
  /// timer's reader; the timer itself did not survive the termination.
  void restoreState(StateReader r) {
    if (gq.length != 0 || _goals.isNotEmpty || suspended.isNotEmpty) {
      throw StateError('an agent\'s state is restored into a runtime that '
          'already holds goals');
    }
    nextGoalId = r.uint();
    heap.HP = r.uint();
    failedGoals
      ..clear()
      ..addAll([for (var i = r.uint(); i > 0; i--) r.str()]);
    for (var i = r.uint(); i > 0; i--) {
      gq.enqueue(r.goalRef());
    }
    for (var i = r.uint(); i > 0; i--) {
      final id = r.uint();
      final args = <int, Term>{};
      for (var j = r.uint(); j > 0; j--) {
        final slot = r.uint();
        args[slot] = r.term();
      }
      _goal(id).env = CallEnv(args: args);
    }
    for (var i = r.uint(); i > 0; i--) {
      final id = r.uint();
      _goal(id).setProgram(r.optStr());
    }
    for (var i = r.uint(); i > 0; i--) {
      final id = r.uint();
      _goal(id).setModule(r.module());
    }
    for (var i = r.uint(); i > 0; i--) {
      final id = r.uint();
      _goal(id)
        ..budget = r.uint()
        ..budgetSeq = ++_goalSeq;
    }
    for (var i = r.uint(); i > 0; i--) {
      final reader = r.cell();
      suspended[reader] = {for (var j = r.uint(); j > 0; j--) r.goalRef()};
    }
    for (var i = r.uint(); i > 0; i--) {
      final goal = r.goalRef();
      _suspendedOn[goal] = {for (var j = r.uint(); j > 0; j--) r.cell()};
    }
    for (var i = r.uint(); i > 0; i--) {
      final id = r.uint();
      _goal(id)
        ..waitReader = r.cell()
        ..waitSeq = ++_goalSeq;
    }
    for (var i = r.uint(); i > 0; i--) {
      final id = r.uint();
      final writer = r.cell();
      _idleWaits[id] = (writer: writer, reader: r.cell());
    }
    placeStreams.restoreState(r);
  }

  /// The goals held, by id, and the program key each runs: what a restarted
  /// agent's host makes runners for.
  Map<GoalId, Object?> get goalPrograms => Map.unmodifiable({
        for (final g in _goals.values)
          if (g.hasProgram) g.id: g.program,
      });

  /// Remove a goal from every entry of [suspended] it is in: those of the
  /// readers it waits on, which [_suspendedOn] holds.
  void _removeFromSuspended(GoalRef goal) {
    final readers = _suspendedOn.remove(goal);
    if (readers == null) return;
    for (final readerId in readers) {
      final goals = suspended[readerId];
      if (goals == null) continue;
      goals.remove(goal);
      if (goals.isEmpty) suspended.remove(readerId);
    }
  }
}
