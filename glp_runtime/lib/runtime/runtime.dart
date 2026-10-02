import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspend_ops.dart';
import 'commit.dart';
import 'abandon.dart';
import 'fairness.dart';
import 'body_kernels.dart';
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/multiagent/mad_context.dart' show MadContext;
import 'package:glp_runtime/bytecode/runner.dart'
    show CallEnv, GoalRunner;
import 'package:glp_runtime/runtime/glp_activation.dart' show GlpChannelHandle;

class GlpRuntime {
  final HeapFCP heap;
  final GoalQueue gq;
  final BodyKernelRegistry bodyKernels;

  /// Shared runners map: program key → GoalRunner (object or byte loop).
  /// Used by the Scheduler to find the runner for a goal's program.
  /// Runtime-registered runners (extension point; used by `run/2` to route a
  /// launched goal to its module's ByteRunner; also the seam the retired
  /// dynamic-dispatch path once used).
  final Map<Object?, GoalRunner> runners = {};

  /// GLP channel handles: module name → GlpChannelHandle. Read by the runner's
  /// Distribute/Transmit opcodes to route RPCs via GLP channels. Currently
  /// unpopulated — the dynamic-dispatch path that registered handles was retired.
  final Map<String, GlpChannelHandle> glpChannels = {};

  final Map<GoalId, int> _budgets = <GoalId, int>{};
  final Map<GoalId, CallEnv> _goalEnvs = <GoalId, CallEnv>{};
  final Map<GoalId, Object?> _goalPrograms = <GoalId, Object?>{};
  final Map<GoalId, Object?> _goalModuleContexts = <GoalId, Object?>{};  // Module context for RPC;
  /// Per-goal module VALUE — the ModuleTerm whose code the goal's PC indexes
  /// into. The `self_module`/`run` substrate: every goal carries its module,
  /// spawned goals inherit it. Distinct from _goalModuleContexts (RPC routing).
  final Map<GoalId, Object?> _goalModules = <GoalId, Object?>{};

  // Goal ID counter for spawn
  int nextGoalId = 10000;  // Start at 10000 to avoid collisions with test goal IDs

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

  // Wait state tracking for wait() guards
  // Maps goalId to the reader ID that the timer will signal
  // When goal resumes, we check if this reader is bound (timer fired)
  final Map<int, int> _waitReaders = <int, int>{};

  // Suspension tracking for scheduler-IRMA integration (spec section 8.4)
  // Maps reader varId -> Set<GoalRef> of goals blocked on that reader
  // Updated by suspendGoalFCP, cleared when goals reactivate
  final Map<int, Set<GoalRef>> suspended = <int, Set<GoalRef>>{};

  // The readers each goal in [suspended] waits on, its suspension set W: the
  // index by which a goal reactivated leaves [suspended] at the cost of its
  // own readers (dGLP Reduce, S' = S \ {(G, W) : G ∈ R}; IGLP dglp.tex,
  // Definition "dGLP Transition System").  Until 2026-10-02 it left by a
  // visit to every entry of [suspended], so a wakeup cost in the number of
  // goals suspended.
  final Map<GoalRef, Set<int>> _suspendedOn = <GoalRef, Set<int>>{};

  // Infrastructure goal IDs (spec §3.4): serve goals spawned by auto-activation.
  // Their suspension does not affect user goal status determination.
  final Set<int> infrastructureGoalIds = {};

  // F — the failed goals of the dGLP and madGLP Reduce transactions. A reduction
  // has three outcomes; a goal that fails joins F and the agent goes on reducing
  // the rest of its queue, since no transaction ends a computation. The
  // scheduler folds this into the run's status without stopping the drain.
  // Recorded as the text of the failed goal, which is what a diagnostic needs.
  final List<String> failedGoals = [];

  // madGLP context (set when running in multiagent mode)
  // Used by '_cold_send' kernel to access globalization infrastructure
  Object? madContext;

  /// The person's identity this runtime holds: the key `self_key/1` answers,
  /// `sign/3` signs under, and the compiler certifies modules with. Set by the
  /// engine at construction (multiagent/identity.dart).
  PersonIdentity? identity;

  // Output callback for '_output'/1 kernel.
  // If set, called instead of print(). Used by tests and Flutter UI.
  void Function(String)? outputCallback;

  /// Check if a goal has a pending wait and if the timer has fired
  /// Returns null if no wait state, true if timer fired, false if still waiting
  bool? checkWaitState(int goalId) {
    final readerId = _waitReaders[goalId];
    if (readerId == null) return null;
    // Check if the writer has been bound (timer fired)
    return heap.isFullyBound(readerId);
  }

  /// Clear wait state for a goal (after timer completes)
  void clearWaitState(int goalId) {
    _waitReaders.remove(goalId);
  }

  /// Set wait state for a goal
  void setWaitReader(int goalId, int readerId) {
    _waitReaders[goalId] = readerId;
  }

  /// Get the wait reader for a goal (if any)
  int? getWaitReader(int goalId) => _waitReaders[goalId];

  // when_idle (GLP-Spec appendix-guards.tex at e3a8d52, the time guards):
  // "when_idle suspends while the machine has a Reduce or a Communicate to
  // make, and succeeds when it has none."  A goal suspends on it as on wait/1:
  // on the reader of a fresh variable, whose writer the scheduler binds when
  // the machine is idle (Scheduler.drainWithStatus), which re-tries the goal.
  // Keyed by goal, in the order the goals first suspended on it.
  final Map<int, ({int writer, int reader})> _idleWaits = {};

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
  int idleReader(int goalId) {
    final w = _idleWaits[goalId];
    if (w != null) return w.reader;
    final (writer, reader) = heap.allocateVariable();
    _idleWaits[goalId] = (writer: writer, reader: reader);
    return reader;
  }

  /// Goal [goalId] has passed when_idle: it no longer waits on it.
  void clearIdleWait(int goalId) => _idleWaits.remove(goalId);

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
      final reactivated = heap.bindWriterConst(w.writer, 0);
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

  /// Commit writer bindings using FCP-exact semantics
  /// sigmaHat: Map from varId to tentative value
  List<GoalRef> commitSigmaHat(Map<int, Object?> sigmaHat) {
    final acts = CommitOps.applySigmaHatFCP(
      heap: heap,
      sigmaHat: sigmaHat,
    );
    _enqueueAll(acts);
    return acts;
  }

  /// Legacy commit method (deprecated - for backward compatibility)
  /// TODO: Remove after runner.dart updated to use commitSigmaHat
  List<GoalRef> commitWriters(Iterable<int> writerIds) {
    throw UnimplementedError('Legacy commitWriters deprecated - use commitSigmaHat');
  }

  /// Legacy abandon method (deprecated)
  /// TODO: Remove after runner.dart updated
  List<GoalRef> abandonWriter(int writerId) {
    throw UnimplementedError('Legacy abandonWriter deprecated - FCP has no abandon');
  }

  /// Suspend goal using FCP-exact shared suspension records
  void suspendGoalFCP({
    required int goalId,
    required int kappa,
    required Set<int> readerVarIds,
  }) {
    // Track which readers have this goal suspended (spec section 8.4)
    final goalRef = GoalRef(goalId, kappa);
    for (final readerId in readerVarIds) {
      suspended.putIfAbsent(readerId, () => <GoalRef>{}).add(goalRef);
    }
    if (readerVarIds.isNotEmpty) {
      _suspendedOn.putIfAbsent(goalRef, () => <int>{}).addAll(readerVarIds);
    }

    SuspendOps.suspendGoalFCP(
      heap: heap,
      goalId: goalId,
      kappa: kappa,
      readerVarIds: readerVarIds,
    );
  }

  bool tailReduce(GoalId g) {
    final current = _budgets[g] ?? tailRecursionBudgetInit;
    final next = nextTailBudget(current);
    if (next == 0) {
      _budgets[g] = resetTailBudget();
      return true;
    } else {
      _budgets[g] = next;
      return false;
    }
  }

  int budgetOf(GoalId g) => _budgets[g] ?? tailRecursionBudgetInit;

  void setGoalEnv(GoalId g, CallEnv env) {
    _goalEnvs[g] = env;
  }

  CallEnv? getGoalEnv(GoalId g) => _goalEnvs[g];

  void setGoalProgram(GoalId g, Object? program) {
    _goalPrograms[g] = program;
  }

  Object? getGoalProgram(GoalId g) => _goalPrograms[g];

  /// Set module context for a goal (for distribute/transmit handlers)
  void setGoalModuleContext(GoalId g, Object? ctx) {
    _goalModuleContexts[g] = ctx;
  }

  /// Get module context for a goal
  Object? getGoalModuleContext(GoalId g) => _goalModuleContexts[g];

  /// Set the module VALUE a goal runs (its ModuleTerm) — read back by
  /// `self_module`, inherited by spawned children.
  void setGoalModule(GoalId g, Object? module) {
    _goalModules[g] = module;
  }

  /// Get the module VALUE a goal runs (its ModuleTerm), or null if unset.
  Object? getGoalModule(GoalId g) => _goalModules[g];

  void _enqueueAll(List<GoalRef> acts) {
    for (final a in acts) {
      enqueueReactivatedGoal(a);
    }
  }

  /// Enqueue a reactivated goal and clean up from suspended map
  /// Use this instead of gq.enqueue() when reactivating suspended goals
  void enqueueReactivatedGoal(GoalRef goal) {
    gq.enqueue(goal);
    // Clean up from suspended map - goal is now reactivated
    _removeFromSuspended(goal);
  }

  /// [goal] is taken from the queue to be tried, so it waits on nothing: it
  /// leaves [suspended], as a goal reactivated does (dGLP Reduce, S' = S \
  /// {(G, W) : G ∈ R}).  The goals a commit's bindings wake are put in the
  /// queue by the runner itself and not through [enqueueReactivatedGoal], and
  /// until 2026-10-02 such a goal stayed in [suspended] for the rest of the
  /// run: after 30 days of sGLP's social graph the map held 464,267 goals, 631
  /// of them suspended.
  void goalTaken(GoalRef goal) => _removeFromSuspended(goal);

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
