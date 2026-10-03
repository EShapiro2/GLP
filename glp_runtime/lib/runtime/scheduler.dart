import 'dart:async';
import 'runtime.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'terms.dart';

/// Result of scheduler execution
enum ExecutionStatus {
  succeeded,  // All goals completed successfully
  failed,     // A goal failed (no matching clause)
  suspended,  // Goals remain suspended (waiting on unbound readers)
  capped,     // The cycle cap stopped the drain with goals still queued
}

/// Result from drain operation
class DrainResult {
  /// How many goals the drain took from the queue and tried, which is what
  /// its callers count against a cycle cap and report.  The drain keeps no
  /// record of which goals they were: until 2026-10-02 this was a list of
  /// their ids, kept for the whole of a REPL goal's run and copied by
  /// [Scheduler.drainAsyncWithStatus], eight bytes a try.  A caller that
  /// wants the ids hands the drain a list for them ([Scheduler.drain]).
  final int goalsRun;
  final ExecutionStatus status;
  final Set<int> blockingReaders;  // addresses of readers causing suspension (per spec 8.4)

  final List<String> Function() _suspendedGoals;

  /// The goals the drain left suspended, as text, made when it is first read:
  /// a drain whose caller does not read it formats none of them.
  late final List<String> suspendedGoals = _suspendedGoals();

  DrainResult(this.goalsRun, this.status, List<String> suspendedGoals, [this.blockingReaders = const {}])
      : _suspendedGoals = (() => suspendedGoals);

  /// A result whose suspended goals are formatted by [suspendedGoals] when the
  /// list is first read.
  DrainResult.deferred(this.goalsRun, this.status, List<String> Function() suspendedGoals, [this.blockingReaders = const {}])
      : _suspendedGoals = suspendedGoals;
}

class Scheduler {
  final GlpRuntime rt;
  final Map<Object?, GoalRunner> runners;

  /// Optional trace sink. When set, trace output (reductions, suspensions,
  /// failures) goes through this callback instead of print().
  void Function(String)? traceSink;

  Scheduler({required this.rt, GoalRunner? runner, Map<Object?, GoalRunner>? runners, this.traceSink})
      : runners = runners ?? (runner != null ? {null: runner} : {});

  /// Query variable names: maps writerAddr to original name from query (e.g., "X", "Xs")
  Map<int, String> _queryVarNames = {};

  /// Variable display map: maps actual addr to display number (1, 2, 3...)
  /// Only used for fresh variables created during execution
  Map<int, int> _varDisplayMap = {};
  int _nextDisplayId = 1;

  /// Set query variable names (call before draining)
  void setQueryVarNames(Map<String, int> varWriters) {
    _queryVarNames.clear();
    for (final entry in varWriters.entries) {
      _queryVarNames[entry.value] = entry.key;
    }
  }

  /// Get display name for a variable
  /// Uses original name if it's a query variable, otherwise X1, X2, etc.
  String _getVarDisplayName(int addr) {
    // For readers, try to find writer's name first
    if (rt.heap.isReader(addr)) {
      // Use tryWriterForReader for imported reader support (returns null instead of throwing)
      final writerAddr = rt.heap.tryWriterForReader(addr);
      if (writerAddr != null && _queryVarNames.containsKey(writerAddr)) {
        return _queryVarNames[writerAddr]!;
      }
    }
    // Check if this address has an original name
    if (_queryVarNames.containsKey(addr)) {
      return _queryVarNames[addr]!;
    }
    // Otherwise assign a fresh display ID
    final displayId = _varDisplayMap.putIfAbsent(addr, () => _nextDisplayId++);
    return 'X$displayId';
  }

  /// Reset display numbering for a new query
  void resetDisplayNumbering() {
    _queryVarNames.clear();
    _varDisplayMap.clear();
    _nextDisplayId = 1;
  }

  /// [term] as the trace, a failed goal and the suspended list show it: a
  /// binding's chain followed, an address met again on [path] shown
  /// `<circular>`; a list in brackets, its elements comma-separated and a tail
  /// that is no further cell after ` | `; a conjunction in parentheses; a
  /// variable by its display name, a reader marked `?` where [markReaders].
  ///
  /// The text is written with a stack of its own, piece by piece in the order
  /// the recursion it replaces wrote it, the same addresses added to [path]
  /// and the same display names given out, in the same order, so the text is
  /// the same.  Until 2026-10-02 that recursion took a Dart frame or more for
  /// each element of a list whose tail is a variable, as every tail of a list
  /// built on the heap is, and a failed goal holding a list of 50,000
  /// elements overflowed the Dart stack in its own text (long_list_walks_test).
  String _formatTerm(Term term, {bool markReaders = true, Set<int>? path}) {
    final seen = path ?? <int>{};
    final out = StringBuffer();
    // What remains to be written, the next on top: a term to format, a piece
    // of text, or the tail of a list cell whose head has just been written.
    final pending = <Object>[term];
    while (pending.isNotEmpty) {
      final next = pending.removeLast();
      if (next is String) {
        out.write(next);
        continue;
      }
      if (next is _ListTail) {
        // What follows a list element is decided by its cell's tail, after the
        // element, whose text may have added to [seen].
        final tail = next.tail;
        if (tail is ConstTerm && (tail.value == 'nil' || tail.value == null)) {
          out.write(']'); // Proper list ending
        } else if (tail is StructTerm && tail.functor == '.') {
          // The next cell: its element after a comma.
          final head = tail.args[0];
          final rest = tail.args[1];
          out.write(', ');
          pending
            ..add(_ListTail(rest))
            ..add(head);
        } else if (tail is VarRef && seen.contains(tail.addr)) {
          out.write(' | <circular>]'); // Circular tail
        } else {
          // A variable tail, or a tail that is no list
          out.write(' | ');
          pending
            ..add(']')
            ..add(tail);
        }
        continue;
      }

      // Dereference in a loop to avoid recursive ? markers
      var current = next as Term;
      var circular = false;

      // Follow VarRef chains with cycle detection
      while (current is VarRef) {
        final addr = current.addr;

        // Check for cycle
        if (seen.contains(addr)) {
          circular = true;
          break;
        }

        // Try to dereference
        final derefResult = rt.heap.derefAddr(addr);

        if (derefResult is VarRef) {
          // Still unbound - stop here
          break;
        } else if (derefResult is Term) {
          // Bound to a value - follow it
          seen.add(addr);
          current = derefResult;
        } else {
          // VariableEntry or other - stop
          break;
        }
      }
      if (circular) {
        out.write('<circular>');
        continue;
      }

      // Format the dereferenced value
      if (current is ConstTerm) {
        if (current.value == 'nil') {
          out.write('[]');
        } else if (current.value == null) {
          out.write('<null>');
        } else {
          out.write(current.value.toString());
        }
      } else if (current is VarRef) {
        final addr = current.addr;
        final name = _getVarDisplayName(addr);
        final isReader = rt.heap.isReader(addr);
        out.write((markReaders && isReader) ? '$name?' : name);
      } else if (current is StructTerm) {
        if (current.functor == '.' && current.args.length == 2) {
          // Special formatting for list structures: the first element, and
          // what follows it decided by the cell's tail ([_ListTail]).
          final head = current.args[0];
          final tail = current.args[1];
          out.write('[');
          pending
            ..add(_ListTail(tail))
            ..add(head);
        } else if (current.functor == ',' && current.args.length == 2) {
          // Special formatting for conjunction
          out.write('(');
          pending
            ..add(')')
            ..add(current.args[1])
            ..add(', ')
            ..add(current.args[0]);
        } else {
          // General structure formatting, the first argument on top
          out
            ..write(current.functor)
            ..write('(');
          pending.add(')');
          for (var i = current.args.length - 1; i >= 0; i--) {
            pending.add(current.args[i]);
            if (i > 0) pending.add(', ');
          }
        }
      } else {
        out.write(current.toString());
      }
    }
    return out.toString();
  }

  String _formatGoal(int goalId, String procName, CallEnv? env) {
    if (env == null) return procName;

    // Extract arguments from environment
    final args = <String>[];
    for (int i = 0; i < 10; i++) {
      final arg = env.arg(i);
      if (arg != null) {
        args.add(_formatTerm(arg));
      } else {
        break;
      }
    }

    if (args.isEmpty) return procName;
    return '$procName(${args.join(', ')})';
  }

  /// Format a binding for display: "X = value" or "X1 = value"
  String formatBinding(int varId, dynamic value) {
    final name = _getVarDisplayName(varId);
    String valueStr;
    if (value is Term) {
      valueStr = _formatTerm(value, markReaders: false);
    } else if (value is String) {
      valueStr = value;
    } else if (value == null || value == 'nil') {
      valueStr = '[]';
    } else {
      valueStr = value.toString();
    }
    // Clean up Const(...) wrapper if present
    if (valueStr.startsWith('Const(') && valueStr.endsWith(')')) {
      valueStr = valueStr.substring(6, valueStr.length - 1);
    }
    return '$name = $valueStr';
  }

  /// Send trace output to traceSink if set, otherwise to print().
  void _trace(String line) {
    if (traceSink != null) {
      traceSink!(line);
    } else {
      print(line);
    }
  }

  /// Reduce goals from the queue, at most [maxCycles] of them.  Their number
  /// is the result's [DrainResult.goalsRun]; [goalIds], when given, has the
  /// id of each appended as it is taken.
  DrainResult drainWithStatus({int maxCycles = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false, List<int>? goalIds}) {
    // Track suspended goals by ID, each with the means to its text.
    final suspendedGoals = <int, String Function()>{};
    var cycles = 0;
    // F at entry: a goal that failed in an earlier drain has already been
    // counted, so only what this drain adds bears on its status.
    final failedAtEntry = rt.failedGoals.length;

    while (cycles < maxCycles) {
      // when_idle (GLP-Spec appendix-guards.tex, e3a8d52; IGLP eadadcd,
      // Implementation Notes, "The when_idle Guard"): whenever the machine is
      // idle --- its queue empty and, in madGLP, its outbox too --- the goal
      // that has waited longest on when_idle is re-tried, one at a time; its
      // guard succeeds, the machine having no Reduce or Communicate to make,
      // and what it does may give the machine work before the next.  While
      // the outbox holds a message the drain ends with the goals waiting, and
      // the agent drains again after its Sends ([drainAndSend]).
      if (rt.gq.length == 0) {
        if (rt.isIdle && rt.wakeIdle()) continue;
        break;
      }
      final act = rt.gq.dequeue();
      if (act == null) break;
      // The goal taken waits on nothing: it leaves the runtime's suspended
      // map, whichever way it was woken (GlpRuntime.goalTaken).
      rt.goalTaken(act);
      goalIds?.add(act.id);
      final env = rt.getGoalEnv(act.id);
      final program = rt.getGoalProgram(act.id);
      var runner = runners[program];
      // Fall back to rt.runners (runtime-registered runners, if any)
      runner ??= rt.runners[program];
      if (runner == null) {
        throw StateError('No runner found for program $program for goal ${act.id}');
      }
      // Find procedure name from PC for trace
      final procName = runner.procNameForPc(act.pc) ?? '?';
      // The goal's text is made only where it is read: the trace, when the
      // drain is traced; F, when the goal fails; and the suspended list, when
      // a caller reads it.  Until 2026-10-02 every goal was formatted before
      // its reduction, traced or not, so a reduction cost time in the size of
      // its goal's terms (d43543ee, 2025-11-09).
      final goalStr = debug ? _formatGoal(act.id, procName, env) : null;

      // Check if this is a query wrapper goal (skip display)
      final isQueryWrapper = procName.startsWith('query__');

      // Create context, with the reduction callback for the trace when the
      // drain is traced.  Whether the goal reduced is the context's [reduced],
      // set at each reduction traced or not: until 2026-10-02 it was the
      // callback that recorded it, which is why the callback was always set
      // and every goal was formatted for it (the bug of 2026-01-31, a goal
      // that reduced read as failed when the callback was not set).
      final cx = RunnerContext(
        rt: rt,
        goalId: act.id,
        kappa: act.pc,
        env: env,
        goalHead: goalStr,
        goalProcName: procName,
        showBindings: showBindings,
        debugOutput: debugOutput,
        termFormatter: (term, {bool markReaders = true}) => _formatTerm(term, markReaders: markReaders),
        onReduction: debug
            ? (goalId, head, body) {
                // Skip query wrapper goals
                if (head.contains('query__')) return;
                // Print reduction when it occurs (at Commit)
                // Strip /arity suffix from procedure names for standard GLP syntax
                final cleanHead = head.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
                final cleanBody = body.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
                // No goal ID prefix - clean output
                _trace('$cleanHead :- $cleanBody');
              }
            : null,
      );
      final result = runner.runWithStatus(cx);

      // Track if reduction occurred; a goal that reduced leaves the suspended
      // list.
      final hadReduction = cx.reduced;
      if (hadReduction) suspendedGoals.remove(act.id);

      // Track suspended goals (always, not just in debug mode)
      if (result == RunResult.suspended) {
        // Track this suspended goal: its text is the traced one, or is made
        // when the suspended list is read.
        suspendedGoals[act.id] = goalStr != null
            ? () => goalStr
            : () => _formatGoal(act.id, procName, env);
        // Show suspension if debug and no reduction
        if (debug && !hadReduction && !isQueryWrapper) {
          final cleanGoal = goalStr!.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          _trace('$cleanGoal → suspended');
        }
      } else if (result == RunResult.terminated) {
        // Goal terminated - check if it reduced (success) or just terminated (failure)
        if (!hadReduction && !isQueryWrapper) {
          // Terminated without reduction = failed.
          if (debug) {
            final cleanGoal = goalStr!.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
            _trace('$cleanGoal → failed');
          }
          // Fail, per the dGLP and madGLP Reduce transactions: Q' = Q_r,
          // S' = S, F' = F ∪ {A}. The queue has already advanced — the goal was
          // dequeued — and the run continues. A failed goal makes the
          // configuration terminal only when no class is enabled in it, which
          // one failure among many active goals is not. The drain therefore
          // keeps reducing; F alone decides the run's status, below.  A goal
          // that failed made no binding, so its text now is its text before
          // the run.
          rt.failedGoals.add(goalStr ?? _formatGoal(act.id, procName, env));
          suspendedGoals.remove(act.id);
        } else {
          // Goal terminated successfully (with reduction) - remove from suspended list
          suspendedGoals.remove(act.id);
        }
      }
      cycles++;
    }

    // Determine final status.
    //
    // A goal that joined F during this drain makes the run's status failed,
    // without having stopped the drain: the agent kept reducing the rest of its
    // queue, which is what the Reduce transactions require. The outcome of a run
    // is (G_0 :- G_n) under the accumulated substitution and G_n includes the
    // failed goals, so a run with a non-empty F is a failed run.
    final hasFailed = rt.failedGoals.length > failedAtEntry;

    // A drain that exits with goals still queued stopped at its cap, and that
    // is its own outcome: what is left is runnable, not suspended, and the run
    // is not quiescent. Reading it as `suspended` called a half-run a settled
    // one, which is how a boot of 1010 goals reported itself finished at 1000.
    // It outranks the other outcomes here because it says the drain did not
    // end: F is recovered cumulatively by whoever drains again
    // ([drainToQuiescence], [drainAsyncWithStatus]), so a failure is not lost.
    // A goal waiting on when_idle at the cap, the machine idle, would be
    // re-tried next: the drain stopped at its cap too.  One waiting while the
    // outbox holds a message waits for the agent's Sends, not for this drain.
    final ExecutionStatus status;
    if (rt.gq.length > 0 || (rt.hasIdleWaits && rt.isIdle)) {
      status = ExecutionStatus.capped;
    } else if (hasFailed) {
      status = ExecutionStatus.failed;
    } else if (suspendedGoals.isNotEmpty) {
      status = ExecutionStatus.suspended;
    } else {
      status = ExecutionStatus.succeeded;
    }

    List<String> suspendedList() => suspendedGoals.values.map((g) =>
      g().replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(')
    ).toList();

    // Per spec section 8.4: collect blocking readers from runtime's suspended set
    // rt.suspended maps reader addr -> Set<GoalRef> of goals blocked on that reader
    final blockingReaders = status == ExecutionStatus.suspended
        ? rt.suspended.keys.toSet()
        : <int>{};

    return DrainResult.deferred(cycles, status, suspendedList, blockingReaders);
  }

  /// Reduce until quiescent: the queue empty and nothing runnable.
  ///
  /// One [drainWithStatus] need not reach quiescence — it stops at its own
  /// cycle cap with goals still queued, which is [ExecutionStatus.capped] — so
  /// this repeats it until it does. [chunk] is how many cycles one such drain
  /// takes and is invisible in the result.
  ///
  /// [maxCycles] is a safety net against a program that never quiesces, not a
  /// budget a run may silently exceed: a run that reaches it returns `capped`
  /// with its queue non-empty, and its caller reports that. F is cumulative
  /// across the drains this call makes, so a goal that failed in any of them
  /// fails the whole, whatever the last drain returned.
  DrainResult drainToQuiescence({int maxCycles = 1000000, int chunk = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false}) {
    var run = 0;
    final failedAtEntry = rt.failedGoals.length;
    var last = DrainResult(0, ExecutionStatus.succeeded, const []);

    while (run < maxCycles) {
      final left = maxCycles - run;
      final result = drainWithStatus(
        maxCycles: chunk < left ? chunk : left,
        debug: debug,
        showBindings: showBindings,
        debugOutput: debugOutput,
      );
      run += result.goalsRun;
      last = result;
      if (result.status != ExecutionStatus.capped) break;
      // A capped drain that ran nothing cannot be resumed — the queue reports
      // work it will not hand over — and looping on it would never end.
      if (result.goalsRun == 0) break;
    }

    final status = rt.gq.length > 0 || (rt.hasIdleWaits && rt.isIdle)
        ? ExecutionStatus.capped
        : (rt.failedGoals.length > failedAtEntry
            ? ExecutionStatus.failed
            : last.status);
    final lastResult = last;
    return DrainResult.deferred(run, status, () => lastResult.suspendedGoals,
        lastResult.blockingReaders);
  }

  /// One event's cycle at a madGLP agent (IGLP, Implementation Notes,
  /// "Event-driven execution"): reduce until quiescent, then perform the
  /// Sends, which [send] makes.
  ///
  /// A goal waiting on when_idle is not re-tried while the outbox holds a
  /// message to send --- "a queued outbound message is a Communicate still to
  /// make" (IGLP eadadcd, Implementation Notes, "The when_idle Guard") --- so
  /// the Sends may leave the machine idle with goals waiting.  The cycle then
  /// reduces again, which re-tries them one at a time, and sends again, until
  /// no goal waits on an idle machine.  A drain followed by a single flush, as
  /// the agents ran an event until 2026-10-02, left such a goal waiting for
  /// the agent's next incoming message.
  ///
  /// [maxCycles] bounds the goals the whole cycle runs, as it bounds
  /// [drainToQuiescence]'s: a program that goes on idling and sending never
  /// quiesces, and the cycle then returns `capped`.  F is cumulative across
  /// the drains, so a goal that failed in any of them fails the whole.
  DrainResult drainAndSend(void Function() send,
      {int maxCycles = 1000000, bool debug = false}) {
    var run = 0;
    final failedAtEntry = rt.failedGoals.length;
    DrainResult last;
    while (true) {
      last = drainToQuiescence(maxCycles: maxCycles - run, debug: debug);
      run += last.goalsRun;
      send();
      if (last.status == ExecutionStatus.capped) break;
      if (!(rt.hasIdleWaits && rt.isIdle)) break;
      if (run >= maxCycles) break;
    }

    final status = rt.gq.length > 0 || (rt.hasIdleWaits && rt.isIdle)
        ? ExecutionStatus.capped
        : (rt.failedGoals.length > failedAtEntry
            ? ExecutionStatus.failed
            : last.status);
    final lastResult = last;
    return DrainResult.deferred(run, status, () => lastResult.suspendedGoals,
        lastResult.blockingReaders);
  }

  /// Async drain that waits for pending timers to fire.  [goalIds], when
  /// given, has the id of each goal run appended, as [drainWithStatus]'s.
  ///
  /// With [send] the drain is a madGLP agent's (IGLP, Implementation Notes,
  /// "Event-driven execution": the agent "reduces until quiescent ... and
  /// then performs its Sends"): each drain is followed by the Sends, which
  /// [send] makes, and the drain is made again while a goal waits on
  /// when_idle on the machine the Sends leave idle, as [drainAndSend]'s is
  /// for the agents.  The REPL's goal in madGLP mode is drained so
  /// (GlpEngine.runGoal); until 2026-10-02 it made no Sends, and a goal there
  /// waiting on when_idle waited for as long as a message sat in the outbox.
  Future<DrainResult> drainAsyncWithStatus({int maxCycles = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false, List<int>? goalIds, void Function()? send}) async {
    final failedAtEntry = rt.failedGoals.length;
    var totalCycles = 0;
    ExecutionStatus lastStatus = ExecutionStatus.succeeded;
    List<String> Function() lastSuspended = () => [];
    Set<int> lastBlockingReaders = {};

    while (totalCycles < maxCycles) {
      // Run synchronous drain until queue is empty
      final result = drainWithStatus(
        maxCycles: maxCycles - totalCycles,
        debug: debug,
        showBindings: showBindings,
        debugOutput: debugOutput,
        goalIds: goalIds,
      );
      totalCycles += result.goalsRun;
      lastStatus = result.status;
      lastSuspended = () => result.suspendedGoals;
      lastBlockingReaders = result.blockingReaders;

      // The Sends, and the drain again while they leave the machine idle with
      // a goal waiting on when_idle, which it re-tries; at the cap the run
      // stopped with that goal still to re-try ([drainAndSend]).
      if (send != null) {
        send();
        if (rt.hasIdleWaits && rt.isIdle) {
          if (totalCycles < maxCycles) continue;
          lastStatus = ExecutionStatus.capped;
        }
      }

      // A failed drain does not end the computation either: a pending timer is
      // a class that becomes enabled, so the configuration is not terminal and
      // the run goes on. The status is recovered from F below rather than from
      // the last drain, which would otherwise erase an earlier failure.
      if (rt.pendingTimers <= 0) {
        break;
      }

      // Wait a small amount for timers to fire
      if (debugOutput) {
        print('[DEBUG] Waiting for ${rt.pendingTimers} pending timer(s)...');
      }

      // Poll with a small delay until queue has work or no more timers
      while (rt.gq.length == 0 && rt.pendingTimers > 0 && totalCycles < maxCycles) {
        await Future.delayed(Duration(milliseconds: 10));
      }
    }

    // F is cumulative across the drains this call made, so a failure in any of
    // them is a failure of the whole, whatever the last drain returned.
    final status = rt.failedGoals.length > failedAtEntry
        ? ExecutionStatus.failed
        : lastStatus;
    return DrainResult.deferred(totalCycles, status, lastSuspended, lastBlockingReaders);
  }
}

/// The tail of a list cell whose element [Scheduler._formatTerm] has just
/// written: what follows the element is decided by it once the element is
/// written.
class _ListTail {
  final Term tail;
  const _ListTail(this.tail);
}
