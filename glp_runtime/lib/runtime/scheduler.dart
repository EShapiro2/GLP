import 'dart:async';
import 'runtime.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'terms.dart';

/// Result of scheduler execution
enum ExecutionStatus {
  succeeded,  // All goals completed successfully
  failed,     // A goal failed (no matching clause)
  suspended,  // Goals remain suspended (waiting on unbound readers)
}

/// Result from drain operation
class DrainResult {
  final List<int> goalsRan;
  final ExecutionStatus status;
  final List<String> suspendedGoals;
  final Set<int> blockingReaders;  // addresses of readers causing suspension (per spec 8.4)

  DrainResult(this.goalsRan, this.status, this.suspendedGoals, [this.blockingReaders = const {}]);
}

class Scheduler {
  final GlpRuntime rt;
  final Map<Object?, BytecodeRunner> runners;

  /// Optional trace sink. When set, trace output (reductions, suspensions,
  /// failures) goes through this callback instead of print().
  void Function(String)? traceSink;

  Scheduler({required this.rt, BytecodeRunner? runner, Map<Object?, BytecodeRunner>? runners, this.traceSink})
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

  String _formatTerm(Term term, {bool markReaders = true, Set<int>? path}) {
    path ??= <int>{};

    // Dereference in a loop to avoid recursive ? markers
    var current = term;

    // Follow VarRef chains with cycle detection
    while (current is VarRef) {
      final addr = current.addr;
      
      // Check for cycle
      if (path.contains(addr)) {
        return '<circular>';
      }

      // Try to dereference
      final derefResult = rt.heap.derefAddr(addr);
      
      if (derefResult is VarRef) {
        // Still unbound - stop here
        break;
      } else if (derefResult is Term) {
        // Bound to a value - follow it
        path.add(addr);
        current = derefResult;
      } else {
        // VariableEntry or other - stop
        break;
      }
    }

    // Format the dereferenced value
    if (current is ConstTerm) {
      if (current.value == 'nil') return '[]';
      if (current.value == null) return '<null>';
      return current.value.toString();
    } else if (current is VarRef) {
      final addr = current.addr;
      final name = _getVarDisplayName(addr);
      final isReader = rt.heap.isReader(addr);
      return (markReaders && isReader) ? '$name?' : name;
    } else if (current is StructTerm) {
      // Special formatting for list structures
      if (current.functor == '.' && current.args.length == 2) {
        final elements = <String>[];
        var listTerm = current;

        while (true) {
          if (listTerm is! StructTerm || listTerm.functor != '.') break;

          final head = listTerm.args[0];
          final tail = listTerm.args[1];

          // Format head element with cycle detection
          String headStr = _formatTerm(head, markReaders: markReaders, path: path);
          elements.add(headStr);

          // Process tail
          if (tail is ConstTerm && (tail.value == 'nil' || tail.value == null)) {
            break; // Proper list ending
          } else if (tail is StructTerm && tail.functor == '.') {
            listTerm = tail;
          } else if (tail is VarRef) {
            // Check for circular tail
            if (path.contains(tail.addr)) {
              return '[${elements.join(', ')} | <circular>]';
            }
            final tailStr = _formatTerm(tail, markReaders: markReaders, path: path);
            return '[${elements.join(', ')} | $tailStr]';
          } else {
            // Non-list tail
            final tailStr = _formatTerm(tail, markReaders: markReaders, path: path);
            return '[${elements.join(', ')} | $tailStr]';
          }
        }

        return '[${elements.join(', ')}]';
      }

      // Special formatting for conjunction
      if (current.functor == ',' && current.args.length == 2) {
        final left = _formatTerm(current.args[0], markReaders: markReaders, path: path);
        final right = _formatTerm(current.args[1], markReaders: markReaders, path: path);
        return '($left, $right)';
      }

      // General structure formatting with cycle detection
      final args = current.args.map((a) => _formatTerm(a, markReaders: markReaders, path: path)).join(', ');
      return '${current.functor}($args)';
    }
    return current.toString();
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

  DrainResult drainWithStatus({int maxCycles = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false}) {
    final ran = <int>[];
    final suspendedGoals = <int, String>{}; // Track suspended goals by ID
    var cycles = 0;
    var hasFailed = false;

    while (rt.gq.length > 0 && cycles < maxCycles) {
      final act = rt.gq.dequeue();
      if (act == null) break;
      ran.add(act.id);
      final env = rt.getGoalEnv(act.id);
      final program = rt.getGoalProgram(act.id);
      var runner = runners[program];
      // Fall back to rt.runners (e.g., runners registered by _activate kernel)
      runner ??= rt.runners[program];
      if (runner == null) {
        throw StateError('No runner found for program $program for goal ${act.id}');
      }
      // Find procedure name from PC for trace
      String procName = '?';
      for (final entry in runner.prog.labels.entries) {
        if (entry.value == act.pc) {
          procName = entry.key;
          break;
        }
      }
      final goalStr = _formatGoal(act.id, procName, env);

      // Check if this is a query wrapper goal (skip display)
      final isQueryWrapper = procName.startsWith('query__');

      // Track if reduction occurs
      var hadReduction = false;

      // Get module context if set for this goal
      final moduleContext = rt.getGoalModuleContext(act.id);

      // Create context with reduction callback for trace
      // NOTE: Always set onReduction callback to track hadReduction correctly!
      // Otherwise hadReduction stays false even when goal reduces, causing
      // false "failure" detection when debug=false. Bug found 2026-01-31.
      final cx = RunnerContext(
        rt: rt,
        goalId: act.id,
        kappa: act.pc,
        env: env,
        goalHead: goalStr,
        goalProcName: procName,
        showBindings: showBindings,
        debugOutput: debugOutput,
        moduleContext: moduleContext,
        termFormatter: (term, {bool markReaders = true}) => _formatTerm(term, markReaders: markReaders),
        onReduction: (goalId, head, body) {
          // Skip query wrapper goals
          if (head.contains('query__')) {
            hadReduction = true;
            suspendedGoals.remove(goalId);
            return;
          }
          // Print reduction when it occurs (at Commit) - only when debug=true
          if (debug) {
            // Strip /arity suffix from procedure names for standard GLP syntax
            final cleanHead = head.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
            final cleanBody = body.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
            // No goal ID prefix - clean output
            _trace('$cleanHead :- $cleanBody');
          }
          hadReduction = true;
          // Remove from suspended list if it reduced
          suspendedGoals.remove(goalId);
        },
      );
      final result = runner.runWithStatus(cx);

      // Track suspended goals (always, not just in debug mode)
      if (result == RunResult.suspended) {
        // Track this suspended goal
        suspendedGoals[act.id] = goalStr;
        // Show suspension if debug and no reduction
        if (debug && !hadReduction && !isQueryWrapper) {
          final cleanGoal = goalStr.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          _trace('$cleanGoal → suspended');
        }
      } else if (result == RunResult.terminated) {
        // Goal terminated - check if it reduced (success) or just terminated (failure)
        if (!hadReduction && !isQueryWrapper) {
          // Terminated without reduction = failed
          if (debug) {
            final cleanGoal = goalStr.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
            _trace('$cleanGoal → failed');
          }
          hasFailed = true;
          suspendedGoals.remove(act.id);
          break; // Stop on failure
        } else {
          // Goal terminated successfully (with reduction) - remove from suspended list
          suspendedGoals.remove(act.id);
        }
      }
      cycles++;
    }

    // Determine final status
    // Per spec §3.4: exclude infrastructure goals (serve goals from auto-activation)
    // from status determination — their suspension is normal steady state
    final userSuspendedGoals = Map<int, String>.from(suspendedGoals)
      ..removeWhere((goalId, _) => rt.infrastructureGoalIds.contains(goalId));

    final ExecutionStatus status;
    if (hasFailed) {
      status = ExecutionStatus.failed;
    } else if (userSuspendedGoals.isNotEmpty) {
      status = ExecutionStatus.suspended;
    } else {
      status = ExecutionStatus.succeeded;
    }

    final suspendedList = userSuspendedGoals.values.map((g) =>
      g.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(')
    ).toList();

    // Per spec section 8.4: collect blocking readers from runtime's suspended set
    // rt.suspended maps reader addr -> Set<GoalRef> of goals blocked on that reader
    final blockingReaders = status == ExecutionStatus.suspended
        ? rt.suspended.keys.toSet()
        : <int>{};

    return DrainResult(ran, status, suspendedList, blockingReaders);
  }

  /// Legacy drain for backward compatibility
  List<int> drain({int maxCycles = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false}) {
    return drainWithStatus(maxCycles: maxCycles, debug: debug, showBindings: showBindings, debugOutput: debugOutput).goalsRan;
  }

  /// Async drain that waits for pending timers to fire.
  Future<DrainResult> drainAsyncWithStatus({int maxCycles = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false}) async {
    final ran = <int>[];
    var totalCycles = 0;
    ExecutionStatus lastStatus = ExecutionStatus.succeeded;
    List<String> lastSuspended = [];
    Set<int> lastBlockingReaders = {};

    while (totalCycles < maxCycles) {
      // Run synchronous drain until queue is empty
      final result = drainWithStatus(
        maxCycles: maxCycles - totalCycles,
        debug: debug,
        showBindings: showBindings,
        debugOutput: debugOutput,
      );
      ran.addAll(result.goalsRan);
      totalCycles += result.goalsRan.length;
      lastStatus = result.status;
      lastSuspended = result.suspendedGoals;
      lastBlockingReaders = result.blockingReaders;

      // Stop on failure
      if (lastStatus == ExecutionStatus.failed) {
        break;
      }

      // If no pending timers, we're done
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

    return DrainResult(ran, lastStatus, lastSuspended, lastBlockingReaders);
  }

  /// Legacy async drain for backward compatibility
  Future<List<int>> drainAsync({int maxCycles = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false}) async {
    final result = await drainAsyncWithStatus(maxCycles: maxCycles, debug: debug, showBindings: showBindings, debugOutput: debugOutput);
    return result.goalsRan;
  }
}
