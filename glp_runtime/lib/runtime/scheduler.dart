import 'dart:async';
import 'runtime.dart';
import '../bytecode/runner.dart';
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

  DrainResult(this.goalsRan, this.status, this.suspendedGoals);
}

class Scheduler {
  final GlpRuntime rt;
  final Map<Object?, BytecodeRunner> runners;

  Scheduler({required this.rt, BytecodeRunner? runner, Map<Object?, BytecodeRunner>? runners})
      : runners = runners ?? (runner != null ? {null: runner} : {});

  /// Query variable names: maps varId to original name from query (e.g., "X", "Xs")
  Map<int, String> _queryVarNames = {};

  /// Variable display map: maps actual varId to display number (1, 2, 3...)
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
  String _getVarDisplayName(int varId) {
    // Check if this is a query variable with an original name
    if (_queryVarNames.containsKey(varId)) {
      return _queryVarNames[varId]!;
    }
    // Otherwise assign a fresh display ID
    final displayId = _varDisplayMap.putIfAbsent(varId, () => _nextDisplayId++);
    return 'X$displayId';
  }

  /// Reset display numbering for a new query
  void resetDisplayNumbering() {
    _queryVarNames.clear();
    _varDisplayMap.clear();
    _nextDisplayId = 1;
  }

  String _formatTerm(Term term, {bool markReaders = true}) {
    // Dereference in a loop to avoid recursive ? markers
    var current = term;

    // Follow VarRef chains
    while (current is VarRef) {
      if (current.isReader) {
        final wid = rt.heap.writerIdForReader(current.varId);
        if (wid != null && rt.heap.isWriterBound(wid)) {
          final value = rt.heap.valueOfWriter(wid);
          if (value != null) {
            current = value;
          } else {
            break;
          }
        } else {
          break;
        }
      } else {
        // Writer VarRef
        if (rt.heap.isWriterBound(current.varId)) {
          final value = rt.heap.valueOfWriter(current.varId);
          if (value != null) {
            current = value;
          } else {
            break;
          }
        } else {
          break;
        }
      }
    }

    // Format the dereferenced value
    if (current is ConstTerm) {
      if (current.value == 'nil') return '[]';
      if (current.value == null) return '<null>';
      return current.value.toString();
    } else if (current is VarRef && !current.isReader) {
      final name = _getVarDisplayName(current.varId);
      return name;
    } else if (current is VarRef && current.isReader) {
      final name = _getVarDisplayName(current.varId);
      return markReaders ? '$name?' : name;
    } else if (current is StructTerm) {
      // Special formatting for list structures
      if (current.functor == '.' && current.args.length == 2) {
        final elements = <String>[];
        var listTerm = current;
        final visited = <int>{};

        while (true) {
          if (listTerm is! StructTerm || listTerm.functor != '.') break;

          final head = listTerm.args[0];
          final tail = listTerm.args[1];

          // Format head element
          String headStr = _formatTerm(head, markReaders: markReaders);

          // Check for circular reference in head (if VarRef)
          if (head is VarRef && visited.contains(head.varId)) {
            headStr = '<circular>';
          } else if (head is VarRef) {
            visited.add(head.varId);
          }

          elements.add(headStr);

          // Process tail
          if (tail is ConstTerm && (tail.value == 'nil' || tail.value == null)) {
            break; // Proper list ending
          } else if (tail is StructTerm && tail.functor == '.') {
            listTerm = tail;
          } else if (tail is VarRef) {
            // Unbound tail - improper list
            if (visited.contains(tail.varId)) {
              return '[${elements.join(', ')} | <circular>]';
            }
            visited.add(tail.varId);
            final tailStr = _formatTerm(tail, markReaders: markReaders);
            return '[${elements.join(', ')} | $tailStr]';
          } else {
            // Non-list tail
            final tailStr = _formatTerm(tail, markReaders: markReaders);
            return '[${elements.join(', ')} | $tailStr]';
          }
        }

        return '[${elements.join(', ')}]';
      }

      // General structure formatting
      final args = current.args.map((a) => _formatTerm(a, markReaders: markReaders)).join(', ');
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
      final runner = runners[program];
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

      // Create context with reduction callback for trace
      final cx = RunnerContext(
        rt: rt,
        goalId: act.id,
        kappa: act.pc,
        env: env,
        goalHead: goalStr,
        showBindings: showBindings,
        debugOutput: debugOutput,
        onReduction: debug ? (goalId, head, body) {
          // Skip query wrapper goals
          if (head.contains('query__')) {
            hadReduction = true;
            suspendedGoals.remove(goalId);
            return;
          }
          // Print reduction when it occurs (at Commit)
          // Strip /arity suffix from procedure names for standard GLP syntax
          final cleanHead = head.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          final cleanBody = body.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          // No goal ID prefix - clean output
          print('$cleanHead :- $cleanBody');
          hadReduction = true;
          // Remove from suspended list if it reduced
          suspendedGoals.remove(goalId);
        } : null,
      );
      final result = runner.runWithStatus(cx);

      // Show suspension/failure if no reduction occurred
      if (debug && !hadReduction && !isQueryWrapper) {
        if (result == RunResult.suspended) {
          // Strip /arity suffix for standard GLP syntax
          final cleanGoal = goalStr.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          print('$cleanGoal → suspended');
          // Track this suspended goal
          suspendedGoals[act.id] = goalStr;
        } else if (result == RunResult.terminated) {
          // Terminated without reduction = failed
          // Strip /arity suffix for standard GLP syntax
          final cleanGoal = goalStr.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          print('$cleanGoal → failed');
          hasFailed = true;
          // Remove from suspended list if it was there
          suspendedGoals.remove(act.id);
          break; // Stop on failure
        }
      } else if (result == RunResult.terminated) {
        // Goal terminated successfully (with or without reduction) - remove from suspended list
        suspendedGoals.remove(act.id);
      }
      cycles++;
    }

    // Determine final status
    final ExecutionStatus status;
    if (hasFailed) {
      status = ExecutionStatus.failed;
    } else if (suspendedGoals.isNotEmpty) {
      status = ExecutionStatus.suspended;
    } else {
      status = ExecutionStatus.succeeded;
    }

    final suspendedList = suspendedGoals.values.map((g) =>
      g.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(')
    ).toList();

    return DrainResult(ran, status, suspendedList);
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

    return DrainResult(ran, lastStatus, lastSuspended);
  }

  /// Legacy async drain for backward compatibility
  Future<List<int>> drainAsync({int maxCycles = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false}) async {
    final result = await drainAsyncWithStatus(maxCycles: maxCycles, debug: debug, showBindings: showBindings, debugOutput: debugOutput);
    return result.goalsRan;
  }
}
