import 'runtime.dart';
import '../bytecode/runner.dart';
import 'terms.dart';

class Scheduler {
  final GlpRuntime rt;
  final Map<Object?, BytecodeRunner> runners;

  Scheduler({required this.rt, BytecodeRunner? runner, Map<Object?, BytecodeRunner>? runners})
      : runners = runners ?? (runner != null ? {null: runner} : {});

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
      final displayId = current.varId >= 1000 ? current.varId - 1000 : current.varId;
      return 'X$displayId';
    } else if (current is VarRef && current.isReader) {
      final displayId = current.varId >= 1000 ? current.varId - 1000 : current.varId;
      return markReaders ? 'X$displayId?' : 'X$displayId';
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
      final args = current.args.map((a) => _formatTerm(a, markReaders: markReaders)).join(',');
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

  List<int> drain({int maxCycles = 1000, bool debug = false, bool showBindings = true, bool debugOutput = false}) {
    final ran = <int>[];
    final suspendedGoals = <int, String>{}; // Track suspended goals by ID
    var cycles = 0;
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

      // DEBUG: Deep trace for goal arguments
      // if (debug && act.id >= 10002 && act.id <= 10002) {
      //   print('[DEEP TRACE Goal ${act.id}] Raw argument state:');
      //   for (int slot = 0; slot < 3; slot++) {
      //     final rVal = env?.readerBySlot[slot];
      //     final wVal = env?.writerBySlot[slot];
      //     print('  Slot $slot:');
      //     if (rVal != null) {
      //       print('    env.r($slot) = R$rVal');
      //       final wid = rt.heap.writerIdForReader(rVal);
      //       if (wid != null) {
      //         print('    writerIdForReader(R$rVal) = W$wid');
      //         final bound = rt.heap.isWriterBound(wid);
      //         print('    isWriterBound(W$wid) = $bound');
      //         if (bound) {
      //           final value = rt.heap.valueOfWriter(wid);
      //           print('    valueOfWriter(W$wid) = $value');
      //         }
      //       }
      //     }
      //     if (wVal != null) {
      //       print('    env.w($slot) = W$wVal');
      //       final bound = rt.heap.isWriterBound(wVal);
      //       print('    isWriterBound(W$wVal) = $bound');
      //       if (bound) {
      //         final value = rt.heap.valueOfWriter(wVal);
      //         print('    valueOfWriter(W$wVal) = $value');
      //       }
      //     }
      //     if (rVal == null && wVal == null) {
      //       print('    (empty slot)');
      //     }
      //   }
      // }

      // Track queue length before execution
      final queueBefore = rt.gq.length;

      // Track if reduction occurs
      var hadReduction = false;

      // Create context with reduction callback for trace
      final cx = RunnerContext(
        rt: rt,
        goalId: act.id,
        moduleName: act.moduleName,
        kappa: act.pc,
        env: env,
        goalHead: goalStr,
        showBindings: showBindings,
        debugOutput: debugOutput,
        onReduction: debug ? (goalId, head, body) {
          // Print reduction when it occurs (at Commit)
          // Strip /arity suffix from procedure names for standard GLP syntax
          final cleanHead = head.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          final cleanBody = body.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          print('$goalId: $cleanHead :- $cleanBody');
          hadReduction = true;
          // Remove from suspended list if it reduced
          suspendedGoals.remove(goalId);
        } : null,
      );
      final result = runner.runWithStatus(cx);

      // Track queue length after execution to detect spawned goals
      final queueAfter = rt.gq.length;
      final spawnedCount = queueAfter - queueBefore;

      // Show suspension/failure if no reduction occurred
      if (debug && !hadReduction) {
        if (result == RunResult.suspended) {
          // Strip /arity suffix for standard GLP syntax
          final cleanGoal = goalStr.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          print('${act.id}: $cleanGoal → suspended');
          // Track this suspended goal
          suspendedGoals[act.id] = goalStr;
        } else if (result == RunResult.terminated) {
          // Terminated without reduction = failed
          // Strip /arity suffix for standard GLP syntax
          final cleanGoal = goalStr.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(');
          print('${act.id}: $cleanGoal → failed');
          // Remove from suspended list if it was there
          suspendedGoals.remove(act.id);

          // Stop execution on goal failure
          if (debug && suspendedGoals.isNotEmpty) {
            // Show suspended goals (resolvent) before stopping
            final resolvent = suspendedGoals.values.map((g) =>
              g.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(')
            ).toList();
            print('Resolvent (suspended): ${resolvent.join(', ')}');
          }
          return ran;
        }
      } else if (result == RunResult.terminated) {
        // Goal terminated successfully (with or without reduction) - remove from suspended list
        suspendedGoals.remove(act.id);
      }
      cycles++;
    }

    // Show final resolvent (suspended goals that never resumed) at normal termination
    if (debug && suspendedGoals.isNotEmpty) {
      final resolvent = suspendedGoals.values.map((g) =>
        g.replaceAllMapped(RegExp(r'(\w+)/\d+\('), (m) => '${m.group(1)}(')
      ).toList();
      print('Resolvent (suspended): ${resolvent.join(', ')}');
    }

    return ran;
  }
}
