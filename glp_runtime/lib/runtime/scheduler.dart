import 'runtime.dart';
import '../bytecode/runner.dart';
import 'terms.dart';

class Scheduler {
  final GlpRuntime rt;
  final Map<Object?, BytecodeRunner> runners;

  Scheduler({required this.rt, BytecodeRunner? runner, Map<Object?, BytecodeRunner>? runners})
      : runners = runners ?? (runner != null ? {null: runner} : {});

  String _formatTerm(Term term, {bool markReaders = true}) {
    if (term is ConstTerm) {
      if (term.value == 'nil') return '[]';
      if (term.value == null) return '<null>';  // Distinguish from nil
      return term.value.toString();
    } else if (term is VarRef) {
      final varId = term.varId;
      if (rt.heap.isBound(varId)) {
        final value = rt.heap.getValue(varId);
        if (value != null) {
          final formatted = _formatTerm(value, markReaders: markReaders);
          // Mark readers with ? suffix
          return (term.isReader && markReaders) ? '$formatted?' : formatted;
        }
      }
      // Unbound variable
      return term.isReader
          ? (markReaders ? 'R$varId?' : 'R$varId')
          : 'W$varId';
    } else if (term is StructTerm) {
      final args = term.args.map((a) => _formatTerm(a, markReaders: markReaders)).join(',');
      return '${term.functor}($args)';
    }
    return term.toString();
  }

  String _formatGoal(int goalId, String procName, CallEnv? env) {
    if (env == null) return procName;

    // Extract arguments from environment
    final args = <String>[];
    for (int i = 0; i < 10; i++) {
      final w = env.writerBySlot[i];
      final r = env.readerBySlot[i];
      if (w != null) {
        args.add(_formatTerm(VarRef(w, isReader: false)));
      } else if (r != null) {
        args.add(_formatTerm(VarRef(r, isReader: true)));
      } else {
        break;
      }
    }

    if (args.isEmpty) return procName;
    return '$procName(${args.join(', ')})';
  }

  List<int> drain({int maxCycles = 1000, bool debug = false}) {
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

      // Track queue length before execution
      final queueBefore = rt.gq.length;

      // Track if reduction occurs
      var hadReduction = false;

      // Create context with reduction callback for trace
      final cx = RunnerContext(
        rt: rt,
        goalId: act.id,
        kappa: act.pc,
        env: env,
        goalHead: goalStr,
        onReduction: debug ? (goalId, head, body) {
          // Print reduction when it occurs (at Commit)
          print('$goalId: $head :- $body');
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
          print('${act.id}: $goalStr → suspended');
          // Track this suspended goal
          suspendedGoals[act.id] = goalStr;
        } else if (result == RunResult.terminated) {
          // Terminated without reduction = failed
          print('${act.id}: $goalStr → failed');
          // Remove from suspended list if it was there
          suspendedGoals.remove(act.id);
        }
      }
      cycles++;
    }

    // Show final resolvent (suspended goals that never resumed) if debug is on
    if (debug && suspendedGoals.isNotEmpty) {
      final resolvent = suspendedGoals.values.toList();
      print('Resolvent: ${resolvent.join(', ')}');
    }

    return ran;
  }
}
