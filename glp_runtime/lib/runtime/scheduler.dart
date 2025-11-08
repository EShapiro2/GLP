import 'runtime.dart';
import '../bytecode/runner.dart';
import 'terms.dart';

class Scheduler {
  final GlpRuntime rt;
  final Map<Object?, BytecodeRunner> runners;

  Scheduler({required this.rt, BytecodeRunner? runner, Map<Object?, BytecodeRunner>? runners})
      : runners = runners ?? (runner != null ? {null: runner} : {});

  String _formatTerm(Term term) {
    if (term is ConstTerm) {
      if (term.value == null) return '[]';
      return term.value.toString();
    } else if (term is WriterTerm) {
      final wid = term.writerId;
      if (rt.heap.isWriterBound(wid)) {
        final value = rt.heap.valueOfWriter(wid);
        if (value != null) return 'W$wid=${_formatTerm(value)}';
      }
      return 'W$wid';
    } else if (term is ReaderTerm) {
      final rid = term.readerId;
      final wid = rt.heap.writerIdForReader(rid);
      if (wid != null && rt.heap.isWriterBound(wid)) {
        final value = rt.heap.valueOfWriter(wid);
        if (value != null) return 'R$rid=${_formatTerm(value)}';
      }
      return 'R$rid';
    } else if (term is StructTerm) {
      final args = term.args.map((a) => _formatTerm(a)).join(',');
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
        args.add(_formatTerm(WriterTerm(w)));
      } else if (r != null) {
        args.add(_formatTerm(ReaderTerm(r)));
      } else {
        break;
      }
    }

    if (args.isEmpty) return procName;
    return '$procName(${args.join(', ')})';
  }

  List<int> drain({int maxCycles = 1000, bool debug = false}) {
    final ran = <int>[];
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
      if (debug) {
        // Find procedure name from PC
        String procName = '?';
        for (final entry in runner.prog.labels.entries) {
          if (entry.value == act.pc) {
            procName = entry.key;
            break;
          }
        }
        final goalStr = _formatGoal(act.id, procName, env);
        print('${act.id}: $goalStr');
      }
      final cx = RunnerContext(rt: rt, goalId: act.id, kappa: act.pc, env: env);
      final result = runner.runWithStatus(cx);
      if (debug) {
        if (result == RunResult.suspended) {
          print('  → suspended');
        } else if (result == RunResult.terminated) {
          print('  → ok');
        } else {
          print('  → $result');
        }
      }
      cycles++;
    }
    return ran;
  }
}
