import 'runtime.dart';
import '../bytecode/runner.dart';

class Scheduler {
  final GlpRuntime rt;
  final BytecodeRunner runner;

  Scheduler({required this.rt, required this.runner});

  List<int> drain({int maxCycles = 1000}) {
    final ran = <int>[];
    var cycles = 0;
    while (rt.gq.length > 0 && cycles < maxCycles) {
      final act = rt.gq.dequeue();
      if (act == null) break;
      ran.add(act.id);
      final cx = RunnerContext(rt: rt, goalId: act.id, kappa: act.pc);
      runner.run(cx);
      cycles++;
    }
    return ran;
  }
}
