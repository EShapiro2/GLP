import '../bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';

class ModuleRegistry {
  final Map<String, Map<String, BytecodeProgram>> _mods = {};
  void register({required String module, required String predSig, required BytecodeProgram program}) {
    final m = _mods.putIfAbsent(module, () => <String, BytecodeProgram>{});
    m[predSig] = program;
  }
  BytecodeProgram? getProgram(String module, String predSig) {
    final m = _mods[module];
    return m == null ? null : m[predSig];
  }
}

class CallSpec {
  final String module;
  final String predSig;
  final CallEnv env;
  final int kappa;
  CallSpec({required this.module, required this.predSig, required this.env, required this.kappa});
}

class GoalDirectory {
  final Map<int, CallSpec> _byGoal = {};
  void add(int goalId, CallSpec spec) { _byGoal[goalId] = spec; }
  CallSpec? get(int goalId) => _byGoal[goalId];
  void remove(int goalId) { _byGoal.remove(goalId); }
}

class LoaderHost {
  final GlpRuntime rt;
  final ModuleRegistry registry;
  final GoalDirectory goals = GoalDirectory();

  LoaderHost({required this.rt, required this.registry});

  void activate({required int goalId, required String module, required String predSig, required CallEnv env, int kappa = 1}) {
    final prog = registry.getProgram(module, predSig);
    if (prog == null) {
      throw StateError('No program for $module::$predSig');
    }
    goals.add(goalId, CallSpec(module: module, predSig: predSig, env: env, kappa: kappa));
    rt.gq.enqueue(GoalRef(goalId, kappa));
  }

  List<String> drain({int maxCycles = 1000}) {
    final out = <String>[];
    var cycles = 0;
    while (rt.gq.length > 0 && cycles < maxCycles) {
      final act = rt.gq.dequeue();
      if (act == null) break;
      final spec = goals.get(act.id);
      if (spec == null) {
        out.add('WARN: no CallSpec for goal ${act.id}');
        continue;
      }
      final prog = registry.getProgram(spec.module, spec.predSig);
      if (prog == null) {
        out.add('WARN: no program for ${spec.module}::${spec.predSig}');
        continue;
      }
      final runner = BytecodeRunner(prog);
      final res = runner.runWithStatus(RunnerContext(
        rt: rt,
        goalId: act.id,
        kappa: act.pc,
        env: spec.env,
        onActivation: (a) => out.add('ACTIVATED: goal ${a.id}@${a.pc}'),
      ));
      switch (res) {
        case RunResult.suspended:
          out.add('SUSPENDED: goal ${act.id}@${act.pc}');
          break;
        case RunResult.terminated:
          out.add('TERMINATED: goal ${act.id}');
          goals.remove(act.id);
          break;
        case RunResult.yielded:
          out.add('YIELDED: goal ${act.id}');
          break;
        case RunResult.outOfReductions:
          out.add('OUT OF REDUCTIONS: goal ${act.id}');
          break;
      }
      cycles++;
    }
    return out;
  }
}
