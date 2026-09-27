// glp_runtime/lib/sglp/person.dart
//
// person/2, the entry point of the asking clause of a simulation program
// (svGLP, sections/sglp.tex, Definition "Simulation Program"):
//
//   the simulation program of (M, P, pi) is the sGLP program that runs at each
//   agent a in P the canonical compilation of M with, in the asking clause of
//   each volitional procedure of interactive type T, the goal construct(T, .)
//   replaced by p(., S) with the same first argument, p the person procedure
//   that pi(a) declares for T and S a seed derived from the population's seed,
//   a and the identifier of the asked goal.
//
// The compiled asking clause calls person(T, X) where the canonical
// compilation calls construct(T, X) (GLP, 2026-09-27: one entry point, in
// programs/system/, which declares it `procedure(X) person(Constant?, X)`).
// T is the constant whose name is the interactive type as its person
// declaration writes it --- 'Menu' for `Menu =::= p`, 'Menu?' for
// `Menu? =::= p` --- and X is the argument construct(T, X) is given.
//
// At the Ask, the reduction of the asked goal g at agent a by its asking
// clause, person(T, X):
//   1. finds a's person procedure p for T: a's kinds, one per dimension, drawn
//      from the run's seed (Population.kindsOf), and the one of them that
//      declares T;
//   2. spawns p(X, S) at a, S = Population.personSeed(a, the identifier of g);
//   3. registers X as an interactive variable of a, for the log.
// It is a body kernel and not a clause, so it runs inside the Ask: X is an
// interactive variable of a from the Ask on, and no goal can assign it before
// it is registered.
//
// person(T, X) fails, with a diagnostic, outside an sGLP run with a run
// declaration, at a goal at no agent, where a's stochastic person declares no
// person procedure for T, and where X is not an unbound variable.

import '../bytecode/runner.dart' show CallEnv;
import '../engine_v2/interp.dart' show ByteRunner;
import '../runtime/body_kernels.dart' show BodyKernelResult;
import '../runtime/machine_state.dart' show GoalRef;
import '../runtime/runtime.dart';
import '../runtime/terms.dart';
import 'draws.dart' as draws;
import 'simulation.dart' show PersonSpawn;

/// Register sGLP's kernel onto [rt]: person/2.  Called from the `GlpEngine`
/// constructor, beside the module kernels.
void registerSglpKernels(GlpRuntime rt) {
  rt.bodyKernels.register('person', 2, personKernel);
}

BodyKernelResult personKernel(GlpRuntime rt, List<Object?> args) {
  final asked = rt.currentGoalId;
  String call() => 'person(${_show(rt, args[0])}, ${_show(rt, args[1])})';
  BodyKernelResult refuse(String why) {
    print('ERROR: ${call()} failed: $why');
    return BodyKernelResult.fail;
  }

  final sim = rt.sim;
  final population = sim?.population;
  if (sim == null || population == null || asked == null) {
    return refuse('person/2 runs only in an sGLP run of a program with a run '
        'declaration');
  }

  final t = args[0] is Term ? rt.heap.dereference(args[0] as Term) : null;
  if (t is! ConstTerm || t.value is! String) {
    return refuse('its first argument is not the constant naming an '
        'interactive type');
  }
  final type = t.value as String;

  final agent = sim.agentOf(asked);
  if (agent == null) {
    return refuse('the asking goal is at no agent: an agent\'s goals descend '
        'from the initial goal the run places at it');
  }

  final person = population.personProcedure(agent, type);
  if (person == null) {
    return refuse('the stochastic person of agent $agent '
        '(${population.kindsOf(agent).join(', ')}) declares no person '
        'procedure for $type');
  }

  final x = args[1];
  if (x is! VarRef || rt.heap.derefAddr(x.addr) is! VarRef) {
    return refuse('its second argument is not an unbound variable, the '
        'interactive variable of the Ask');
  }

  // The person procedure, in the code of the asking goal's program: renamed
  // M:p in a directory program, bare in a single-module one.
  final program = rt.getGoalProgram(asked);
  final runner = rt.runners[program];
  if (runner is! ByteRunner) {
    return refuse('no code for the program of the asking goal');
  }
  final image = runner.image;
  final entry = image.entryOffsetOf('${person.module}:${person.procedure}/2') ??
      image.entryOffsetOf('${person.procedure}/2');
  if (entry == null) {
    return refuse('the person procedure ${person.procedure}/2 of the kind '
        '"${person.kind}" is not in the compiled program');
  }

  // p(X, S) at the agent: S from the run's seed, the agent and the asked
  // goal's identifier; the goal named as the body goal of the Ask it stands
  // in for.
  final askedLineage = sim.lineageOf(asked);
  final seed = population.personSeed(agent, askedLineage);
  // A goal's arguments are variables: the seed is passed as the reader of a
  // variable assigned it, as a posted goal's constant is.
  final (seedWriter, seedReader) = rt.heap.allocateVariable();
  rt.heap.bindWriterConst(seedWriter, seed);
  final goalId = rt.nextGoalId++;
  rt.setGoalEnv(goalId, CallEnv(args: {0: x, 1: VarRef(seedReader)}));
  rt.setGoalProgram(goalId, program);
  rt.setGoalModule(goalId, rt.getGoalModule(asked));
  if (rt.infrastructureGoalIds.contains(asked)) {
    rt.infrastructureGoalIds.add(goalId);
  }
  sim.setLineage(
      goalId, draws.childLineage(askedLineage, rt.currentSpawnOrdinal ?? 0));
  sim.placeAt(goalId, agent);

  // X is an interactive variable of the agent from the Ask on.
  sim.log?.register(agent, x);

  sim.onPerson?.call(PersonSpawn(agent, type, person.kind, person.procedure,
      seed, askedLineage, goalId));
  rt.gq.enqueue(GoalRef(goalId, entry));
  return BodyKernelResult.success;
}

String _show(GlpRuntime rt, Object? a) {
  if (a is! Term) return '$a';
  final d = rt.heap.dereference(a);
  if (d is ConstTerm) return '${d.value}';
  if (d is VarRef) return '_';
  return '$d';
}
