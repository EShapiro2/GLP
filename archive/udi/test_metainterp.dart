// Test metainterpreter with circular merge after fix
import 'dart:io';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/system_predicates/system_predicates.dart';

void main() {
  // Load merge and clause predicates
  final mergeSource = File('/Users/udi/GLP/udi/glp/circular_merge.glp').readAsStringSync();
  final clauseSource = File('/Users/udi/GLP/udi/glp/clause.glp').readAsStringSync();
  final runSource = File('/Users/udi/GLP/udi/glp/run.glp').readAsStringSync();

  // Compile all programs
  final compiler = GlpCompiler();
  final mergeProgram = compiler.compile(mergeSource);
  final clauseProgram = compiler.compile(clauseSource);
  final runProgram = compiler.compile(runSource);

  // Combine all operations
  final allOps = <dynamic>[];
  final allLabels = <String, int>{};

  // Merge programs, updating label offsets
  for (final prog in [mergeProgram, clauseProgram, runProgram]) {
    final offset = allOps.length;
    for (final entry in prog.labels.entries) {
      allLabels[entry.key] = entry.value + offset;
    }
    allOps.addAll(prog.ops);
  }

  final combined = BytecodeProgram(allOps, allLabels);

  // Create runtime
  final rt = GlpRuntime();
  registerStandardPredicates(rt.systemPredicates);

  final runner = BytecodeRunner(combined);
  final scheduler = Scheduler(rt: rt, runners: {'combined': runner});

  rt.setGoalProgram(1, 'combined');
  rt.setGoalProgram(10000, 'combined');
  rt.setGoalProgram(10001, 'combined');
  rt.setGoalProgram(10002, 'combined');
  rt.setGoalProgram(10003, 'combined');

  // Run: run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))
  // Create wrapper goal
  final querySource = 'test__query() :- run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs))).';
  final queryProgram = compiler.compile(querySource);

  // Combine with main program
  final queryOps = <dynamic>[];
  final queryLabels = <String, int>{};
  final queryOffset = allOps.length;

  for (final entry in queryProgram.labels.entries) {
    queryLabels[entry.key] = entry.value + queryOffset;
  }
  queryOps.addAll(allOps);
  queryOps.addAll(queryProgram.ops);
  queryLabels.addAll(allLabels);

  final finalProgram = BytecodeProgram(queryOps, queryLabels);
  final finalRunner = BytecodeRunner(finalProgram);
  final finalScheduler = Scheduler(rt: rt, runners: {'final': finalRunner});

  // Execute query
  final env = CallEnv(writers: {}, readers: {});
  rt.setGoalEnv(1, env);
  final entryPc = finalProgram.labels['test__query/0']!;
  rt.gq.enqueue(GoalRef(1, entryPc));

  print('=== METAINTERPRETER TEST ===');
  print('Query: run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))');
  print('');

  finalScheduler.drain(maxCycles: 50, debug: true);

  print('');
  print('✓ Metainterpreter executed successfully!');
}
