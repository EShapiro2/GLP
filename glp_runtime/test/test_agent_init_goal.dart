/// Test to debug agent_init goal setup - mimics Flutter app behavior
import 'dart:io';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/compiler/compiler.dart';

void main() async {
  print('=== Testing agent_init goal setup ===\n');

  // Load the program (just user program for now - like REPL does)
  final glpSource = File('../programs/multiagent/social_agent_v2.glp').readAsStringSync();

  // Compile user program
  final userCompiler = GlpCompiler();
  final userProgram = userCompiler.compile(glpSource);

  // Combine with empty stdlib (simplified test)
  final combinedProgram = userProgram;

  // Check entry point
  final entryPC = combinedProgram.labels['agent_init/3'];
  print('Entry PC for agent_init/3: $entryPC');
  if (entryPC == null) {
    print('ERROR: agent_init/3 not found!');
    print('Available labels: ${combinedProgram.labels.keys.take(20)}...');
    return;
  }

  // Create runtime (like Flutter app does)
  final rt = GlpRuntime();
  final heap = rt.heap;

  // Create external channel (like AgentIOContext does)
  final userChannel = createExternalChannel(heap, 'user');
  final netChannel = createExternalChannel(heap, 'net');

  print('\nUser channel: $userChannel');
  print('  inputWriterAddr: ${userChannel.inputWriterAddr}');
  print('  inputReaderAddr: ${userChannel.inputReaderAddr}');
  print('  outputWriterAddr: ${userChannel.outputWriterAddr}');
  print('  outputReaderAddr: ${userChannel.outputReaderAddr}');

  // Build channel terms (like buildChannelTerm does)
  final userChTerm = buildChannelTerm(userChannel);
  final netChTerm = buildChannelTerm(netChannel);

  print('\nUser channel term: $userChTerm');
  print('Net channel term: $netChTerm');

  // Allocate and bind arguments (like _startAgentGoal does)
  // Arg 0: agentId - _? mode (reader)
  final (arg0Writer, arg0Reader) = heap.allocateVariable();
  heap.bindVariable(arg0Writer, ConstTerm('alice'));
  print('\nArg 0: writer=$arg0Writer, reader=$arg0Reader, value=alice');
  print('  isWriterBound(arg0Writer): ${heap.isWriterBound(arg0Writer)}');
  print('  isReaderBound(arg0Reader): ${heap.isReaderBound(arg0Reader)}');
  print('  getReaderValue(arg0Reader): ${heap.getReaderValue(arg0Reader)}');

  // Arg 1: userChannelTerm - Channel? mode (reader)
  final (arg1Writer, arg1Reader) = heap.allocateVariable();
  heap.bindVariable(arg1Writer, userChTerm);
  print('\nArg 1: writer=$arg1Writer, reader=$arg1Reader, value=$userChTerm');
  print('  isWriterBound(arg1Writer): ${heap.isWriterBound(arg1Writer)}');
  print('  isReaderBound(arg1Reader): ${heap.isReaderBound(arg1Reader)}');
  print('  getReaderValue(arg1Reader): ${heap.getReaderValue(arg1Reader)}');

  // Arg 2: netChannelTerm - Channel? mode (reader)
  final (arg2Writer, arg2Reader) = heap.allocateVariable();
  heap.bindVariable(arg2Writer, netChTerm);
  print('\nArg 2: writer=$arg2Writer, reader=$arg2Reader, value=$netChTerm');
  print('  isWriterBound(arg2Writer): ${heap.isWriterBound(arg2Writer)}');
  print('  isReaderBound(arg2Reader): ${heap.isReaderBound(arg2Reader)}');
  print('  getReaderValue(arg2Reader): ${heap.getReaderValue(arg2Reader)}');

  // Set up arguments using READER addresses
  final argSlots = <int, Term>{
    0: VarRef(arg0Reader),
    1: VarRef(arg1Reader),
    2: VarRef(arg2Reader),
  };

  print('\nArgSlots:');
  for (final entry in argSlots.entries) {
    final term = entry.value;
    if (term is VarRef) {
      print('  ${entry.key}: VarRef(${term.addr}), isReader=${heap.isReader(term.addr)}');
    }
  }

  // Set up goal environment
  final env = CallEnv(args: argSlots);
  rt.setGoalEnv(100, env);
  rt.setGoalProgram(100, 'main');

  // Create scheduler
  final runner = BytecodeRunner(combinedProgram);
  final scheduler = Scheduler(rt: rt, runners: {'main': runner});

  // Enqueue goal
  rt.gq.enqueue(GoalRef(100, entryPC));

  print('\n=== Running goal ===');
  final result = scheduler.drainWithStatus(
    maxCycles: 100,
    debug: true,  // Enable trace output
    debugOutput: true,  // Enable detailed debug output
  );

  print('\n=== Result ===');
  print('Status: ${result.status}');
  print('Goals ran: ${result.goalsRan}');
  print('Suspended goals: ${result.suspendedGoals}');
  print('Blocking readers: ${result.blockingReaders}');
}
