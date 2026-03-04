/// Diagnostic: test project-linked modules + mad_boot.glp
///
/// Simulates what main_cssg_mad_modules.dart does in each isolate:
///   1. Create GlpEngine
///   2. enableMadGLP
///   3. loadProject(cssg_modules)
///   4. loadSource(mad_boot.glp)
///   5. Check labels
///   6. Run parent_init/4 for Alice
///
/// Run: dart test/debug_modules_boot.dart
import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';

void main() async {
  final projectDir = '../programs/cssg_modules';
  final bootPath = '../programs/cssg_modules/mad_boot.glp';
  final stdlibDir = '../programs/stdlib';

  print('=== Debug: project-linked modules + mad_boot.glp ===\n');

  // 1. Create engine, enable mad
  final engine = GlpEngine(stdlibDir: stdlibDir)..strictTypes = false;
  engine.enableMadGLP(agentId: 'alice');
  print('Engine created, madGLP enabled for alice');

  // 2. Load project
  engine.loadProject(projectDir);
  print('Project loaded from $projectDir');

  // 3. Load boot source
  final bootSource = File(bootPath).readAsStringSync();
  engine.loadSource(bootSource, filename: 'source_0');
  print('Boot source loaded (mad_boot.glp)');

  // 4. Check combined program labels
  final program = engine.combinedProgram;
  print('\n--- Combined program: ${program.ops.length} ops, ${program.labels.length} labels ---');

  final keyLabels = [
    'parent_init/4', 'child_init/3',
    'agent/4', 'ui_mediator/5', 'merge/3', 'tee/3',
    'send_to_user_tagged/3', 'ui_actor/3',
    'extract_child_stream/2',
    'alice4/1', 'bob4/1', 'carol4/1', 'dave4/1',
    'send_to_net/1', 'global_send/3',
    'boot:merge/3', 'boot:tee/3', 'boot:send_to_user_tagged/3',
    'agent:agent/4', 'mediator:ui_mediator/5',
    'actors:alice4/1', 'actors:carol4/1',
  ];

  for (final key in keyLabels) {
    final pc = program.labels[key];
    print('  $key -> ${pc != null ? "PC=$pc" : "NOT FOUND"}');
  }

  // 5. Try to run parent_init/4 for Alice
  print('\n--- Running parent_init(alice, carol, 4, NetIn) ---');

  final runtime = engine.runtime;
  final ctx = engine.madContext!;

  // Wire output callback
  runtime.outputCallback = (text) {
    print('  OUTPUT: $text');
  };

  // Wire outbound messages (just log them)
  ctx.onMessageReady = (dest, msg) async {
    print('  MAD_SEND to $dest: ${msg.runtimeType}');
  };

  // Serializer entry for netIn
  final (netInWriter, netInReader) = runtime.heap.allocateVariable();
  ctx.wp.initializeSerializerEntry(netInWriter);

  // Net input stream
  final netInput = InputInjector(runtime.heap, 'net', netInWriter);

  // Build args: parent_init(alice, carol, 4, NetIn)
  final args = <int, rt.Term>{};
  var idx = 0;

  // Arg 0: alice
  final (a0w, a0r) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(a0w, rt.ConstTerm('alice'));
  args[idx++] = rt.VarRef(a0r);

  // Arg 1: carol (child name)
  final (a1w, a1r) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(a1w, rt.ConstTerm('carol'));
  args[idx++] = rt.VarRef(a1r);

  // Arg 2: 4 (play number)
  final (a2w, a2r) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(a2w, rt.ConstTerm(4));
  args[idx++] = rt.VarRef(a2r);

  // Arg 3: NetIn (reader end of netInput stream)
  final (a3w, a3r) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(a3w, rt.VarRef(netInReader));
  args[idx++] = rt.VarRef(a3r);

  // Lookup label
  final entryPC = program.labels['parent_init/4'];
  if (entryPC == null) {
    print('ERROR: parent_init/4 not found!');
    return;
  }
  print('parent_init/4 at PC=$entryPC');

  // Set up goal
  final env = CallEnv(args: args);
  runtime.setGoalEnv(1, env);
  runtime.setGoalProgram(1, 'main');
  runtime.gq.enqueue(GoalRef(1, entryPC));

  print('GQ length before run: ${runtime.gq.length}');

  // Create runner and scheduler
  final runner = BytecodeRunner(program);
  final scheduler = Scheduler(rt: runtime, runners: {'main': runner},
    traceSink: (line) {
      // Only print trace lines for certain interesting goals
      if (line.contains('parent_init') || line.contains('send_to_net') ||
          line.contains('agent') || line.contains('ERROR') ||
          line.contains('FAIL') || line.contains('merge') ||
          line.contains('alice4') || line.contains('ui_actor') ||
          line.contains('tee') || line.contains('mediator') ||
          line.contains('send_to_user')) {
        print('  TRACE: $line');
      }
    });
  scheduler.resetDisplayNumbering();

  // Run
  final result = scheduler.drainWithStatus(debug: true);
  print('\nExecution status: ${result.status}');
  print('Goals ran: ${result.goalsRan.length}');
  print('GQ after: ${runtime.gq.length}');

  // Flush mad messages
  final flushed = ctx.flushMessages();
  print('Messages flushed: $flushed');

  print('\n=== Done ===');
}
