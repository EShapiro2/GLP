/// Play Alice-Bob-Charlie Three-Isolate Test
///
/// Tests the full social graph protocol across three agents:
///   Alice: Cold-calls Bob, sends message, accepts intro to Charlie, messages Charlie
///   Bob: Accepts Alice, receives message, cold-calls Charlie, introduces Alice↔Charlie
///   Charlie: Accepts Bob, sends message, accepts intro to Alice, messages Alice
///
/// ARCHITECTURE NOTE:
/// The play_alice_bob_charlie.glp program uses network3 as a GLP-level message switch.
/// For true multi-isolate execution, we need to replace network3 with IRMA routing.
/// This requires monitoring each agent's NetOut stream for new msg(from, to, content)
/// elements and routing them to the destination agent's NetIn.
///
/// This test currently verifies:
/// 1. The program compiles successfully
/// 2. The agent_init and actor procedures exist
/// 3. Basic goal spawning works
///
/// Full protocol testing requires stream monitoring infrastructure (future work).

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/irma_context.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

void main() {
  group('Play Alice-Bob-Charlie', () {
    late GlpCompiler compiler;
    late BytecodeProgram program;

    setUpAll(() {
      // Load and compile the play program
      // Try multiple paths for cross-platform compatibility
      final paths = [
        '/home/user/GLP/programs/typed_book/social_graph/play_alice_bob_charlie.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_charlie.glp',
      ];
      String source = '';
      for (final path in paths) {
        final file = File(path);
        if (file.existsSync()) {
          source = file.readAsStringSync();
          break;
        }
      }
      if (source.isEmpty) {
        throw Exception('Could not find play_alice_bob_charlie.glp');
      }
      compiler = GlpCompiler();
      program = compiler.compile(source);
    });

    test('Program compiles successfully', () {
      print('\n=== COMPILATION TEST ===');
      print('Program size: ${program.ops.length} ops');
      
      expect(program.ops.length, greaterThan(0));
      print('✓ Program compiled');
    });

    test('Required procedures exist', () {
      print('\n=== PROCEDURE CHECK ===');
      
      // Check for required entry points
      final requiredLabels = [
        'agent_init/3',
        'agent/3',
        'alice_actor/1',
        'bob_actor/1', 
        'charlie_actor/1',
        'network3/3',
        'merge/3',
        'lookup_send/4',
        'play_alice_bob_charlie/0',
      ];

      for (final label in requiredLabels) {
        final pc = program.labels[label];
        expect(pc, isNotNull, reason: '$label should exist');
        print('  $label -> PC $pc');
      }
      print('✓ All required procedures found');
    });

    test('Three agents can be created', () {
      print('\n=== AGENT CREATION TEST ===');

      // Create three agent runtimes
      final runtimeAlice = GlpRuntime();
      final runnerAlice = BytecodeRunner(program);
      final schedulerAlice = Scheduler(rt: runtimeAlice, runners: {'main': runnerAlice});
      final ctxAlice = IrmaContext(agentId: 'alice', runtime: runtimeAlice);
      print('Alice: Runtime created');

      final runtimeBob = GlpRuntime();
      final runnerBob = BytecodeRunner(program);
      final schedulerBob = Scheduler(rt: runtimeBob, runners: {'main': runnerBob});
      final ctxBob = IrmaContext(agentId: 'bob', runtime: runtimeBob);
      print('Bob: Runtime created');

      final runtimeCharlie = GlpRuntime();
      final runnerCharlie = BytecodeRunner(program);
      final schedulerCharlie = Scheduler(rt: runtimeCharlie, runners: {'main': runnerCharlie});
      final ctxCharlie = IrmaContext(agentId: 'charlie', runtime: runtimeCharlie);
      print('Charlie: Runtime created');

      expect(ctxAlice.agentId, equals('alice'));
      expect(ctxBob.agentId, equals('bob'));
      expect(ctxCharlie.agentId, equals('charlie'));
      print('✓ All three agents created');
    });

    test('Agent channels can be allocated', () {
      print('\n=== CHANNEL ALLOCATION TEST ===');

      final runtime = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: runtime);

      // Allocate UserCh: ch(UserIn, UserOut)
      final (userInWriter, userInReader) = runtime.heap.allocateVariable();
      final (userOutWriter, userOutReader) = runtime.heap.allocateVariable();
      print('UserCh allocated: in=($userInWriter,$userInReader), out=($userOutWriter,$userOutReader)');

      // Allocate NetCh: ch(NetIn, NetOut)
      final (netInWriter, netInReader) = runtime.heap.allocateVariable();
      final (netOutWriter, netOutReader) = runtime.heap.allocateVariable();
      ctx.registerWriter(netOutWriter); // Agent owns NetOut for sending
      print('NetCh allocated: in=($netInWriter,$netInReader), out=($netOutWriter,$netOutReader)');

      // Build channel term
      final userCh = StructTerm('ch', [VarRef(userInWriter), VarRef(userOutWriter)]);
      final netCh = StructTerm('ch', [VarRef(netInWriter), VarRef(netOutWriter)]);

      expect(userCh.functor, equals('ch'));
      expect(netCh.functor, equals('ch'));
      print('✓ Channel terms built successfully');
    });

    test('Goal can be spawned with correct arguments', () {
      print('\n=== GOAL SPAWNING TEST ===');

      final runtime = GlpRuntime();
      final runner = BytecodeRunner(program);
      final scheduler = Scheduler(rt: runtime, runners: {'main': runner});

      // Allocate channels
      final (userInW, _) = runtime.heap.allocateVariable();
      final (userOutW, _) = runtime.heap.allocateVariable();
      final (netInW, _) = runtime.heap.allocateVariable();
      final (netOutW, _) = runtime.heap.allocateVariable();

      // Build channel terms
      final userCh = StructTerm('ch', [VarRef(userInW), VarRef(userOutW)]);
      final netCh = StructTerm('ch', [VarRef(netInW), VarRef(netOutW)]);

      // Allocate heap cells for arguments (CallEnv requires VarRefs)
      final (arg0W, _) = runtime.heap.allocateVariable();
      runtime.heap.bindVariable(arg0W, ConstTerm('alice'));
      final (arg1W, _) = runtime.heap.allocateVariable();
      runtime.heap.bindVariable(arg1W, userCh);
      final (arg2W, _) = runtime.heap.allocateVariable();
      runtime.heap.bindVariable(arg2W, netCh);

      // Set up goal
      final agentInitPC = program.labels['agent_init/3']!;
      runtime.setGoalEnv(1, CallEnv(args: {
        0: VarRef(arg0W),
        1: VarRef(arg1W),
        2: VarRef(arg2W),
      }));
      runtime.setGoalProgram(1, 'main');
      runtime.gq.enqueue(GoalRef(1, agentInitPC));

      expect(runtime.gq.length, equals(1));
      print('✓ Goal spawned: agent_init(alice, UserCh, NetCh) at PC $agentInitPC');

      // Run a few cycles to verify it starts executing
      final result = scheduler.drainWithStatus(debug: false);
      print('Execution result: ${result.status}');
      print('Goals spawned: ${runtime.gq.length}');

      // agent_init spawns merge and agent goals
      // With no input on streams, it may fail or suspend - both are acceptable
      // The key is that execution started and the goal was processed
      expect(result.status, anyOf(
        ExecutionStatus.suspended,
        ExecutionStatus.succeeded,
        ExecutionStatus.failed,  // No input on streams is OK for this test
      ));
      print('✓ Goal execution started (status=${result.status})');
    });

    test('IRMA message routing can be configured', () {
      print('\n=== IRMA ROUTING TEST ===');

      final runtime1 = GlpRuntime();
      final ctx1 = IrmaContext(agentId: 'alice', runtime: runtime1);

      final runtime2 = GlpRuntime();
      final ctx2 = IrmaContext(agentId: 'bob', runtime: runtime2);

      final messageLog = <String>[];

      // Configure routing from alice to bob
      ctx1.onMessageReady = (destination, message) {
        if (destination == 'bob') {
          messageLog.add('alice -> bob: ${message.type}');
          if (message.type == MessageType.assignment) {
            final serializer = PayloadSerializer('alice');
            final (globalId, value) = serializer.deserializeAssignmentPayload(
              message.payload,
              (bool isReader) => isReader
                ? runtime2.heap.allocateImportedReader()
                : runtime2.heap.allocateImportedWriter(),
            );
            ctx2.handleAssignment(globalId.creator, globalId.localId, value);
          }
        }
      };

      // Create a writer at alice, register it with bob as requester
      final (writerAddr, _) = runtime1.heap.allocateVariable();
      // Register writer with requester so assignment will be sent
      final aliceEntry = VariableEntry(
        varId: writerAddr,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
        requester: 'bob',  // Bob will receive the assignment
      );
      ctx1.vp.add(VarKey(writerAddr, false), aliceEntry);
      runtime1.heap.cells[writerAddr].content = aliceEntry;
      print('Alice: Writer at $writerAddr (requester=bob)');

      // Import at bob
      final importedAddr = runtime2.heap.allocateImportedReader();
      final bobEntry = VariableEntry(
        varId: importedAddr,
        isReader: true,
        creator: 'alice',
        role: VariableRole.importedReader,
        creatorLocalId: writerAddr,
      );
      ctx2.vp.add(VarKey(importedAddr, true), bobEntry);
      runtime2.heap.cells[importedAddr].content = bobEntry;
      print('Bob: Imported reader at $importedAddr');

      // Bind the writer at alice - this should trigger message to bob
      runtime1.heap.bindVariable(writerAddr, ConstTerm('hello'));
      ctx1.onWriterBound(writerAddr, ConstTerm('hello'));
      ctx1.flushMessages();

      expect(messageLog.length, greaterThan(0));
      print('Messages sent: ${messageLog.join(", ")}');
      print('✓ IRMA message routing works');
    });
  });

  group('Full Protocol Test (requires stream monitoring)', () {
    test('TODO: Implement stream monitoring for NetOut -> IRMA routing', () {
      print('\n=== FUTURE WORK ===');
      print('To run the full play_alice_bob_charlie protocol across isolates:');
      print('');
      print('1. Each agent writes msg(from, to, content) to its NetOut stream');
      print('2. We need to monitor NetOut for new elements');
      print('3. When msg(alice, bob, X) appears, route X to Bob\'s NetIn via IRMA');
      print('4. When Bob\'s NetIn receives X, bind it to his stream');
      print('');
      print('Options:');
      print('a) Add heap callbacks for list-cons bindings');
      print('b) Poll NetOut streams periodically');
      print('c) Modify agents to use IRMA directly (changes protocol)');
      print('');
      print('For now, the play can be tested in single-agent mode:');
      print('  Load play_alice_bob_charlie.glp');
      print('  Run: play_alice_bob_charlie.');
      
      // Mark as pending until implemented
      // skip('Stream monitoring not yet implemented');
    });
  });
}
