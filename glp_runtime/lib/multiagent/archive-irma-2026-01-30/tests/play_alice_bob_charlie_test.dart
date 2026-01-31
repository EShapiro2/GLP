/// Full Play Alice-Bob-Charlie with Three Isolates
///
/// STATUS: WORK IN PROGRESS - Goals now execute but protocol doesn't complete
///
/// FIXED: Goals no longer fail immediately. The fix was to create proper
/// reader cells for goal arguments (allocateVariable + bindVariable) instead
/// of using storeTermOnHeap which creates ValueTag cells. GLP expects reader
/// references for input arguments (Channel? in procedure declarations).
///
/// REMAINING ISSUES:
/// 1. No messages are exchanged between agents (all "flushed 0 messages")
/// 2. Goals complete too quickly without running the full protocol
/// 3. Warnings: "got local VarRef at X, expected VariableEntry" suggest
///    IRMA doesn't recognize locally-created variables
///
/// ROOT CAUSE: The channel variables (userInWriter, userOutWriter, etc.)
/// are not registered with IRMA's variable table, so when the protocol
/// tries to write to UserOut or read from UserIn?, IRMA doesn't know
/// these are variables that need network routing.
///
/// NEXT STEPS:
/// 1. Register channel variables with IrmaContext (registerWriter, etc.)
/// 2. Verify the network output stream (NetOut) is properly monitored
/// 3. Add debug tracing to see where the protocol gets stuck
///
/// For now, the cold-call and friend-introduction isolate tests demonstrate
/// that IRMA routing works correctly at the lower level.
///
/// Runs the complete social graph protocol across three Dart isolates:
/// 1. Alice cold-calls Bob (Bob accepts)
/// 2. Alice sends "Hi Bob, this is Alice" to Bob
/// 3. Bob cold-calls Charlie (Charlie accepts)
/// 4. Charlie sends "Hi Bob, this is Charlie" to Bob
/// 5. Bob introduces Alice to Charlie (both accept)
/// 6. Alice sends "Hi Charlie, this is Alice" to Charlie
/// 7. Charlie responds "Hi Alice, this is Charlie" to Alice
///
/// Architecture:
/// - Each agent runs in its own isolate with independent GlpRuntime
/// - IRMA Network Transaction replaces network3 GLP procedure
/// - Main isolate coordinates message routing between agents
/// - Agents use registerNetworkOutput/handleNetworkMessage for cold-calls
/// - Friend channels use standard IRMA Communicate Transaction

import 'dart:async';
import 'dart:io';
import 'dart:isolate';
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

/// Message types for inter-isolate communication
sealed class IsolateMessage {}

class NetworkMsg extends IsolateMessage {
  final String from;
  final String to;
  final List<int> payload;
  final MessageType type;
  NetworkMsg(this.from, this.to, this.payload, this.type);

  @override
  String toString() => 'NetworkMsg($from->$to, $type)';
}

class Ready extends IsolateMessage {
  final String agentId;
  final SendPort sendPort;
  Ready(this.agentId, this.sendPort);
}

class Start extends IsolateMessage {}

class Tick extends IsolateMessage {}

class Status extends IsolateMessage {
  final String agentId;
  final String status; // 'running', 'suspended', 'completed'
  final int goalsRemaining;
  Status(this.agentId, this.status, this.goalsRemaining);
}

class Done extends IsolateMessage {
  final String agentId;
  final bool success;
  final String? message;
  Done(this.agentId, this.success, {this.message});
}

/// Configuration passed to agent isolate
class AgentConfig {
  final String agentId;
  final String programSource;
  final SendPort mainPort;

  AgentConfig(this.agentId, this.programSource, this.mainPort);
}

/// Agent isolate entry point
void agentIsolate(AgentConfig config) async {
  final agentId = config.agentId;
  final receivePort = ReceivePort();

  print('[$agentId] Starting isolate');

  // Compile program
  final compiler = GlpCompiler();
  final program = compiler.compile(config.programSource);
  print('[$agentId] Program compiled: ${program.ops.length} ops');

  // Create runtime
  final runtime = GlpRuntime();
  final runner = BytecodeRunner(program);
  final scheduler = Scheduler(rt: runtime, runners: {'main': runner});
  final ctx = IrmaContext(agentId: agentId, runtime: runtime);

  // Message routing to main isolate
  ctx.onMessageReady = (dest, msg) {
    print('[$agentId] Sending ${msg.type} to $dest');
    config.mainPort.send(NetworkMsg(agentId, dest, msg.payload, msg.type));
  };

  // Allocate channels
  // UserCh: ch(UserIn?, UserOut) - user/actor side
  final (userInWriter, userInReader) = runtime.heap.allocateVariable();
  final (userOutWriter, userOutReader) = runtime.heap.allocateVariable();

  // NetCh: ch(NetIn?, NetOut) - network side
  final (netInWriter, netInReader) = runtime.heap.allocateVariable();
  final (netOutWriter, netOutReader) = runtime.heap.allocateVariable();

  // Register IRMA network streams
  ctx.registerNetworkInput(netInWriter);
  ctx.registerNetworkOutput(netOutWriter);

  print('[$agentId] Channels allocated');
  print('[$agentId]   UserCh: in=($userInWriter,$userInReader), out=($userOutWriter,$userOutReader)');
  print('[$agentId]   NetCh: in=($netInWriter,$netInReader), out=($netOutWriter,$netOutReader)');

  // Build channel terms for agent_init
  // Channel structure: ch(In?, Out) where In? is reader, Out is writer
  final userCh = StructTerm('ch', [VarRef(userInReader), VarRef(userOutWriter)]);
  final netCh = StructTerm('ch', [VarRef(netInReader), VarRef(netOutWriter)]);

  // Build channel term for actor (reversed - actor writes to UserIn, reads from UserOut)
  final actorCh = StructTerm('ch', [VarRef(userOutReader), VarRef(userInWriter)]);

  // Create proper argument cells for agent_init
  // Each argument needs a writer/reader pair - we bind the writer and pass the reader
  // This mimics how GLP passes arguments to procedure calls
  final (idArgWriter, idArgReader) = runtime.heap.allocateVariable();
  final (userChArgWriter, userChArgReader) = runtime.heap.allocateVariable();
  final (netChArgWriter, netChArgReader) = runtime.heap.allocateVariable();

  // Bind the argument writers to their values
  runtime.heap.bindVariable(idArgWriter, ConstTerm(agentId));
  runtime.heap.bindVariable(userChArgWriter, userCh);
  runtime.heap.bindVariable(netChArgWriter, netCh);

  // Spawn agent_init goal with READER references (as GLP expects for Channel? args)
  final agentInitPC = program.labels['agent_init/3']!;
  runtime.setGoalEnv(1, CallEnv(args: {
    0: VarRef(idArgReader),
    1: VarRef(userChArgReader),
    2: VarRef(netChArgReader),
  }));
  runtime.setGoalProgram(1, 'main');
  runtime.gq.enqueue(GoalRef(1, agentInitPC));
  print('[$agentId] Spawned agent_init with readers: id=$idArgReader, userCh=$userChArgReader, netCh=$netChArgReader');

  // Spawn actor goal
  final actorLabel = '${agentId}_actor/1';
  final actorPC = program.labels[actorLabel];
  if (actorPC == null) {
    print('[$agentId] ERROR: Actor $actorLabel not found');
    config.mainPort.send(Done(agentId, false, message: 'Actor not found'));
    return;
  }

  // Create proper argument cell for actor
  final (actorChArgWriter, actorChArgReader) = runtime.heap.allocateVariable();
  runtime.heap.bindVariable(actorChArgWriter, actorCh);

  runtime.setGoalEnv(2, CallEnv(args: {0: VarRef(actorChArgReader)}));
  runtime.setGoalProgram(2, 'main');
  runtime.gq.enqueue(GoalRef(2, actorPC));
  print('[$agentId] Spawned $actorLabel with reader: actorCh=$actorChArgReader');

  // Signal ready
  config.mainPort.send(Ready(agentId, receivePort.sendPort));

  // Message handling loop
  await for (final msg in receivePort) {
    if (msg is Start || msg is Tick) {
      // Run scheduler with debug to see what's happening
      final result = scheduler.drainWithStatus(debug: true);

      // Process suspensions
      if (result.status == ExecutionStatus.suspended) {
        ctx.processSuspension(result.blockingReaders);
      }

      // Flush messages
      ctx.flushMessages();

      // Report status
      final status = runtime.gq.isEmpty ? 'completed' :
                     (result.status == ExecutionStatus.suspended ? 'suspended' : 'running');
      config.mainPort.send(Status(agentId, status, runtime.gq.length));

      if (runtime.gq.isEmpty) {
        print('[$agentId] All goals completed');
        config.mainPort.send(Done(agentId, true, message: 'completed'));
      }

    } else if (msg is NetworkMsg) {
      print('[$agentId] Received ${msg.type} from ${msg.from}');

      if (msg.type == MessageType.agentMessage) {
        ctx.handleNetworkMessage(msg.from, msg.payload);
      } else if (msg.type == MessageType.assignment) {
        final serializer = PayloadSerializer('');
        final (globalId, value) = serializer.deserializeAssignmentPayload(
          msg.payload,
          (isReader) => isReader
            ? runtime.heap.allocateImportedReader()
            : runtime.heap.allocateImportedWriter(),
          onVariableImported: (localAddr, isReader, globalId, pairedReaderCreatorLocalId) {
            ctx.attachImportedVariableEntry(localAddr, isReader, globalId, msg.from,
              pairedReaderCreatorLocalId: pairedReaderCreatorLocalId);
          },
        );
        print('[$agentId] Assignment: ${globalId.creator}:${globalId.localId} := $value');
        ctx.handleAssignment(globalId.creator, globalId.localId, value);
      } else if (msg.type == MessageType.readRequest) {
        final serializer = PayloadSerializer('');
        final varId = serializer.deserializeReadRequestPayload(msg.payload);
        print('[$agentId] Read request for $varId from ${msg.from}');
        ctx.handleReadRequest(varId, msg.from);
      }

      // Flush any response messages
      ctx.flushMessages();
    }
  }
}

void main() {
  group('Full Play Alice-Bob-Charlie with Isolates', () {
    late String programSource;

    setUpAll(() {
      final paths = [
        '/home/user/GLP/programs/typed_book/social_graph/play_alice_bob_charlie.glp',
        '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_charlie.glp',
      ];
      for (final path in paths) {
        final file = File(path);
        if (file.existsSync()) {
          programSource = file.readAsStringSync();
          return;
        }
      }
      throw Exception('Could not find play_alice_bob_charlie.glp');
    });

    test('Full protocol with IRMA routing', () async {
      print('\n=== FULL PLAY TEST ===\n');

      final mainReceivePort = ReceivePort();
      final completer = Completer<void>();

      final agents = <String, SendPort>{};
      final completed = <String>{};
      var tickCount = 0;
      const maxTicks = 100;
      Timer? tickTimer;

      mainReceivePort.listen((msg) {
        if (msg is Ready) {
          print('[MAIN] ${msg.agentId} ready');
          agents[msg.agentId] = msg.sendPort;

          if (agents.length == 3) {
            print('[MAIN] All agents ready, starting protocol');
            for (final port in agents.values) {
              port.send(Start());
            }

            // Set up tick timer to drive execution
            tickTimer = Timer.periodic(Duration(milliseconds: 50), (_) {
              tickCount++;
              if (tickCount > maxTicks) {
                print('[MAIN] Max ticks reached');
                tickTimer?.cancel();
                if (!completer.isCompleted) {
                  completer.completeError('Timeout after $maxTicks ticks');
                }
                return;
              }

              for (final port in agents.values) {
                port.send(Tick());
              }
            });
          }

        } else if (msg is NetworkMsg) {
          print('[MAIN] Routing: ${msg.from} -> ${msg.to}: ${msg.type}');
          final targetPort = agents[msg.to];
          if (targetPort != null) {
            targetPort.send(msg);
          } else {
            print('[MAIN] WARNING: Unknown destination ${msg.to}');
          }

        } else if (msg is Status) {
          // Print periodic status
          if (tickCount % 10 == 0) {
            print('[MAIN] ${msg.agentId}: ${msg.status}, goals=${msg.goalsRemaining}');
          }

        } else if (msg is Done) {
          print('[MAIN] ${msg.agentId} done: ${msg.message}');
          completed.add(msg.agentId);

          if (completed.length == 3) {
            print('[MAIN] All agents completed!');
            tickTimer?.cancel();
            if (!completer.isCompleted) {
              completer.complete();
            }
          }
        }
      });

      // Spawn agent isolates
      print('[MAIN] Spawning isolates...');

      await Isolate.spawn(
        agentIsolate,
        AgentConfig('alice', programSource, mainReceivePort.sendPort),
      );

      await Isolate.spawn(
        agentIsolate,
        AgentConfig('bob', programSource, mainReceivePort.sendPort),
      );

      await Isolate.spawn(
        agentIsolate,
        AgentConfig('charlie', programSource, mainReceivePort.sendPort),
      );

      // Wait for completion
      try {
        await completer.future.timeout(Duration(seconds: 30));
        print('\n=== TEST PASSED ===');
        print('All three agents completed the full protocol');
        print('Ticks used: $tickCount');
      } catch (e) {
        print('\n=== TEST FAILED ===');
        print('Error: $e');
        print('Completed agents: $completed');
        rethrow;
      } finally {
        tickTimer?.cancel();
        mainReceivePort.close();
      }

      expect(completed, containsAll(['alice', 'bob', 'charlie']));
    });
  });
}
