/// Isolate-Based Cold-Call Test
///
/// Tests the Network Transaction across actual Dart isolates.
/// Alice and Bob each run in their own isolate with separate GlpRuntime instances.
/// Communication happens via SendPort/ReceivePort, simulating real network transport.
///
/// Scenario:
/// 1. Alice creates response variable Resp (writer) and Resp? (reader)
/// 2. Alice sends msg(bob, ping(Resp)) on her NetOut
/// 3. Message is serialized and sent via SendPort to Bob's isolate
/// 4. Bob receives on NetIn, extracts imported writer for Resp
/// 5. Bob binds Resp = pong
/// 6. Assignment message is serialized and sent via SendPort to Alice's isolate
/// 7. Alice receives assignment, binds her Resp? reader
/// 8. Test verifies Alice's Resp? == pong

import 'dart:async';
import 'dart:isolate';
import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/irma_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

/// Message types for inter-isolate communication
sealed class IsolateMessage {}

/// Network message from one agent to another
class NetworkMsg extends IsolateMessage {
  final String from;
  final String to;
  final List<int> payload;
  final MessageType type;
  NetworkMsg(this.from, this.to, this.payload, this.type);
}

/// Signal that agent has completed setup
class Ready extends IsolateMessage {
  final String agentId;
  final SendPort sendPort;
  Ready(this.agentId, this.sendPort);
}

/// Signal to start the protocol
class Start extends IsolateMessage {}

/// Signal that agent has completed
class Done extends IsolateMessage {
  final String agentId;
  final bool success;
  final String? resultValue;
  final String? error;
  Done(this.agentId, this.success, {this.resultValue, this.error});
}

/// Alice's isolate entry point
void aliceIsolate(SendPort mainPort) async {
  final receivePort = ReceivePort();

  // Set up runtime and context
  final runtime = GlpRuntime();
  final ctx = IrmaContext(agentId: 'alice', runtime: runtime);

  // Track the response variable for verification
  late int respWriter;
  late int respReader;

  // Message routing to main isolate (which forwards to Bob)
  ctx.onMessageReady = (dest, msg) {
    print('[ALICE] Sending ${msg.type} to $dest');
    mainPort.send(NetworkMsg('alice', dest, msg.payload, msg.type));
  };

  // Set up network streams
  final (netOutWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkOutput(netOutWriter);

  final (netInWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkInput(netInWriter);

  // Signal ready
  mainPort.send(Ready('alice', receivePort.sendPort));

  // Wait for messages
  await for (final msg in receivePort) {
    if (msg is Start) {
      print('[ALICE] Starting protocol');

      // Create response variable
      // Alice keeps READER (to receive response), exports WRITER (for Bob to bind)
      final (w, r) = runtime.heap.allocateVariable();
      respWriter = w;
      respReader = r;
      ctx.registerWriter(respWriter);

      // Build and send: msg(bob, ping(Resp))
      final pingTerm = StructTerm('ping', [VarRef(respWriter)]);
      final msgTerm = StructTerm('msg', [ConstTerm('bob'), pingTerm]);

      final (tailWriter, tailReader) = runtime.heap.allocateVariable();
      final consCell = StructTerm('.', [msgTerm, VarRef(tailReader)]);

      print('[ALICE] Sending: $msgTerm');
      runtime.heap.bindVariable(netOutWriter, consCell);
      ctx.flushMessages();

    } else if (msg is NetworkMsg) {
      print('[ALICE] Received ${msg.type} from ${msg.from}');

      if (msg.type == MessageType.assignment) {
        // Handle assignment from Bob
        final serializer = PayloadSerializer('');
        final (globalId, value) = serializer.deserializeAssignmentPayload(
          msg.payload,
          (isReader) => runtime.heap.allocateImportedReader(),
        );
        print('[ALICE] Assignment: ${globalId.creator}:${globalId.localId} := $value');
        ctx.handleAssignment(globalId.creator, globalId.localId, value);

        // Check if our response variable is now bound
        final respValue = runtime.heap.derefAddr(respWriter);
        print('[ALICE] Resp value after assignment: $respValue');

        if (respValue is ConstTerm && respValue.value == 'pong') {
          mainPort.send(Done('alice', true, resultValue: 'pong'));
        } else if (respValue is StructTerm) {
          mainPort.send(Done('alice', true, resultValue: respValue.toString()));
        } else {
          mainPort.send(Done('alice', false, error: 'Resp not bound correctly: $respValue'));
        }
      } else if (msg.type == MessageType.agentMessage) {
        // Handle network message (shouldn't happen in this test)
        ctx.handleNetworkMessage(msg.from, msg.payload);
        ctx.flushMessages();
      }
    }
  }
}

/// Bob's isolate entry point
void bobIsolate(SendPort mainPort) async {
  final receivePort = ReceivePort();

  // Set up runtime and context
  final runtime = GlpRuntime();
  final ctx = IrmaContext(agentId: 'bob', runtime: runtime);

  // Message routing to main isolate (which forwards to Alice)
  ctx.onMessageReady = (dest, msg) {
    print('[BOB] Sending ${msg.type} to $dest');
    mainPort.send(NetworkMsg('bob', dest, msg.payload, msg.type));
  };

  // Set up network streams
  final (netOutWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkOutput(netOutWriter);

  final (netInWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkInput(netInWriter);

  // Signal ready
  mainPort.send(Ready('bob', receivePort.sendPort));

  // Wait for messages
  await for (final msg in receivePort) {
    if (msg is NetworkMsg) {
      print('[BOB] Received ${msg.type} from ${msg.from}');

      if (msg.type == MessageType.agentMessage) {
        // Handle network message from Alice
        ctx.handleNetworkMessage(msg.from, msg.payload);

        // Check what we received on NetIn
        final netInValue = runtime.heap.derefAddr(netInWriter);
        print('[BOB] NetIn value: $netInValue');

        if (netInValue is StructTerm && netInValue.functor == '.') {
          final content = netInValue.args[0];
          print('[BOB] Received content: $content');

          if (content is StructTerm && content.functor == 'ping') {
            // Extract the response variable (imported writer)
            final respRef = content.args[0];
            if (respRef is VarRef) {
              final respAddr = respRef.addr;
              print('[BOB] Binding response at addr $respAddr to pong');

              // Bind the response
              runtime.heap.bindVariable(respAddr, ConstTerm('pong'));
              ctx.flushMessages();

              mainPort.send(Done('bob', true));
            } else {
              mainPort.send(Done('bob', false, error: 'Response not a VarRef: $respRef'));
            }
          } else {
            mainPort.send(Done('bob', false, error: 'Unexpected content: $content'));
          }
        } else {
          mainPort.send(Done('bob', false, error: 'NetIn not a cons cell: $netInValue'));
        }
      }
    }
  }
}

void main() {
  group('Isolate Cold-Call', () {
    test('Alice sends ping(Resp) to Bob, Bob responds with pong', () async {
      final mainReceivePort = ReceivePort();
      final completer = Completer<void>();

      SendPort? alicePort;
      SendPort? bobPort;
      Done? aliceResult;
      Done? bobResult;

      // Message router
      mainReceivePort.listen((msg) {
        if (msg is Ready) {
          print('[MAIN] ${msg.agentId} ready');
          if (msg.agentId == 'alice') {
            alicePort = msg.sendPort;
          } else if (msg.agentId == 'bob') {
            bobPort = msg.sendPort;
          }

          // When both ready, start protocol
          if (alicePort != null && bobPort != null) {
            print('[MAIN] Both agents ready, starting protocol');
            alicePort!.send(Start());
          }
        } else if (msg is NetworkMsg) {
          print('[MAIN] Routing ${msg.type} from ${msg.from} to ${msg.to}');
          if (msg.to == 'alice') {
            alicePort?.send(msg);
          } else if (msg.to == 'bob') {
            bobPort?.send(msg);
          }
        } else if (msg is Done) {
          print('[MAIN] ${msg.agentId} done: success=${msg.success}, result=${msg.resultValue}, error=${msg.error}');
          if (msg.agentId == 'alice') {
            aliceResult = msg;
          } else if (msg.agentId == 'bob') {
            bobResult = msg;
          }

          // Complete when both done
          if (aliceResult != null && bobResult != null && !completer.isCompleted) {
            completer.complete();
          }
        }
      });

      // Spawn isolates
      print('[MAIN] Spawning Alice isolate');
      await Isolate.spawn(aliceIsolate, mainReceivePort.sendPort);

      print('[MAIN] Spawning Bob isolate');
      await Isolate.spawn(bobIsolate, mainReceivePort.sendPort);

      // Wait for completion with timeout
      await completer.future.timeout(
        Duration(seconds: 10),
        onTimeout: () {
          fail('Test timed out waiting for agents to complete');
        },
      );

      // Verify results
      expect(bobResult?.success, isTrue, reason: 'Bob should complete successfully');
      expect(aliceResult?.success, isTrue, reason: 'Alice should complete successfully');
      expect(aliceResult?.resultValue, equals('pong'), reason: 'Alice should receive pong');

      mainReceivePort.close();
    });
  });
}
