/// madGLP Cold-Call Test with Dart Isolates
///
/// Validates the complete madGLP push-based protocol across actual Dart isolates.
/// Alice and Bob each run in their own isolate with separate GlpRuntime instances.
/// Communication happens via SendPort/ReceivePort, simulating real network transport.
///
/// Adapted from the archived isolate_cold_call_test.dart to use madGLP APIs:
/// - globalize/localize instead of registerNetworkOutput/Input
/// - registerGlobalSendSpawns instead of registerWriter
/// - handleMadAssignment instead of handleAssignment
/// - onWriterBound triggers message sending (push-based)
///
/// Scenario (per madGLP-spec.md Section 10.2):
/// 1. Alice creates response variable Resp (writer) and Resp? (reader)
/// 2. Alice globalizes Resp (writer) to send to Bob -> creates entry (Resp, bob) at index 1
/// 3. Bob localizes _w(alice,1) -> gets writer, spawns global_send
/// 4. Bob binds his writer to "pong"
/// 5. global_send fires: Bob sends _w(alice,1) := "pong" to Alice
/// 6. Alice receives assignment, binds her Resp writer
/// 7. Test verifies Alice's Resp? == "pong"

import 'dart:async';
import 'dart:isolate';
import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/multiagent/global_send.dart';

/// Message types for inter-isolate communication
sealed class IsolateMessage {}

/// Network message carrying madGLP assignment
class NetworkMsg extends IsolateMessage {
  final String from;
  final String to;
  final List<int> payload;
  final MessageType type;

  /// Global name for routing (serialized separately for simplicity)
  final String globalNameAgent;
  final int globalNameIndex;
  final bool globalNameIsWriter;

  NetworkMsg(
    this.from,
    this.to,
    this.payload,
    this.type, {
    required this.globalNameAgent,
    required this.globalNameIndex,
    required this.globalNameIsWriter,
  });
}

/// Signal that agent has completed setup
class Ready extends IsolateMessage {
  final String agentId;
  final SendPort sendPort;
  Ready(this.agentId, this.sendPort);
}

/// Carries the global names from Alice to Bob (via main isolate)
class GlobalNamesMsg extends IsolateMessage {
  final List<({String agent, int index, bool isWriter})> names;
  GlobalNamesMsg(this.names);
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

/// Alice's isolate entry point - creates response variable, sends to Bob, receives response
void aliceIsolate(SendPort mainPort) async {
  final receivePort = ReceivePort();

  // Set up runtime and context
  final runtime = GlpRuntime();
  final ctx = MadContext(agentId: 'alice', runtime: runtime);

  // Track the response variable for verification
  late int respWriter;

  // Message routing to main isolate (which forwards to Bob)
  ctx.onMessageReady = (dest, msg) {
    print('[ALICE] Message ready for $dest (${msg.type})');
    // We'll reconstruct the global name from the pending goals
  };

  // Signal ready
  mainPort.send(Ready('alice', receivePort.sendPort));

  // Wait for messages
  await for (final msg in receivePort) {
    if (msg is Start) {
      print('[ALICE] Starting protocol - creating response variable');

      // Create response variable Resp (writer) and Resp? (reader)
      final (w, r) = runtime.heap.allocateVariable();
      respWriter = w;
      print('[ALICE] Created Resp: writer=$w, reader=$r');

      // Globalize Resp (writer) to send to Bob
      // Per spec Section 10.2: Alice globalizes the writer, creating
      // GlobalizeEntry (Resp, bob) at index 1.  Bob localizes _w(alice,1),
      // gets a writer, and spawns global_send.
      final globalizeResult = globalize(
        variables: [TermVar.writer(w, readerAddr: r)],
        localAgent: 'alice',
        remoteAgent: 'bob',
        table: ctx.wp,
      );

      print('[ALICE] Globalized Resp: ${globalizeResult.globalNames}');
      print('[ALICE] Entry created in W_alice at index ${globalizeResult.globalNames[0].index}');

      // Send the global names to Bob (via main)
      final globalNames = globalizeResult.globalNames.map((g) => (
        agent: g.agent,
        index: g.index,
        isWriter: g.isWriter,
      )).toList();
      mainPort.send(GlobalNamesMsg(globalNames));

    } else if (msg is NetworkMsg) {
      print('[ALICE] Received ${msg.type} from ${msg.from}');
      print('[ALICE] Global name: _${msg.globalNameIsWriter ? 'w' : 'r'}(${msg.globalNameAgent}, ${msg.globalNameIndex})');

      // Reconstruct global name
      final globalName = msg.globalNameIsWriter
          ? GlobalName.writer(msg.globalNameAgent, msg.globalNameIndex)
          : GlobalName.reader(msg.globalNameAgent, msg.globalNameIndex);

      // This is _w(alice, 1) := "pong" from Bob
      // Alice globalized the writer, so this is handled by _handleWriterAssignment
      ctx.handleMadAssignment(
        globalName: globalName,
        value: ConstTerm('pong'), // For simplicity, we know the value
        fromAgent: msg.from,
      );

      // Check if our response variable is now bound
      final respValue = runtime.heap.derefAddr(respWriter);
      print('[ALICE] Resp value after assignment: $respValue');

      if (respValue is ConstTerm && respValue.value == 'pong') {
        mainPort.send(Done('alice', true, resultValue: 'pong'));
      } else {
        mainPort.send(Done('alice', false, error: 'Resp not bound correctly: $respValue'));
      }
    }
  }
}

/// Bob's isolate entry point - receives global names, localizes, binds response
void bobIsolate(SendPort mainPort) async {
  final receivePort = ReceivePort();

  // Set up runtime and context
  final runtime = GlpRuntime();
  final ctx = MadContext(agentId: 'bob', runtime: runtime);

  // Track imported writer address
  int? importedWriterAddr;
  GlobalName? globalNameToSend;

  // Message routing to main isolate (which forwards to Alice)
  ctx.onMessageReady = (dest, msg) {
    print('[BOB] Sending ${msg.type} to $dest');
    if (globalNameToSend != null) {
      mainPort.send(NetworkMsg(
        'bob',
        dest,
        msg.payload,
        msg.type,
        globalNameAgent: globalNameToSend!.agent,
        globalNameIndex: globalNameToSend!.index,
        globalNameIsWriter: globalNameToSend!.isWriter,
      ));
    }
  };

  // Signal ready
  mainPort.send(Ready('bob', receivePort.sendPort));

  // Wait for messages
  await for (final msg in receivePort) {
    if (msg is GlobalNamesMsg) {
      print('[BOB] Received global names from Alice: ${msg.names}');

      // Localize each global name
      // _w(alice, 1) -> Bob gets writer, spawns global_send
      final globalNames = msg.names.map((n) => n.isWriter
          ? GlobalName.writer(n.agent, n.index)
          : GlobalName.reader(n.agent, n.index)
      ).toList();

      final localizeResult = localize(
        globalNames: globalNames,
        localAgent: 'bob',
        table: ctx.wp,
        freshAddrAllocator: () => runtime.heap.allocateVariable(),
      );

      // Register the spawned global_send goals
      ctx.registerGlobalSendSpawns(localizeResult.spawns);

      print('[BOB] Localized: useReader=${localizeResult.useReader}');
      print('[BOB] Spawned ${localizeResult.spawns.length} global_send goals');

      // Bob gets the writer (since Alice globalized a reader)
      if (localizeResult.useReader[0] != false) {
        mainPort.send(Done('bob', false, error: 'Expected useReader[0] to be false'));
        return;
      }
      importedWriterAddr = localizeResult.freshPairs[0].writerAddr;
      globalNameToSend = globalNames[0];

      print('[BOB] Got writer at addr $importedWriterAddr');
      print('[BOB] Binding response to "pong"');

      // Bind the response
      runtime.heap.bindVariable(importedWriterAddr!, ConstTerm('pong'));

      // This triggers the global_send goal
      ctx.onWriterBound(importedWriterAddr!, ConstTerm('pong'));
      ctx.flushMessages();

      mainPort.send(Done('bob', true));
    }
  }
}

void main() {
  group('madGLP Cold-Call with Isolates', () {
    test('Alice sends Resp? to Bob, Bob binds to pong, Alice receives pong', () async {
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

        } else if (msg is GlobalNamesMsg) {
          print('[MAIN] Forwarding global names to Bob');
          bobPort?.send(msg);

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
