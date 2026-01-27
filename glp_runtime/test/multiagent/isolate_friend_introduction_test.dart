/// Isolate-Based Friend-Mediated Introduction Test
///
/// Tests the friend-mediated introduction protocol across three Dart isolates.
/// Bob introduces Alice to Charlie by creating a shared channel.
///
/// Protocol (per irmaGLP spec Section 6.2):
/// 1. Bob creates channel variables: CA (Alice→Charlie writer), CA? (reader)
///                                   AC (Charlie→Alice writer), AC? (reader)
/// 2. Bob sends ch(AC?, CA) to Alice via cold-call (Alice gets reader from Charlie, writer to Charlie)
/// 3. Bob sends ch(CA?, AC) to Charlie via cold-call (Charlie gets reader from Alice, writer to Alice)
/// 4. Alice binds CA = "hello_charlie" (sends to Charlie)
/// 5. Bob routes: receives assignment for CA?, forwards to Charlie
/// 6. Charlie receives "hello_charlie" on CA?
/// 7. Charlie binds AC = "hello_alice" (sends to Alice)
/// 8. Bob routes: receives assignment for AC?, forwards to Alice
/// 9. Alice receives "hello_alice" on AC?
///
/// Key insight: Bob is creator of all channel variables and serves as routing hub.

import 'dart:async';
import 'dart:isolate';
import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
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
}

class Ready extends IsolateMessage {
  final String agentId;
  final SendPort sendPort;
  Ready(this.agentId, this.sendPort);
}

class Start extends IsolateMessage {}

class Done extends IsolateMessage {
  final String agentId;
  final bool success;
  final String? message;
  final String? error;
  Done(this.agentId, this.success, {this.message, this.error});
}

/// Alice's isolate: receives introduction from Bob, sends message to Charlie, receives reply
void aliceIsolate(SendPort mainPort) async {
  final receivePort = ReceivePort();
  final runtime = GlpRuntime();
  final ctx = IrmaContext(agentId: 'alice', runtime: runtime);

  // Channel endpoints Alice will receive from Bob
  int? acReaderAddr;  // Reader to receive from Charlie (AC?)
  int? caWriterAddr;  // Writer to send to Charlie (CA)

  // Track received message from Charlie
  String? receivedFromCharlie;

  ctx.onMessageReady = (dest, msg) {
    print('[ALICE] Sending ${msg.type} to $dest');
    mainPort.send(NetworkMsg('alice', dest, msg.payload, msg.type));
  };

  // Set up network streams
  final (netOutWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkOutput(netOutWriter);
  final (netInWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkInput(netInWriter);

  mainPort.send(Ready('alice', receivePort.sendPort));

  await for (final msg in receivePort) {
    if (msg is Start) {
      print('[ALICE] Waiting for introduction from Bob...');

    } else if (msg is NetworkMsg) {
      print('[ALICE] Received ${msg.type} from ${msg.from}');

      if (msg.type == MessageType.agentMessage) {
        // Receive introduction from Bob
        ctx.handleNetworkMessage(msg.from, msg.payload);

        // Extract channel from NetIn
        final netInValue = runtime.heap.derefAddr(netInWriter);
        print('[ALICE] NetIn: $netInValue');

        if (netInValue is StructTerm && netInValue.functor == '.') {
          final content = netInValue.args[0];
          print('[ALICE] Received: $content');

          // Expecting ch(AC?, CA) - reader to receive from Charlie, writer to send to Charlie
          if (content is StructTerm && content.functor == 'ch' && content.args.length == 2) {
            final acRef = content.args[0];  // AC? - imported reader from Bob
            final caRef = content.args[1];  // CA - imported writer from Bob

            if (acRef is VarRef && caRef is VarRef) {
              acReaderAddr = acRef.addr;
              caWriterAddr = caRef.addr;
              print('[ALICE] Got channel: AC?=$acReaderAddr, CA=$caWriterAddr');

              // Send message to Charlie by binding CA
              print('[ALICE] Sending "hello_charlie" to Charlie via CA');
              runtime.heap.bindVariable(caWriterAddr!, ConstTerm('hello_charlie'));
              ctx.flushMessages();

              // Request the value of AC? (so Bob records us as requester)
              // In real GLP, this would happen when Alice's goal suspends on AC?
              ctx.processSuspension({acReaderAddr!});
              ctx.flushMessages();
            }
          }
        }

      } else if (msg.type == MessageType.assignment) {
        // Receive message from Charlie (via Bob's routing)
        final serializer = PayloadSerializer('');
        final (globalId, value) = serializer.deserializeAssignmentPayload(
          msg.payload,
          (isReader) => runtime.heap.allocateImportedReader(),
        );
        print('[ALICE] Assignment: ${globalId.creator}:${globalId.localId} := $value');
        ctx.handleAssignment(globalId.creator, globalId.localId, value);

        // Check if AC? is now bound
        if (acReaderAddr != null) {
          final acValue = runtime.heap.derefAddr(acReaderAddr!);
          print('[ALICE] AC? value: $acValue');
          if (acValue is ConstTerm) {
            receivedFromCharlie = acValue.value.toString();
            print('[ALICE] Received from Charlie: $receivedFromCharlie');
            mainPort.send(Done('alice', true, message: receivedFromCharlie));
          }
        }
      }
    }
  }
}

/// Bob's isolate: creates channel, introduces Alice to Charlie, routes messages
void bobIsolate(SendPort mainPort) async {
  final receivePort = ReceivePort();
  final runtime = GlpRuntime();
  final ctx = IrmaContext(agentId: 'bob', runtime: runtime);

  // Channel variables Bob creates
  late int caWriter, caReader;  // CA: Alice→Charlie direction
  late int acWriter, acReader;  // AC: Charlie→Alice direction

  ctx.onMessageReady = (dest, msg) {
    print('[BOB] Sending ${msg.type} to $dest');
    mainPort.send(NetworkMsg('bob', dest, msg.payload, msg.type));
  };

  // Set up network streams
  final (netOutWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkOutput(netOutWriter);
  final (netInWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkInput(netInWriter);

  mainPort.send(Ready('bob', receivePort.sendPort));

  await for (final msg in receivePort) {
    if (msg is Start) {
      print('[BOB] Creating channel and introducing Alice to Charlie');

      // Create CA channel (Alice → Charlie)
      final caPair = runtime.heap.allocateVariable();
      caWriter = caPair.$1;
      caReader = caPair.$2;
      ctx.registerWriter(caWriter);
      ctx.registerCreatedReader(caReader);
      print('[BOB] Created CA: writer=$caWriter, reader=$caReader');

      // Create AC channel (Charlie → Alice)
      final acPair = runtime.heap.allocateVariable();
      acWriter = acPair.$1;
      acReader = acPair.$2;
      ctx.registerWriter(acWriter);
      ctx.registerCreatedReader(acReader);
      print('[BOB] Created AC: writer=$acWriter, reader=$acReader');

      // Send ch(AC?, CA) to Alice
      // Alice gets: reader to receive from Charlie (AC?), writer to send to Charlie (CA)
      final aliceChannel = StructTerm('ch', [VarRef(acReader), VarRef(caWriter)]);
      final msgToAlice = StructTerm('msg', [ConstTerm('alice'), aliceChannel]);

      final (tail1W, tail1R) = runtime.heap.allocateVariable();
      final cons1 = StructTerm('.', [msgToAlice, VarRef(tail1R)]);
      print('[BOB] Sending to Alice: $aliceChannel');
      runtime.heap.bindVariable(netOutWriter, cons1);
      ctx.flushMessages();

      // Send ch(CA?, AC) to Charlie
      // Charlie gets: reader to receive from Alice (CA?), writer to send to Alice (AC)
      final charlieChannel = StructTerm('ch', [VarRef(caReader), VarRef(acWriter)]);
      final msgToCharlie = StructTerm('msg', [ConstTerm('charlie'), charlieChannel]);

      final (tail2W, tail2R) = runtime.heap.allocateVariable();
      final cons2 = StructTerm('.', [msgToCharlie, VarRef(tail2R)]);
      print('[BOB] Sending to Charlie: $charlieChannel');
      runtime.heap.bindVariable(tail1W, cons2);
      ctx.flushMessages();

    } else if (msg is NetworkMsg) {
      print('[BOB] Received ${msg.type} from ${msg.from}');

      if (msg.type == MessageType.assignment) {
        // Route assignment between Alice and Charlie
        final serializer = PayloadSerializer('');
        final (globalId, value) = serializer.deserializeAssignmentPayload(
          msg.payload,
          (isReader) => runtime.heap.allocateImportedReader(),
        );
        print('[BOB] Assignment: ${globalId.creator}:${globalId.localId} := $value');

        // Handle the assignment - this may trigger forwarding to the reader's requester
        ctx.handleAssignment(globalId.creator, globalId.localId, value);
        ctx.flushMessages();

      } else if (msg.type == MessageType.readRequest) {
        // Handle read request from Alice or Charlie
        final serializer = PayloadSerializer('');
        final varId = serializer.deserializeReadRequestPayload(msg.payload);
        print('[BOB] Read request for varId=$varId from ${msg.from}');
        ctx.handleReadRequest(varId, msg.from);
        ctx.flushMessages();
      }
    }
  }
}

/// Charlie's isolate: receives introduction from Bob, receives message from Alice, sends reply
void charlieIsolate(SendPort mainPort) async {
  final receivePort = ReceivePort();
  final runtime = GlpRuntime();
  final ctx = IrmaContext(agentId: 'charlie', runtime: runtime);

  // Channel endpoints Charlie will receive from Bob
  int? caReaderAddr;  // Reader to receive from Alice (CA?)
  int? acWriterAddr;  // Writer to send to Alice (AC)

  // Track received message from Alice
  String? receivedFromAlice;

  ctx.onMessageReady = (dest, msg) {
    print('[CHARLIE] Sending ${msg.type} to $dest');
    mainPort.send(NetworkMsg('charlie', dest, msg.payload, msg.type));
  };

  // Set up network streams
  final (netOutWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkOutput(netOutWriter);
  final (netInWriter, _) = runtime.heap.allocateVariable();
  ctx.registerNetworkInput(netInWriter);

  mainPort.send(Ready('charlie', receivePort.sendPort));

  await for (final msg in receivePort) {
    if (msg is Start) {
      print('[CHARLIE] Waiting for introduction from Bob...');

    } else if (msg is NetworkMsg) {
      print('[CHARLIE] Received ${msg.type} from ${msg.from}');

      if (msg.type == MessageType.agentMessage) {
        // Receive introduction from Bob
        ctx.handleNetworkMessage(msg.from, msg.payload);

        // Extract channel from NetIn
        final netInValue = runtime.heap.derefAddr(netInWriter);
        print('[CHARLIE] NetIn: $netInValue');

        if (netInValue is StructTerm && netInValue.functor == '.') {
          final content = netInValue.args[0];
          print('[CHARLIE] Received: $content');

          // Expecting ch(CA?, AC) - reader to receive from Alice, writer to send to Alice
          if (content is StructTerm && content.functor == 'ch' && content.args.length == 2) {
            final caRef = content.args[0];  // CA? - imported reader from Bob
            final acRef = content.args[1];  // AC - imported writer from Bob

            if (caRef is VarRef && acRef is VarRef) {
              caReaderAddr = caRef.addr;
              acWriterAddr = acRef.addr;
              print('[CHARLIE] Got channel: CA?=$caReaderAddr, AC=$acWriterAddr');

              // Request the value of CA? (triggers read request to Bob)
              // In a real scenario, this would happen when Charlie's goal suspends on CA?
              // For now, we manually trigger the request
              ctx.processSuspension({caReaderAddr!});
              ctx.flushMessages();
            }
          }
        }

      } else if (msg.type == MessageType.assignment) {
        // Receive message from Alice (via Bob's routing)
        final serializer = PayloadSerializer('');
        final (globalId, value) = serializer.deserializeAssignmentPayload(
          msg.payload,
          (isReader) => runtime.heap.allocateImportedReader(),
        );
        print('[CHARLIE] Assignment: ${globalId.creator}:${globalId.localId} := $value');
        ctx.handleAssignment(globalId.creator, globalId.localId, value);

        // Check if CA? is now bound
        if (caReaderAddr != null) {
          final caValue = runtime.heap.derefAddr(caReaderAddr!);
          print('[CHARLIE] CA? value: $caValue');
          if (caValue is ConstTerm) {
            receivedFromAlice = caValue.value.toString();
            print('[CHARLIE] Received from Alice: $receivedFromAlice');

            // Send reply to Alice by binding AC
            print('[CHARLIE] Sending "hello_alice" to Alice via AC');
            runtime.heap.bindVariable(acWriterAddr!, ConstTerm('hello_alice'));
            ctx.flushMessages();
          }
        }
      }
    }
  }
}

void main() {
  group('Isolate Friend-Mediated Introduction', () {
    test('Bob introduces Alice to Charlie, they exchange messages', () async {
      final mainReceivePort = ReceivePort();
      final completer = Completer<void>();

      SendPort? alicePort;
      SendPort? bobPort;
      SendPort? charliePort;
      Done? aliceResult;
      Done? charlieResult;
      int readyCount = 0;

      mainReceivePort.listen((msg) {
        if (msg is Ready) {
          print('[MAIN] ${msg.agentId} ready');
          if (msg.agentId == 'alice') alicePort = msg.sendPort;
          if (msg.agentId == 'bob') bobPort = msg.sendPort;
          if (msg.agentId == 'charlie') charliePort = msg.sendPort;

          readyCount++;
          if (readyCount == 3) {
            print('[MAIN] All agents ready, starting protocol');
            // Bob starts first (creates channel and sends introductions)
            bobPort!.send(Start());
            // Alice and Charlie wait for introductions
            alicePort!.send(Start());
            charliePort!.send(Start());
          }

        } else if (msg is NetworkMsg) {
          print('[MAIN] Routing ${msg.type} from ${msg.from} to ${msg.to}');
          if (msg.to == 'alice') alicePort?.send(msg);
          if (msg.to == 'bob') bobPort?.send(msg);
          if (msg.to == 'charlie') charliePort?.send(msg);

        } else if (msg is Done) {
          print('[MAIN] ${msg.agentId} done: success=${msg.success}, message=${msg.message}');
          if (msg.agentId == 'alice') aliceResult = msg;
          if (msg.agentId == 'charlie') charlieResult = msg;

          // Complete when Alice receives message from Charlie
          if (aliceResult != null && aliceResult!.success && !completer.isCompleted) {
            completer.complete();
          }
        }
      });

      // Spawn isolates
      print('[MAIN] Spawning isolates...');
      await Isolate.spawn(aliceIsolate, mainReceivePort.sendPort);
      await Isolate.spawn(bobIsolate, mainReceivePort.sendPort);
      await Isolate.spawn(charlieIsolate, mainReceivePort.sendPort);

      // Wait for completion
      await completer.future.timeout(
        Duration(seconds: 15),
        onTimeout: () => fail('Test timed out'),
      );

      // Verify
      expect(aliceResult?.success, isTrue, reason: 'Alice should receive message from Charlie');
      expect(aliceResult?.message, equals('hello_alice'), reason: 'Alice should receive "hello_alice"');

      mainReceivePort.close();
    });
  });
}
