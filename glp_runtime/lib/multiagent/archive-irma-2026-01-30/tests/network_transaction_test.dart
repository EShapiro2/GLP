/// Network Transaction Tests (Section 5.4)
///
/// Tests the Network Transaction implementation for cold-call protocol.
/// When agent p writes msg(q, X) to its network output stream, the
/// content X is exported and delivered to q's network input stream.

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/irma_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

void main() {
  group('Network Transaction', () {
    test('basic cold-call: Alice sends msg(bob, hello) to Bob', () {
      // Create two agents
      final aliceRuntime = GlpRuntime();
      final bobRuntime = GlpRuntime();

      final aliceCtx = IrmaContext(agentId: 'alice', runtime: aliceRuntime);
      final bobCtx = IrmaContext(agentId: 'bob', runtime: bobRuntime);

      // Track messages sent from Alice
      final messagesFromAlice = <OutboundMessage>[];
      aliceCtx.onMessageReady = (dest, msg) {
        print('[TEST] Alice -> $dest: ${msg.type}');
        messagesFromAlice.add(msg);
      };

      // Set up Bob's network input stream
      final (bobNetInWriter, bobNetInReader) = bobRuntime.heap.allocateVariable();
      bobCtx.registerNetworkInput(bobNetInWriter);

      // Set up Alice's network output stream
      final (aliceNetOutWriter, aliceNetOutReader) = aliceRuntime.heap.allocateVariable();
      aliceCtx.registerNetworkOutput(aliceNetOutWriter);

      // Alice writes msg(bob, hello) to her NetOut
      // Create cons cell: [msg(bob, hello) | Tail]
      final (tailWriter, tailReader) = aliceRuntime.heap.allocateVariable();
      final msgTerm = StructTerm('msg', [ConstTerm('bob'), ConstTerm('hello')]);
      final consCell = StructTerm('.', [msgTerm, VarRef(tailReader)]);

      print('[TEST] Alice binding NetOut to: $consCell');
      aliceRuntime.heap.bindVariable(aliceNetOutWriter, consCell);

      // Flush Alice's messages
      aliceCtx.flushMessages();

      // Verify Alice sent an agentMessage to bob
      expect(messagesFromAlice.length, equals(1));
      expect(messagesFromAlice[0].destination, equals('bob'));
      expect(messagesFromAlice[0].type, equals(MessageType.agentMessage));

      // Deliver message to Bob
      final msg = messagesFromAlice[0];
      bobCtx.handleNetworkMessage('alice', msg.payload);

      // Verify Bob's NetIn has the message
      final bobNetInValue = bobRuntime.heap.derefAddr(bobNetInWriter);
      print('[TEST] Bob NetIn value: $bobNetInValue');

      // Should be a cons cell containing 'hello'
      expect(bobNetInValue, isA<StructTerm>());
      final bobCons = bobNetInValue as StructTerm;
      expect(bobCons.functor, equals('.'));
      expect(bobCons.args[0], isA<ConstTerm>());
      expect((bobCons.args[0] as ConstTerm).value, equals('hello'));
    });

    test('cold-call with response variable', () {
      // Create two agents
      final aliceRuntime = GlpRuntime();
      final bobRuntime = GlpRuntime();

      final aliceCtx = IrmaContext(agentId: 'alice', runtime: aliceRuntime);
      final bobCtx = IrmaContext(agentId: 'bob', runtime: bobRuntime);

      // Message routing
      final messageQueue = <String, List<OutboundMessage>>{};

      aliceCtx.onMessageReady = (dest, msg) {
        print('[TEST] Alice -> $dest: ${msg.type}');
        messageQueue.putIfAbsent(dest, () => []).add(msg);
      };

      bobCtx.onMessageReady = (dest, msg) {
        print('[TEST] Bob -> $dest: ${msg.type}');
        messageQueue.putIfAbsent(dest, () => []).add(msg);
      };

      // Set up network streams
      final (bobNetInWriter, _) = bobRuntime.heap.allocateVariable();
      bobCtx.registerNetworkInput(bobNetInWriter);

      final (aliceNetInWriter, _) = aliceRuntime.heap.allocateVariable();
      aliceCtx.registerNetworkInput(aliceNetInWriter);

      final (aliceNetOutWriter, _) = aliceRuntime.heap.allocateVariable();
      aliceCtx.registerNetworkOutput(aliceNetOutWriter);

      // Alice creates a response variable
      // Alice keeps the READER (to read response), sends WRITER (for Bob to bind)
      final (respWriter, respReader) = aliceRuntime.heap.allocateVariable();
      aliceCtx.registerWriter(respWriter);  // Register in V_p for cross-agent communication
      aliceCtx.registerCreatedReader(respReader);  // Reader is exported

      // Alice writes msg(bob, intro(alice, alice, Resp)) to NetOut
      // This is the cold-call introduction request
      // Bob receives the WRITER so he can bind the response
      final introTerm = StructTerm('intro', [
        ConstTerm('alice'),
        ConstTerm('alice'),
        VarRef(respWriter),  // Response WRITER for Bob to bind
      ]);
      final msgTerm = StructTerm('msg', [ConstTerm('bob'), introTerm]);

      final (tailWriter, tailReader) = aliceRuntime.heap.allocateVariable();
      final consCell = StructTerm('.', [msgTerm, VarRef(tailReader)]);

      print('[TEST] Alice sending: $msgTerm');
      aliceRuntime.heap.bindVariable(aliceNetOutWriter, consCell);
      aliceCtx.flushMessages();

      // Verify Alice exported the response variable
      print('[TEST] Alice V_p entries: ${aliceCtx.vp.entries}');

      // Deliver to Bob
      expect(messageQueue['bob']?.length, equals(1));
      final msgToBob = messageQueue['bob']![0];
      expect(msgToBob.type, equals(MessageType.agentMessage));

      bobCtx.handleNetworkMessage('alice', msgToBob.payload);

      // Verify Bob received the message with imported response variable
      final bobNetInValue = bobRuntime.heap.derefAddr(bobNetInWriter);
      print('[TEST] Bob received: $bobNetInValue');

      expect(bobNetInValue, isA<StructTerm>());
      final bobCons = bobNetInValue as StructTerm;
      expect(bobCons.functor, equals('.'));

      final receivedIntro = bobCons.args[0];
      expect(receivedIntro, isA<StructTerm>());
      final introStruct = receivedIntro as StructTerm;
      expect(introStruct.functor, equals('intro'));
      expect(introStruct.args.length, equals(3));

      // The response variable should be imported to Bob
      final bobRespRef = introStruct.args[2];
      expect(bobRespRef, isA<VarRef>());
      final bobRespAddr = (bobRespRef as VarRef).addr;

      // Bob has an imported writer for the response
      print('[TEST] Bob V_p: ${bobCtx.vp.entries}');

      // Bob binds the response (accepts the friend request)
      final acceptTerm = StructTerm('accept', [ConstTerm('bob_channel')]);
      print('[TEST] Bob binding response to: $acceptTerm');
      bobRuntime.heap.bindVariable(bobRespAddr, acceptTerm);
      bobCtx.flushMessages();

      // Verify Bob sent assignment to Alice
      expect(messageQueue['alice']?.length, equals(1));
      final msgToAlice = messageQueue['alice']![0];
      expect(msgToAlice.type, equals(MessageType.assignment));

      // Deliver to Alice
      final serializer = PayloadSerializer('');
      final (globalId, value) = serializer.deserializeAssignmentPayload(
        msgToAlice.payload,
        (isReader) => aliceRuntime.heap.allocateImportedReader(),
      );
      print('[TEST] Alice received assignment: $globalId := $value');
      aliceCtx.handleAssignment(globalId.creator, globalId.localId, value);

      // Verify Alice's response variable is now bound
      final aliceRespValue = aliceRuntime.heap.derefAddr(respWriter);
      print('[TEST] Alice Resp value: $aliceRespValue');

      expect(aliceRespValue, isA<StructTerm>());
      final respStruct = aliceRespValue as StructTerm;
      expect(respStruct.functor, equals('accept'));
    });

    test('multiple messages on NetOut stream', () {
      final aliceRuntime = GlpRuntime();
      final aliceCtx = IrmaContext(agentId: 'alice', runtime: aliceRuntime);

      final messagesFromAlice = <OutboundMessage>[];
      aliceCtx.onMessageReady = (dest, msg) {
        print('[TEST] Alice -> $dest: ${msg.type}');
        messagesFromAlice.add(msg);
      };

      // Set up Alice's network output stream
      final (netOutWriter, _) = aliceRuntime.heap.allocateVariable();
      aliceCtx.registerNetworkOutput(netOutWriter);

      // Write first message: msg(bob, hello)
      final (tail1Writer, tail1Reader) = aliceRuntime.heap.allocateVariable();
      final msg1 = StructTerm('msg', [ConstTerm('bob'), ConstTerm('hello')]);
      final cons1 = StructTerm('.', [msg1, VarRef(tail1Reader)]);
      aliceRuntime.heap.bindVariable(netOutWriter, cons1);
      aliceCtx.flushMessages();

      expect(messagesFromAlice.length, equals(1));
      expect(messagesFromAlice[0].destination, equals('bob'));

      // Write second message: msg(charlie, hi)
      final (tail2Writer, tail2Reader) = aliceRuntime.heap.allocateVariable();
      final msg2 = StructTerm('msg', [ConstTerm('charlie'), ConstTerm('hi')]);
      final cons2 = StructTerm('.', [msg2, VarRef(tail2Reader)]);
      aliceRuntime.heap.bindVariable(tail1Writer, cons2);
      aliceCtx.flushMessages();

      expect(messagesFromAlice.length, equals(2));
      expect(messagesFromAlice[1].destination, equals('charlie'));

      // Write third message: msg(bob, goodbye)
      final (tail3Writer, tail3Reader) = aliceRuntime.heap.allocateVariable();
      final msg3 = StructTerm('msg', [ConstTerm('bob'), ConstTerm('goodbye')]);
      final cons3 = StructTerm('.', [msg3, VarRef(tail3Reader)]);
      aliceRuntime.heap.bindVariable(tail2Writer, cons3);
      aliceCtx.flushMessages();

      expect(messagesFromAlice.length, equals(3));
      expect(messagesFromAlice[2].destination, equals('bob'));

      print('[TEST] All messages sent: ${messagesFromAlice.map((m) => m.destination)}');
    });
  });
}
