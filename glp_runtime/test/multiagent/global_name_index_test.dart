/// A global name on the wire is a variable's name, and a program's term is a
/// term (IGLP 1e01638).  "A global name has two presentations.  In a message
/// between agents it stands in the place of a variable and is encoded as one
/// ...  A ground structure _w(P, I) or _r(P, I) that a program puts in a
/// message is a term, encoded as a structure and arriving as one; only a
/// variable is encoded as a global name" (madglp-spec.tex:17).  The code
/// format's tag 2 carries "u8 polarity ..., agent p, clen index i.  An agent
/// is an atom and an index is a non-negative integer, and the reader refuses
/// a global name whose agent or index is anything else" (code-format-
/// fragment.tex:32); "a ground term contains tags 1 and 3 only" (:35).
///
/// GLP #11 Cowork, 2026-11-02 07:39 UTC, items 2 and 3, as revised at 10:16
/// UTC: the refusal is of tag 2, an index being a clen and an agent's bytes
/// the agent's UTF-8 symbolic name in this runtime; the refusal of a tag-3
/// structure `_w(alice, 2.0)` made at 9cd4549b goes, and such a structure,
/// `_w(5, 2)` and `_w(alice, 2)` among them, arrives as the structure.
/// Covering tests: a tag-2 name whose agent is not an atom is refused at
/// receipt, the refusal naming it; `_w(5, 2)` arrives as the structure, with
/// no throw; a sent ground `'_w'(p, 2)` arrives as the structure.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:test/test.dart';

/// The 2-ary structure [functor]([agent], [index]): a program's term.
WStruct _struct(String functor, WireTerm index, {WireTerm? agent}) =>
    WStruct(functor, [agent ?? WConst(WString('alice')), index]);

/// Bytes that are no UTF-8 text, and so name no atom.
final Uint8List _notUtf8 = Uint8List.fromList([0xff, 0xfe]);

/// A value message to the reader name `_r(alice, 1)`: got(N).
Uint8List _gotValue(WireTerm n) =>
    encodeMessageToBytes(WireValueMessage(WireAssignment(
      gIsReader: true,
      gAgent: Uint8List.fromList(utf8.encode('alice')),
      gEpoch: 1,
      gIndex: 1,
      value: WStruct('got', [n]),
    ), senderEpoch: 1));

/// Structures of the form of a global name, each with how it prints: each is
/// a program's term, whatever its arguments.
final Map<String, WStruct> _structures = {
  "_w(Const(alice),Const(2.0))": _struct('_w', WConst(WFloat(2.0))),
  "_r(Const(alice),Const(2.0))": _struct('_r', WConst(WFloat(2.0))),
  "_w(Const(alice),Const(-1))": _struct('_w', WConst(WInt(-1))),
  "_w(Const(alice),Const(two))": _struct('_w', WConst(WString('two'))),
  "_r(Const(alice),f(Const(2)))":
      _struct('_r', WStruct('f', [WConst(WInt(2))])),
  "_w(Const(5),Const(2))":
      _struct('_w', WConst(WInt(2)), agent: WConst(WInt(5))),
  "_w(Const(alice),Const(2))": _struct('_w', WConst(WInt(2))),
};

/// Run [f], giving back what it printed.
List<String> _printed(void Function() f) {
  final lines = <String>[];
  runZoned(f,
      zoneSpecification: ZoneSpecification(
          print: (self, parent, zone, line) => lines.add(line)));
  return lines;
}

void main() {
  group('the decoding of a received message', () {
    for (final MapEntry(key: shown, value: structure) in _structures.entries) {
      test('a structure $shown arrives as the structure, no global name', () {
        final (_, value) =
            PayloadCodec.deserializeGlobalSendPayload(_gotValue(structure));
        expect('${(value as StructTerm).args.single}', shown);
        expect(value.args.single, isA<StructTerm>());
        expect(extractGlobalNames(value), isEmpty);
      });
    }

    test('a structure _w(alice, 2.0) as the whole value arrives as the '
        'structure', () {
      final payload = encodeMessageToBytes(WireValueMessage(WireAssignment(
        gIsReader: true,
        gAgent: Uint8List.fromList(utf8.encode('alice')),
        gEpoch: 1,
        gIndex: 1,
        value: _struct('_w', WConst(WFloat(2.0))),
      ), senderEpoch: 1));
      final (_, value) = PayloadCodec.deserializeGlobalSendPayload(payload);
      expect('$value', '_w(Const(alice),Const(2.0))');
    });

    test('a tag-2 variable is received as its name, in the place of a '
        'variable', () {
      final (_, value) = PayloadCodec.deserializeGlobalSendPayload(_gotValue(
          WVar.symbolic(isReader: false, agent: 'alice', epoch: 1, index: 2)));
      expect((value as StructTerm).args.single,
          GlobalName.writer('alice', 2).toTerm());
      expect(extractGlobalNames(value).map((g) => '$g'), ['_w(alice, 1, 2)']);
    });

    test('a tag-2 variable whose agent is not an atom is refused, the '
        'refusal naming it', () {
      for (final isReader in [false, true]) {
        final shown = '${isReader ? '_r' : '_w'}(0xfffe, 1, 2)';
        expect(
            () => PayloadCodec.deserializeGlobalSendPayload(_gotValue(
                WVar(isReader: isReader, agent: _notUtf8, epoch: 1, index: 2))),
            throwsA(isA<GlobalNameRefusal>()
                .having((r) => r.name, 'name', shown)
                .having((r) => r.message, 'message',
                    'global name $shown has an agent that is not an atom')));
      }
    });

    test('a value message addressed to a name whose agent is not an atom is '
        'refused', () {
      final payload = encodeMessageToBytes(WireValueMessage(WireAssignment(
        gIsReader: true,
        gAgent: _notUtf8,
        gEpoch: 1,
        gIndex: 1,
        value: WConst(WInt(1)),
      ), senderEpoch: 1));
      expect(() => PayloadCodec.deserializeGlobalSendPayload(payload),
          throwsA(isA<GlobalNameRefusal>()
              .having((r) => r.name, 'name', '_r(0xfffe, 1, 1)')));
    });

    test('a request and an acknowledgement whose agent is not an atom are '
        'refused', () {
      for (final payload in [
        encodeMessageToBytes(WireRequestMessage(agent: _notUtf8, epoch: 1, index: 3, senderEpoch: 1)),
        encodeMessageToBytes(WireAckMessage(agent: _notUtf8, epoch: 1, index: 3, senderEpoch: 1)),
      ]) {
        expect(() => PayloadCodec.decodeLinkPayload(payload).name,
            throwsA(isA<GlobalNameRefusal>()
                .having((r) => r.name, 'name', '_r(0xfffe, 1, 3)')));
      }
    });
  });

  group('the encoding', () {
    test("a ground '_w'(p, 2) is a tag-3 structure, its functor on the wire",
        () {
      final ground =
          StructTerm('_w', [ConstTerm('p'), ConstTerm(2)]);
      final w = PayloadCodec.termToWire(ground);
      expect(w, isA<WStruct>());
      final bytes = encodeTermToBytes(w);
      expect(bytes.first, 3, reason: 'tag 3, a structure');
      expect(PayloadCodec.wireToTerm(decodeTermFromBytes(bytes)).toString(),
          '_w(Const(p),Const(2))');
    });

    test('a global name in the place of a variable is a tag-2 variable', () {
      final w = PayloadCodec.termToWire(GlobalName.reader('p', 2).toTerm());
      expect(w, isA<WVar>());
      expect(encodeTermToBytes(w).first, 2, reason: 'tag 2, a variable');
    });

    test('the encoding of a ground term carries no tag-2 variable: decoding '
        'one as a ground term is refused', () {
      final bytes = encodeTermToBytes(WStruct('m',
          [WVar.symbolic(isReader: true, agent: 'p', epoch: 1, index: 2)]));
      expect(() => PayloadCodec.wireToTerm(decodeTermFromBytes(bytes)),
          throwsA(isA<WireFormatException>()));
    });

    test('a structure presents no global name, whatever its arguments', () {
      for (final index in <Object>[2, 2.0, -1, 'two']) {
        expect(
            extractGlobalNames(StructTerm(
                'm', [StructTerm('_w', [ConstTerm('alice'), ConstTerm(index)])])),
            isEmpty,
            reason: "'_w'(alice, $index)");
      }
    });
  });

  group('a value on a link whose entry stands', () {
    late GlpRuntime rt;
    late MadContext bob;
    late HeapCell writer;
    setUp(() {
      rt = GlpRuntime();
      bob = MadContext(agentId: 'bob', runtime: rt);
      writer = rt.heap.allocateVariable().$1;
      bob.wp.addLocalizeEntry(writer, GlobalName.reader('alice', 1));
    });

    test('carrying a tag-2 name whose agent is not an atom is refused at '
        'receipt, the refusal naming it, and nothing is received', () {
      final printed = _printed(() => bob.handleIncomingPayload(
          payload: _gotValue(WVar(isReader: false, agent: _notUtf8, epoch: 1, index: 2)),
          fromAgent: 'alice'));
      expect(printed, [
        '[REFUSED] bob: a message from alice is refused at receipt: global '
            'name _w(0xfffe, 1, 2) has an agent that is not an atom'
      ]);
      expect(rt.heap.isFullyBound(writer), isFalse,
          reason: 'nothing is assigned');
      expect(bob.wp.findByRemote(GlobalName.reader('alice', 1)), isNotNull,
          reason: 'no Receive took the message: the entry stands');
      expect(bob.mp.totalLength, 0, reason: 'nothing is acknowledged');
    });

    test('a request whose agent is not an atom is refused at receipt', () {
      final printed = _printed(() => bob.handleIncomingPayload(
          payload:
              encodeMessageToBytes(WireRequestMessage(agent: _notUtf8, epoch: 1, index: 3, senderEpoch: 1)),
          fromAgent: 'alice'));
      expect(printed, [
        '[REFUSED] bob: a message from alice is refused at receipt: global '
            'name _r(0xfffe, 1, 3) has an agent that is not an atom'
      ]);
      expect(bob.mp.totalLength, 0);
    });

    for (final MapEntry(key: shown, value: structure) in _structures.entries) {
      test('carrying the structure $shown is received: the writer is '
          'assigned the structure, and no link is made', () {
        final printed = _printed(() => bob.handleIncomingPayload(
            payload: _gotValue(structure), fromAgent: 'alice'));
        expect(printed, isEmpty);
        expect(rt.heap.isFullyBound(writer), isTrue);
        expect('${rt.heap.derefAddr(writer)}', 'got($shown)');
        expect(bob.wp.findByRemote(GlobalName.reader('alice', 1)), isNull);
        expect(bob.wp.localizeEntryCount, 0);
        expect(bob.up.count, 0, reason: 'no imported writer is recorded');
        expect(bob.globalSendRegistry.pendingCount, 0,
            reason: 'no global_send goal is spawned');
      });
    }
  });

  test("a sent ground '_w'(p, 2) arrives as the structure", () {
    // Alice sends got('_w'(p, 2)) on the link _r(alice, 1) to Bob, the
    // structure a program built: a term, encoded as a structure.
    final art = GlpRuntime();
    final alice = MadContext(agentId: 'alice', runtime: art);
    final payloads = <List<int>>[];
    alice.onMessageReady = (_, msg) => payloads.add(msg.payload);
    final sent =
        StructTerm('got', [StructTerm('_w', [ConstTerm('p'), ConstTerm(2)])]);
    alice.send(sent, false, 'alice', 1, 1, 'bob');
    alice.flushMessages();
    expect(alice.wp.globalizeEntryCount, 0, reason: 'no variable, no entry');
    expect(alice.globalSendRegistry.pendingCount, 0);
    final payload = payloads.single;
    expect(_contains(payload, utf8.encode('_w')), isTrue,
        reason: 'the functor travels: a tag-3 structure');

    final brt = GlpRuntime();
    final bob = MadContext(agentId: 'bob', runtime: brt);
    final writer = brt.heap.allocateVariable().$1;
    bob.wp.addLocalizeEntry(writer, GlobalName.reader('alice', 1));
    final printed = _printed(
        () => bob.handleIncomingPayload(payload: payload, fromAgent: 'alice'));
    expect(printed, isEmpty);
    expect('${brt.heap.derefAddr(writer)}', 'got(_w(Const(p),Const(2)))');
    expect(bob.up.count, 0, reason: 'no imported writer is recorded');
    expect(bob.globalSendRegistry.pendingCount, 0,
        reason: 'no global_send goal is spawned');
  });
}

bool _contains(List<int> haystack, List<int> needle) {
  for (var i = 0; i + needle.length <= haystack.length; i++) {
    var ok = true;
    for (var j = 0; j < needle.length; j++) {
      if (haystack[i + j] != needle[j]) {
        ok = false;
        break;
      }
    }
    if (ok) return true;
  }
  return false;
}
