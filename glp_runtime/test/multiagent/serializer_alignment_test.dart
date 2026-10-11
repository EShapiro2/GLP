/// S2 — serializer alignment, canonical path (appendix §cf-terms).
///
/// Variables travel as global names per Definition Globalize: tag-2 variables,
/// not `_w`/`_r` structures. No original-creator ids, no paired-reader field,
/// no serializer string marker — the serializer tail is the variable `_w(q,0)`.
///
/// These exercise the canonical codec (`PayloadCodec`) directly; it is the
/// one encoding of madGLP payloads, which the multiagent and isolate suites run
/// over.
library;

import 'dart:typed_data';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/wire/artefact.dart' show Artefact, glpIsaVersion;
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:test/test.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell, CellTag;

/// A globalized term: a structure carrying a writer and a reader global name.
Term _globalized() => StructTerm('msg', [
      StructTerm('_w', [ConstTerm('alice'), ConstTerm(3)]),
      StructTerm('_r', [ConstTerm('alice'), ConstTerm(4)]),
      ConstTerm('hello'),
    ]);

/// A cell of serial number [id], the same cell for the same number: these
/// tests name a variable by its cell's number, a variable occurrence being
/// the cell itself (IGLP app:in-heap, Variable pairs).
final _cells = <int, HeapCell>{};
HeapCell _c(int id) =>
    _cells.putIfAbsent(id, () => HeapCell(null, CellTag.WrtTag, id));

void main() {
  group('global names ride as tag-2 variables', () {
    test('createGlobalSendPayload / deserialize round-trips the global names',
        () {
      final g = GlobalName.writer('alice', 7);
      final payload = PayloadCodec.createGlobalSendPayload(g, _globalized());
      final (gn, term) = PayloadCodec.deserializeGlobalSendPayload(payload);
      expect(gn.isWriter, isTrue);
      expect(gn.agent, 'alice');
      expect(gn.index, 7);
      // Embedded global names decode back to `_w`/`_r` structures.
      final s = term as StructTerm;
      expect(s.functor, 'msg');
      final w = s.args[0] as StructTerm;
      expect(w.functor, '_w');
      expect((w.args[0] as ConstTerm).value, 'alice');
      expect((w.args[1] as ConstTerm).value, 3);
      final r = s.args[1] as StructTerm;
      expect(r.functor, '_r');
      expect((r.args[1] as ConstTerm).value, 4);
    });

    test('the functor bytes _w / _r do not appear on the wire (tag 2, not 3)',
        () {
      // A message to _w(alice,0) is a serializer message, and carries a
      // serial (code format, Messages, version 3).
      final payload = Uint8List.fromList(PayloadCodec.createSerializerPayload(
          GlobalName.writer('alice', 0), _globalized(),
          serial: 1));
      // A tag-3 structure would embed the functor string "_w"/"_r" verbatim.
      expect(_contains(payload, '_w'.codeUnits), isFalse);
      expect(_contains(payload, '_r'.codeUnits), isFalse);
    });
  });

  group('serializer message', () {
    test('tail is the variable _w(q,0), not a string marker', () {
      final payload = PayloadCodec.createSerializerPayload(
        GlobalName.writer('queen', 0),
        StructTerm('intro', [ConstTerm('hi')]),
        serial: 1,
      );
      final (gn, value) = PayloadCodec.deserializeGlobalSendPayload(payload);
      expect(gn.isWriter, isTrue);
      expect(gn.index, 0);
      final cell = value as StructTerm;
      expect(cell.functor, '.');
      // Head is the content; tail is _w(queen, 0).
      final head = cell.args[0] as StructTerm;
      expect(head.functor, 'intro');
      final tail = cell.args[1] as StructTerm;
      expect(tail.functor, '_w');
      expect((tail.args[0] as ConstTerm).value, 'queen');
      expect((tail.args[1] as ConstTerm).value, 0);
      // No '#serializer' string marker on the wire.
      expect(_contains(Uint8List.fromList(payload), '#serializer'.codeUnits),
          isFalse);
    });

    test('the serial rides with the message and decodes back', () {
      final payload = PayloadCodec.createSerializerPayload(
          GlobalName.writer('queen', 0), ConstTerm('hi'),
          serial: 17);
      final (:name, :value, :serial) =
          PayloadCodec.deserializeValuePayload(payload);
      expect('$name', '_w(queen, 0)');
      expect(serial, 17);
      expect(((value as StructTerm).args[0] as ConstTerm).value, 'hi');
      // A value message on any other name has none.
      expect(
          PayloadCodec.deserializeValuePayload(PayloadCodec
                  .createGlobalSendPayload(GlobalName.reader('queen', 3),
                      ConstTerm('hi')))
              .serial,
          isNull);
    });
  });

  // Remark "Cold-Call via Serializer", item 1: the message is sent "under a
  // serial i that p allocates from its global-name counter (§app:in-networking),
  // so that (p, i) is unique for the whole run as every global name is";
  // Remark "Serializer vs Regular Global Links": '_send' on index 0 sends
  // "under a fresh serial".
  group("'_send' on index 0 takes a serial from the global-name counter", () {
    List<({GlobalName name, Term value, int? serial})> sendAll(
        void Function(MadContext ctx, GlpRuntime rt) sends) {
      final rt = GlpRuntime();
      final ctx = MadContext(agentId: 'alice', runtime: rt);
      final payloads = <List<int>>[];
      ctx.onMessageReady = (_, msg) => payloads.add(msg.payload);
      sends(ctx, rt);
      ctx.flushMessages();
      return [for (final p in payloads) PayloadCodec.deserializeValuePayload(p)];
    }

    test('one serial per message, after the names of the term it carries', () {
      late GlobalWritersTable wp;
      final sent = sendAll((ctx, rt) {
        wp = ctx.wp;
        for (var k = 0; k < 2; k++) {
          final (_, r) = rt.heap.allocateVariable();
          ctx.send(StructTerm('m', [VarRef(r)]), true, 'bob', 0, 'bob');
        }
      });
      expect(sent.map((m) => '${m.name}'), ['_w(bob, 0)', '_w(bob, 0)']);
      // The first message's reader takes index 1 and its serial 2; the second
      // message's reader 3 and its serial 4: one counter for both.
      expect(sent.map((m) => m.serial), [2, 4]);
      expect(
          sent.map((m) =>
              '${extractGlobalNames((m.value as StructTerm).args[0]).single}'),
          ['_r(alice, 1)', '_r(alice, 3)']);
      expect(wp.nextIndex, 5);
    });

    test('the same term sent twice is two cold calls, under two serials', () {
      final sent = sendAll((ctx, rt) {
        ctx.send(ConstTerm('hello'), true, 'bob', 0, 'bob');
        ctx.send(ConstTerm('hello'), true, 'bob', 0, 'bob');
      });
      expect(sent.map((m) => m.serial), [1, 2]);
    });

    test('a value on a link of index above 0 takes no serial', () {
      final sent = sendAll((ctx, rt) {
        ctx.send(ConstTerm('v'), false, 'alice', 7, 'bob');
      });
      expect(sent.single.serial, isNull);
      expect('${sent.single.name}', '_r(alice, 7)');
    });
  });

  group('ground-term canonicality across agents', () {
    test('the same ground term yields identical bytes regardless of agent', () {
      final ground = StructTerm('pair', [
        ConstTerm(42),
        StructTerm('.', [ConstTerm('x'), ConstTerm(nil)]),
      ]);
      final a = PayloadCodec.serializeAgentMessage(ground);
      final b = PayloadCodec.serializeAgentMessage(ground);
      expect(a, b);
    });

    test('serializeAgentMessage rejects a non-ground term', () {
      expect(() => PayloadCodec.serializeAgentMessage(StructTerm('m', [VarRef(_c(5))])),
          throwsA(isA<WireFormatException>()));
    });
  });

  group('module constant (§cf-terms tag 6)', () {
    Artefact artefact() => Artefact.fromCompiled(
          ops: const [],
          hM: Uint8List(32),
          moduleName: 'shipped_probe',
          isaVersion: glpIsaVersion,
        );

    test('a ModuleTerm ships as constant tag 6 and decodes to a ModuleTerm',
        () {
      final sent = ModuleTerm(artefact(), name: 'shipped_probe');
      final bytes =
          Uint8List.fromList(PayloadCodec.serializeAgentMessage(sent));
      // Term tag 1 (constant), constant tag 6 (module).
      expect(bytes.sublist(0, 2), [1, 6]);
      final back = PayloadCodec.wireToTerm(decodeTermFromBytes(bytes));
      expect(back, isA<ModuleTerm>());
      final received = back as ModuleTerm;
      expect(received.name, 'shipped_probe');
      expect((received.artefact as Artefact).toBytes(),
          (sent.artefact as Artefact).toBytes());
    });

    test('a module embedded in a structure round-trips', () {
      final sent = StructTerm('ship', [
        ModuleTerm(artefact(), name: 'shipped_probe'),
        ConstTerm('hello'),
      ]);
      final bytes =
          Uint8List.fromList(PayloadCodec.serializeAgentMessage(sent));
      final back =
          PayloadCodec.wireToTerm(decodeTermFromBytes(bytes)) as StructTerm;
      expect(back.functor, 'ship');
      expect(back.args[0], isA<ModuleTerm>());
      expect((back.args[1] as ConstTerm).value, 'hello');
    });
  });
}

bool _contains(List<int> haystack, List<int> needle) {
  if (needle.isEmpty) return true;
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
