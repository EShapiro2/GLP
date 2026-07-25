/// S2 — serializer alignment, canonical path (appendix §wf-terms).
///
/// Variables travel as global names per Definition Globalize: tag-2 variables,
/// not `_w`/`_r` structures. No original-creator ids, no paired-reader field,
/// no serializer string marker — the serializer tail is the variable `_w(q,0)`.
///
/// These exercise the canonical codec path (`PayloadCodec`) directly. The
/// switched-on multiagent/isolate suites are run separately with
/// GLP_WIRE_CANONICAL=1.
library;

import 'dart:typed_data';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/wire/artefact.dart' show Artefact;
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:test/test.dart';

/// A globalized term: a structure carrying a writer and a reader global name.
Term _globalized() => StructTerm('msg', [
      StructTerm('_w', [ConstTerm('alice'), ConstTerm(3)]),
      StructTerm('_r', [ConstTerm('alice'), ConstTerm(4)]),
      ConstTerm('hello'),
    ]);

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
      final payload = Uint8List.fromList(PayloadCodec.createGlobalSendPayload(
          GlobalName.writer('alice', 0), _globalized()));
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
  });

  group('ground-term canonicality across agents', () {
    test('the same ground term yields identical bytes regardless of agent', () {
      final ground = StructTerm('pair', [
        ConstTerm(42),
        StructTerm('.', [ConstTerm('x'), ConstTerm('nil')]),
      ]);
      final a = PayloadCodec.serializeAgentMessage(ground);
      final b = PayloadCodec.serializeAgentMessage(ground);
      expect(a, b);
    });

    test('serializeAgentMessage rejects a non-ground term', () {
      expect(() => PayloadCodec.serializeAgentMessage(StructTerm('m', [VarRef(5)])),
          throwsA(isA<WireFormatException>()));
    });
  });

  group('module constant (§wf-terms tag 6)', () {
    Artefact artefact() => Artefact.fromCompiled(
          ops: const [],
          hM: Uint8List(32),
          moduleName: 'shipped_probe',
          isaVersion: 'glp-isa-1',
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
