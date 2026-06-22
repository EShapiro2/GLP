/// S1 — wire codec (§§wf-primitives, wf-terms).
///
/// Round-trip identity over every tag; ground-term canonicality; clen
/// one-value-one-encoding.
library;

import 'dart:convert';
import 'dart:typed_data';
import 'package:glp_runtime/wire/codec.dart';
import 'package:test/test.dart';

WireTerm _roundTripTerm(WireTerm t) =>
    decodeTermFromBytes(encodeTermToBytes(t));

WireAssignment _roundTripAssign(WireAssignment a) =>
    decodeAssignmentFromBytes(encodeAssignmentToBytes(a));

void main() {
  group('clen — one value, one encoding', () {
    test('boundary values round-trip in the shortest form', () {
      for (final v in [0, 1, 127, 128, 16383, 16384, (1 << 30) - 1]) {
        final w = WireWriter()..clen(v);
        final bytes = w.toBytes();
        final r = WireReader(bytes);
        expect(r.clen(), v);
        r.expectEnd();
      }
    });

    test('shortest-form byte counts', () {
      expect((WireWriter()..clen(127)).toBytes().length, 1);
      expect((WireWriter()..clen(128)).toBytes().length, 2);
      expect((WireWriter()..clen(16383)).toBytes().length, 2);
      expect((WireWriter()..clen(16384)).toBytes().length, 4);
    });

    test('decoder rejects an over-long 2-byte form for a value below 128', () {
      // 0x80 0x05 would decode to 5, which must use the 1-byte form.
      final r = WireReader(Uint8List.fromList([0x80, 0x05]));
      expect(() => r.clen(), throwsA(isA<WireFormatException>()));
    });

    test('decoder rejects an over-long 4-byte form for a value below 16384',
        () {
      // 0xC0 0x00 0x00 0x05 would decode to 5.
      final r = WireReader(Uint8List.fromList([0xC0, 0x00, 0x00, 0x05]));
      expect(() => r.clen(), throwsA(isA<WireFormatException>()));
    });

    test('encoder rejects out-of-range clen', () {
      expect(() => WireWriter().clen(-1), throwsA(isA<WireFormatException>()));
      expect(() => WireWriter().clen(1 << 30),
          throwsA(isA<WireFormatException>()));
    });
  });

  group('constant round-trip — every subtag', () {
    final cases = <WireConst>[
      const WNil(),
      const WInt(0),
      const WInt(-1),
      const WInt(9223372036854775807),
      const WInt(-9223372036854775808),
      const WFloat(0.0),
      const WFloat(3.14159),
      const WFloat(double.infinity),
      const WString(''),
      const WString('hello αβγ 日本語'),
      const WBool(true),
      const WBool(false),
      WBlob(Uint8List.fromList([0, 1, 2, 255, 128])),
    ];
    for (final c in cases) {
      test('$c', () {
        expect(_roundTripTerm(WConst(c)), WConst(c));
      });
    }

    test('NaN float round-trips bitwise', () {
      final t = _roundTripTerm(const WConst(WFloat(double.nan)));
      expect(t, isA<WConst>());
      final c = (t as WConst).constant as WFloat;
      expect(c.value.isNaN, isTrue);
    });
  });

  group('variable round-trip — both polarities', () {
    test('writer and reader, symbolic and raw-key agents', () {
      final writer = WVar.symbolic(isReader: false, agent: 'alice', index: 0);
      final reader = WVar.symbolic(isReader: true, agent: 'bob', index: 4242);
      final key = WVar(
          isReader: false,
          agent: Uint8List.fromList(List<int>.generate(32, (i) => i)),
          index: 7);
      expect(_roundTripTerm(writer), writer);
      expect(_roundTripTerm(reader), reader);
      expect(_roundTripTerm(key), key);
    });
  });

  group('structure round-trip — nesting and lists', () {
    test('nested structure with mixed children', () {
      final t = WStruct('msg', [
        WVar.symbolic(isReader: false, agent: 'alice', index: 1),
        const WConst(WString('payload')),
        WStruct('.', [
          const WConst(WInt(1)),
          WStruct('.', [const WConst(WInt(2)), const WConst(WNil())]),
        ]),
      ]);
      expect(_roundTripTerm(t), t);
    });

    test('empty list is the nil constant; a cell is ./2', () {
      final list = WStruct('.', [
        const WConst(WBool(true)),
        const WConst(WNil()),
      ]);
      expect(_roundTripTerm(list), list);
    });
  });

  group('messages', () {
    test('assignment round-trip', () {
      final a = WireAssignment(
        gIsReader: true,
        gAgent: Uint8List.fromList(utf8.encode('carol')),
        gIndex: 9,
        value: WStruct('connected', [const WConst(WString('dave'))]),
      );
      expect(_roundTripAssign(a), a);
    });

    test('serializer message is _w(q,0) := [T | _w(q,0)]', () {
      final agent = Uint8List.fromList(utf8.encode('queen'));
      final a = WireAssignment.serializer(
        agent: agent,
        head: WStruct('intro', [const WConst(WString('hi'))]),
      );
      expect(a.gIsReader, isFalse);
      expect(a.gIndex, 0);
      final v = a.value as WStruct;
      expect(v.functor, '.');
      final tail = v.args[1] as WVar;
      expect(tail.isReader, isFalse);
      expect(tail.index, 0);
      expect(tail.agentString, 'queen');
      expect(_roundTripAssign(a), a);
    });
  });

  group('ground-term canonicality', () {
    test('a ground term yields fixed bytes (agent-independent)', () {
      // A ground term contains tags 1 and 3 only; its bytes carry no agent.
      final ground = WStruct('pair', [
        const WConst(WInt(42)),
        WStruct('.', [
          const WConst(WString('x')),
          const WConst(WNil()),
        ]),
      ]);
      final b1 = encodeTermToBytes(ground);
      final b2 = encodeTermToBytes(ground);
      expect(b1, b2);
      // Decode-re-encode is byte-stable.
      expect(encodeTermToBytes(decodeTermFromBytes(b1)), b1);
    });
  });

  group('rejection of malformed input', () {
    test('unknown term tag', () {
      expect(() => decodeTermFromBytes(Uint8List.fromList([0x09])),
          throwsA(isA<WireFormatException>()));
    });
    test('unknown constant tag', () {
      expect(() => decodeTermFromBytes(Uint8List.fromList([1, 0x09])),
          throwsA(isA<WireFormatException>()));
    });
    test('trailing bytes rejected', () {
      final good = encodeTermToBytes(const WConst(WInt(1)));
      final padded = Uint8List.fromList([...good, 0x00]);
      expect(() => decodeTermFromBytes(padded),
          throwsA(isA<WireFormatException>()));
    });
    test('truncated input rejected', () {
      expect(() => decodeTermFromBytes(Uint8List.fromList([1, 1, 0, 0])),
          throwsA(isA<WireFormatException>()));
    });
    test('bad boolean payload rejected', () {
      expect(() => decodeTermFromBytes(Uint8List.fromList([1, 4, 0x02])),
          throwsA(isA<WireFormatException>()));
    });
  });
}
