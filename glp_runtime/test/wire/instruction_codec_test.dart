/// S3 — instruction encoding (§4).
///
/// Golden encodings per opcode; encode→decode→encode byte-idempotence over a
/// corpus covering every §4.2 opcode (the instruction classes carry no value
/// equality, so byte-stability is the faithful round-trip check); label
/// stripping + proc resolution via encodeCode/decodeCode.
library;

import 'dart:typed_data';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/opcodes_v2.dart' as opv2;
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/instruction_codec.dart';
import 'package:test/test.dart';

// Fixed proc table for tests: label <-> index.
const _procLabels = ['p/1', 'q/2', 'guard_ok/1'];
int _procIndexOf(String l) {
  final i = _procLabels.indexOf(l);
  if (i < 0) throw StateError('unknown proc label $l');
  return i;
}

String _procNameOf(int i) => _procLabels[i];

// ctarget: clause labels "L<n>" map to n; decode yields "#<n>".
int _ctargetOf(String l) =>
    l.startsWith('#') ? int.parse(l.substring(1)) : int.parse(l.substring(1));

Uint8List _enc(Object op) {
  final w = WireWriter();
  encodeInstruction(w, op, procIndexOf: _procIndexOf, ctargetOf: _ctargetOf);
  return w.toBytes();
}

Object _dec(Uint8List b) {
  final r = WireReader(b);
  final op = decodeInstruction(r,
      procNameOf: _procNameOf, ctargetLabelOf: (i) => '#$i');
  r.expectEnd();
  return op;
}

void main() {
  group('golden encodings', () {
    test('operandless opcodes are a single byte', () {
      expect(_enc(ClauseTry()), [0x01]);
      expect(_enc(NoMoreClauses()), [0x03]);
      expect(_enc(Commit()), [0x04]);
      expect(_enc(Proceed()), [0x05]);
      expect(_enc(Halt()), [0x06]);
      expect(_enc(Nop()), [0x07]);
      expect(_enc(Deallocate()), [0x38]);
      expect(_enc(Otherwise()), [0x46]);
    });

    test('clause_next carries a clen ctarget', () {
      expect(_enc(ClauseNext('L5')), [0x02, 0x05]);
    });

    test('head_constant: tag, constant payload, argSlot', () {
      // int 7 -> constant tag 1 + i64; argSlot 2.
      expect(_enc(HeadConstant(7, 2)),
          [0x10, 0x01, 0, 0, 0, 0, 0, 0, 0, 7, 0x02]);
    });

    test('unify_constant nil -> constant tag 0', () {
      expect(_enc(UnifyConstant('nil')), [0x21, 0x00]);
    });

    test('unify_constant string', () {
      // string "ab" -> tag 3, clen 2, bytes 'a''b'.
      expect(_enc(UnifyConstant('ab')), [0x21, 0x03, 0x02, 0x61, 0x62]);
    });

    test('head_variable: polarity then varIndex', () {
      expect(_enc(opv2.HeadVariable(3, isReader: false)), [0x14, 0x00, 0x03]);
      expect(_enc(opv2.HeadVariable(3, isReader: true)), [0x14, 0x01, 0x03]);
    });

    test('get_variable: polarity, varIndex, argSlot', () {
      expect(_enc(opv2.GetVariable(1, 2, isReader: false)),
          [0x15, 0x00, 0x01, 0x02]);
    });

    test('head_structure: functor, arity, argSlot', () {
      // functor "f" -> clen 1 + 'f'; arity 2; argSlot 0.
      expect(_enc(HeadStructure('f', 2, 0)),
          [0x12, 0x01, 0x66, 0x02, 0x00]);
    });

    test('guard: proc index, arity, negated', () {
      expect(_enc(Guard('guard_ok', 1, negated: false)),
          [0x40, 0x02, 0x01, 0x00]);
      expect(_enc(Guard('guard_ok', 1, negated: true)),
          [0x40, 0x02, 0x01, 0x01]);
    });

    test('spawn / requeue: proc index, arity', () {
      expect(_enc(Spawn('p/1', 1)), [0x50, 0x00, 0x01]);
      expect(_enc(Requeue('q/2', 2)), [0x51, 0x01, 0x02]);
    });

    test('ground: varIndex, negated', () {
      expect(_enc(Ground(4, negated: true)), [0x41, 0x04, 0x01]);
    });

    test('retired distribute/transmit are not in the wire ISA', () {
      expect(() => _enc(Distribute(3, 'm', 2)),
          throwsA(isA<WireFormatException>()));
      expect(() => _enc(Transmit(1, 'm', 2)),
          throwsA(isA<WireFormatException>()));
    });
  });

  group('round-trip (byte idempotence) over every opcode', () {
    final corpus = <Object>[
      ClauseTry(),
      ClauseNext('L7'),
      NoMoreClauses(),
      Commit(),
      Proceed(),
      Halt(),
      Nop(),
      HeadConstant(42, 1),
      HeadNil(0),
      HeadStructure('foo', 3, 2),
      HeadList(1),
      opv2.HeadVariable(5, isReader: true),
      opv2.GetVariable(1, 2, isReader: false),
      opv2.GetValue(2, 3, isReader: true),
      opv2.UnifyVariable(4, isReader: false),
      UnifyConstant('hello'),
      UnifyVoid(count: 3),
      UnifyStructure('bar', 2),
      Push(6),
      Pop(6),
      opv2.PutVariable(1, 0, isReader: true),
      PutConstant(3.14, 1),
      PutNil(2),
      PutList(3),
      PutStructure('baz', 1, 0),
      opv2.SetVariable(7, isReader: false),
      SetConstant(true),
      Allocate(4),
      Deallocate(),
      PutBoundConst('nil', 1),
      PutBoundNil(2),
      Guard('guard_ok', 1, negated: true),
      Ground(2, negated: false),
      Known(3, negated: true),
      opv2.Unknown(4),
      NoReaders(5, negated: false),
      GroundEqual(1, 2, negated: true),
      Otherwise(),
      Spawn('p/1', 1),
      Requeue('q/2', 2),
    ];

    test('corpus covers every wire opcode', () {
      // 40 wire opcodes: 42 in the original table minus the retired
      // distribute/transmit (0x52–0x53), now reserved.
      expect(corpus.length, 40);
    });

    for (final op in corpus) {
      test('round-trips ${op.runtimeType} (${_enc(op)[0].toRadixString(16)})',
          () {
        final b1 = _enc(op);
        final decoded = _dec(b1);
        final b2 = _enc(decoded);
        expect(b2, b1, reason: 'encode∘decode must be byte-stable');
      });
    }

    test('decoded objects carry the right fields (spot checks)', () {
      final s = _dec(_enc(Spawn('p/1', 1))) as Spawn;
      expect(s.procedureLabel, 'p/1');
      expect(s.arity, 1);
      final g = _dec(_enc(Guard('guard_ok', 1, negated: true))) as Guard;
      expect(g.procedureLabel, 'guard_ok'); // bare name restored from the signature
      expect(g.arity, 1);
      expect(g.negated, isTrue);
      final hv = _dec(_enc(opv2.HeadVariable(5, isReader: true)))
          as opv2.HeadVariable;
      expect(hv.varIndex, 5);
      expect(hv.isReader, isTrue);
      final hc = _dec(_enc(HeadConstant(42, 1))) as HeadConstant;
      expect(hc.value, 42);
      expect(hc.argSlot, 1);
    });
  });

  group('encodeCode / decodeCode', () {
    test('strips Label ops and round-trips a procedure body', () {
      final ops = <Object>[
        Label('p/0'),
        ClauseTry(),
        Commit(),
        Spawn('p/1', 1),
        Proceed(),
      ];
      final bytes = encodeCode(ops, procIndexOf: _procIndexOf);
      final back = decodeCode(bytes, procNameOf: _procNameOf);
      // Label is erased: 5 ops in, 4 out.
      expect(back.length, 4);
      expect(back[0], isA<ClauseTry>());
      expect(back[3], isA<Proceed>());
      // Re-encode is byte-stable.
      final bytes2 = encodeCode(back, procIndexOf: _procIndexOf);
      expect(bytes2, bytes);
    });

    test('Label cannot be encoded as a standalone instruction', () {
      expect(() => _enc(Label('x')), throwsA(isA<WireFormatException>()));
    });
  });
}
