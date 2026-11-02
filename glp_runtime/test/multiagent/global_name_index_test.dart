/// A global name's index is a non-negative integer, and the reader refuses a
/// global name whose index is anything else (IGLP code-format-fragment.tex,
/// Terms, tag 2, at IGLP 9606236; GLP #11 Cowork, 2026-11-01 22:28 UTC: "Make
/// the reader refuse it, with its diagnostic naming the name; covering test: a
/// _w(p, 2.0) in a payload is refused").
///
/// A tag-2 variable's index is a clen, so only a global name that travelled as
/// the 2-ary structure `_w(P, I)` or `_r(P, I)` can carry another index.  The
/// refusal is at the decoding of the payload, as a forged module's is
/// (module_receipt_test.dart): the receiver assigns nothing, keeps the entry,
/// acknowledges nothing, and its runtime prints the refusal, naming the name.
/// Until 2026-11-02 `_w(p, 2.0)` was taken as `_w(p, 2)`.
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

/// The 2-ary structure [functor](alice, [index]), a global name as it travels
/// when it is no tag-2 variable.
WStruct _name(String functor, WireTerm index) =>
    WStruct(functor, [WConst(WString('alice')), index]);

/// A value message to the reader name `_r(alice, 1)`: got(N).
Uint8List _gotValue(WireTerm n) =>
    encodeMessageToBytes(WireValueMessage(WireAssignment(
      gIsReader: true,
      gAgent: Uint8List.fromList(utf8.encode('alice')),
      gIndex: 1,
      value: WStruct('got', [n]),
    )));

/// Global names whose index is not a non-negative integer, each with how the
/// refusal names it.
final Map<String, WStruct> _refused = {
  '_w(alice, 2.0)': _name('_w', WConst(WFloat(2.0))),
  '_r(alice, 2.0)': _name('_r', WConst(WFloat(2.0))),
  '_w(alice, -1)': _name('_w', WConst(WInt(-1))),
  '_w(alice, two)': _name('_w', WConst(WString('two'))),
  '_r(alice, f(Const(2)))': _name('_r', WStruct('f', [WConst(WInt(2))])),
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
    for (final MapEntry(key: shown, value: name) in _refused.entries) {
      test('a global name $shown is refused, the refusal naming it', () {
        expect(
            () => PayloadCodec.deserializeGlobalSendPayload(_gotValue(name)),
            throwsA(isA<GlobalNameRefusal>()
                .having((r) => r.name, 'name', shown)
                .having((r) => r.message, 'message', contains(shown))));
      });
    }

    test('a global name _w(alice, 2.0) as the whole value is refused', () {
      final payload = encodeMessageToBytes(WireValueMessage(WireAssignment(
        gIsReader: true,
        gAgent: Uint8List.fromList(utf8.encode('alice')),
        gIndex: 1,
        value: _name('_w', WConst(WFloat(2.0))),
      )));
      expect(() => PayloadCodec.deserializeGlobalSendPayload(payload),
          throwsA(isA<GlobalNameRefusal>()));
    });

    test('a global name _w(alice, 2) that travelled as a structure is '
        'received as the name', () {
      final (_, value) = PayloadCodec.deserializeGlobalSendPayload(
          _gotValue(_name('_w', WConst(WInt(2)))));
      expect(extractGlobalNames(value).map((g) => '$g'), ['_w(alice, 2)']);
    });

    test('a tag-2 variable is received as its name', () {
      final (_, value) = PayloadCodec.deserializeGlobalSendPayload(_gotValue(
          WVar.symbolic(isReader: false, agent: 'alice', index: 2)));
      expect(extractGlobalNames(value).map((g) => '$g'), ['_w(alice, 2)']);
    });
  });

  test('GlobalName.fromStruct presents no name whose index is not a '
      'non-negative integer', () {
    for (final index in <Object>[2.0, -1, 'two']) {
      expect(
          GlobalName.fromStruct(
              StructTerm('_w', [ConstTerm('alice'), ConstTerm(index)])),
          isNull,
          reason: '_w(alice, $index)');
    }
    expect(
        '${GlobalName.fromStruct(StructTerm('_w', [ConstTerm('alice'), ConstTerm(2)]))}',
        '_w(alice, 2)');
  });

  group('a value on a link whose entry stands', () {
    late GlpRuntime rt;
    late MadContext bob;
    late HeapCell writer;
    setUp(() {
      rt = GlpRuntime();
      bob = MadContext(agentId: 'bob', runtime: rt);
      writer = rt.heap.allocateVariable().$1;
      bob.wp.addLocalizeEntry(writer, 'alice', 1);
    });

    test('carrying _w(alice, 2.0) is refused at receipt, the refusal naming '
        'it, and nothing is received', () {
      final printed = _printed(() => bob.handleIncomingPayload(
          payload: _gotValue(_name('_w', WConst(WFloat(2.0)))),
          fromAgent: 'alice'));
      expect(printed, [
        '[REFUSED] bob: a message from alice is refused at receipt: global '
            'name _w(alice, 2.0) has index 2.0, which is not a non-negative '
            'integer'
      ]);
      expect(rt.heap.isFullyBound(writer), isFalse,
          reason: 'nothing is assigned');
      expect(bob.wp.findByRemote('alice', 1), isNotNull,
          reason: 'no Receive took the message: the entry stands');
      expect(bob.mp.totalLength, 0, reason: 'nothing is acknowledged');
    });

    test('carrying _w(alice, 2) is received: the writer is assigned', () {
      final printed = _printed(() => bob.handleIncomingPayload(
          payload: _gotValue(_name('_w', WConst(WInt(2)))),
          fromAgent: 'alice'));
      expect(printed, isEmpty);
      expect(rt.heap.isFullyBound(writer), isTrue);
      expect(bob.wp.findByRemote('alice', 1), isNull);
    });
  });
}
