/// The walks of a term on a goal's path, on terms of 50,000 elements: storing
/// a term on the heap (HeapFCP.storeTermOnHeap), the canonical encoding and
/// its decoding (PayloadCodec.termToWire and wireToTerm, and codec.dart's
/// encodeTerm and decodeTerm), the variables and global names of a term and
/// their substitution (MadContext's globalization, mad_helpers'
/// globalizeTermWithResult, extractGlobalNames and localizeTermWithResult),
/// and printing (StructTerm.toString, which the madGLP traces print, and the
/// scheduler's text of a goal, which a failed goal carries).
///
/// Until 2026-10-02 each recursed a Dart frame or more a structure argument,
/// so a list of some tens of thousands of elements overflowed the Dart stack
/// (Integration #4 Code, 2026-10-02 19:05 UTC, E4; GLP #3 Cowork, 2026-10-02
/// 20:58 UTC, answering "19:05" Q3: "storeTermOnHeap and termToWire walk with a
/// stack of their own").  Each keeps a stack of its own now and visits a term
/// in the order the recursion did, so the heap's addresses, the encoding's
/// bytes and the order of a term's variables are as they were: the tests here
/// fix that order on small terms and the layout of §cf-terms on long ones
/// (IGLP code-format-fragment.tex, "Terms": "A globalized term is encoded by a
/// tagged recursion ... 3 structure --- string functor, clen arity n, then the
/// n argument encodings in order").
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:test/test.dart';

const int _n = 50000;

final String _root = File('../programs/self.glp').absolute.path;

/// The list [1, 2, ..., n] as a Dart term, its cells '.'/2 and its end nil.
Term _ints(int n) {
  Term t = ConstTerm('nil');
  for (var k = n; k >= 1; k--) {
    t = StructTerm('.', [ConstTerm(k), t]);
  }
  return t;
}

/// s(s(...s(0)...)), [depth] deep.
Term _nest(int depth) {
  Term t = ConstTerm(0);
  for (var i = 0; i < depth; i++) {
    t = StructTerm('s', [t]);
  }
  return t;
}

/// The elements of the list [t], a Dart term, in order; its end must be nil.
List<Object?> _elements(Term t) {
  final out = <Object?>[];
  var cur = t;
  while (cur is StructTerm && cur.functor == '.' && cur.args.length == 2) {
    out.add((cur.args[0] as ConstTerm).value);
    cur = cur.args[1];
  }
  expect(cur, isA<ConstTerm>());
  expect((cur as ConstTerm).value, 'nil');
  return out;
}

/// The depth of s(s(...s(0)...)), a Dart term.
int _depth(Term t) {
  var d = 0;
  var cur = t;
  while (cur is StructTerm && cur.functor == 's') {
    d++;
    cur = cur.args.single;
  }
  expect((cur as ConstTerm).value, 0);
  return d;
}

/// The encoding of [1, ..., n] as §cf-terms lays it out, written node by node.
Uint8List _listBytes(int n) {
  final w = WireWriter();
  for (var k = 1; k <= n; k++) {
    w.u8(3); // structure
    w.string('.');
    w.clen(2);
    w.u8(1); // constant
    w.u8(1); // integer
    w.i64(k);
  }
  w.u8(1); // constant
  w.u8(0); // nil
  return w.toBytes();
}

void main() {
  group('storeTermOnHeap', () {
    test("stores each argument's cells before its structure's, left to right",
        () {
      final heap = HeapFCP();
      final base = heap.HP;
      // f(a, g(b, c), d): a, then g's b and c and g, then d, then f.
      final addr = heap.storeTermOnHeap(StructTerm('f', [
        ConstTerm('a'),
        StructTerm('g', [ConstTerm('b'), ConstTerm('c')]),
        ConstTerm('d'),
      ]));
      // A cell's serial number is its place in the order of allocation.
      expect(addr.id, base + 5);
      final f = addr.content as StructTerm;
      expect(f.args.map((a) => (a as VarRef).addr.id),
          [base, base + 3, base + 4]);
      final g = (f.args[1] as VarRef).addr.content as StructTerm;
      expect(g.args.map((a) => (a as VarRef).addr.id), [base + 1, base + 2]);
      expect(((f.args[2] as VarRef).addr.content as ConstTerm).value, 'd');
    });

    test('stores a list of 50,000 elements', () {
      final heap = HeapFCP();
      final base = heap.HP;
      final addr = heap.storeTermOnHeap(_ints(_n));
      // The elements and the nil first, then the cells, the last one first.
      expect(addr.id, base + 2 * _n);
      final values = <Object?>[];
      Object cell = addr.content as Term;
      while (cell is StructTerm) {
        values.add(
            ((cell.args[0] as VarRef).addr.content as ConstTerm).value);
        cell = (cell.args[1] as VarRef).addr.content as Term;
      }
      expect((cell as ConstTerm).value, 'nil');
      expect(values, [for (var k = 1; k <= _n; k++) k]);
    });

    test('stores a nesting 50,000 deep', () {
      final heap = HeapFCP();
      var cell = heap.storeTermOnHeap(_nest(_n)).content as Term;
      var depth = 0;
      while (cell is StructTerm) {
        depth++;
        cell = (cell.args.single as VarRef).addr.content as Term;
      }
      expect(depth, _n);
    });
  });

  group('the canonical encoding', () {
    test('a list of 50,000 is encoded as §cf-terms lays it out', () {
      final bytes = encodeTermToBytes(PayloadCodec.termToWire(_ints(_n)));
      expect(bytes, _listBytes(_n));
    });

    test('a list of 50,000 decodes back to itself', () {
      final back = PayloadCodec.wireToTerm(decodeTermFromBytes(_listBytes(_n)));
      expect(_elements(back), [for (var k = 1; k <= _n; k++) k]);
    });

    test('a nesting 50,000 deep encodes and decodes', () {
      final bytes = encodeTermToBytes(PayloadCodec.termToWire(_nest(_n)));
      expect(bytes.length, _n * 4 + 10,
          reason: 'each s/1 is a tag, a one-byte length, s and a one-byte '
              'arity; the 0 a tag, a constant tag and eight bytes');
      expect(_depth(PayloadCodec.wireToTerm(decodeTermFromBytes(bytes))), _n);
    });

    test('a value message and a serializer message carrying 50,000 elements '
        'round-trip', () {
      final (g, value) = PayloadCodec.deserializeGlobalSendPayload(
          PayloadCodec.createGlobalSendPayload(
              GlobalName.reader('alice', 7), _ints(_n)));
      expect('$g', '_r(alice, 7)');
      expect(_elements(value).length, _n);

      final (g0, cell) = PayloadCodec.deserializeGlobalSendPayload(
          PayloadCodec.createSerializerPayload(
              GlobalName.writer('bob', 0), _ints(_n)));
      expect('$g0', '_w(bob, 0)');
      final c = cell as StructTerm;
      expect(_elements(c.args[0]).length, _n);
      expect('${c.args[1]}', '_w(Const(bob),Const(0))');
    });
  });

  group('global names', () {
    test('50,000 global names in a term are met in order of occurrence, and '
        'each becomes its variable', () {
      Term names = ConstTerm('nil');
      for (var i = _n; i >= 1; i--) {
        names = StructTerm('.', [
          StructTerm('_r', [ConstTerm('alice'), ConstTerm(i)]),
          names
        ]);
      }
      final found = extractGlobalNames(names);
      expect(found.map((g) => g.index), [for (var i = 1; i <= _n; i++) i]);

      // A fresh pair for each name, as Localize makes them, the reader to take
      // each name's place (Definition Localize, case 2).
      final rt = GlpRuntime();
      final pairs = [
        for (var i = 0; i < _n; i++)
          (() {
            final (w, r) = rt.heap.allocateVariable();
            return FreshPair(w, r);
          })()
      ];
      final result = LocalizeResult(
          freshPairs: pairs,
          useReader: List.filled(_n, true),
          spawns: const []);
      var local = localizeTermWithResult(names, found, result);
      for (var i = 0; i < _n; i++) {
        final cell = local as StructTerm;
        expect((cell.args[0] as VarRef).addr, result.freshPairs[i].readerAddr);
        local = cell.args[1];
      }
      expect((local as ConstTerm).value, 'nil');
    });

    test("50,000 variables of a term are globalized in order of occurrence", () {
      final rt = GlpRuntime();
      final ctx = MadContext(agentId: 'alice', runtime: rt);
      final payloads = <List<int>>[];
      ctx.onMessageReady = (_, msg) => payloads.add(msg.payload);
      Term readers = ConstTerm('nil');
      final addrs = <HeapCell>[];
      for (var i = 0; i < _n; i++) {
        addrs.add(rt.heap.allocateVariable().$2);
      }
      for (var i = _n - 1; i >= 0; i--) {
        readers = StructTerm('.', [VarRef(addrs[i]), readers]);
      }
      ctx.send(readers, false, 'bob', 3, 'bob');
      ctx.flushMessages();
      final (_, sent) =
          PayloadCodec.deserializeGlobalSendPayload(payloads.single);
      final names = extractGlobalNames(sent);
      expect(names.map((g) => '$g'),
          [for (var i = 1; i <= _n; i++) '_r(alice, $i)'],
          reason: 'Globalize allocates the indices in the order the readers '
              'occur, from 1');
    });
  });

  group('printing', () {
    test("StructTerm.toString of a list of 50,000", () {
      final expected = StringBuffer();
      for (var k = 1; k <= _n; k++) {
        expected.write('.(Const($k),');
      }
      expected.write('Const(nil)');
      for (var k = 1; k <= _n; k++) {
        expected.write(')');
      }
      expect(_ints(_n).toString(), expected.toString());
      expect(StructTerm('f', []).toString(), 'f()');
      expect(
          StructTerm('f', [
            StructTerm('g', [
              ConstTerm('x'),
              VarRef(HeapCell(null, CellTag.WrtTag, 3))
            ]),
            ConstTerm(2)
          ]).toString(),
          'f(g(Const(x),Var@3),Const(2))');
    });

    test('a failed goal holding a list of 50,000 carries its text', () async {
      final e = GlpEngine(rootSelfGlpPath: _root)
        ..loadSource('''
procedure gen(Integer?, Stream(Integer), Done).
gen(0, [], done).
gen(N, [N?|Xs?], D?) :- N? > 0 | N1 := N? - 1, gen(N1?, Xs, D).

procedure fail_(Integer?).
fail_(N) :- gen(N?, Xs, D), fail_done(D?, Xs?).
procedure fail_done(Done?, Stream(Integer)?).
fail_done(done, Xs) :- empty(Xs?).
procedure empty(Stream(Integer)?).
empty(Xs) :- Xs? =?= [] | true.
''');
      e.maxCycles = 10000000;
      final r = await e.runGoal('fail_($_n)');
      expect(r.error, isNull);
      expect(r.status, ExecutionStatus.failed);
      // The list's cells on the heap have variables for tails, each shown
      // after ` | ` (Scheduler's text of a term, unchanged).
      final expected = StringBuffer();
      for (var k = _n; k >= 1; k--) {
        expected.write('[$k | ');
      }
      expected.write('[]');
      for (var k = 1; k <= _n; k++) {
        expected.write(']');
      }
      final failed = e.runtime.failedGoals.single;
      expect(failed, endsWith('(${expected.toString()})'));
      expect(failed, startsWith('empty'));
    });
  });

  group('sign/3 and signature/2', () {
    test('sign and signature of a list of 50,000 give it back, in order',
        () async {
      final out = <String>[];
      final identity = PersonIdentity.generate();
      final e = GlpEngine(rootSelfGlpPath: _root, identity: identity);
      e.enableMadGLP(agentId: 'alice');
      e.runtime.outputCallback = out.add;
      final dirOf = NetworkDirectory()..register('alice', identity.pub);
      final client = SimulationNetworkClient(
          selfId: 'alice', directory: dirOf, sendToRouter: (_, __) {});
      client.putIdentity(identity.pub, identity.priv);
      e.madContext!.network = client;
      e.maxCycles = 10000000;
      final dir = Directory.systemTemp.createTempSync('glp_long_sign_');
      try {
        final f = File('${dir.path}/long_sign.glp')
          ..writeAsStringSync('''
procedure gen(Integer?, Stream(Integer), Done).
gen(0, [], done).
gen(N, [N?|Xs?], D?) :- N? > 0 | N1 := N? - 1, gen(N1?, Xs, D).

procedure long_sign(Integer?).
long_sign(N) :- gen(N?, Xs, D), sign_done(D?, Xs?).
procedure sign_done(Done?, Stream(Integer)?).
sign_done(done, Xs) :- self_key(K), sign(Xs?, K?, S), signature(S?, Sig),
    report(Sig?).
procedure report(Signature?).
report(signed(_, _, T)) :- send_to_user([T?]).
report(unsigned) :- send_to_user([unsigned]).
''');
        e.loadFile(f.path);
        final r = await e.runGoal('long_sign($_n)');
        expect(r.error, isNull);
        expect(r.status, ExecutionStatus.succeeded);
        expect(out, hasLength(1));
        final items =
            out.single.substring(1, out.single.length - 1).split(', ');
        expect(items, [for (var k = _n; k >= 1; k--) '$k']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });
  });
}
