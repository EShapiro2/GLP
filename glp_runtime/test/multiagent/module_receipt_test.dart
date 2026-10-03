/// A module a message carries is a Module value only where its certificate
/// verifies, and a message carrying one whose certificate does not is refused
/// at receipt, with the reason (GLP #3 Cowork, 2026-10-02 20:58 UTC, answering
/// Integration's 20:31 UTC item 4: "a received module whose certificate does
/// not verify is not a Module value and the message carrying it is refused at
/// receipt, with the reason, as IGLP's loader refuses at adoption; nothing is
/// delivered as text").
///
/// The certificate is checked as the loader's step 1 checks it (IGLP
/// code-format-fragment.tex, Loader: "Computes SHA-256 of the body and
/// verifies it equals the compiled identity in the certificate; verifies the
/// certificate's signature under the key the certificate carries"), over the
/// body as it arrived; a module refused a certificate carries none.
///
/// The refusal is at the decoding of the payload, before any Receive: "A
/// payload is one assignment message in the canonical encoding" (IGLP
/// app:in-networking, "Payloads"), a module constant decodes "to the Module
/// constant" (§cf-terms, constant tag 6), and a module that is no Module value
/// leaves the payload no message, so no Receive transaction (Definition
/// "madGLP Receive Transaction") takes it.  The receiver assigns nothing,
/// keeps the entry, acknowledges nothing, holds and reports nothing, and its
/// runtime prints the refusal; the sender is told nothing, a reader-name value
/// staying pending.
///
/// Until 2026-10-02 the decoding made any module a message carried a Module
/// value, unchecked: since b2af4559 such a module never ran, but
/// decompose_module/4 reported its forged key (GLP-Spec appendix-guards.tex:
/// "A module value therefore cannot be forged by writing a file: a file whose
/// certificate does not check is text and is not a Module, which is what
/// decompose_module/4 rests on").
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/bytecode/opcodes.dart' as op;
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:test/test.dart';

final PersonIdentity _compiler = PersonIdentity.generate();

/// A compiled program exporting hello/1, its certificate [signer]'s, or
/// refused one where [signer] is null.
Artefact _artefact(PersonIdentity? signer) => Artefact.fromCompiled(
      ops: [
        op.Label('hello/1'),
        op.ClauseTry(),
        op.HeadConstant('done', 0),
        op.Commit(),
        op.Proceed(),
        op.Label('hello/1_end'),
        op.NoMoreClauses(),
      ],
      hM: Uint8List(32),
      moduleName: 'probe',
      isaVersion: glpIsaVersion,
      exports: const [ArtefactExport('hello', 1, 'procedure hello(Constant).')],
      signer: signer,
    );

/// The certified artefact's bytes, with the byte at [offset] from the end
/// complemented.
Uint8List _forgedAt(int offsetFromEnd) {
  final b = Uint8List.fromList(_artefact(_compiler).toBytes());
  final i = b.length - offsetFromEnd;
  b[i] = b[i] ^ 0xff;
  return b;
}

/// The certified artefact's bytes with its module name changed, a byte of the
/// body: it still parses, and no longer hashes to its compiled identity.
Uint8List _forgedBody() {
  final b = Uint8List.fromList(_artefact(_compiler).toBytes());
  final name = utf8.encode('probe');
  for (var i = 0; i + name.length <= b.length; i++) {
    var hit = true;
    for (var j = 0; j < name.length; j++) {
      if (b[i + j] != name[j]) {
        hit = false;
        break;
      }
    }
    if (hit) {
      b[i] = 'q'.codeUnitAt(0);
      return b;
    }
  }
  throw StateError('module name not found in the artefact');
}

/// The bytes of a module constant: certified, refused a certificate, a body
/// forged, a signature forged, and bytes that are no artefact; each with the
/// reason it is refused, or null.
final Map<String, (Uint8List, String?)> _modules = {
  'certified': (_artefact(_compiler).toBytes(), null),
  'refused a certificate': (
    _artefact(null).toBytes(),
    'it carries no certificate (it was refused one, or no one compiled it for '
        'a person)'
  ),
  'a body that does not hash to its compiled identity': (
    _forgedBody(),
    'its body does not hash to the compiled identity its certificate names'
  ),
  'a signature that does not verify': (
    _forgedAt(1),
    "its certificate's signature does not verify under the key it carries"
  ),
  'bytes that are no artefact': (
    Uint8List.fromList([1, 2, 3]),
    'it is not an artefact'
  ),
};

/// A value message to the reader name `_r(alice, 1)`: ship(M), M the module
/// constant whose artefact bytes are [bytes].
Uint8List _shipValue(Uint8List bytes) =>
    encodeMessageToBytes(WireValueMessage(WireAssignment(
      gIsReader: true,
      gAgent: Uint8List.fromList(utf8.encode('alice')),
      gIndex: 1,
      value: WStruct('ship', [WConst(WModule(bytes))]),
    )));

/// A cold call to bob carrying msg(bob, ship(M)).
Uint8List _shipColdCall(Uint8List bytes) =>
    encodeMessageToBytes(WireValueMessage(WireAssignment.serializer(
      agent: Uint8List.fromList(utf8.encode('bob')),
      head: WStruct('msg', [
        WConst(WString('bob')),
        WStruct('ship', [WConst(WModule(bytes))]),
      ]),
    )));

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
    for (final MapEntry(key: what, value: (bytes, reason)) in _modules.entries) {
      test('a module $what ${reason == null ? 'is a Module value' : 'is refused'}',
          () {
        final payload = _shipValue(bytes);
        if (reason == null) {
          final (g, value) = PayloadCodec.deserializeGlobalSendPayload(payload);
          expect('$g', '_r(alice, 1)');
          final m = (value as StructTerm).args.single;
          expect(m, isA<ModuleTerm>());
          expect((m as ModuleTerm).name, 'probe');
          expect((m.artefact as Artefact).compiledIdentity,
              _artefact(_compiler).compiledIdentity);
        } else {
          expect(
              () => PayloadCodec.deserializeGlobalSendPayload(payload),
              throwsA(isA<ModuleRefusal>()
                  .having((r) => r.reason, 'reason', startsWith(reason))));
        }
      });
    }
  });

  group('a value on a link whose entry stands', () {
    late GlpRuntime rt;
    late MadContext bob;
    late int writer;
    setUp(() {
      rt = GlpRuntime();
      bob = MadContext(agentId: 'bob', runtime: rt);
      writer = rt.heap.allocateVariable().$1;
      bob.wp.addLocalizeEntry(writer, 'alice', 1);
    });

    test('carrying a certified module is received: the writer is assigned, '
        'the entry removed and the value acknowledged', () {
      final printed = _printed(() => bob.handleIncomingPayload(
          payload: _shipValue(_modules['certified']!.$1), fromAgent: 'alice'));
      expect(printed, isEmpty);
      final v = rt.heap.getValue(writer) as StructTerm;
      expect(v.functor, 'ship');
      expect(v.args.single, isA<ModuleTerm>());
      expect(bob.wp.findByRemote('alice', 1), isNull);
      expect(bob.mp.totalLength, 1, reason: 'ack(_r(alice, 1))');
    });

    for (final MapEntry(key: what, value: (bytes, reason))
        in _modules.entries.where((e) => e.value.$2 != null)) {
      test('carrying a module $what is refused at receipt, with the reason, '
          'and nothing is received', () {
        final printed = _printed(() => bob.handleIncomingPayload(
            payload: _shipValue(bytes), fromAgent: 'alice'));
        expect(printed, hasLength(1));
        expect(printed.single,
            startsWith('[REFUSED] bob: a message from alice is refused at '
                'receipt: '));
        expect(printed.single, contains(reason!));
        expect(rt.heap.isFullyBound(writer), isFalse,
            reason: 'nothing is delivered, as a Module or as text');
        expect(bob.wp.findByRemote('alice', 1), isNotNull,
            reason: 'no Receive took the message: the entry stands');
        expect(bob.mp.totalLength, 0, reason: 'nothing is acknowledged');
      });
    }
  });

  test('a cold call carrying a forged module is refused, and the next one '
      'is the first element of the network input', () {
    final rt = GlpRuntime();
    final bob = MadContext(agentId: 'bob', runtime: rt);
    final netIn = rt.heap.allocateVariable().$1;
    bob.wp.initializeSerializerEntry(netIn);

    final printed = _printed(() => bob.handleIncomingPayload(
        payload: _shipColdCall(_forgedBody()), fromAgent: 'alice'));
    expect(printed.single, contains('is refused at receipt'));
    expect(rt.heap.isFullyBound(netIn), isFalse);
    expect(bob.wp.serializerWriterAddr, netIn);

    bob.handleIncomingPayload(
        payload: _shipColdCall(_modules['certified']!.$1), fromAgent: 'alice');
    final cell = rt.heap.getValue(netIn) as StructTerm;
    final msg = rt.heap.dereference(cell.args[0]) as StructTerm;
    expect((msg.args[1] as StructTerm).args.single, isA<ModuleTerm>());
  });

  group('two agents in isolates', () {
    late IsolateManager manager;
    setUp(() => manager = IsolateManager());
    tearDown(() async => manager.shutdown());

    test("alice's own module, refused a certificate, is refused at bob's "
        'receipt, and her next message is received', () async {
      // programs/tests/mad_ship_refused: alice's program calls send_to_net/1,
      // OS-privileged, so it is refused a certificate (IGLP
      // code-format-fragment.tex, Program Artefact); she sends bob her module
      // value and then the atom after, and bob reports each message he
      // receives.  At f69ae04b bob reported got_module and after.
      final boot = File('../programs/tests/mad_ship_refused_boot.glp');
      final config = BootLoader().load(boot.readAsStringSync());
      config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
      config.programDir =
          File('../programs/tests/mad_ship_refused').absolute.path;
      await manager.boot(config);
      manager.start();
      await manager.settle();
      expect(manager.faults, isEmpty);
      expect(manager.outputOf('bob'), ['after'],
          reason: 'the message carrying the module is refused at receipt and '
              "never reaches bob's network input; the next one does");
    }, timeout: const Timeout(Duration(seconds: 60)));
  });
}
