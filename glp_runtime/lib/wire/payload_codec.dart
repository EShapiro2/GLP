/// The madGLP payloads over the wire codec: an assignment message in the
/// canonical encoding (IGLP app:in-networking, "Payloads").
///
/// Normative source: the IGLP paper appendix `app:code-format`, §§cf-primitives,
/// cf-terms (IGLP db0a117, code-format version 4). Variables travel as global
/// names per Definition Globalize: a tag-2 variable with a u8 polarity (0
/// writer `_w(p,e,i)`, 1 reader `_r(p,e,i)`), the agent, a clen epoch and a
/// clen index. There is no original-creator identifier, no paired-reader
/// field, and no serializer string marker — the serializer message's tail is
/// the encoded variable `_w(q,0,0)`.  Every message carries the epoch of its
/// sender after its kind byte, and a fourth kind, gone, answers traffic for
/// an end the receiver lost.
///
/// On the madGLP send path a term is globalized before serialization
/// (mad_context), so its variables are already global names, each a
/// [GlobalNameTerm].  This codec maps those to the codec's tag-2 variable
/// form, and maps tag-2 variables back to them on receipt, for the localize
/// machinery (extractGlobalNames / localizeTermWithResult).  A structure, a
/// program's ground `'_w'(P, I)` or `'_r'(P, I)` among them, is a tag-3
/// structure both ways: "A ground structure `_w(P, I)` or `_r(P, I)` that a
/// program puts in a message is a term, encoded as a structure and arriving
/// as one ...; only a variable is encoded as a global name" (IGLP
/// madglp-spec.tex:17, at IGLP 1e01638).
library;

import 'dart:convert';
import 'dart:typed_data';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/wire/artefact.dart' show Artefact, ModuleRefusal;
import 'package:glp_runtime/wire/codec.dart';

class PayloadCodec {
  // ==========================================================================
  // madGLP assignment payloads
  // ==========================================================================

  /// Encode the value message `G := T↑` (kind 0): the u8 kind, the clen
  /// epoch of the sender, the global name G (polarity, agent, epoch, index),
  /// then the encoding of T.
  static List<int> createGlobalSendPayload(GlobalName globalName, Term value,
      {required int senderEpoch}) {
    return encodeMessageToBytes(WireValueMessage(
        WireAssignment(
          gIsReader: globalName.isReader,
          gAgent: _agentBytes(globalName.agent),
          gEpoch: globalName.epoch,
          gIndex: globalName.index,
          value: termToWire(value),
        ),
        senderEpoch: senderEpoch));
  }

  /// Encode a serializer (cold-call) message to agent q: the value message
  /// `_w(q,0) := [T↑ | _w(q,0)]` under the sender's [serial], "an index the
  /// sender allocates from its global-name counter" in its epoch
  /// [senderEpoch], "so that (p, e, i) is unique for the whole run"
  /// (§cf-terms, Messages; Remark "Cold-Call via Serializer").  The tail is
  /// the encoded variable `_w(q,0,0)`.
  static List<int> createSerializerPayload(
      GlobalName serializerName, Term content,
      {required int serial, required int senderEpoch}) {
    assert(serializerName.isSerializer,
        'Serializer payload requires _w(agent, 0, 0) global name');
    return encodeMessageToBytes(WireValueMessage(
        WireAssignment.serializer(
          agent: _agentBytes(serializerName.agent),
          head: termToWire(content),
          serial: serial,
        ),
        senderEpoch: senderEpoch));
  }

  /// Encode a request `req(_r(p,e,i))` (kind 1): u8 kind, clen sender epoch,
  /// polarity (1), agent, clen epoch, clen index — no term.
  static List<int> createRequestPayload(GlobalName globalName,
      {required int senderEpoch}) {
    assert(globalName.isReader, 'a request carries a reader name');
    return encodeMessageToBytes(WireRequestMessage.symbolic(
        agent: globalName.agent,
        epoch: globalName.epoch,
        index: globalName.index,
        senderEpoch: senderEpoch));
  }

  /// Encode an acknowledgement `ack(_r(p,e,i))` (kind 2): u8 kind, clen
  /// sender epoch, polarity (1), agent, clen epoch, clen index — no term.
  static List<int> createAckPayload(GlobalName globalName,
      {required int senderEpoch}) {
    assert(globalName.isReader, 'an acknowledgement carries a reader name');
    return encodeMessageToBytes(WireAckMessage.symbolic(
        agent: globalName.agent,
        epoch: globalName.epoch,
        index: globalName.index,
        senderEpoch: senderEpoch));
  }

  /// Encode a `gone(G)` (kind 3), which answers traffic for the end G its
  /// sender lost (IGLP db0a117, "Requests and Acknowledgements"): u8 kind,
  /// clen sender epoch, polarity of G, agent, clen epoch, clen index — no
  /// term.
  static List<int> createGonePayload(GlobalName globalName,
      {required int senderEpoch}) {
    return encodeMessageToBytes(WireGoneMessage.symbolic(
        isReader: globalName.isReader,
        agent: globalName.agent,
        epoch: globalName.epoch,
        index: globalName.index,
        senderEpoch: senderEpoch));
  }

  /// Decode a request, acknowledgement or gone payload (kind 1, 2 or 3) to
  /// its kind, the global name it carries and its sender's epoch. Throws on a
  /// value message or malformed bytes, and [GlobalNameRefusal] where the
  /// name's agent is not an atom.
  static ({int kind, GlobalName name, int senderEpoch}) decodeLinkPayload(
      List<int> payload) {
    final m = decodeMessageFromBytes(_asUint8(payload));
    if (m is! WireLinkMessage) {
      throw WireFormatException(
          'expected request/acknowledgement/gone, got value message');
    }
    final agent = _agentAtom(m.agent, m.isReader, m.epoch, m.index);
    return (
      kind: m.kind,
      name: m.isReader
          ? GlobalName.reader(agent, m.index, epoch: m.epoch)
          : GlobalName.writer(agent, m.index, epoch: m.epoch),
      senderEpoch: m.senderEpoch,
    );
  }

  /// The epoch of the sender a message [payload] carries after its kind
  /// byte, whatever its kind.
  static int senderEpochOf(List<int> payload) {
    final r = WireReader(_asUint8(payload));
    r.u8();
    return r.clen();
  }

  /// [payload] with its sender's epoch rewritten to [senderEpoch]
  /// ([messageWithSenderEpoch]).
  static Uint8List withSenderEpoch(List<int> payload, int senderEpoch) =>
      messageWithSenderEpoch(_asUint8(payload), senderEpoch);

  /// Decode a global-send (value) payload to (GlobalName, Term). Embedded
  /// variables return as [GlobalNameTerm]s for the localize machinery, and
  /// structures as structures. Throws on a request or acknowledgement message.
  ///
  /// This is the decoding of a received message, and a module constant it
  /// carries decodes to a Module value only where the module's artefact is a
  /// certified compiled program, as the loader's step 1 checks it
  /// ([Artefact.certify]); where one is not, the payload is no message and
  /// [ModuleRefusal] is thrown with the reason: "a received module whose
  /// certificate does not verify is not a Module value and the message
  /// carrying it is refused at receipt, with the reason ... nothing is
  /// delivered as text" (GLP #3 Cowork, 2026-10-02 20:58 UTC).  Until
  /// 2026-10-02 any module a message carried decoded to a Module value.
  ///
  /// A global name the message carries whose agent is not an atom leaves the
  /// payload no message in the same way, and [GlobalNameRefusal] is thrown,
  /// naming the name ([_agentAtom]): "An agent is an atom and an index is a
  /// non-negative integer, and the reader refuses a global name whose agent
  /// or index is anything else" (IGLP code-format-fragment.tex, Terms, tag 2).
  /// An index is a clen, a non-negative integer by its encoding.
  ///
  /// A serializer message's serial is not returned; the Receive transaction
  /// takes it from [deserializeValuePayload].
  static (GlobalName, Term) deserializeGlobalSendPayload(List<int> payload) {
    final (:name, :value, serial: _, senderEpoch: _) =
        deserializeValuePayload(payload);
    return (name, value);
  }

  /// [deserializeGlobalSendPayload] with the serial and the sender's epoch: a
  /// serializer message to `_w(q,0)` carries the serial its sender allocated,
  /// which with the sender and its epoch names the cold call for the whole
  /// run (§cf-terms, Messages); [serial] is null for every other value
  /// message.
  static ({GlobalName name, Term value, int? serial, int senderEpoch})
      deserializeValuePayload(List<int> payload) {
    final m = decodeMessageFromBytes(_asUint8(payload));
    if (m is! WireValueMessage) {
      throw WireFormatException('expected value message (kind 0)');
    }
    final a = m.assignment;
    final agent = _agentAtom(a.gAgent, a.gIsReader, a.gEpoch, a.gIndex);
    final globalName = a.gIsReader
        ? GlobalName.reader(agent, a.gIndex, epoch: a.gEpoch)
        : GlobalName.writer(agent, a.gIndex, epoch: a.gEpoch);
    return (
      name: globalName,
      value: _wireToTerm(a.value, receipt: true),
      serial: a.serial,
      senderEpoch: m.senderEpoch,
    );
  }

  /// Serialize a ground term as a canonical agent-message payload. Throws if a
  /// variable is present (`termToWire` rejects a VarRef; a ground term carries
  /// no global name, and a structure `'_w'(P, I)` in it is a structure). The
  /// bytes are agent-independent.
  static List<int> serializeAgentMessage(Term term) {
    return encodeTermToBytes(termToWire(term));
  }

  // ==========================================================================
  // Term <-> wire mapping
  // ==========================================================================

  /// Map a runtime [Term] to a [WireTerm]. A global name in the place of a
  /// variable, a [GlobalNameTerm], becomes a tag-2 variable; every structure,
  /// `'_w'(P, I)` and `'_r'(P, I)` among them, stays a structure, so a ground
  /// term maps to tags 1 and 3 only (IGLP code-format-fragment.tex, Terms:
  /// "a ground term contains tags 1 and 3 only").  Until 2026-11-02 a
  /// structure `'_w'(p, i)` with an atom and an integer for arguments became
  /// a tag-2 variable.
  ///
  /// The walk keeps a stack of its own, a frame for each structure being
  /// mapped, and takes each structure's arguments left to right, as the
  /// recursion it replaces did, so a term's encoding is the same byte for
  /// byte.  Until 2026-10-02 it recursed once a structure argument, and a
  /// nesting 2,000 deep overflowed the Dart stack (dart run, at f69ae04b), a
  /// list being as deep as it is long (GLP #3 Cowork, 2026-10-02 20:58 UTC,
  /// answering Integration's 19:05 UTC Q3: "storeTermOnHeap and termToWire
  /// walk with a stack of their own").
  static WireTerm termToWire(Term term) {
    final leaf = _leafToWire(term);
    if (leaf != null) return leaf;
    // Each frame: a structure, and its arguments mapped so far.
    final frames = <(StructTerm, List<WireTerm>)>[
      (term as StructTerm, <WireTerm>[])
    ];
    WireTerm? mapped; // the structure just mapped, for its parent
    while (true) {
      final (source, args) = frames.last;
      if (mapped != null) {
        args.add(mapped);
        mapped = null;
      }
      if (args.length == source.args.length) {
        frames.removeLast();
        final w = WStruct(source.functor, args);
        if (frames.isEmpty) return w;
        mapped = w;
        continue;
      }
      final arg = source.args[args.length];
      final l = _leafToWire(arg);
      if (l != null) {
        args.add(l);
      } else {
        frames.add((arg as StructTerm, <WireTerm>[]));
      }
    }
  }

  /// [termToWire] of [term] where it maps without descending: a constant, a
  /// module, or a global name; null for a structure, whose arguments are
  /// mapped in turn.  A variable, or a term of any other kind, is refused.
  static WireTerm? _leafToWire(Term term) {
    if (term is ConstTerm) {
      return WConst(_constToWire(term.value));
    } else if (term is ModuleTerm) {
      // §cf-terms constant tag 6: a Module constant travels as its artefact
      // bytes — the form in which compiled programs ship.
      return WConst(WModule((term.artefact as Artefact).toBytes()));
    } else if (term is GlobalNameTerm) {
      return WVar.symbolic(
          isReader: term.isReader,
          agent: term.agent,
          epoch: term.epoch,
          index: term.index);
    } else if (term is StructTerm) {
      return null;
    } else if (term is VarRef) {
      throw WireFormatException(
          'non-globalized VarRef on the wire: @${term.addr} '
          '(terms must be globalized before serialization)');
    } else {
      throw WireFormatException(
          'cannot serialize term type: ${term.runtimeType}');
    }
  }

  /// Map the encoding of a ground term back to a runtime [Term].  A ground
  /// term contains tags 1 and 3 only (IGLP code-format-fragment.tex, Terms),
  /// so a tag-2 variable is refused, [WireFormatException]: its users read
  /// ground content (signature/2, the adoption vocabulary), and only a
  /// received message carries a global name ([deserializeValuePayload]).  Until
  /// 2026-11-02 a tag-2 variable became the structure `'_w'(p, i)` here.
  ///
  /// A module constant maps to the Module value its artefact parses to, its
  /// certificate unchecked.  The decoding of a received message checks it
  /// ([deserializeGlobalSendPayload]); this mapping's other users are
  /// signature/2's reading of a signed term (body_kernels.dart) and the
  /// adoption vocabulary's decodeMessage, which are no receipt of a message,
  /// and whether a signed term's module is checked is put to GLP (2026-10-02).
  ///
  /// The walk keeps a stack of its own, as [termToWire] does, and builds each
  /// structure over its arguments mapped left to right; until 2026-10-02 it
  /// recursed once a structure argument and overflowed the Dart stack on a
  /// long list.
  static Term wireToTerm(WireTerm w) => _wireToTerm(w, receipt: false);

  /// [wireToTerm]; where [receipt], the decoding of a received message
  /// ([deserializeValuePayload]), each module constant's artefact certified,
  /// and each tag-2 variable a [GlobalNameTerm] whose agent is an atom.
  ///
  /// A structure is a structure: "A ground structure `_w(P, I)` or `_r(P, I)`
  /// that a program puts in a message is a term, encoded as a structure and
  /// arriving as one" (IGLP madglp-spec.tex:17, at IGLP 1e01638).  At
  /// 9cd4549b, earlier on 2026-11-02, a structure `'_w'(P, I)` whose index was
  /// not a non-negative integer was refused here.
  static Term _wireToTerm(WireTerm w, {required bool receipt}) {
    if (w is! WStruct) return _leafToTerm(w, receipt: receipt);
    // Each frame: a structure, and its arguments mapped so far.
    final frames = <(WStruct, List<Term>)>[(w, <Term>[])];
    Term? mapped; // the structure just mapped, for its parent
    while (true) {
      final (source, args) = frames.last;
      if (mapped != null) {
        args.add(mapped);
        mapped = null;
      }
      if (args.length == source.args.length) {
        frames.removeLast();
        final t = StructTerm(source.functor, args);
        if (frames.isEmpty) return t;
        mapped = t;
        continue;
      }
      final arg = source.args[args.length];
      if (arg is WStruct) {
        frames.add((arg, <Term>[]));
      } else {
        args.add(_leafToTerm(arg, receipt: receipt));
      }
    }
  }

  /// [wireToTerm] of a constant or a variable; where [receipt], a module's
  /// artefact certified, and refused ([ModuleRefusal]) where it is no
  /// certified compiled program, and a variable a [GlobalNameTerm] whose
  /// agent is an atom ([_agentAtom]); where not, a variable refused.
  static Term _leafToTerm(WireTerm w, {required bool receipt}) {
    switch (w) {
      case WConst(:final constant):
        if (constant is WModule) {
          // §cf-terms constant tag 6 decodes to the Module constant itself.
          if (receipt) {
            final (:artefact, :refusal) =
                Artefact.certify(constant.artefactBytes);
            if (refusal != null) {
              throw ModuleRefusal(artefact?.moduleName, refusal);
            }
            return ModuleTerm(artefact!, name: artefact.moduleName);
          }
          final artefact = Artefact.fromBytes(constant.artefactBytes);
          return ModuleTerm(artefact, name: artefact.moduleName);
        }
        return ConstTerm(_constFromWire(constant));
      case WVar(:final isReader, :final agent, :final epoch, :final index):
        if (!receipt) {
          throw WireFormatException('a ground term contains tags 1 and 3 '
              'only, and this one carries the variable '
              '${_shownName(isReader, agent, epoch, index)}');
        }
        return GlobalNameTerm(
            isReader: isReader,
            agent: _agentAtom(agent, isReader, epoch, index),
            epoch: epoch,
            index: index);
      case WStruct():
        throw ArgumentError('a structure is mapped by wireToTerm, not here');
    }
  }

  /// The agent of the global name `_w(agent, index)` or `_r(agent, index)`
  /// ([isReader]) as an atom: "An agent is an atom ..., and the reader refuses
  /// a global name whose agent ... is anything else" (IGLP
  /// code-format-fragment.tex, Terms, tag 2).  An agent's bytes are, in the
  /// one realisation this runtime has, its UTF-8 symbolic name (§cf-primitives,
  /// agent; Implementation Notes: "agent identifiers in global names remain
  /// symbolic"); bytes that are no UTF-8 text name no atom, and
  /// [GlobalNameRefusal] is thrown, naming the name.
  static String _agentAtom(
      Uint8List agent, bool isReader, int epoch, int index) {
    try {
      return utf8.decode(agent);
    } on FormatException {
      throw GlobalNameRefusal(_shownName(isReader, agent, epoch, index));
    }
  }

  /// The global name as a diagnostic names it: its agent as text where the
  /// bytes are UTF-8, and as `0x` and their hexadecimal digits otherwise.
  static String _shownName(
      bool isReader, Uint8List agent, int epoch, int index) {
    String shownAgent;
    try {
      shownAgent = utf8.decode(agent);
    } on FormatException {
      shownAgent =
          '0x${agent.map((b) => b.toRadixString(16).padLeft(2, '0')).join()}';
    }
    return '${isReader ? '_r' : '_w'}($shownAgent, $epoch, $index)';
  }

  static WireConst _constToWire(Object? v) {
    // The empty list, the runtime's [nil], is constant tag 0; a string,
    // 'nil' among them, tag 3 ([wireConstFromValue]).
    if (v is Nil) return const WNil();
    if (v is int) return WInt(v);
    if (v is double) return WFloat(v);
    if (v is String) return WString(v);
    if (v is bool) return WBool(v);
    if (v is Uint8List) return WBlob(v);
    if (v is List<int>) return WBlob(Uint8List.fromList(v));
    throw WireFormatException(
        'cannot serialize constant of type ${v.runtimeType}');
  }

  static Object? _constFromWire(WireConst c) {
    switch (c) {
      case WNil():
        return nil;
      case WInt(:final value):
        return value;
      case WFloat(:final value):
        return value;
      case WString(:final value):
        return value;
      case WBool(:final value):
        return value;
      case WBlob(:final value):
        return value;
      case WModule():
        // Handled in wireToTerm — a module decodes to a ModuleTerm, never to
        // a ConstTerm payload.
        throw WireFormatException(
            'module constant reached the ConstTerm mapping');
    }
  }

  static Uint8List _agentBytes(String agent) =>
      Uint8List.fromList(utf8.encode(agent));

  static Uint8List _asUint8(List<int> b) =>
      b is Uint8List ? b : Uint8List.fromList(b);
}

/// A received message is refused: a global name it carries has an agent that
/// is not an atom (IGLP code-format-fragment.tex, Terms, tag 2: "the reader
/// refuses a global name whose agent or index is anything else").  Its index
/// and its epoch are clens, non-negative integers by their encoding.
class GlobalNameRefusal extends WireFormatException {
  /// The global name, as `_w(0xfffe, 1, 2)`.
  final String name;

  GlobalNameRefusal(this.name)
      : super('global name $name has an agent that is not an atom');

  @override
  String toString() => 'GlobalNameRefusal: $message';
}
