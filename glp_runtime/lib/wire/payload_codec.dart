/// The madGLP payloads over the wire codec: an assignment message in the
/// canonical encoding (IGLP app:in-networking, "Payloads").
///
/// Normative source: the IGLP paper appendix `app:code-format`, §§cf-primitives,
/// cf-terms. Variables travel as global names per Definition Globalize: a tag-2
/// variable with a u8 polarity (0 writer `_w(p,i)`, 1 reader `_r(p,i)`), the
/// agent, and a clen index. There is no original-creator identifier, no
/// paired-reader field, and no serializer string marker — the serializer
/// message's tail is the encoded variable `_w(q,0)`.
///
/// On the madGLP send path a term is globalized before serialization
/// (mad_context), so its variables are already `_w(p,i)` / `_r(p,i)` structures.
/// This codec maps those structures to the codec's tag-2 variable form, and
/// maps them back on receipt, so the localize machinery (extractGlobalNames /
/// localizeTermWithResult) is unchanged.
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

  /// Encode the value message `G := T↑` (kind 0): the u8 kind, the global
  /// name G (polarity, agent, index), then the encoding of T.
  static List<int> createGlobalSendPayload(GlobalName globalName, Term value) {
    return encodeMessageToBytes(WireValueMessage(WireAssignment(
      gIsReader: globalName.isReader,
      gAgent: _agentBytes(globalName.agent),
      gIndex: globalName.index,
      value: termToWire(value),
    )));
  }

  /// Encode a serializer (cold-call) message to agent q: the value message
  /// `_w(q,0) := [T↑ | _w(q,0)]`. The tail is the encoded variable `_w(q,0)`.
  static List<int> createSerializerPayload(
      GlobalName serializerName, Term content) {
    assert(serializerName.isWriter && serializerName.index == 0,
        'Serializer payload requires _w(agent, 0) global name');
    return encodeMessageToBytes(WireValueMessage(WireAssignment.serializer(
      agent: _agentBytes(serializerName.agent),
      head: termToWire(content),
    )));
  }

  /// Encode a request `req(_r(p,i))` (kind 1): u8 kind, polarity (1), agent,
  /// clen index — no term.
  static List<int> createRequestPayload(GlobalName globalName) {
    assert(globalName.isReader, 'a request carries a reader name');
    return encodeMessageToBytes(WireRequestMessage.symbolic(
        agent: globalName.agent, index: globalName.index));
  }

  /// Encode an acknowledgement `ack(_r(p,i))` (kind 2): u8 kind, polarity (1),
  /// agent, clen index — no term.
  static List<int> createAckPayload(GlobalName globalName) {
    assert(globalName.isReader, 'an acknowledgement carries a reader name');
    return encodeMessageToBytes(WireAckMessage.symbolic(
        agent: globalName.agent, index: globalName.index));
  }

  /// Decode a request or acknowledgement payload (kind 1 or 2) to its reader
  /// global name. Throws on a value message or malformed bytes.
  static GlobalName decodeRequestOrAckPayload(List<int> payload) {
    final m = decodeMessageFromBytes(_asUint8(payload));
    return switch (m) {
      WireRequestMessage(:final agentString, :final index) =>
        GlobalName.reader(agentString, index),
      WireAckMessage(:final agentString, :final index) =>
        GlobalName.reader(agentString, index),
      WireValueMessage() => throw WireFormatException(
          'expected request/acknowledgement, got value message'),
    };
  }

  /// Decode a global-send (value) payload to (GlobalName, Term). Embedded
  /// variables return as `_w(p,i)` / `_r(p,i)` structures for the localize
  /// machinery. Throws on a request or acknowledgement message.
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
  static (GlobalName, Term) deserializeGlobalSendPayload(List<int> payload) {
    final m = decodeMessageFromBytes(_asUint8(payload));
    if (m is! WireValueMessage) {
      throw WireFormatException('expected value message (kind 0)');
    }
    final a = m.assignment;
    final agent = utf8.decode(a.gAgent);
    final globalName = a.gIsReader
        ? GlobalName.reader(agent, a.gIndex)
        : GlobalName.writer(agent, a.gIndex);
    return (globalName, _wireToTerm(a.value, certify: true));
  }

  /// Serialize a ground term as a canonical agent-message payload. Throws if a
  /// variable is present (`termToWire` rejects a VarRef; a ground term carries
  /// no `_w`/`_r` global names). The bytes are agent-independent.
  static List<int> serializeAgentMessage(Term term) {
    return encodeTermToBytes(termToWire(term));
  }

  // ==========================================================================
  // Term <-> wire mapping
  // ==========================================================================

  /// Map a runtime [Term] to a [WireTerm]. Global-name structures `_w(p,i)` /
  /// `_r(p,i)` become tag-2 variables; all other structures stay structures.
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
  /// module, or a global-name structure; null for any other structure, whose
  /// arguments are mapped in turn.  A variable, or a term of any other kind,
  /// is refused.
  static WireTerm? _leafToWire(Term term) {
    if (term is ConstTerm) {
      return WConst(_constToWire(term.value));
    } else if (term is ModuleTerm) {
      // §cf-terms constant tag 6: a Module constant travels as its artefact
      // bytes — the form in which compiled programs ship.
      return WConst(WModule((term.artefact as Artefact).toBytes()));
    } else if (term is StructTerm) {
      return _asGlobalName(term);
    } else if (term is VarRef) {
      throw WireFormatException(
          'non-globalized VarRef on the wire: @${term.addr} '
          '(terms must be globalized before serialization)');
    } else {
      throw WireFormatException(
          'cannot serialize term type: ${term.runtimeType}');
    }
  }

  /// Map a [WireTerm] back to a runtime [Term]. Tag-2 variables become
  /// `_w(p,i)` / `_r(p,i)` structures.
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
  static Term wireToTerm(WireTerm w) => _wireToTerm(w, certify: false);

  /// [wireToTerm], each module constant's artefact certified where [certify]
  /// ([deserializeGlobalSendPayload]).
  static Term _wireToTerm(WireTerm w, {required bool certify}) {
    if (w is! WStruct) return _leafToTerm(w, certify: certify);
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
        args.add(_leafToTerm(arg, certify: certify));
      }
    }
  }

  /// [wireToTerm] of a constant or a variable; a module's artefact certified
  /// where [certify], and refused ([ModuleRefusal]) where it is no certified
  /// compiled program.
  static Term _leafToTerm(WireTerm w, {required bool certify}) {
    switch (w) {
      case WConst(:final constant):
        if (constant is WModule) {
          // §cf-terms constant tag 6 decodes to the Module constant itself.
          if (certify) {
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
      case WVar(:final isReader, :final index):
        final functor = isReader ? '_r' : '_w';
        return StructTerm(
            functor, [ConstTerm(w.agentString), ConstTerm(index)]);
      case WStruct():
        throw ArgumentError('a structure is mapped by wireToTerm, not here');
    }
  }

  /// Recognise a `_w(p,i)` / `_r(p,i)` global-name structure and convert it to
  /// the codec's tag-2 variable. Returns null for any other structure.
  static WVar? _asGlobalName(StructTerm s) {
    if ((s.functor != '_w' && s.functor != '_r') || s.args.length != 2) {
      return null;
    }
    final agent = s.args[0];
    final index = s.args[1];
    if (agent is! ConstTerm ||
        agent.value is! String ||
        index is! ConstTerm ||
        index.value is! int) {
      return null;
    }
    return WVar.symbolic(
      isReader: s.functor == '_r',
      agent: agent.value as String,
      index: index.value as int,
    );
  }

  static WireConst _constToWire(Object? v) {
    // The runtime represents the empty list as ConstTerm('nil').
    if (v == 'nil') return const WNil();
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
        return 'nil';
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
