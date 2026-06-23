/// Canonical madGLP payload path over the wire codec (D3, behind the switch).
///
/// Normative source: the IGLP paper appendix `app:wire-format`, §§wf-primitives,
/// wf-terms. Variables travel as global names per Definition Globalize: a tag-2
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
///
/// This path is selected by [WireFlags.canonical]; the legacy serializer
/// remains the live default until the S7 cutover.
library;

import 'dart:convert';
import 'dart:typed_data';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/wire/codec.dart';

class PayloadCodec {
  // ==========================================================================
  // madGLP assignment payloads
  // ==========================================================================

  /// Encode the assignment `G := T↑`: the global name G (polarity, agent,
  /// index) followed by the encoding of T.
  static List<int> createGlobalSendPayload(GlobalName globalName, Term value) {
    return encodeAssignmentToBytes(WireAssignment(
      gIsReader: globalName.isReader,
      gAgent: _agentBytes(globalName.agent),
      gIndex: globalName.index,
      value: termToWire(value),
    ));
  }

  /// Encode a serializer (cold-call) message to agent q: the assignment
  /// `_w(q,0) := [T↑ | _w(q,0)]`. The tail is the encoded variable `_w(q,0)`.
  static List<int> createSerializerPayload(
      GlobalName serializerName, Term content) {
    assert(serializerName.isWriter && serializerName.index == 0,
        'Serializer payload requires _w(agent, 0) global name');
    return encodeAssignmentToBytes(WireAssignment.serializer(
      agent: _agentBytes(serializerName.agent),
      head: termToWire(content),
    ));
  }

  /// Decode a global-send payload to (GlobalName, Term). Embedded variables
  /// return as `_w(p,i)` / `_r(p,i)` structures for the localize machinery.
  static (GlobalName, Term) deserializeGlobalSendPayload(List<int> payload) {
    final a = decodeAssignmentFromBytes(_asUint8(payload));
    final agent = utf8.decode(a.gAgent);
    final globalName = a.gIsReader
        ? GlobalName.reader(agent, a.gIndex)
        : GlobalName.writer(agent, a.gIndex);
    return (globalName, wireToTerm(a.value));
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
  static WireTerm termToWire(Term term) {
    if (term is ConstTerm) {
      return WConst(_constToWire(term.value));
    } else if (term is StructTerm) {
      final gn = _asGlobalName(term);
      if (gn != null) return gn;
      return WStruct(term.functor, term.args.map(termToWire).toList());
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
  static Term wireToTerm(WireTerm w) {
    switch (w) {
      case WConst(:final constant):
        return ConstTerm(_constFromWire(constant));
      case WVar(:final isReader, :final index):
        final functor = isReader ? '_r' : '_w';
        return StructTerm(
            functor, [ConstTerm(w.agentString), ConstTerm(index)]);
      case WStruct(:final functor, :final args):
        return StructTerm(functor, args.map(wireToTerm).toList());
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
    }
  }

  static Uint8List _agentBytes(String agent) =>
      Uint8List.fromList(utf8.encode(agent));

  static Uint8List _asUint8(List<int> b) =>
      b is Uint8List ? b : Uint8List.fromList(b);
}
