/// GLP wire-format codec — primitives, terms, and assignment/serializer
/// messages.
///
/// Normative source: the IGLP paper appendix `app:wire-format`
/// (`/Users/udi/Grassroots/IGLP/sections/wire-format-fragment.tex`),
/// §§wf-primitives and wf-terms. This library is the byte-level realisation of
/// the canonical encoding e: an injective function from globalized terms and
/// assignment messages to byte strings.
///
/// The codec is pure bytes. It does not interpret an agent's content (a public
/// key in the current realisation, a UTF-8 symbolic name in the simulation
/// realisation); agents are raw byte strings here, and the String<->bytes
/// choice belongs to the realisation that drives the codec.
library;

import 'dart:convert';
import 'dart:typed_data';

// ============================================================================
// Wire term model (§wf-terms)
// ============================================================================

/// A constant payload (§wf-terms, the constant node's u8 subtag and payload).
sealed class WireConst {
  const WireConst();
}

/// 0 nil — the empty list.
class WNil extends WireConst {
  const WNil();
  @override
  bool operator ==(Object other) => other is WNil;
  @override
  int get hashCode => 0;
}

/// 1 integer — i64.
class WInt extends WireConst {
  final int value;
  const WInt(this.value);
  @override
  bool operator ==(Object other) => other is WInt && other.value == value;
  @override
  int get hashCode => value.hashCode;
}

/// 2 float — f64.
class WFloat extends WireConst {
  final double value;
  const WFloat(this.value);
  @override
  bool operator ==(Object other) =>
      other is WFloat && _f64Bits(other.value) == _f64Bits(value);
  @override
  int get hashCode => _f64Bits(value).hashCode;
}

/// 3 string — UTF-8 string.
class WString extends WireConst {
  final String value;
  const WString(this.value);
  @override
  bool operator ==(Object other) => other is WString && other.value == value;
  @override
  int get hashCode => value.hashCode;
}

/// 4 boolean.
class WBool extends WireConst {
  final bool value;
  const WBool(this.value);
  @override
  bool operator ==(Object other) => other is WBool && other.value == value;
  @override
  int get hashCode => value.hashCode;
}

/// 5 blob — opaque byte string (the form in which compiled modules ship).
class WBlob extends WireConst {
  final Uint8List value;
  WBlob(this.value);
  @override
  bool operator ==(Object other) =>
      other is WBlob && _bytesEqual(other.value, value);
  @override
  int get hashCode => Object.hashAll(value);
}

/// A globalized term (§wf-terms).
sealed class WireTerm {
  const WireTerm();
}

/// 1 constant.
class WConst extends WireTerm {
  final WireConst constant;
  const WConst(this.constant);
  @override
  bool operator ==(Object other) =>
      other is WConst && other.constant == constant;
  @override
  int get hashCode => constant.hashCode;
}

/// 2 variable — a global name per Definition Globalize: polarity (writer
/// `_w(p,i)` / reader `_r(p,i)`), agent p (raw bytes), index i.
class WVar extends WireTerm {
  /// false = writer `_w(p,i)`, true = reader `_r(p,i)` (the polarity u8).
  final bool isReader;
  final Uint8List agent;
  final int index;
  WVar({required this.isReader, required this.agent, required this.index});

  /// Build a variable whose agent is a UTF-8 symbolic name (simulation
  /// realisation).
  WVar.symbolic(
      {required this.isReader, required String agent, required this.index})
      : agent = Uint8List.fromList(utf8.encode(agent));

  /// The agent decoded as a UTF-8 symbolic name (simulation realisation).
  String get agentString => utf8.decode(agent);

  @override
  bool operator ==(Object other) =>
      other is WVar &&
      other.isReader == isReader &&
      other.index == index &&
      _bytesEqual(other.agent, agent);
  @override
  int get hashCode => Object.hash(isReader, index, Object.hashAll(agent));
}

/// 3 structure — functor, arity n, then n arguments in order.
class WStruct extends WireTerm {
  final String functor;
  final List<WireTerm> args;
  const WStruct(this.functor, this.args);
  @override
  bool operator ==(Object other) =>
      other is WStruct &&
      other.functor == functor &&
      _listEqual(other.args, args);
  @override
  int get hashCode => Object.hash(functor, Object.hashAll(args));
}

/// An assignment message `G := T↑` (§wf-terms, Messages). G is a global name;
/// the message carries G's polarity, agent, and index, then the encoding of T.
class WireAssignment {
  /// Polarity of G: false = writer `_w`, true = reader `_r`.
  final bool gIsReader;
  final Uint8List gAgent;
  final int gIndex;
  final WireTerm value;
  WireAssignment(
      {required this.gIsReader,
      required this.gAgent,
      required this.gIndex,
      required this.value});

  /// The serializer (cold-call) message to agent q: the assignment
  /// `_w(q,0) := [T↑ | _w(q,0)]`. G is the writer `_w(q,0)`; the value is the
  /// list cell whose head is T and whose tail is the variable `_w(q,0)`.
  factory WireAssignment.serializer(
      {required Uint8List agent, required WireTerm head}) {
    final tail = WVar(isReader: false, agent: agent, index: 0);
    return WireAssignment(
      gIsReader: false,
      gAgent: agent,
      gIndex: 0,
      value: WStruct('.', [head, tail]),
    );
  }

  @override
  bool operator ==(Object other) =>
      other is WireAssignment &&
      other.gIsReader == gIsReader &&
      other.gIndex == gIndex &&
      other.value == value &&
      _bytesEqual(other.gAgent, gAgent);
  @override
  int get hashCode =>
      Object.hash(gIsReader, gIndex, value, Object.hashAll(gAgent));
}

/// Thrown when a byte string is not a valid canonical encoding.
class WireFormatException implements Exception {
  final String message;
  WireFormatException(this.message);
  @override
  String toString() => 'WireFormatException: $message';
}

// ============================================================================
// Primitive writer (§wf-primitives)
// ============================================================================

/// Big-endian, shortest-form primitive encoder.
class WireWriter {
  final BytesBuilder _b = BytesBuilder(copy: false);

  void u8(int v) {
    if (v < 0 || v > 0xFF) {
      throw WireFormatException('u8 out of range: $v');
    }
    _b.addByte(v);
  }

  void i64(int v) {
    final d = ByteData(8);
    d.setInt64(0, v, Endian.little);
    _b.add(d.buffer.asUint8List());
  }

  void f64(double v) {
    final d = ByteData(8);
    d.setFloat64(0, v, Endian.little);
    _b.add(d.buffer.asUint8List());
  }

  /// Compact length: shortest of 1, 2, or 4 bytes; value below 2^30.
  void clen(int v) {
    if (v < 0 || v >= (1 << 30)) {
      throw WireFormatException('clen out of range: $v');
    }
    if (v < 128) {
      _b.addByte(v);
    } else if (v < 16384) {
      _b.addByte(0x80 | (v >> 8));
      _b.addByte(v & 0xFF);
    } else {
      _b.addByte(0xC0 | (v >> 24));
      _b.addByte((v >> 16) & 0xFF);
      _b.addByte((v >> 8) & 0xFF);
      _b.addByte(v & 0xFF);
    }
  }

  void string(String s) {
    final raw = utf8.encode(s);
    clen(raw.length);
    _b.add(raw);
  }

  void bytes(List<int> raw) {
    clen(raw.length);
    _b.add(raw);
  }

  /// 32 raw bytes — SHA-256 of the hashed content. No length prefix.
  void hash(List<int> h) {
    if (h.length != 32) {
      throw WireFormatException('hash must be 32 bytes, got ${h.length}');
    }
    _b.add(h);
  }

  Uint8List toBytes() => _b.toBytes();
}

// ============================================================================
// Primitive reader (§wf-primitives)
// ============================================================================

/// Big-endian primitive decoder. Rejects longer-than-necessary clen forms
/// (one value, one encoding).
class WireReader {
  final Uint8List data;
  int _off;

  WireReader(this.data) : _off = 0;

  int get offset => _off;
  bool get atEnd => _off >= data.length;

  void _need(int n) {
    if (_off + n > data.length) {
      throw WireFormatException('unexpected end of input');
    }
  }

  int u8() {
    _need(1);
    return data[_off++];
  }

  int i64() {
    _need(8);
    final v =
        ByteData.sublistView(data, _off, _off + 8).getInt64(0, Endian.little);
    _off += 8;
    return v;
  }

  double f64() {
    _need(8);
    final v =
        ByteData.sublistView(data, _off, _off + 8).getFloat64(0, Endian.little);
    _off += 8;
    return v;
  }

  int clen() {
    _need(1);
    final first = data[_off];
    if (first < 0x80) {
      _off += 1;
      return first;
    } else if (first < 0xC0) {
      _need(2);
      final v = ((first & 0x3F) << 8) | data[_off + 1];
      if (v < 128) {
        throw WireFormatException('over-long clen: $v in 2-byte form');
      }
      _off += 2;
      return v;
    } else {
      _need(4);
      final v = ((first & 0x3F) << 24) |
          (data[_off + 1] << 16) |
          (data[_off + 2] << 8) |
          data[_off + 3];
      if (v < 16384) {
        throw WireFormatException('over-long clen: $v in 4-byte form');
      }
      _off += 4;
      return v;
    }
  }

  String string() {
    final n = clen();
    _need(n);
    final s = utf8.decode(data.sublist(_off, _off + n));
    _off += n;
    return s;
  }

  Uint8List bytes() {
    final n = clen();
    _need(n);
    final out = Uint8List.sublistView(data, _off, _off + n);
    _off += n;
    return Uint8List.fromList(out);
  }

  Uint8List hash() {
    _need(32);
    final out = Uint8List.fromList(data.sublist(_off, _off + 32));
    _off += 32;
    return out;
  }

  /// Verify the input is fully consumed (no trailing bytes).
  void expectEnd() {
    if (!atEnd) {
      throw WireFormatException(
          'trailing bytes: ${data.length - _off} unconsumed');
    }
  }
}

// ============================================================================
// Constant payload (§wf-terms) — reused by instruction operands (§4.1)
// ============================================================================

/// Write a constant payload: u8 constant tag, then its payload. No leading
/// term tag — this is the form an instruction `constant` operand uses.
void encodeConstantPayload(WireWriter w, WireConst c) {
  switch (c) {
    case WNil():
      w.u8(0);
    case WInt(:final value):
      w.u8(1);
      w.i64(value);
    case WFloat(:final value):
      w.u8(2);
      w.f64(value);
    case WString(:final value):
      w.u8(3);
      w.string(value);
    case WBool(:final value):
      w.u8(4);
      w.u8(value ? 1 : 0);
    case WBlob(:final value):
      w.u8(5);
      w.bytes(value);
  }
}

/// Map a runtime constant value (the `Object?` an instruction or ConstTerm
/// carries) to a [WireConst]. The runtime represents the empty list as the
/// atom `'nil'`.
WireConst wireConstFromValue(Object? v) {
  if (v == 'nil') return const WNil();
  if (v is int) return WInt(v);
  if (v is double) return WFloat(v);
  if (v is String) return WString(v);
  if (v is bool) return WBool(v);
  if (v is Uint8List) return WBlob(v);
  if (v is List<int>) return WBlob(Uint8List.fromList(v));
  throw WireFormatException('cannot encode constant of type ${v.runtimeType}');
}

/// Inverse of [wireConstFromValue].
Object? valueOfWireConst(WireConst c) {
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

WireConst decodeConstantPayload(WireReader r) {
  final tag = r.u8();
  switch (tag) {
    case 0:
      return const WNil();
    case 1:
      return WInt(r.i64());
    case 2:
      return WFloat(r.f64());
    case 3:
      return WString(r.string());
    case 4:
      final b = r.u8();
      if (b != 0 && b != 1) {
        throw WireFormatException('boolean payload not 0/1: $b');
      }
      return WBool(b == 1);
    case 5:
      return WBlob(r.bytes());
    default:
      throw WireFormatException('unknown constant tag: $tag');
  }
}

// ============================================================================
// Term encoding (§wf-terms)
// ============================================================================

void encodeTerm(WireWriter w, WireTerm t) {
  switch (t) {
    case WConst(:final constant):
      w.u8(1);
      encodeConstantPayload(w, constant);
    case WVar(:final isReader, :final agent, :final index):
      w.u8(2);
      w.u8(isReader ? 1 : 0);
      w.bytes(agent);
      w.clen(index);
    case WStruct(:final functor, :final args):
      w.u8(3);
      w.string(functor);
      w.clen(args.length);
      for (final a in args) {
        encodeTerm(w, a);
      }
  }
}

WireTerm decodeTerm(WireReader r) {
  final tag = r.u8();
  switch (tag) {
    case 1:
      return WConst(decodeConstantPayload(r));
    case 2:
      final pol = r.u8();
      if (pol != 0 && pol != 1) {
        throw WireFormatException('variable polarity not 0/1: $pol');
      }
      final agent = r.bytes();
      final index = r.clen();
      return WVar(isReader: pol == 1, agent: agent, index: index);
    case 3:
      final functor = r.string();
      final arity = r.clen();
      final args = <WireTerm>[];
      for (var i = 0; i < arity; i++) {
        args.add(decodeTerm(r));
      }
      return WStruct(functor, args);
    default:
      throw WireFormatException('unknown term tag: $tag');
  }
}

Uint8List encodeTermToBytes(WireTerm t) {
  final w = WireWriter();
  encodeTerm(w, t);
  return w.toBytes();
}

WireTerm decodeTermFromBytes(Uint8List b) {
  final r = WireReader(b);
  final t = decodeTerm(r);
  r.expectEnd();
  return t;
}

// ============================================================================
// Message encoding (§wf-terms, Messages)
// ============================================================================

void encodeAssignment(WireWriter w, WireAssignment a) {
  w.u8(a.gIsReader ? 1 : 0);
  w.bytes(a.gAgent);
  w.clen(a.gIndex);
  encodeTerm(w, a.value);
}

WireAssignment decodeAssignment(WireReader r) {
  final pol = r.u8();
  if (pol != 0 && pol != 1) {
    throw WireFormatException('assignment polarity not 0/1: $pol');
  }
  final agent = r.bytes();
  final index = r.clen();
  final value = decodeTerm(r);
  return WireAssignment(
      gIsReader: pol == 1, gAgent: agent, gIndex: index, value: value);
}

Uint8List encodeAssignmentToBytes(WireAssignment a) {
  final w = WireWriter();
  encodeAssignment(w, a);
  return w.toBytes();
}

WireAssignment decodeAssignmentFromBytes(Uint8List b) {
  final r = WireReader(b);
  final a = decodeAssignment(r);
  r.expectEnd();
  return a;
}

// ============================================================================
// Helpers
// ============================================================================

int _f64Bits(double v) =>
    (ByteData(8)..setFloat64(0, v, Endian.big)).getInt64(0, Endian.big);

bool _bytesEqual(List<int> a, List<int> b) {
  if (a.length != b.length) return false;
  for (var i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}

bool _listEqual(List<Object?> a, List<Object?> b) {
  if (a.length != b.length) return false;
  for (var i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
