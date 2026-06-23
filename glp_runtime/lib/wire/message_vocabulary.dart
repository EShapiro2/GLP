/// Adoption / handshake message vocabulary (D3 wire format, §8) — byte forms.
///
/// Normative source: the IGLP paper appendix `app:wire-format`, §wf-handshake.
/// These are ground terms exchanged between runtimes on the attested channel,
/// encoded per §3 (the term codec). Hash values inside terms are blob constants
/// (32 bytes for identities); the shipped artefact is a blob of its bytes.
///
/// This module builds the terms and their canonical bytes. It does NOT decide
/// who sends what when — the adoption/handshake protocol wiring is C4's, gated
/// on the attestation work (C3); S6 is byte forms only. The actual ed25519
/// sign/verify is the networking seam's; [signedBytes] yields the exact bytes
/// to sign or verify, namely e(sig(HSrc, Term)).
///
/// Messages build runtime [Term]s and encode through the same term↔wire mapping
/// the rest of the system uses ([PayloadCodec.termToWire]), so a message's bytes
/// match what the runtime would produce for the equivalent term. Atoms
/// (`accept`/`decline`) are `ConstTerm` atoms; identities/artefacts are blob
/// `ConstTerm`s.
library;

import 'dart:typed_data';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:glp_runtime/wire/codec.dart';

/// The functor of the signed-content structure, fixed by the specification.
const String sigFunctor = 'sig';

// ============================================================================
// Constructors (§8)
// ============================================================================

/// `offer(HSrc, tau(HSrcDef, TypeName))` — the adoption offer: the contract's
/// module identity, and the root channel's type identity (the pair of the
/// module identity of the type's defining source and the type's name).
Term offer(Uint8List hSrc, Uint8List hSrcDef, String typeName) =>
    StructTerm('offer', [_blob(hSrc), _tau(hSrcDef, typeName)]);

/// `accept` — the offeree's consent.
Term accept() => ConstTerm('accept');

/// `decline` — the offeree's refusal.
Term decline() => ConstTerm('decline');

/// `ship(Artefact, HBin, HSrc)` — the module bytecode as a blob, its artefact
/// identity, and the source identity it is certified to compile from.
Term ship(Uint8List artefact, Uint8List hBin, Uint8List hSrc) =>
    StructTerm('ship', [_blob(artefact), _blob(hBin), _blob(hSrc)]);

/// `handshake(HSrc, tau(HSrcDef, TypeName))` — exchanged by the two runtimes on
/// a conversation before any application traffic; both ends must present equal
/// values.
Term handshake(Uint8List hSrc, Uint8List hSrcDef, String typeName) =>
    StructTerm('handshake', [_blob(hSrc), _tau(hSrcDef, typeName)]);

/// The signed-content structure `sig(HSrc, Term)` whose canonical bytes are
/// what `sign`/`verify` operate on (§8). The functor is fixed by the spec.
Term sigStructure(Uint8List hSrc, Term term) =>
    StructTerm(sigFunctor, [_blob(hSrc), term]);

Term _tau(Uint8List hSrcDef, String typeName) =>
    StructTerm('tau', [_blob(hSrcDef), ConstTerm(typeName)]);

Term _blob(Uint8List bytes) => ConstTerm(bytes);

// ============================================================================
// Encoding / decoding (§3 term codec)
// ============================================================================

/// Canonical bytes e(T) of a ground message term.
Uint8List encodeMessage(Term term) =>
    encodeTermToBytes(PayloadCodec.termToWire(term));

/// Decode canonical bytes back to a runtime term.
Term decodeMessage(Uint8List bytes) =>
    PayloadCodec.wireToTerm(decodeTermFromBytes(bytes));

/// The bytes to sign or verify for a term: e(sig(HSrc, Term)) — the canonical
/// encoding of the 2-ary `sig` structure of the calling instance's module
/// identity and the (ground) term. The ed25519 sign/verify over these bytes is
/// the networking seam's.
Uint8List signedBytes(Uint8List hSrc, Term term) =>
    encodeMessage(sigStructure(hSrc, term));

// ============================================================================
// Field extraction (byte-form helpers; protocol logic is C4's)
// ============================================================================

bool isAccept(Term t) => t is ConstTerm && t.value == 'accept';
bool isDecline(Term t) => t is ConstTerm && t.value == 'decline';

/// Parse `offer(HSrc, tau(HSrcDef, TypeName))`.
({Uint8List hSrc, Uint8List hSrcDef, String typeName}) parseOffer(Term t) {
  final s = _expectStruct(t, 'offer', 2);
  final tau = _expectStruct(s.args[1], 'tau', 2);
  return (
    hSrc: _blobOf(s.args[0]),
    hSrcDef: _blobOf(tau.args[0]),
    typeName: _stringOf(tau.args[1]),
  );
}

/// Parse `ship(Artefact, HBin, HSrc)`.
({Uint8List artefact, Uint8List hBin, Uint8List hSrc}) parseShip(Term t) {
  final s = _expectStruct(t, 'ship', 3);
  return (
    artefact: _blobOf(s.args[0]),
    hBin: _blobOf(s.args[1]),
    hSrc: _blobOf(s.args[2]),
  );
}

/// Parse `handshake(HSrc, tau(HSrcDef, TypeName))`.
({Uint8List hSrc, Uint8List hSrcDef, String typeName}) parseHandshake(Term t) {
  final s = _expectStruct(t, 'handshake', 2);
  final tau = _expectStruct(s.args[1], 'tau', 2);
  return (
    hSrc: _blobOf(s.args[0]),
    hSrcDef: _blobOf(tau.args[0]),
    typeName: _stringOf(tau.args[1]),
  );
}

StructTerm _expectStruct(Term t, String functor, int arity) {
  if (t is! StructTerm || t.functor != functor || t.args.length != arity) {
    throw WireFormatException('expected $functor/$arity, got $t');
  }
  return t;
}

Uint8List _blobOf(Term t) {
  if (t is ConstTerm) {
    final v = t.value;
    if (v is Uint8List) return v;
    if (v is List<int>) return Uint8List.fromList(v);
  }
  throw WireFormatException('expected a blob constant, got $t');
}

String _stringOf(Term t) {
  if (t is ConstTerm && t.value is String) return t.value as String;
  throw WireFormatException('expected a string/atom constant, got $t');
}
