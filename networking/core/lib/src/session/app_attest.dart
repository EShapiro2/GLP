/// Apple App Attest: the verifier.
///
/// Spec §Session Establishment. The producing device generates its attestation
/// key in the Secure Enclave and has Apple attest it once, over a challenge
/// naming the agent's identity public key. The attestation object is CBOR
/// carrying a certificate chain, which this file validates against the pinned
/// Apple App Attest root, and an `authData` blob binding the key to our
/// application.
///
/// The steps are Apple's, from "Validating apps that connect to your server"
/// (developer.apple.com/documentation/devicecheck).
///
/// WHAT APPLE'S SERVERS ARE NEEDED FOR, and it is not this: creating the
/// attestation contacts Apple, once per key, on the producing device —
/// `attestKey` "accesses a remote Apple server". Verifying one contacts
/// nobody. Apple documents no revocation mechanism for App Attest keys, and
/// the receipt's fraud-risk metric is a separate optional call about our own
/// devices, which has no place in a peer-to-peer session. So an iOS verifier
/// is offline outright, which the Android one is not.
library;

import 'dart:typed_data';

import 'package:asn1lib/asn1lib.dart';
import 'package:cbor/cbor.dart';
import 'package:cryptography/dart.dart' show DartSha256;

import 'attestation_roots.dart';
import 'x509.dart';

/// The `fmt` an App Attest attestation object carries.
const String kAppAttestFormat = 'apple-appattest';

/// OID of the credential certificate extension holding the nonce.
const String kAppAttestNonceOid = '1.2.840.113635.100.8.2';

/// AAGUIDs Apple documents: production, and the development sandbox.
const String kAaguidProduction = 'appattest';
const String kAaguidDevelopment = 'appattestdevelop';

/// What an App Attest attestation established, once it verified.
class AppAttestResult {
  /// The credential certificate, whose subject key is the attestation key that
  /// signs each session's digest.
  final X509Certificate credCert;

  /// The key identifier — SHA-256 of the attested public key — as `authData`
  /// carries it.
  final Uint8List keyIdentifier;

  /// The application identity this key was attested under: the App ID whose
  /// SHA-256 is `authData`'s rpIdHash.
  ///
  /// This is what `onPeerConnected` carries as the attested application
  /// identity on iOS. App Attest produces no hash of the running binary, and
  /// this file does not pretend it does.
  final String appId;

  const AppAttestResult({
    required this.credCert,
    required this.keyIdentifier,
    required this.appId,
  });
}

/// Verify an App Attest attestation object.
///
/// [attestationObject] is the CBOR blob the device produced. [expectedAppId]
/// is `<teamID>.<bundleID>`. [expectedChallenge] is the agent's identity
/// public key — §Session Establishment has the attestation made "over a
/// challenge naming the agent's identity public key", and on iOS that reaches
/// the device as the client data whose SHA-256 is the clientDataHash.
///
/// Throws [X509Exception] with the reason on any failure. Every failure here
/// is an attestation "offered and found invalid", which tears the session
/// down, so nothing in this path may fail open.
AppAttestResult verifyAppAttestAttestation({
  required Uint8List attestationObject,
  required String expectedAppId,
  required Uint8List expectedChallenge,
  required DateTime at,
  List<Uint8List>? pinnedRoots,
  bool allowDevelopmentEnvironment = false,
}) {
  final CborValue decoded;
  try {
    decoded = cborDecode(attestationObject);
  } catch (e) {
    throw X509Exception('Attestation object is not CBOR: $e');
  }
  if (decoded is! CborMap) {
    throw const X509Exception('Attestation object is not a CBOR map');
  }
  final CborMap object = decoded;

  CborValue? at_(String key) {
    for (final entry in object.entries) {
      final k = entry.key;
      if (k is CborString && k.toString() == key) return entry.value;
    }
    return null;
  }

  final fmt = at_('fmt');
  if (fmt is! CborString || fmt.toString() != kAppAttestFormat) {
    throw X509Exception(
      'Attestation format is $fmt, not $kAppAttestFormat',
    );
  }

  final authDataValue = at_('authData');
  if (authDataValue is! CborBytes) {
    throw const X509Exception('authData is not a byte string');
  }
  final authData = Uint8List.fromList(authDataValue.bytes);

  final attStmt = at_('attStmt');
  if (attStmt is! CborMap) {
    throw const X509Exception('attStmt is not a CBOR map');
  }
  final CborMap statement = attStmt;
  CborValue? stmt(String key) {
    for (final entry in statement.entries) {
      final k = entry.key;
      if (k is CborString && k.toString() == key) return entry.value;
    }
    return null;
  }

  final x5c = stmt('x5c');
  if (x5c is! CborList || x5c.isEmpty) {
    throw const X509Exception('attStmt carries no x5c chain');
  }
  final chain = <Uint8List>[];
  for (final cert in x5c) {
    if (cert is! CborBytes) {
      throw const X509Exception('an x5c entry is not a byte string');
    }
    chain.add(Uint8List.fromList(cert.bytes));
  }

  // 1. The chain, to the pinned Apple root.
  final credCert = validateChain(
    chain: chain,
    pinnedRoots: pinnedRoots ?? appAttestRootsDer(),
    at: at,
  );

  // 2-3. nonce = SHA-256(authData || clientDataHash), the clientDataHash being
  // SHA-256 of the challenge, which is the agent's identity key.
  final clientDataHash = _sha256(expectedChallenge);
  final expectedNonce = _sha256(
    Uint8List.fromList([...authData, ...clientDataHash]),
  );

  // 4. The credential certificate's nonce extension must carry it.
  final nonceExt = credCert.extensions[kAppAttestNonceOid];
  if (nonceExt == null) {
    throw const X509Exception(
      'The credential certificate carries no App Attest nonce extension',
    );
  }
  final carriedNonce = _nonceFromExtension(nonceExt);
  if (!_sameBytes(carriedNonce, expectedNonce)) {
    throw const X509Exception(
      'The attestation nonce does not match this agent\'s identity key and '
      'authData, so the attestation names some other key',
    );
  }

  // 5. The key identifier is SHA-256 of the attested public key, and authData
  // must carry that same identifier as its credentialId.
  final publicKeyPoint = _uncompressedPoint(credCert);
  final keyIdentifier = _sha256(publicKeyPoint);

  // 6-8. authData: rpIdHash(32) | flags(1) | counter(4) | aaguid(16) |
  // credentialIdLength(2) | credentialId.
  if (authData.length < 55) {
    throw X509Exception('authData is ${authData.length} bytes, too short');
  }
  final rpIdHash = Uint8List.sublistView(authData, 0, 32);
  if (!_sameBytes(rpIdHash, _sha256(_ascii(expectedAppId)))) {
    throw X509Exception(
      'authData names a different application than $expectedAppId',
    );
  }
  final counter = ByteData.sublistView(authData, 33, 37).getUint32(0);
  if (counter != 0) {
    throw X509Exception(
      'An attestation\'s counter must be 0 and is $counter',
    );
  }
  final aaguid = _trimZeros(Uint8List.sublistView(authData, 37, 53));
  final aaguidText = String.fromCharCodes(aaguid);
  if (aaguidText == kAaguidDevelopment) {
    if (!allowDevelopmentEnvironment) {
      throw const X509Exception(
        'The attestation is from the App Attest development sandbox and this '
        'build accepts production attestations only',
      );
    }
  } else if (aaguidText != kAaguidProduction) {
    throw X509Exception('Unrecognised App Attest aaguid "$aaguidText"');
  }
  final credentialIdLength =
      ByteData.sublistView(authData, 53, 55).getUint16(0);
  if (authData.length < 55 + credentialIdLength) {
    throw const X509Exception('authData is shorter than its credentialId');
  }
  final credentialId =
      Uint8List.sublistView(authData, 55, 55 + credentialIdLength);
  if (!_sameBytes(credentialId, keyIdentifier)) {
    throw const X509Exception(
      'authData\'s credentialId is not the SHA-256 of the attested key',
    );
  }

  return AppAttestResult(
    credCert: credCert,
    keyIdentifier: keyIdentifier,
    appId: expectedAppId,
  );
}

/// The nonce extension is `SEQUENCE { [1] EXPLICIT OCTET STRING }`.
Uint8List _nonceFromExtension(Uint8List der) {
  final top = ASN1Parser(der).nextObject();
  if (top is! ASN1Sequence || top.elements.isEmpty) {
    throw const X509Exception('The nonce extension is not a SEQUENCE');
  }
  final tagged = top.elements.first;
  final inner = ASN1Parser(tagged.valueBytes()).nextObject();
  if (inner is! ASN1OctetString) {
    throw const X509Exception('The nonce is not an OCTET STRING');
  }
  return Uint8List.fromList(inner.valueBytes());
}

/// The credential certificate's subject key as an uncompressed EC point,
/// which is what Apple hashes to form the key identifier.
Uint8List _uncompressedPoint(X509Certificate cert) {
  final spki = ASN1Parser(cert.subjectPublicKeyInfoDer).nextObject();
  if (spki is! ASN1Sequence || spki.elements.length != 2) {
    throw const X509Exception('subjectPublicKeyInfo is malformed');
  }
  final bits = spki.elements[1];
  if (bits is! ASN1BitString) {
    throw const X509Exception('subjectPublicKey is not a BIT STRING');
  }
  final raw = bits.valueBytes();
  if (raw.length < 2) throw const X509Exception('subjectPublicKey is empty');
  // Drop the BIT STRING's unused-bit count.
  final point = Uint8List.fromList(raw.sublist(1));
  if (point.first != 0x04) {
    throw const X509Exception('The attested key is not an uncompressed point');
  }
  return point;
}

Uint8List _sha256(Uint8List input) =>
    Uint8List.fromList(const DartSha256().hashSync(input).bytes);

Uint8List _ascii(String s) => Uint8List.fromList(s.codeUnits);

Uint8List _trimZeros(Uint8List b) {
  var end = b.length;
  while (end > 0 && b[end - 1] == 0) {
    end--;
  }
  return Uint8List.sublistView(b, 0, end);
}

bool _sameBytes(Uint8List a, Uint8List b) {
  if (a.length != b.length) return false;
  var diff = 0;
  for (var i = 0; i < a.length; i++) {
    diff |= a[i] ^ b[i];
  }
  return diff == 0;
}
