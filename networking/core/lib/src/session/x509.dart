/// The slice of X.509 that attestation verification needs, and no more.
///
/// Both platform attestations are certificate chains verified against a pinned
/// root (spec §Session Establishment), so the layer needs to parse a
/// certificate, verify one certificate's signature under another's key, and
/// walk a chain to a pinned anchor. It does not need a general PKI: no name
/// constraints, no policy processing, no path-length arithmetic beyond the
/// basic constraint, and no revocation — what revocation each platform has is
/// outside the session and is Section 2.5's subject, not this file's.
///
/// Deliberately strict. Anything unrecognised is rejected rather than skipped:
/// a chain is an adversary's input, and "unknown, so ignore it" is how
/// verifiers are defeated.
library;

import 'dart:typed_data';

import 'package:asn1lib/asn1lib.dart';
import 'package:pointycastle/export.dart';

/// A certificate failed to parse, or a chain failed to validate.
class X509Exception implements Exception {
  final String message;
  const X509Exception(this.message);
  @override
  String toString() => 'X509Exception: $message';
}

// Signature algorithm OIDs.
const String _oidSha256Rsa = '1.2.840.113549.1.1.11';
const String _oidSha384Rsa = '1.2.840.113549.1.1.12';
const String _oidSha512Rsa = '1.2.840.113549.1.1.13';
const String _oidEcdsaSha256 = '1.2.840.10045.4.3.2';
const String _oidEcdsaSha384 = '1.2.840.10045.4.3.3';

// Key algorithm OIDs.
const String _oidRsaEncryption = '1.2.840.113549.1.1.1';
const String _oidEcPublicKey = '1.2.840.10045.2.1';

// Named curves.
const String _oidPrime256v1 = '1.2.840.10045.3.1.7';
const String _oidSecp384r1 = '1.3.132.0.34';

/// DER digest identifiers for RSA PKCS#1 v1.5, as pointycastle's [RSASigner]
/// wants them.
const Map<String, String> _rsaDigestIdentifier = {
  _oidSha256Rsa: '0609608648016503040201',
  _oidSha384Rsa: '0609608648016503040202',
  _oidSha512Rsa: '0609608648016503040203',
};

/// A parsed X.509 certificate.
class X509Certificate {
  /// The certificate as it arrived. Pinning compares these bytes.
  final Uint8List der;

  /// The `tbsCertificate` encoding — what the signature is over.
  final Uint8List tbsBytes;

  /// OID of the algorithm that signed this certificate.
  final String signatureAlgorithm;

  /// The signature, with the BIT STRING's unused-bit count stripped.
  final Uint8List signature;

  /// The `issuer` and `subject` Names, as DER. Chaining compares these bytes
  /// rather than decoding to strings: a Name is a structured value and string
  /// comparison of it is a well-known source of mismatch.
  final Uint8List issuerDer;
  final Uint8List subjectDer;

  final DateTime notBefore;
  final DateTime notAfter;

  /// `subjectPublicKeyInfo`, whole.
  final Uint8List subjectPublicKeyInfoDer;

  /// Extensions by OID. The value is the extension's DER, unwrapped from its
  /// OCTET STRING.
  final Map<String, Uint8List> extensions;

  /// Extension OIDs this certificate marks critical.
  final Set<String> criticalExtensions;

  X509Certificate._({
    required this.der,
    required this.tbsBytes,
    required this.signatureAlgorithm,
    required this.signature,
    required this.issuerDer,
    required this.subjectDer,
    required this.notBefore,
    required this.notAfter,
    required this.subjectPublicKeyInfoDer,
    required this.extensions,
    required this.criticalExtensions,
  });

  /// Parse a DER certificate.
  factory X509Certificate.fromDer(Uint8List der) {
    try {
      return _parse(der);
    } on X509Exception {
      rethrow;
    } catch (e) {
      throw X509Exception('Malformed certificate: $e');
    }
  }

  static X509Certificate _parse(Uint8List der) {
    final top = ASN1Parser(der).nextObject();
    if (top is! ASN1Sequence) {
      throw const X509Exception('Certificate is not a SEQUENCE');
    }
    final parts = top.elements;
    if (parts.length != 3) {
      throw const X509Exception('Certificate does not have three fields');
    }
    final tbs = parts[0];
    if (tbs is! ASN1Sequence) {
      throw const X509Exception('tbsCertificate is not a SEQUENCE');
    }
    final sigAlg = parts[1];
    if (sigAlg is! ASN1Sequence) {
      throw const X509Exception('signatureAlgorithm is not a SEQUENCE');
    }
    final sigBits = parts[2];
    if (sigBits is! ASN1BitString) {
      throw const X509Exception('signatureValue is not a BIT STRING');
    }

    final fields = tbs.elements;
    if (fields.length < 6) {
      throw const X509Exception('tbsCertificate is too short');
    }
    // [0] EXPLICIT version is optional; everything after it shifts.
    var i = fields[0].tag == 0xA0 ? 1 : 0;
    i++; // serialNumber
    i++; // signature (repeats signatureAlgorithm)
    if (i + 3 >= fields.length) {
      throw const X509Exception('tbsCertificate is missing required fields');
    }
    final issuer = fields[i++];
    final validity = fields[i++];
    final subject = fields[i++];
    final spki = fields[i++];
    if (validity is! ASN1Sequence) {
      throw const X509Exception('validity is not a SEQUENCE');
    }
    final validityParts = validity.elements;
    if (validityParts.length != 2) {
      throw const X509Exception('validity does not have two times');
    }

    final extensions = <String, Uint8List>{};
    final critical = <String>{};
    for (var j = i; j < fields.length; j++) {
      // [3] EXPLICIT Extensions. [1] and [2] are the unique IDs, unused here.
      if (fields[j].tag != 0xA3) continue;
      final inner = ASN1Parser(fields[j].valueBytes()).nextObject();
      if (inner is! ASN1Sequence) {
        throw const X509Exception('extensions is not a SEQUENCE');
      }
      for (final ext in inner.elements) {
        if (ext is! ASN1Sequence) {
          throw const X509Exception('an extension is not a SEQUENCE');
        }
        final e = ext.elements;
        if (e.length < 2) {
          throw const X509Exception('an extension is too short');
        }
        final oid = e[0];
        if (oid is! ASN1ObjectIdentifier || oid.identifier == null) {
          throw const X509Exception('an extension has no OID');
        }
        var isCritical = false;
        var valueIndex = 1;
        if (e.length == 3) {
          final flag = e[1];
          if (flag is! ASN1Boolean) {
            throw const X509Exception('extension criticality is not a BOOLEAN');
          }
          isCritical = flag.booleanValue;
          valueIndex = 2;
        }
        final value = e[valueIndex];
        if (value is! ASN1OctetString) {
          throw const X509Exception('extension value is not an OCTET STRING');
        }
        extensions[oid.identifier!] = Uint8List.fromList(value.valueBytes());
        if (isCritical) critical.add(oid.identifier!);
      }
    }

    final algOid = sigAlg.elements.first;
    if (algOid is! ASN1ObjectIdentifier || algOid.identifier == null) {
      throw const X509Exception('signatureAlgorithm has no OID');
    }

    final sigValue = sigBits.valueBytes();
    if (sigValue.isEmpty) {
      throw const X509Exception('signatureValue is empty');
    }

    return X509Certificate._(
      der: der,
      tbsBytes: Uint8List.fromList(tbs.encodedBytes),
      signatureAlgorithm: algOid.identifier!,
      // The leading octet of a BIT STRING is its unused-bit count.
      signature: Uint8List.fromList(sigValue.sublist(1)),
      issuerDer: Uint8List.fromList(issuer.encodedBytes),
      subjectDer: Uint8List.fromList(subject.encodedBytes),
      notBefore: _time(validityParts[0]),
      notAfter: _time(validityParts[1]),
      subjectPublicKeyInfoDer: Uint8List.fromList(spki.encodedBytes),
      extensions: extensions,
      criticalExtensions: critical,
    );
  }

  static DateTime _time(ASN1Object o) {
    if (o is ASN1UtcTime) return o.dateTimeValue.toUtc();
    if (o is ASN1GeneralizedTime) return o.dateTimeValue.toUtc();
    throw const X509Exception('validity carries an unrecognised time type');
  }

  /// True where [at] falls inside this certificate's validity window.
  bool isValidAt(DateTime at) {
    final t = at.toUtc();
    return !t.isBefore(notBefore) && !t.isAfter(notAfter);
  }

  /// Verify that [signed] was signed by this certificate's subject key.
  ///
  /// Returns false rather than throwing on a signature that simply does not
  /// verify; throws [X509Exception] where the algorithm or key is one this
  /// slice of X.509 does not implement, which is a different fact and must not
  /// be reported as a forgery.
  bool verifies(X509Certificate signed) {
    final rsaDigest = _rsaDigestIdentifier[signed.signatureAlgorithm];
    if (rsaDigest != null) {
      final key = _rsaPublicKey();
      final signer = RSASigner(_digestFor(signed.signatureAlgorithm), rsaDigest)
        ..init(false, PublicKeyParameter<RSAPublicKey>(key));
      try {
        return signer.verifySignature(
          signed.tbsBytes,
          RSASignature(signed.signature),
        );
      } catch (_) {
        return false;
      }
    }
    if (signed.signatureAlgorithm == _oidEcdsaSha256 ||
        signed.signatureAlgorithm == _oidEcdsaSha384) {
      final key = _ecPublicKey();
      final signer = ECDSASigner(_digestFor(signed.signatureAlgorithm), null)
        ..init(false, PublicKeyParameter<ECPublicKey>(key));
      final ecSig = _decodeEcdsaSignature(signed.signature);
      if (ecSig == null) return false;
      try {
        return signer.verifySignature(signed.tbsBytes, ecSig);
      } catch (_) {
        return false;
      }
    }
    throw X509Exception(
      'Unsupported signature algorithm ${signed.signatureAlgorithm}',
    );
  }

  static Digest _digestFor(String oid) {
    switch (oid) {
      case _oidSha256Rsa:
      case _oidEcdsaSha256:
        return SHA256Digest();
      case _oidSha384Rsa:
      case _oidEcdsaSha384:
        return SHA384Digest();
      case _oidSha512Rsa:
        return SHA512Digest();
      default:
        throw X509Exception('Unsupported digest for $oid');
    }
  }

  /// The subject public key as an EC key. Throws where it is not one.
  ECPublicKey _ecPublicKey() {
    final (algOid, params, bits) = _spki();
    if (algOid != _oidEcPublicKey) {
      throw X509Exception('Subject key is $algOid, not EC');
    }
    if (params is! ASN1ObjectIdentifier || params.identifier == null) {
      throw const X509Exception('EC key has no named curve');
    }
    final ECDomainParameters domain;
    switch (params.identifier!) {
      case _oidPrime256v1:
        domain = ECCurve_secp256r1();
      case _oidSecp384r1:
        domain = ECCurve_secp384r1();
      default:
        throw X509Exception('Unsupported curve ${params.identifier}');
    }
    if (bits.isEmpty || bits.first != 0x04) {
      throw const X509Exception('EC point is not in uncompressed form');
    }
    return ECPublicKey(domain.curve.decodePoint(bits), domain);
  }

  /// The subject public key as an RSA key. Throws where it is not one.
  RSAPublicKey _rsaPublicKey() {
    final (algOid, _, bits) = _spki();
    if (algOid != _oidRsaEncryption) {
      throw X509Exception('Subject key is $algOid, not RSA');
    }
    final inner = ASN1Parser(bits).nextObject();
    if (inner is! ASN1Sequence) {
      throw const X509Exception('RSA key is not a SEQUENCE');
    }
    final e = inner.elements;
    if (e.length != 2) {
      throw const X509Exception('RSA key does not have two integers');
    }
    final modulus = e[0];
    final exponent = e[1];
    if (modulus is! ASN1Integer || exponent is! ASN1Integer) {
      throw const X509Exception('RSA key fields are not INTEGERs');
    }
    return RSAPublicKey(modulus.valueAsBigInteger, exponent.valueAsBigInteger);
  }

  /// (algorithm OID, algorithm parameters, subjectPublicKey bits).
  (String, ASN1Object?, Uint8List) _spki() {
    final spki = ASN1Parser(subjectPublicKeyInfoDer).nextObject();
    if (spki is! ASN1Sequence) {
      throw const X509Exception('subjectPublicKeyInfo is not a SEQUENCE');
    }
    final e = spki.elements;
    if (e.length != 2) {
      throw const X509Exception('subjectPublicKeyInfo is malformed');
    }
    final alg = e[0];
    final bits = e[1];
    if (alg is! ASN1Sequence || bits is! ASN1BitString) {
      throw const X509Exception('subjectPublicKeyInfo fields are malformed');
    }
    final algParts = alg.elements;
    if (algParts.isEmpty) {
      throw const X509Exception('key algorithm is empty');
    }
    final oid = algParts[0];
    if (oid is! ASN1ObjectIdentifier || oid.identifier == null) {
      throw const X509Exception('key algorithm has no OID');
    }
    final raw = bits.valueBytes();
    if (raw.isEmpty) throw const X509Exception('subjectPublicKey is empty');
    return (
      oid.identifier!,
      algParts.length > 1 ? algParts[1] : null,
      Uint8List.fromList(raw.sublist(1)),
    );
  }

  /// Decode a DER `SEQUENCE { r INTEGER, s INTEGER }` into an [ECSignature].
  static ECSignature? _decodeEcdsaSignature(Uint8List der) {
    try {
      final seq = ASN1Parser(der).nextObject();
      if (seq is! ASN1Sequence) return null;
      final e = seq.elements;
      if (e.length != 2) return null;
      final r = e[0];
      final s = e[1];
      if (r is! ASN1Integer || s is! ASN1Integer) return null;
      return ECSignature(r.valueAsBigInteger, s.valueAsBigInteger);
    } catch (_) {
      return null;
    }
  }
}

/// Validate [chain] — leaf first, each certificate signed by the next — and
/// anchor it at one of [pinnedRoots].
///
/// Returns the leaf. Throws [X509Exception] with the reason on any failure,
/// and the reason is what the session teardown reports.
///
/// The anchor is either the last certificate being byte-identical to a pinned
/// root, or the last certificate being signed by one. Both are accepted
/// because a producer may or may not ship the root it chains to, and neither
/// is weaker: in both cases the trust ends at bytes this build carries.
X509Certificate validateChain({
  required List<Uint8List> chain,
  required List<Uint8List> pinnedRoots,
  required DateTime at,
}) {
  if (chain.isEmpty) {
    throw const X509Exception('Empty certificate chain');
  }
  if (pinnedRoots.isEmpty) {
    throw const X509Exception('No pinned roots to anchor against');
  }

  final certs = chain.map(X509Certificate.fromDer).toList(growable: false);
  final roots =
      pinnedRoots.map(X509Certificate.fromDer).toList(growable: false);

  for (final cert in certs) {
    if (!cert.isValidAt(at)) {
      throw X509Exception(
        'A certificate in the chain is outside its validity window '
        '(${cert.notBefore.toIso8601String()} to '
        '${cert.notAfter.toIso8601String()}, checked at '
        '${at.toUtc().toIso8601String()})',
      );
    }
  }

  for (var i = 0; i + 1 < certs.length; i++) {
    final child = certs[i];
    final parent = certs[i + 1];
    if (!_sameBytes(child.issuerDer, parent.subjectDer)) {
      throw X509Exception(
        'Chain link $i does not name its issuer as the next subject',
      );
    }
    if (!parent.verifies(child)) {
      throw X509Exception('Chain link $i does not verify under its issuer');
    }
  }

  final last = certs.last;
  for (final root in roots) {
    if (_sameBytes(last.der, root.der)) return certs.first;
  }
  for (final root in roots) {
    if (!_sameBytes(last.issuerDer, root.subjectDer)) continue;
    if (!root.isValidAt(at)) continue;
    if (root.verifies(last)) return certs.first;
  }
  throw const X509Exception(
    'The chain does not anchor at a pinned root',
  );
}

bool _sameBytes(Uint8List a, Uint8List b) {
  if (a.length != b.length) return false;
  for (var i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
