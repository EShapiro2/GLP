/// Android hardware key attestation: the verifier.
///
/// Spec §Session Establishment. The producing device generates its attestation
/// key in the Android Keystore with our challenge, and the keystore returns an
/// X.509 chain terminating in a Google hardware attestation root. The leaf's
/// KeyDescription extension carries the challenge, the security level, and an
/// attestationApplicationId naming the package and the SHA-256 digest of its
/// signing certificate. This file validates that chain against the pinned
/// roots and reads that extension.
///
/// The schema is Google's, at
/// `source.android.com/docs/security/features/keystore/attestation`.
///
/// What this proves, and the paper says so: that the attestation key lives in
/// the device's secure hardware, and that the application holding it carries
/// the signing identity of the attested binary. It is not a measurement of the
/// running code, and on many devices the application identity is asserted by
/// the operating system rather than by the secure element, so a device whose
/// OS is modified can forge it. The guarantee is against repackaging, not
/// against a compromised operating system.
library;

import 'dart:typed_data';

import 'package:asn1lib/asn1lib.dart';

import 'attestation_roots.dart';
import 'x509.dart';

/// OID of the KeyDescription extension Google's keystore writes.
const String kKeyDescriptionOid = '1.3.6.1.4.1.11129.2.1.17';

/// Tag of `attestationApplicationId` inside an AuthorizationList.
const int kAttestationApplicationIdTag = 709;

/// Where a key lives, per Google's `SecurityLevel`.
enum AndroidSecurityLevel {
  software(0),
  trustedEnvironment(1),
  strongBox(2);

  const AndroidSecurityLevel(this.value);
  final int value;

  /// True where the key is held by secure hardware rather than by software.
  ///
  /// A software-level attestation attests nothing this protocol wants: it says
  /// the key is in the same place an attacker with the device already is.
  bool get isHardwareBacked => this != AndroidSecurityLevel.software;

  static AndroidSecurityLevel? fromValue(int v) {
    for (final level in AndroidSecurityLevel.values) {
      if (level.value == v) return level;
    }
    return null;
  }
}

/// The application identity Android's certificate names — a package name and
/// the SHA-256 digests of the certificates it is signed with.
///
/// This is what `onPeerConnected` carries as the attested application identity.
/// No platform produces a hash of the running binary, and this file does not
/// pretend one does.
class AndroidApplicationIdentity {
  final List<String> packageNames;
  final List<Uint8List> signatureDigests;

  const AndroidApplicationIdentity({
    required this.packageNames,
    required this.signatureDigests,
  });
}

/// A parsed KeyDescription.
class AndroidKeyDescription {
  final int attestationVersion;
  final AndroidSecurityLevel attestationSecurityLevel;
  final int keymasterVersion;
  final AndroidSecurityLevel keymasterSecurityLevel;

  /// What the key was generated with — for us, the agent's identity key.
  final Uint8List attestationChallenge;

  /// The application identity, where the certificate carries one.
  ///
  /// It sits in `softwareEnforced` and not in `teeEnforced`: platform code
  /// populates it, not the secure element. That is exactly the limit the
  /// paper states, and it is why it is read from there rather than from the
  /// hardware-enforced list.
  final AndroidApplicationIdentity? applicationIdentity;

  const AndroidKeyDescription({
    required this.attestationVersion,
    required this.attestationSecurityLevel,
    required this.keymasterVersion,
    required this.keymasterSecurityLevel,
    required this.attestationChallenge,
    required this.applicationIdentity,
  });

  /// Parse the KeyDescription extension of [leaf].
  ///
  /// Throws [X509Exception] where the extension is absent or malformed.
  factory AndroidKeyDescription.fromCertificate(X509Certificate leaf) {
    final ext = leaf.extensions[kKeyDescriptionOid];
    if (ext == null) {
      throw const X509Exception(
        'The leaf carries no KeyDescription extension, so it is not a key '
        'attestation certificate',
      );
    }
    return AndroidKeyDescription.fromDer(ext);
  }

  /// Parse a KeyDescription from its DER.
  factory AndroidKeyDescription.fromDer(Uint8List der) {
    final ASN1Object top;
    try {
      top = ASN1Parser(der).nextObject();
    } catch (e) {
      throw X509Exception('KeyDescription does not parse: $e');
    }
    if (top is! ASN1Sequence) {
      throw const X509Exception('KeyDescription is not a SEQUENCE');
    }
    final e = top.elements;
    if (e.length < 8) {
      throw X509Exception(
        'KeyDescription has ${e.length} fields, fewer than the eight the '
        'schema defines',
      );
    }

    int integerAt(int i, String name) {
      final v = e[i];
      if (v is! ASN1Integer) {
        throw X509Exception('KeyDescription $name is not an INTEGER');
      }
      return v.valueAsBigInteger.toInt();
    }

    AndroidSecurityLevel levelAt(int i, String name) {
      // SecurityLevel is an ENUMERATED (tag 0x0A); asn1lib surfaces it as a
      // generic object, so the single content octet is read directly.
      final v = e[i];
      if (v.tag != 0x0A) {
        throw X509Exception('KeyDescription $name is not an ENUMERATED');
      }
      final bytes = v.valueBytes();
      if (bytes.length != 1) {
        throw X509Exception('KeyDescription $name is not a single octet');
      }
      final level = AndroidSecurityLevel.fromValue(bytes[0]);
      if (level == null) {
        throw X509Exception(
          'KeyDescription $name is ${bytes[0]}, which is not a SecurityLevel '
          'this build knows — an unknown level is refused rather than assumed '
          'safe',
        );
      }
      return level;
    }

    Uint8List octetsAt(int i, String name) {
      final v = e[i];
      if (v is! ASN1OctetString) {
        throw X509Exception('KeyDescription $name is not an OCTET STRING');
      }
      return Uint8List.fromList(v.valueBytes());
    }

    return AndroidKeyDescription(
      attestationVersion: integerAt(0, 'attestationVersion'),
      attestationSecurityLevel: levelAt(1, 'attestationSecurityLevel'),
      keymasterVersion: integerAt(2, 'keymasterVersion'),
      keymasterSecurityLevel: levelAt(3, 'keymasterSecurityLevel'),
      attestationChallenge: octetsAt(4, 'attestationChallenge'),
      // e[5] is uniqueId, e[6] softwareEnforced, e[7] teeEnforced.
      applicationIdentity: _applicationIdentity(e[6]),
    );
  }

  /// Find `attestationApplicationId` [709] in an AuthorizationList and decode
  /// it. Null where the list does not carry one.
  ///
  /// The AuthorizationList's fields are context-specific EXPLICIT tags, and
  /// 709 needs the high-tag-number form, which is walked by hand: the DER is
  /// scanned for the tag rather than fully decoded, because the list carries
  /// dozens of fields this layer has no use for and decoding them all would be
  /// surface for no gain.
  static AndroidApplicationIdentity? _applicationIdentity(ASN1Object list) {
    if (list is! ASN1Sequence) {
      throw const X509Exception('AuthorizationList is not a SEQUENCE');
    }
    for (final field in list.elements) {
      if (_tagNumberOf(field) != kAttestationApplicationIdTag) continue;
      // [709] EXPLICIT wraps an OCTET STRING whose contents are the
      // AttestationApplicationId DER.
      final inner = ASN1Parser(field.valueBytes()).nextObject();
      if (inner is! ASN1OctetString) {
        throw const X509Exception(
          'attestationApplicationId is not an OCTET STRING',
        );
      }
      return _decodeApplicationId(Uint8List.fromList(inner.valueBytes()));
    }
    return null;
  }

  /// The tag number of a context-specific field, decoding the high-tag-number
  /// form. Returns -1 where the object is not context-specific.
  static int _tagNumberOf(ASN1Object o) {
    final bytes = o.encodedBytes;
    if (bytes.isEmpty) return -1;
    final first = bytes[0];
    if (first & 0xC0 != 0x80) return -1; // not context-specific
    final low = first & 0x1F;
    if (low != 0x1F) return low; // low-tag-number form
    var value = 0;
    for (var i = 1; i < bytes.length; i++) {
      value = (value << 7) | (bytes[i] & 0x7F);
      if (bytes[i] & 0x80 == 0) return value;
      if (i > 4) break; // a tag this long is not one we recognise
    }
    return -1;
  }

  /// `AttestationApplicationId ::= SEQUENCE { package_infos SET OF
  /// AttestationPackageInfo, signature_digests SET OF OCTET_STRING }`, with
  /// `AttestationPackageInfo ::= SEQUENCE { package_name OCTET_STRING,
  /// version INTEGER }`.
  static AndroidApplicationIdentity _decodeApplicationId(Uint8List der) {
    final top = ASN1Parser(der).nextObject();
    if (top is! ASN1Sequence || top.elements.length < 2) {
      throw const X509Exception('AttestationApplicationId is malformed');
    }
    final packages = <String>[];
    final infos = top.elements[0];
    if (infos is! ASN1Set) {
      throw const X509Exception('package_infos is not a SET');
    }
    for (final info in infos.elements) {
      if (info is! ASN1Sequence || info.elements.isEmpty) {
        throw const X509Exception('AttestationPackageInfo is malformed');
      }
      final name = info.elements[0];
      if (name is! ASN1OctetString) {
        throw const X509Exception('package_name is not an OCTET STRING');
      }
      packages.add(String.fromCharCodes(name.valueBytes()));
    }

    final digests = <Uint8List>[];
    final digestSet = top.elements[1];
    if (digestSet is! ASN1Set) {
      throw const X509Exception('signature_digests is not a SET');
    }
    for (final d in digestSet.elements) {
      if (d is! ASN1OctetString) {
        throw const X509Exception('a signature digest is not an OCTET STRING');
      }
      digests.add(Uint8List.fromList(d.valueBytes()));
    }

    return AndroidApplicationIdentity(
      packageNames: packages,
      signatureDigests: digests,
    );
  }
}

/// What an Android attestation chain established, once it verified.
class AndroidAttestationResult {
  /// The leaf, whose subject key is the attestation key that signs each
  /// session's digest.
  final X509Certificate leaf;

  final AndroidKeyDescription keyDescription;

  const AndroidAttestationResult({
    required this.leaf,
    required this.keyDescription,
  });
}

/// Validate an Android key attestation chain.
///
/// [chain] is leaf-first DER. [expectedChallenge] is the agent's identity
/// public key — §Session Establishment has the attestation made "over a
/// challenge naming the agent's identity public key", and this is where that
/// binding is checked. [expectedPackageNames] and [expectedSignatureDigests],
/// where given, are the application identity this build accepts.
///
/// Throws [X509Exception] with the reason on any failure. Every failure here
/// is an attestation "offered and found invalid", which tears the session down
/// — so nothing in this path may fail open.
AndroidAttestationResult verifyAndroidAttestation({
  required List<Uint8List> chain,
  required Uint8List expectedChallenge,
  required DateTime at,
  List<Uint8List>? pinnedRoots,
  Set<String>? expectedPackageNames,
  List<Uint8List>? expectedSignatureDigests,
}) {
  final leaf = validateChain(
    chain: chain,
    pinnedRoots: pinnedRoots ?? androidAttestationRootsDer(),
    at: at,
  );

  final description = AndroidKeyDescription.fromCertificate(leaf);

  if (!description.attestationSecurityLevel.isHardwareBacked) {
    throw X509Exception(
      'The attestation is at security level '
      '${description.attestationSecurityLevel.name}, not hardware-backed',
    );
  }

  if (!_sameBytes(description.attestationChallenge, expectedChallenge)) {
    throw const X509Exception(
      'The attestation challenge is not this agent\'s identity key, so the '
      'attestation names some other key and not the one in this session',
    );
  }

  final identity = description.applicationIdentity;
  if (expectedPackageNames != null) {
    if (identity == null) {
      throw const X509Exception(
        'The certificate carries no application identity, and this build '
        'requires one',
      );
    }
    final matched =
        identity.packageNames.any(expectedPackageNames.contains);
    if (!matched) {
      throw X509Exception(
        'The attested package ${identity.packageNames.join(", ")} is not one '
        'this build accepts',
      );
    }
  }
  if (expectedSignatureDigests != null) {
    if (identity == null) {
      throw const X509Exception(
        'The certificate carries no application identity, and this build '
        'requires a signing-certificate digest',
      );
    }
    final matched = identity.signatureDigests.any(
      (d) => expectedSignatureDigests.any((e) => _sameBytes(d, e)),
    );
    if (!matched) {
      throw const X509Exception(
        'The attested signing certificate is not one this build accepts',
      );
    }
  }

  return AndroidAttestationResult(leaf: leaf, keyDescription: description);
}

bool _sameBytes(Uint8List a, Uint8List b) {
  if (a.length != b.length) return false;
  var diff = 0;
  for (var i = 0; i < a.length; i++) {
    diff |= a[i] ^ b[i];
  }
  return diff == 0;
}
