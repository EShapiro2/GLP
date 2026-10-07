/// Attestation fixtures built in the test: certificate chains, keys and
/// signatures that are real arithmetic, so that both halves of the exchange of
/// spec §Session Establishment — the attestation and the per-session signature
/// by the key it carries — can be exercised, which a fixture without its
/// private keys cannot do.
///
/// WHAT THESE ARE, AND WHAT THEY ARE NOT. Every certificate is genuine X.509,
/// every signature genuine ECDSA P-256 with SHA-256, every field the verifiers
/// check is internally consistent, and the formats are the ones Apple
/// ("Validating apps that connect to your server") and Google
/// (source.android.com/docs/security/features/keystore/attestation) document.
/// They chain to test roots, not to Apple's or Google's, and no device produced
/// them: a shared misreading of a platform's format between these builders and
/// the verifiers would pass both. That gap closes only against hardware.
library;

import 'dart:convert';
import 'dart:typed_data';

import 'package:cbor/cbor.dart';
import 'package:cryptography/dart.dart' show DartSha256;
import 'package:pointycastle/export.dart';

// ===== DER =====

Uint8List _length(int n) {
  if (n < 0x80) return Uint8List.fromList([n]);
  final bytes = <int>[];
  for (var v = n; v > 0; v >>= 8) {
    bytes.insert(0, v & 0xFF);
  }
  return Uint8List.fromList([0x80 | bytes.length, ...bytes]);
}

Uint8List derTlv(int tag, List<int> content) =>
    Uint8List.fromList([tag, ..._length(content.length), ...content]);

Uint8List derSeq(List<List<int>> items) =>
    derTlv(0x30, [for (final i in items) ...i]);

Uint8List derSet(List<List<int>> items) =>
    derTlv(0x31, [for (final i in items) ...i]);

Uint8List derInt(BigInt v) {
  if (v == BigInt.zero) return derTlv(0x02, [0]);
  final bytes = <int>[];
  for (var x = v; x > BigInt.zero; x >>= 8) {
    bytes.insert(0, (x & BigInt.from(0xFF)).toInt());
  }
  if (bytes.first & 0x80 != 0) bytes.insert(0, 0);
  return derTlv(0x02, bytes);
}

Uint8List derSmallInt(int v) => derInt(BigInt.from(v));

Uint8List derOid(String dotted) {
  final parts = dotted.split('.').map(int.parse).toList();
  final out = <int>[40 * parts[0] + parts[1]];
  for (final p in parts.skip(2)) {
    final groups = <int>[];
    var v = p;
    do {
      groups.insert(0, v & 0x7F);
      v >>= 7;
    } while (v > 0);
    for (var i = 0; i < groups.length - 1; i++) {
      groups[i] |= 0x80;
    }
    out.addAll(groups);
  }
  return derTlv(0x06, out);
}

Uint8List derOctets(List<int> bytes) => derTlv(0x04, bytes);

Uint8List derBits(List<int> bytes) => derTlv(0x03, [0, ...bytes]);

Uint8List derBool(bool v) => derTlv(0x01, [v ? 0xFF : 0x00]);

Uint8List derEnum(int v) => derTlv(0x0A, [v]);

Uint8List derUtf8(String s) => derTlv(0x0C, utf8.encode(s));

/// `[n] EXPLICIT`, constructed context-specific, low tag numbers only.
Uint8List derExplicit(int n, List<int> content) => derTlv(0xA0 | n, content);

Uint8List derUtcTime(DateTime t) {
  String two(int v) => v.toString().padLeft(2, '0');
  final u = t.toUtc();
  return derTlv(
    0x17,
    ascii.encode('${two(u.year % 100)}${two(u.month)}${two(u.day)}'
        '${two(u.hour)}${two(u.minute)}${two(u.second)}Z'),
  );
}

// ===== Hashing and keys =====

Uint8List sha256(List<int> input) =>
    Uint8List.fromList(const DartSha256().hashSync(input).bytes);

final ECDomainParameters _p256 = ECCurve_secp256r1();

/// A P-256 key pair derived from [seed], so fixtures are reproducible.
class TestEcKey {
  TestEcKey(String seed)
      : d = (_bigInt(sha256(utf8.encode(seed))) % (_p256.n - BigInt.one)) +
            BigInt.one;

  final BigInt d;

  /// The public key as an uncompressed point.
  Uint8List get publicPoint =>
      Uint8List.fromList((_p256.G * d)!.getEncoded(false));

  /// `SubjectPublicKeyInfo` for an EC P-256 key.
  Uint8List get spki => derSeq([
        derSeq([derOid('1.2.840.10045.2.1'), derOid('1.2.840.10045.3.1.7')]),
        derBits(publicPoint),
      ]);

  /// ECDSA with SHA-256 over [message], DER-encoded, deterministic (RFC 6979).
  Uint8List sign(List<int> message) {
    final signer = ECDSASigner(SHA256Digest(), HMac(SHA256Digest(), 64))
      ..init(true, PrivateKeyParameter<ECPrivateKey>(ECPrivateKey(d, _p256)));
    final sig =
        signer.generateSignature(Uint8List.fromList(message)) as ECSignature;
    return derSeq([derInt(sig.r), derInt(sig.s)]);
  }

  static BigInt _bigInt(List<int> bytes) {
    var v = BigInt.zero;
    for (final b in bytes) {
      v = (v << 8) | BigInt.from(b);
    }
    return v;
  }
}

// ===== Certificates =====

const String _oidEcdsaSha256 = '1.2.840.10045.4.3.2';

Uint8List _name(String cn) => derSeq([
      derSet([
        derSeq([derOid('2.5.4.3'), derUtf8(cn)]),
      ]),
    ]);

/// An extension: `SEQUENCE { extnID, critical BOOLEAN DEFAULT FALSE,
/// extnValue OCTET STRING }`.
Uint8List extension(String oid, List<int> value, {bool critical = false}) =>
    derSeq([
      derOid(oid),
      if (critical) derBool(true),
      derOctets(value),
    ]);

/// basicConstraints marking a CA.
Uint8List caExtension() =>
    extension('2.5.29.19', derSeq([derBool(true)]), critical: true);

/// A certificate for [subjectKey], named [subject], issued by [issuerKey]
/// under the name [issuer].
Uint8List certificate({
  required String subject,
  required TestEcKey subjectKey,
  required String issuer,
  required TestEcKey issuerKey,
  required int serial,
  List<Uint8List> extensions = const [],
  DateTime? notBefore,
  DateTime? notAfter,
}) {
  final tbs = derSeq([
    derExplicit(0, derSmallInt(2)),
    derSmallInt(serial),
    derSeq([derOid(_oidEcdsaSha256)]),
    _name(issuer),
    derSeq([
      derUtcTime(notBefore ?? DateTime.utc(2026, 1, 1)),
      derUtcTime(notAfter ?? DateTime.utc(2036, 1, 1)),
    ]),
    _name(subject),
    subjectKey.spki,
    if (extensions.isNotEmpty) derExplicit(3, derSeq(extensions)),
  ]);
  return derSeq([
    tbs,
    derSeq([derOid(_oidEcdsaSha256)]),
    derBits(issuerKey.sign(tbs)),
  ]);
}

/// A self-signed CA root.
Uint8List rootCertificate(String name, TestEcKey key) => certificate(
      subject: name,
      subjectKey: key,
      issuer: name,
      issuerKey: key,
      serial: 1,
      extensions: [caExtension()],
    );

/// The moment the fixtures are verified at: inside every window above.
final DateTime fixtureTime = DateTime.utc(2027, 1, 1);

// ===== App Attest =====

/// An App Attest producer in miniature: a root, an intermediate, and a
/// credential key, attested under [appId].
class AppAttestFixture {
  AppAttestFixture({
    this.appId = 'ABCDE12345.com.eshapiro.grassapp',
    String seed = 'app-attest',
    this.aaguid = 'appattest',
  })  : rootKey = TestEcKey('$seed root'),
        intermediateKey = TestEcKey('$seed intermediate'),
        credentialKey = TestEcKey('$seed credential');

  final String appId;
  final String aaguid;
  final TestEcKey rootKey;
  final TestEcKey intermediateKey;
  final TestEcKey credentialKey;

  late final Uint8List root =
      rootCertificate('Test App Attest Root', rootKey);

  late final Uint8List intermediate = certificate(
    subject: 'Test App Attest CA',
    subjectKey: intermediateKey,
    issuer: 'Test App Attest Root',
    issuerKey: rootKey,
    serial: 2,
    extensions: [caExtension()],
  );

  /// SHA-256 of the credential key's uncompressed point: Apple's key
  /// identifier.
  Uint8List get keyIdentifier => sha256(credentialKey.publicPoint);

  /// `authData` of the attestation: rpIdHash | flags | counter 0 | aaguid |
  /// credentialIdLength | credentialId.
  Uint8List authData({String? forAppId}) {
    final aaguidBytes = Uint8List(16)
      ..setRange(0, aaguid.length, ascii.encode(aaguid));
    return Uint8List.fromList([
      ...sha256(ascii.encode(forAppId ?? appId)),
      0x40,
      0, 0, 0, 0,
      ...aaguidBytes,
      0, 32,
      ...keyIdentifier,
    ]);
  }

  /// The attestation object over [challenge] — the agent's identity key.
  Uint8List attestationObject(Uint8List challenge, {String? forAppId}) {
    final data = authData(forAppId: forAppId);
    final nonce = sha256([...data, ...sha256(challenge)]);
    final credential = certificate(
      subject: 'Test Credential',
      subjectKey: credentialKey,
      issuer: 'Test App Attest CA',
      issuerKey: intermediateKey,
      serial: 3,
      extensions: [
        extension(
          '1.2.840.113635.100.8.2',
          derSeq([derExplicit(1, derOctets(nonce))]),
        ),
      ],
    );
    return Uint8List.fromList(cborEncode(CborMap({
      CborString('fmt'): CborString('apple-appattest'),
      CborString('attStmt'): CborMap({
        CborString('x5c'): CborList([
          CborBytes(credential),
          CborBytes(intermediate),
        ]),
        CborString('receipt'): CborBytes([0x00]),
      }),
      CborString('authData'): CborBytes(data),
    })));
  }

  /// An assertion over [clientDataHash] — the session's digest — by
  /// [signer], the credential key unless another is given.
  Uint8List assertion(
    Uint8List clientDataHash, {
    int counter = 1,
    String? forAppId,
    TestEcKey? signer,
  }) {
    final authenticatorData = Uint8List.fromList([
      ...sha256(ascii.encode(forAppId ?? appId)),
      0x40,
      (counter >> 24) & 0xFF,
      (counter >> 16) & 0xFF,
      (counter >> 8) & 0xFF,
      counter & 0xFF,
    ]);
    final nonce = sha256([...authenticatorData, ...clientDataHash]);
    return Uint8List.fromList(cborEncode(CborMap({
      CborString('signature'):
          CborBytes((signer ?? credentialKey).sign(nonce)),
      CborString('authenticatorData'): CborBytes(authenticatorData),
    })));
  }
}

// ===== Android key attestation =====

/// An Android keystore in miniature: a root, an intermediate, and an
/// attestation key whose leaf names [packageName] at [version], signed with a
/// certificate whose SHA-256 is [signingDigest].
class AndroidFixture {
  AndroidFixture({
    this.packageName = 'com.eshapiro.grassapp',
    this.version = 7,
    String seed = 'android',
    this.securityLevel = 1,
  })  : rootKey = TestEcKey('$seed root'),
        intermediateKey = TestEcKey('$seed intermediate'),
        attestationKey = TestEcKey('$seed attestation'),
        signingDigest = sha256(utf8.encode('$seed signing certificate'));

  final String packageName;
  final int version;
  final int securityLevel;
  final TestEcKey rootKey;
  final TestEcKey intermediateKey;
  final TestEcKey attestationKey;
  final Uint8List signingDigest;

  late final Uint8List root =
      rootCertificate('Test Android Attestation Root', rootKey);

  late final Uint8List intermediate = certificate(
    subject: 'Test Android Attestation CA',
    subjectKey: intermediateKey,
    issuer: 'Test Android Attestation Root',
    issuerKey: rootKey,
    serial: 2,
    extensions: [caExtension()],
  );

  /// Google's KeyDescription over [challenge], the application identity in
  /// softwareEnforced at tag 709.
  static Uint8List keyDescription({
    required Uint8List challenge,
    required String packageName,
    required int version,
    required Uint8List signingDigest,
    int securityLevel = 1,
  }) {
    final applicationId = derSeq([
      derSet([
        derSeq([derOctets(utf8.encode(packageName)), derSmallInt(version)]),
      ]),
      derSet([derOctets(signingDigest)]),
    ]);
    // [709] EXPLICIT: the high-tag-number form 0xBF 0x85 0x45.
    final wrapped = derOctets(applicationId);
    final tagged = Uint8List.fromList(
      [0xBF, 0x85, 0x45, ..._length(wrapped.length), ...wrapped],
    );
    return derSeq([
      derSmallInt(200),
      derEnum(securityLevel),
      derSmallInt(200),
      derEnum(securityLevel),
      derOctets(challenge),
      derOctets(const []),
      derSeq([tagged]),
      derSeq(const []),
    ]);
  }

  /// The attestation leaf for [attestationKey] over [challenge].
  Uint8List leaf(Uint8List challenge) => certificate(
        subject: 'Android Keystore Key',
        subjectKey: attestationKey,
        issuer: 'Test Android Attestation CA',
        issuerKey: intermediateKey,
        serial: 3,
        extensions: [
          extension(
            '1.3.6.1.4.1.11129.2.1.17',
            keyDescription(
              challenge: challenge,
              packageName: packageName,
              version: version,
              signingDigest: signingDigest,
              securityLevel: securityLevel,
            ),
          ),
        ],
      );

  /// The chain over [challenge], leaf first, as the keystore returns it.
  List<Uint8List> chain(Uint8List challenge) =>
      [leaf(challenge), intermediate, root];

  /// The per-session signature over [digest]: SHA256withECDSA by the
  /// attestation key.
  Uint8List sign(Uint8List digest) => attestationKey.sign(digest);
}
