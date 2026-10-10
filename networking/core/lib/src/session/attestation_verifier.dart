/// The verifier of a peer's attestation offer, for both platforms.
///
/// Spec §Session Establishment: "Per session each side sends that attestation
/// together with a signature by the attestation key over the digest
/// H("glp attest" | pk | h), where h is the final Noise handshake hash, and
/// each side verifies both: the attestation against the platform's root, and
/// the signature against the key the attestation carries.  Either failing
/// tears the session down, and onPeerConnected does not fire."
///
/// Verification is arithmetic over bytes against pinned roots, so it lives in
/// the core and runs the same on every platform and in the headless profile:
/// a phone verifies a peer of the other platform, and a server with no
/// attestation of its own verifies both. Production needs a secure element and
/// lives above the core, in the platform's producer.
///
/// THE ATTESTATION FIELD. The evidence framing ([encodeAttestationPayload])
/// carries the attestation as opaque bytes. Those bytes open with a platform
/// tag, so a verifier knows which platform's root and format to check them
/// against:
///
///   0x01 App Attest:   appIdLength(2, big-endian) | appId (UTF-8) |
///                      the attestation object (CBOR), to the end
///   0x02 Android key attestation:
///                      certificateCount(1) |
///                      { certificateLength(4, big-endian) | DER }...,
///                      leaf first, nothing after
///
/// The App ID travels beside the attestation object because App Attest's
/// `authData` carries only its SHA-256, the rpIdHash, under Apple's signature:
/// the identifier is accepted only where its SHA-256 is that hash, and is then
/// the attested application identifier the spec has `onPeerConnected` carry.
///
/// THE SIGNATURE FIELD is the platform's own signature object: on iOS the App
/// Attest assertion (CBOR) over the digest, on Android the DER ECDSA
/// signature over it.
library;

import 'dart:convert';
import 'dart:typed_data';

import 'android_key_attestation.dart';
import 'app_attest.dart';
import 'application_identity.dart';
import 'platform_attestation.dart';

/// Platform tag: an App Attest attestation follows.
const int kAttestationPlatformAppAttest = 0x01;

/// Platform tag: an Android key attestation chain follows.
const int kAttestationPlatformAndroid = 0x02;

/// A platform's long-lived attestation, as the attestation field carries it.
sealed class PlatformAttestationContent {
  const PlatformAttestationContent();
}

/// App Attest: the App ID the key was attested under, and the attestation
/// object.
class AppAttestContent extends PlatformAttestationContent {
  final String appId;
  final Uint8List attestationObject;

  const AppAttestContent({
    required this.appId,
    required this.attestationObject,
  });
}

/// Android key attestation: the certificate chain, leaf first.
class AndroidKeyAttestationContent extends PlatformAttestationContent {
  final List<Uint8List> chain;

  const AndroidKeyAttestationContent(this.chain);
}

/// Frame a platform's attestation for the attestation field.
Uint8List encodePlatformAttestation(PlatformAttestationContent content) {
  switch (content) {
    case AppAttestContent(:final appId, :final attestationObject):
      final id = utf8.encode(appId);
      if (id.isEmpty || id.length > 0xFFFF) {
        throw ArgumentError('An App ID of ${id.length} bytes cannot be framed');
      }
      if (attestationObject.isEmpty) {
        throw ArgumentError('Cannot frame an empty attestation object');
      }
      return Uint8List.fromList([
        kAttestationPlatformAppAttest,
        id.length >> 8,
        id.length & 0xFF,
        ...id,
        ...attestationObject,
      ]);
    case AndroidKeyAttestationContent(:final chain):
      if (chain.isEmpty || chain.length > 0xFF) {
        throw ArgumentError(
          'A chain of ${chain.length} certificates cannot be framed',
        );
      }
      final out = BytesBuilder()
        ..addByte(kAttestationPlatformAndroid)
        ..addByte(chain.length);
      for (final cert in chain) {
        if (cert.isEmpty) {
          throw ArgumentError('Cannot frame an empty certificate');
        }
        final length = ByteData(4)..setUint32(0, cert.length, Endian.big);
        out
          ..add(length.buffer.asUint8List())
          ..add(cert);
      }
      return out.toBytes();
  }
}

/// Read the attestation field. Throws [FormatException] on anything but the
/// two forms above: there is no old version in the wild, and a field this
/// build cannot read is malformed, not an older shape to be read tolerantly.
PlatformAttestationContent decodePlatformAttestation(Uint8List bytes) {
  if (bytes.isEmpty) {
    throw const FormatException('Attestation field is empty');
  }
  switch (bytes[0]) {
    case kAttestationPlatformAppAttest:
      if (bytes.length < 3) {
        throw const FormatException(
          'App Attest field is too short for its App ID length',
        );
      }
      final idLength = (bytes[1] << 8) | bytes[2];
      if (idLength == 0) {
        throw const FormatException('App Attest field carries no App ID');
      }
      final idEnd = 3 + idLength;
      if (bytes.length <= idEnd) {
        throw const FormatException(
          'App Attest field carries no attestation object after its App ID',
        );
      }
      final String appId;
      try {
        appId = utf8.decode(Uint8List.sublistView(bytes, 3, idEnd));
      } on FormatException {
        throw const FormatException('App Attest field\'s App ID is not UTF-8');
      }
      return AppAttestContent(
        appId: appId,
        attestationObject: Uint8List.sublistView(bytes, idEnd),
      );
    case kAttestationPlatformAndroid:
      if (bytes.length < 2) {
        throw const FormatException(
          'Android field is too short for its certificate count',
        );
      }
      final count = bytes[1];
      if (count == 0) {
        throw const FormatException('Android field carries no certificate');
      }
      final chain = <Uint8List>[];
      var offset = 2;
      for (var i = 0; i < count; i++) {
        if (bytes.length < offset + 4) {
          throw FormatException(
            'Android field ends inside certificate $i\'s length',
          );
        }
        final length = ByteData.sublistView(bytes, offset, offset + 4)
            .getUint32(0, Endian.big);
        offset += 4;
        if (length == 0 || bytes.length < offset + length) {
          throw FormatException(
            'Android field\'s certificate $i is empty or truncated',
          );
        }
        chain.add(Uint8List.sublistView(bytes, offset, offset + length));
        offset += length;
      }
      if (offset != bytes.length) {
        throw FormatException(
          'Android field carries ${bytes.length - offset} bytes after its '
          'chain',
        );
      }
      return AndroidKeyAttestationContent(chain);
    default:
      throw FormatException('Unknown attestation platform tag ${bytes[0]}');
  }
}

/// Verifies a peer's offer against the platforms' pinned roots.
///
/// The roots default to the real ones (`attestation_roots.dart`); a test
/// passes its own.
class AttestationVerifier {
  const AttestationVerifier({
    this.clock,
    this.appAttestRoots,
    this.androidRoots,
    this.allowAppAttestDevelopment = false,
  });

  /// The time certificate validity is checked at; the wall clock by default.
  final DateTime Function()? clock;

  /// The App Attest roots to anchor at; Apple's by default.
  final List<Uint8List>? appAttestRoots;

  /// The Android roots to anchor at; Google's two by default.
  final List<Uint8List>? androidRoots;

  /// Whether an attestation from the App Attest development sandbox is
  /// accepted. Production only by default.
  final bool allowAppAttestDevelopment;

  /// The verdict on [evidence], sent by the peer holding [peerIdentityKey],
  /// over this session's [digest] for that key.
  ///
  /// No evidence is [UnattestedPlatform]. Evidence that verifies is
  /// [AttestedApplication] with the identity the platform attested. Anything
  /// else offered — malformed, chained elsewhere, naming another key or
  /// application, signed over another session — is [InvalidAttestation]: it
  /// was "offered and found invalid", and the session is torn down.
  AttestationVerdict verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) {
    if (evidence == null) {
      return const UnattestedPlatform("the peer's platform offers none");
    }
    final at = (clock ?? DateTime.now)();
    try {
      switch (decodePlatformAttestation(evidence.attestation)) {
        case AppAttestContent(:final appId, :final attestationObject):
          verifyAppAttestEvidence(
            attestationObject: attestationObject,
            appId: appId,
            assertion: evidence.signature,
            digest: digest,
            expectedChallenge: peerIdentityKey,
            at: at,
            pinnedRoots: appAttestRoots,
            allowDevelopmentEnvironment: allowAppAttestDevelopment,
          );
          return AttestedApplication(IosApplicationIdentity(appId));
        case AndroidKeyAttestationContent(:final chain):
          final result = verifyAndroidEvidence(
            chain: chain,
            signature: evidence.signature,
            digest: digest,
            expectedChallenge: peerIdentityKey,
            at: at,
            pinnedRoots: androidRoots,
          );
          final identity = result.keyDescription.applicationIdentity;
          if (identity == null) {
            return const InvalidAttestation(
              'The Android attestation names no application',
            );
          }
          return AttestedApplication(identity);
      }
    } catch (e) {
      // Every failure of an offered attestation is a failure: X509Exception
      // and FormatException carry the reason, and anything else a parser
      // throws on an adversary's bytes is the same verdict.
      return InvalidAttestation('$e');
    }
  }
}
