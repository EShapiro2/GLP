/// The platform half of the attestation exchange: the binding to the native
/// producer each platform has — App Attest on iOS, hardware key attestation
/// on Android.
///
/// Spec §Session Establishment: "Each agent holds an attestation key
/// generated in its platform's secure element and attested there once---App
/// Attest on iOS, hardware key attestation on Android---over a challenge
/// naming the agent's identity public key pk.  Per session each side sends
/// that attestation together with a signature by the attestation key over the
/// digest H("glp attest" | pk | h)".
///
/// Production needs a secure element, and so a native channel; verification
/// is the core's ([AttestationVerifier]), the same on every platform and in
/// the headless profile. This class is the binding alone: what crosses the
/// channel is the identity key and the digest one way, and the platform's own
/// attestation and signature objects the other.
///
/// THE CHANNEL, `grassroots/attestation`:
///
///  - `attest` `{identityPublicKey}` → null where this device provides no
///    attestation (an iOS simulator, a device without App Attest, an Android
///    key outside secure hardware); otherwise
///    `{platform: 'ios', appId, attestationObject}` or
///    `{platform: 'android', chain}`, the chain leaf first. The producer
///    generates the attestation key once, attests it over the identity key,
///    and keeps both: the attestation is long-lived.
///  - `sign` `{identityPublicKey, digest}` → the platform's signature over the
///    digest by the key attested for that identity key: on iOS an App Attest
///    assertion with the digest as its clientDataHash, on Android a DER
///    `SHA256withECDSA` signature.
library;

import 'dart:async';

import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';

import 'package:grassroots_networking_core/src/session/attestation_verifier.dart';
import 'package:grassroots_networking_core/src/session/platform_attestation.dart';

/// The method channel the native producers answer on.
const MethodChannel platformAttestationChannel =
    MethodChannel('grassroots/attestation');

/// [PlatformAttestation] over the platform's native producer, verifying with
/// the core's [AttestationVerifier].
class NativePlatformAttestation implements PlatformAttestation {
  NativePlatformAttestation({
    MethodChannel channel = platformAttestationChannel,
    AttestationVerifier verifier = const AttestationVerifier(),
  })  : _channel = channel,
        _verifier = verifier;

  final MethodChannel _channel;
  final AttestationVerifier _verifier;

  /// The long-lived attestation per identity key, obtained once. Concurrent
  /// sessions at startup share one request rather than racing to generate a
  /// key each; a request that fails is forgotten, so a later session asks
  /// again.
  final Map<String, Future<Uint8List?>> _attestations = {};

  @override
  Future<Uint8List?> attestationFor(Uint8List identityPublicKey) {
    final key = _hex(identityPublicKey);
    final pending = _attestations[key];
    if (pending != null) return pending;
    final request = _requestAttestation(identityPublicKey);
    _attestations[key] = request;
    unawaited(request.then<void>(
      (_) {},
      onError: (Object e) {
        debugPrint('[attest] The platform could not attest: $e');
        _attestations.remove(key);
      },
    ));
    return request;
  }

  Future<Uint8List?> _requestAttestation(Uint8List identityPublicKey) async {
    final Map<Object?, Object?>? reply;
    try {
      reply = await _channel.invokeMapMethod<Object?, Object?>(
        'attest',
        {'identityPublicKey': identityPublicKey},
      );
    } on MissingPluginException {
      // No producer is registered in this embedding — a desktop build, or an
      // app that did not register one. That is a platform with no
      // attestation, which the peer reports as unattested.
      return null;
    }
    if (reply == null) return null;
    return encodePlatformAttestation(platformAttestationFromChannel(reply));
  }

  @override
  Future<Uint8List?> signSessionDigest({
    required Uint8List identityPublicKey,
    required Uint8List digest,
  }) async {
    try {
      return await _channel.invokeMethod<Uint8List>(
        'sign',
        {'identityPublicKey': identityPublicKey, 'digest': digest},
      );
    } on MissingPluginException {
      return null;
    }
  }

  @override
  Future<AttestationVerdict> verify({
    required AttestationEvidence? evidence,
    required Uint8List digest,
    required Uint8List peerIdentityKey,
  }) async =>
      _verifier.verify(
        evidence: evidence,
        digest: digest,
        peerIdentityKey: peerIdentityKey,
      );

  static String _hex(Uint8List bytes) =>
      bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
}

/// Read a producer's `attest` reply. Throws [FormatException] on anything but
/// the two shapes the channel defines.
PlatformAttestationContent platformAttestationFromChannel(
  Map<Object?, Object?> reply,
) {
  switch (reply['platform']) {
    case 'ios':
      final appId = reply['appId'];
      final object = reply['attestationObject'];
      if (appId is! String || appId.isEmpty || object is! Uint8List) {
        throw const FormatException(
          'An iOS attestation reply carries no App ID or no attestation object',
        );
      }
      return AppAttestContent(appId: appId, attestationObject: object);
    case 'android':
      final chain = reply['chain'];
      if (chain is! List || chain.isEmpty) {
        throw const FormatException(
          'An Android attestation reply carries no certificate chain',
        );
      }
      return AndroidKeyAttestationContent([
        for (final cert in chain)
          if (cert is Uint8List)
            cert
          else
            throw const FormatException(
              'An Android attestation reply carries a certificate that is '
              'not bytes',
            ),
      ]);
    default:
      throw FormatException(
        'An attestation reply names no known platform: ${reply['platform']}',
      );
  }
}
