import 'dart:convert';

import 'package:flutter_secure_storage/flutter_secure_storage.dart';

import 'models/identity.dart';

/// Persists the device's [GrassrootsIdentity] across sessions.
///
/// Implements the spec's `putIdentity` / `getIdentity` identity lifecycle
/// (`docs/GLP_Networking_API/sections/api.tex` §Identity). The app generates an
/// identity once on first launch ([GrassrootsIdentity.generate]) and calls
/// [putIdentity] to persist it; subsequent launches restore it via
/// [getIdentity]. The Ed25519 key pair lives in the platform secure keystore
/// (iOS Keychain / Android Keystore) via flutter_secure_storage.
class IdentityStore {
  IdentityStore._();

  /// Secure-storage key holding the JSON-encoded identity.
  static const String _storageKey = 'identity';

  static const FlutterSecureStorage _storage = FlutterSecureStorage();

  /// Persist [identity] to the secure keystore, replacing any existing one.
  ///
  /// Spec `putIdentity()`: the app calls this once, right after generating the
  /// identity on first launch, so the same Ed25519 key pair is reused on every
  /// subsequent launch.
  static Future<void> putIdentity(GrassrootsIdentity identity) async {
    await _storage.write(
      key: _storageKey,
      value: jsonEncode(identity.toJson()),
    );
  }

  /// Load the persisted identity, or null if none has been stored yet.
  ///
  /// Spec `getIdentity() -> PubKey`: the spec returns just the public key, but
  /// the Dart layer returns the full [GrassrootsIdentity] because the runtime
  /// needs the private key to sign packets and run the Noise handshake. The
  /// public key the spec refers to is [GrassrootsIdentity.publicKey].
  static Future<GrassrootsIdentity?> getIdentity() async {
    final stored = await _storage.read(key: _storageKey);
    if (stored == null) return null;
    return GrassrootsIdentity.fromMap(
      jsonDecode(stored) as Map<String, dynamic>,
    );
  }

  /// Remove the persisted identity. Used when the user resets their identity.
  static Future<void> clearIdentity() async {
    await _storage.delete(key: _storageKey);
  }
}
