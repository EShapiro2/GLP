import 'dart:convert';

import 'package:flutter_secure_storage/flutter_secure_storage.dart';

import 'package:grassroots_networking_core/src/models/identity.dart';

/// Persists the device's [GrassrootsIdentity] across sessions.
///
/// The persistence "above the networking layer" of spec §Identity: "The key
/// pair is generated and persisted above the networking layer and is supplied
/// to the layer's constructor when it is instantiated"; the API itself has no
/// call that installs an identity. The app generates an identity once on first
/// launch ([GrassrootsIdentity.generate]) and calls [putIdentity] to persist
/// it; subsequent launches restore it via [getIdentity], and the app hands it
/// to the layer's constructor. The Ed25519 key pair lives in the platform secure keystore
/// (iOS Keychain / Android Keystore) via flutter_secure_storage.
class IdentityStore {
  IdentityStore._();

  /// Secure-storage key holding the JSON-encoded identity.
  static const String _storageKey = 'identity';

  static const FlutterSecureStorage _storage = FlutterSecureStorage();

  /// Persist [identity] to the secure keystore, replacing any existing one.
  ///
  /// The app calls this once, right after generating the identity on first
  /// launch, so the same Ed25519 key pair is reused on every subsequent
  /// launch.
  static Future<void> putIdentity(GrassrootsIdentity identity) async {
    await _storage.write(
      key: _storageKey,
      value: jsonEncode(identity.toJson()),
    );
  }

  /// Load the persisted identity, or null if none has been stored yet.
  ///
  /// The full [GrassrootsIdentity], key pair included, which the layer's
  /// constructor takes; not the API's `getIdentity() → PubKey`, which the
  /// layer answers with [GrassrootsIdentity.publicKey].
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
