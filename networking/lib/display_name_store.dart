import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:shared_preferences/shared_preferences.dart';

/// The demo app's display name for this device, persisted in
/// SharedPreferences under a key of its own.
///
/// The name is the app's, outside the layer: the layer's identity is the key
/// pair and nothing else (GNA §2.1), and nothing of the name travels on the
/// wire --- ANNOUNCE carries the key, the protocol version and the signature
/// (§2.4), and the advertisement the recognition token (§3.1).
class DisplayNameStore {
  static const String _key = 'grassroots_demo_display_name_v1';

  /// The stored display name, or [defaultFor] [identity] when none is stored.
  Future<String> load(GrassrootsIdentity identity) async {
    final prefs = await SharedPreferences.getInstance();
    return prefs.getString(_key) ?? defaultFor(identity);
  }

  Future<void> save(String name) async {
    final prefs = await SharedPreferences.getInstance();
    await prefs.setString(_key, name);
  }

  /// Forget the stored name, as when the identity is regenerated: the new key
  /// pair starts with its own default.
  Future<void> clear() async {
    final prefs = await SharedPreferences.getInstance();
    await prefs.remove(_key);
  }

  /// The name shown until the person sets one: `User_` and the first four
  /// bytes of the public key in hex.
  static String defaultFor(GrassrootsIdentity identity) =>
      'User_${identity.publicKey.sublist(0, 4).map((b) => b.toRadixString(16).padLeft(2, '0')).join()}';
}
