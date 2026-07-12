import 'dart:convert' show utf8;
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:cryptography/dart.dart' show DartSha256;

/// Identity provided by GSG layer to Grassroots transport.
///
/// GSG is responsible for:
/// - Generating and persisting the Ed25519 keypair
/// - Passing it to Grassroots at initialization
///
/// Grassroots uses this for:
/// - Deriving the rotating BLE service UUID (Grassroots prefix + first 8 bytes
///   of SHA-256("glp ble suffix" | pubkey | slot))
/// - Signing packets
/// - Peer identification via ANNOUNCE
class GrassrootsIdentity {
  /// Ed25519 public key (32 bytes)
  late final Uint8List publicKey;

  /// Ed25519 private key (64 bytes - seed + public key)
  /// This is kept private and used only for signing
  late final Uint8List privateKey;

  final SimpleKeyPair keyPair;

  /// Optional human-readable nickname for ANNOUNCE (mutable)
  String nickname;

  // Private constructor - use create() factory method instead
  GrassrootsIdentity._internal({
    required this.keyPair,
    required this.nickname,
    required this.publicKey,
    required this.privateKey,
  });

  /// Create identity from a keypair (use this instead of constructor)
  static Future<GrassrootsIdentity> create({
    required SimpleKeyPair keyPair,
    required String nickname,
  }) async {
    final pk = await keyPair.extractPublicKey();
    final publicKey = Uint8List.fromList(pk.bytes);
    if (publicKey.length != 32) {
      throw ArgumentError('Public key must be 32 bytes (Ed25519)');
    }

    final seed = await keyPair.extractPrivateKeyBytes();
    final privateKey = Uint8List.fromList([...seed, ...pk.bytes]);
    if (privateKey.length != 64) {
      throw ArgumentError(
          'Private key must be 64 bytes (Ed25519 seed + pubkey)');
    }

    return GrassrootsIdentity._internal(
      keyPair: keyPair,
      publicKey: publicKey,
      privateKey: privateKey,
      nickname: nickname,
    );
  }

  /// Generate a fresh Ed25519 identity.
  ///
  /// Spec `putIdentity()` (`docs/GLP_Networking_API/sections/api.tex` §Identity)
  /// generates *and* persists; here generation lives on the model and
  /// persistence on `IdentityStore.putIdentity`. The app calls this once on
  /// first launch and persists the result. When [nickname] is omitted a
  /// placeholder is derived from the public key.
  static Future<GrassrootsIdentity> generate({String? nickname}) async {
    final keyPair = await Ed25519().newKeyPair();
    final pk = await keyPair.extractPublicKey();
    final resolvedNickname = nickname ??
        'User_${pk.bytes.sublist(0, 4).map((b) => b.toRadixString(16).padLeft(2, '0')).join()}';
    return create(keyPair: keyPair, nickname: resolvedNickname);
  }

  /// Static 8-byte prefix identifying Grassroots devices on BLE.
  /// First 8 bytes of SHA-256("grassroots").
  static const String grassrootsUuidPrefix = '84c403160871e5ad';

  /// Domain-separation label for the rotating BLE suffix
  /// (spec `GLP_Networking_API` §BLE Identity and Advertising).
  static const String bleSuffixLabel = 'glp ble suffix';

  /// Length of one BLE advertising time slot. Matches the ~15-minute period
  /// of BLE address randomization, so the advertised UUID and the radio MAC
  /// rotate together and neither outlives the other as a tracking handle.
  static const Duration bleSlotDuration = Duration(minutes: 15);

  /// The current BLE time slot: wall-clock milliseconds since epoch divided
  /// by [bleSlotDuration]. Local and unsynchronized across devices — which is
  /// why recognition always matches the current AND adjacent slots
  /// ([candidateServiceUuids]); adjacent-slot coverage absorbs clock skew and
  /// slot-boundary races.
  static int currentBleSlot({DateTime? now}) =>
      (now ?? DateTime.now()).millisecondsSinceEpoch ~/
      bleSlotDuration.inMilliseconds;

  /// Derive a peer's BLE service UUID for a specific time [slot]:
  /// Grassroots prefix (8 bytes) + first 8 bytes of
  /// SHA-256("glp ble suffix" | pubkey | slot), slot encoded as 8-byte
  /// big-endian (spec `GLP_Networking_API` §BLE Identity and Advertising).
  ///
  /// The rotating suffix lets anyone who knows the public key recognize this
  /// agent before connecting (compute the suffix for the current and adjacent
  /// slots and match), while a third party that does not know the key sees a
  /// suffix that changes every slot and cannot use it to track the device.
  /// The UUID is a discovery hint, not an authorization proof: the full
  /// public key is authenticated only by the signed ANNOUNCE.
  static String deriveServiceUuidForSlot(Uint8List pubkey, int slot) {
    if (pubkey.length < 32) {
      throw ArgumentError('Public key must be at least 32 bytes');
    }
    final input = <int>[
      ...utf8.encode(bleSuffixLabel),
      ...pubkey,
      for (var i = 7; i >= 0; i--) (slot >> (8 * i)) & 0xff,
    ];
    final suffixBytes =
        const DartSha256().hashSync(input).bytes.sublist(0, 8);
    final suffix =
        suffixBytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    final hex = '$grassrootsUuidPrefix$suffix';
    return '${hex.substring(0, 8)}-'
        '${hex.substring(8, 12)}-'
        '${hex.substring(12, 16)}-'
        '${hex.substring(16, 20)}-'
        '${hex.substring(20, 32)}';
  }

  /// The service UUIDs by which [pubkey] may currently be advertising:
  /// previous, current, and next slot (lowercase). Recognition matches
  /// against this set rather than a single UUID so unsynchronized clocks and
  /// slot-boundary races never break friend recognition (spec: "late rotation
  /// in the background is absorbed by the adjacent-slot match").
  static Set<String> candidateServiceUuids(Uint8List pubkey, {DateTime? now}) {
    final slot = currentBleSlot(now: now);
    return {
      for (var delta = -1; delta <= 1; delta++)
        deriveServiceUuidForSlot(pubkey, slot + delta),
    };
  }

  /// The BLE service UUID this identity advertises *right now* — the
  /// current-slot derivation. Time-dependent: callers that cache it must
  /// refresh on slot boundaries (the BLE transport re-advertises each slot).
  String get bleServiceUuid =>
      deriveServiceUuidForSlot(publicKey, currentBleSlot());

  /// Short display fingerprint from the first 8 bytes of the public key.
  /// Full verification uses the complete public key.
  String get shortFingerprint {
    return publicKey
        .sublist(0, 8)
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join(':')
        .toUpperCase();
  }

  @override
  String toString() => 'GrassrootsIdentity($nickname)';

  static GrassrootsIdentity fromMap(Map<String, dynamic> map) {
    final pk = Uint8List.fromList(List<int>.from(map['publicKey']));
    final privatek = Uint8List.fromList(List<int>.from(map['privateKey']));
    final keyPair = SimpleKeyPairData(
      privatek.sublist(0, 32),
      publicKey: SimplePublicKey(pk, type: KeyPairType.ed25519),
      type: KeyPairType.ed25519,
    );
    // Use internal constructor since we already have validated keys from storage
    return GrassrootsIdentity._internal(
      keyPair: keyPair,
      publicKey: pk,
      privateKey: privatek,
      nickname: map['nickname'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'publicKey': publicKey,
      'privateKey': privateKey,
      'nickname': nickname,
    };
  }
}
