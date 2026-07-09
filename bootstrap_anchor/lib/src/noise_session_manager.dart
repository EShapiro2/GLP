import 'dart:async';
import 'dart:convert';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart' as libsodium;

import 'identity.dart';
import 'packet.dart';

// Wire-format constants — must match the client's
// lib/src/session/noise_session_manager.dart so the two ends interop.
const _noiseProtocolName = 'Noise_XX_25519_ChaChaPoly_SHA256';
const _handshakePayloadVersion = 2;
const _applicationPayloadVersion = 1;
const _aeadMacLength = 16;
const _signatureLength = 64;

enum NoiseHandshakeRole { initiator, responder }

enum _NoiseHandshakeMessage {
  message1(1),
  message2(2),
  message3(3);

  final int value;
  const _NoiseHandshakeMessage(this.value);

  static _NoiseHandshakeMessage fromValue(int value) {
    return _NoiseHandshakeMessage.values.firstWhere(
      (message) => message.value == value,
      orElse: () => throw FormatException('Unknown Noise message: $value'),
    );
  }
}

class NoiseHandshakeResult {
  final Uint8List? responsePayload;
  final bool sessionEstablished;

  /// The peer identity this handshake message claims (message 1: verified by
  /// its signed identity claim; message 3: verified against the
  /// Noise-authenticated static). Lets the server route the response and map
  /// the connection without any identity on the wire frame.
  final Uint8List remotePubkey;

  const NoiseHandshakeResult({
    required this.remotePubkey,
    this.responsePayload,
    this.sessionEstablished = false,
  });
}

/// Anchor-side Noise XX session state, keyed by the remote peer's pubkey hex.
///
/// The anchor only ever speaks Noise over UDP, so unlike the client there is
/// no per-transport keying. The static Noise key is the X25519 form of the
/// anchor's Ed25519 identity, derived via the standard birational map
/// (libsodium `crypto_sign_ed25519_*_to_curve25519`) — exactly as the client
/// derives its own. Because the public half is a public function of the
/// Ed25519 public key, the client recomputes the anchor's expected static from
/// the anchor's known identity and verifies the Noise-delivered static against
/// it; the anchor performs the reciprocal check on the client's static (see
/// [_verifyRemoteStatic]). A single identity file backs both signing and Noise.
class NoiseSessionManager {
  final AnchorIdentity identity;

  /// libsodium handle providing the Ed25519↔X25519 conversion used to derive
  /// and verify Noise static keys.
  final libsodium.SodiumSumo sodium;

  final Map<String, _SessionEntry> _entries = {};

  Future<SimpleKeyPair>? _staticKeyPairFuture;

  NoiseSessionManager({required this.identity, required this.sodium});

  bool hasSession(String remotePubkeyHex) {
    return _entries[remotePubkeyHex]?.session != null;
  }

  /// The anchor's Noise static public key (X25519). Equals
  /// `pkToCurve25519(identity.publicKey)`; clients verify the handshake-
  /// delivered static against this value.
  Future<Uint8List> staticPublicKey() async {
    final keyPair = await _staticKeyPair();
    final public = await keyPair.extractPublicKey();
    return Uint8List.fromList(public.bytes);
  }

  /// Process an incoming `noiseHandshake` packet. Returns the response payload
  /// (or null if no response should be sent), whether the session is now
  /// established, and the peer identity carried in the handshake envelope.
  Future<NoiseHandshakeResult> handleHandshakePacket(
    GrassrootsPacket packet,
  ) async {
    final (message, remotePubkey, signature, body) =
        _decodeHandshakePayload(packet.payload);
    if (message == _NoiseHandshakeMessage.message1 &&
        !_verifyIdentityClaim(remotePubkey, signature!, body)) {
      throw const FormatException(
          'Noise message 1 identity claim failed verification');
    }
    final remoteHex = _hex(remotePubkey);
    final entry = _entries.putIfAbsent(remoteHex, _SessionEntry.new);

    switch (message) {
      case _NoiseHandshakeMessage.message1:
        return _handleMessage1(entry, remoteHex, remotePubkey, body);
      case _NoiseHandshakeMessage.message2:
        // The anchor never initiates; an inbound message2 is unexpected.
        return NoiseHandshakeResult(remotePubkey: remotePubkey);
      case _NoiseHandshakeMessage.message3:
        return _handleMessage3(entry, remoteHex, remotePubkey, body);
    }
  }

  /// Encrypt a clear application packet using the session for [remotePubkeyHex].
  /// Throws if no session exists.
  Future<GrassrootsPacket> encryptPacket(
    GrassrootsPacket packet, {
    required String remotePubkeyHex,
  }) async {
    if (!packet.type.usesSessionSecurity) return packet;

    final session = _entries[remotePubkeyHex]?.session;
    if (session == null) {
      throw StateError('No Noise session for $remotePubkeyHex');
    }

    final encryptedPayload = await session.encryptPayload(packet);
    return packet.copyWith(
      type: packet.type.secureVariant,
      payload: encryptedPayload,
    );
  }

  /// Decrypt an incoming encrypted packet using the session for
  /// [remotePubkeyHex] — the identity the server resolved for the connection
  /// the packet arrived on. Throws if no session exists or the packet is
  /// malformed.
  Future<GrassrootsPacket> decryptPacket(
    GrassrootsPacket packet, {
    required String remotePubkeyHex,
  }) async {
    if (!packet.type.isSessionEncrypted) {
      throw ArgumentError('Not a session-encrypted packet: ${packet.type}');
    }

    final session = _entries[remotePubkeyHex]?.session;
    if (session == null) {
      throw StateError('No Noise session for $remotePubkeyHex');
    }

    final clearType = packet.type.clearVariant;
    final clearPayload = await session.decryptPayload(packet, clearType);
    return packet.copyWith(
      type: clearType,
      payload: clearPayload,
    );
  }

  void reset(String remotePubkeyHex) {
    _entries.remove(remotePubkeyHex);
  }

  void dispose() {
    _entries.clear();
  }

  Future<NoiseHandshakeResult> _handleMessage1(
    _SessionEntry entry,
    String remoteHex,
    Uint8List remotePubkey,
    Uint8List body,
  ) async {
    final handshake = await _NoiseHandshakeState.create(
      role: NoiseHandshakeRole.responder,
      localStaticKeyPair: await _staticKeyPair(),
    );
    await handshake.readMessage1(body);
    final responseBody = await handshake.writeMessage2();
    entry
      ..session = null
      ..handshake = handshake;
    return NoiseHandshakeResult(
      remotePubkey: remotePubkey,
      responsePayload: _encodeHandshakePayload(
        _NoiseHandshakeMessage.message2,
        identity.publicKey,
        responseBody,
      ),
    );
  }

  Future<NoiseHandshakeResult> _handleMessage3(
    _SessionEntry entry,
    String remoteHex,
    Uint8List remotePubkey,
    Uint8List body,
  ) async {
    final handshake = entry.handshake;
    if (handshake == null ||
        handshake.role != NoiseHandshakeRole.responder ||
        entry.session != null) {
      return NoiseHandshakeResult(remotePubkey: remotePubkey);
    }

    await handshake.readMessage3(body);
    if (!_verifyRemoteStatic(handshake, remotePubkey)) {
      // The presented Noise static does not belong to the claimed Ed25519
      // identity — a tampered handshake or impersonation attempt. Abort.
      _entries.remove(remoteHex);
      return NoiseHandshakeResult(remotePubkey: remotePubkey);
    }
    final session = await handshake.splitForResponder();
    entry
      ..session = session
      ..handshake = null;
    return NoiseHandshakeResult(
      remotePubkey: remotePubkey,
      sessionEstablished: true,
    );
  }

  bool _verifyIdentityClaim(
    Uint8List claim,
    Uint8List signature,
    Uint8List body,
  ) {
    try {
      return sodium.crypto.sign.verifyDetached(
        signature: signature,
        message: Uint8List.fromList([...claim, ...body]),
        publicKey: claim,
      );
    } catch (_) {
      return false;
    }
  }

  Future<SimpleKeyPair> _staticKeyPair() {
    return _staticKeyPairFuture ??= () async {
      // Derive the Noise static key from the Ed25519 identity via the standard
      // birational map, so its public half is recomputable from the identity's
      // public key by any peer (see [_verifyRemoteStatic]). identity.privateKey
      // is the 64-byte Ed25519 secret key (seed ‖ public), as libsodium's
      // sk→curve conversion expects.
      final edSecret =
          libsodium.SecureKey.fromList(sodium, identity.privateKey);
      try {
        final curveSecret = sodium.crypto.sign.skToCurve25519(edSecret);
        try {
          return SimpleKeyPairData(
            curveSecret.extractBytes(),
            publicKey: SimplePublicKey(
              sodium.crypto.sign.pkToCurve25519(identity.publicKey),
              type: KeyPairType.x25519,
            ),
            type: KeyPairType.x25519,
          );
        } finally {
          curveSecret.dispose();
        }
      } finally {
        edSecret.dispose();
      }
    }();
  }

  /// Verify the Noise-delivered remote static key equals the X25519 form of the
  /// claimed sender's Ed25519 identity ([remotePubkey]). A mismatch — a peer
  /// presenting a static that does not belong to the identity it claims, or a
  /// tampered handshake — aborts the handshake. Implements the key-binding
  /// check in `docs/GLP_Networking_API/sections/ip.tex` §IP Connection, and is
  /// the reciprocal of the client's own check.
  bool _verifyRemoteStatic(
    _NoiseHandshakeState handshake,
    Uint8List remotePubkey,
  ) {
    final delivered = handshake.remoteStaticPublicKey;
    if (delivered == null) return false;
    final Uint8List expected;
    try {
      expected = sodium.crypto.sign.pkToCurve25519(remotePubkey);
    } catch (_) {
      // Not a valid Ed25519 point — cannot be an honest peer's key.
      return false;
    }
    return _bytesEqual(delivered.bytes, expected);
  }

  static bool _bytesEqual(List<int> a, List<int> b) {
    if (a.length != b.length) return false;
    var diff = 0;
    for (var i = 0; i < a.length; i++) {
      diff |= a[i] ^ b[i];
    }
    return diff == 0;
  }
}

class _SessionEntry {
  _NoiseTransportSession? session;
  _NoiseHandshakeState? handshake;
}

class _NoiseHandshakeState {
  final NoiseHandshakeRole role;
  final SimpleKeyPair localStaticKeyPair;
  final _NoiseSymmetricState symmetric;
  final X25519 _x25519 = X25519();

  SimpleKeyPair? _localEphemeralKeyPair;
  SimplePublicKey? _remoteEphemeralPublicKey;
  SimplePublicKey? _remoteStaticPublicKey;
  _NoiseTransportSession? _responderSession;

  /// The remote static key decrypted from message 3, or null before then.
  SimplePublicKey? get remoteStaticPublicKey => _remoteStaticPublicKey;

  _NoiseHandshakeState._({
    required this.role,
    required this.localStaticKeyPair,
    required this.symmetric,
  });

  static Future<_NoiseHandshakeState> create({
    required NoiseHandshakeRole role,
    required SimpleKeyPair localStaticKeyPair,
  }) async {
    final symmetric = await _NoiseSymmetricState.initialize();
    return _NoiseHandshakeState._(
      role: role,
      localStaticKeyPair: localStaticKeyPair,
      symmetric: symmetric,
    );
  }

  Future<void> readMessage1(Uint8List body) async {
    if (body.length != 32) {
      throw const FormatException('Noise message 1 must be 32 bytes');
    }
    _remoteEphemeralPublicKey = SimplePublicKey(
      Uint8List.fromList(body),
      type: KeyPairType.x25519,
    );
    await symmetric.mixHash(body);
  }

  Future<Uint8List> writeMessage2() async {
    final remoteEphemeral = _requireRemoteEphemeral();
    _localEphemeralKeyPair = await _x25519.newKeyPair();
    final ephemeral = await _localEphemeralKeyPair!.extractPublicKey();
    await symmetric.mixHash(ephemeral.bytes);
    await symmetric.mixKey(await _dh(_localEphemeralKeyPair!, remoteEphemeral));

    final staticPublic = await localStaticKeyPair.extractPublicKey();
    final encryptedStatic = await symmetric.encryptAndHash(staticPublic.bytes);
    await symmetric.mixKey(await _dh(localStaticKeyPair, remoteEphemeral));

    return Uint8List.fromList([...ephemeral.bytes, ...encryptedStatic]);
  }

  Future<void> readMessage3(Uint8List body) async {
    if (body.length != 48) {
      throw const FormatException('Noise message 3 must be 48 bytes');
    }
    final localEphemeral = _requireLocalEphemeral();
    final remoteStaticBytes = await symmetric.decryptAndHash(body);
    _remoteStaticPublicKey = SimplePublicKey(
      Uint8List.fromList(remoteStaticBytes),
      type: KeyPairType.x25519,
    );
    await symmetric.mixKey(await _dh(localEphemeral, _remoteStaticPublicKey!));
  }

  Future<_NoiseTransportSession> splitForResponder() async {
    return _responderSession ??= await symmetric.split(initiator: false);
  }

  SimpleKeyPair _requireLocalEphemeral() {
    final keyPair = _localEphemeralKeyPair;
    if (keyPair == null) {
      throw StateError('Local ephemeral key is not initialized');
    }
    return keyPair;
  }

  SimplePublicKey _requireRemoteEphemeral() {
    final publicKey = _remoteEphemeralPublicKey;
    if (publicKey == null) {
      throw StateError('Remote ephemeral key is not initialized');
    }
    return publicKey;
  }

  Future<Uint8List> _dh(
    SimpleKeyPair localKeyPair,
    SimplePublicKey remotePublicKey,
  ) async {
    final secret = await _x25519.sharedSecretKey(
      keyPair: localKeyPair,
      remotePublicKey: remotePublicKey,
    );
    return Uint8List.fromList(await secret.extractBytes());
  }
}

class _NoiseSymmetricState {
  final Chacha20 _cipher = Chacha20.poly1305Aead();
  Uint8List chainingKey;
  Uint8List handshakeHash;
  Uint8List? cipherKey;
  int nonce = 0;

  _NoiseSymmetricState._({
    required this.chainingKey,
    required this.handshakeHash,
  });

  static Future<_NoiseSymmetricState> initialize() async {
    final protocolName = utf8.encode(_noiseProtocolName);
    final initialHash = Uint8List(32);
    if (protocolName.length <= 32) {
      initialHash.setRange(0, protocolName.length, protocolName);
    } else {
      initialHash.setAll(0, (await Sha256().hash(protocolName)).bytes);
    }
    return _NoiseSymmetricState._(
      chainingKey: Uint8List.fromList(initialHash),
      handshakeHash: Uint8List.fromList(initialHash),
    );
  }

  Future<void> mixHash(List<int> data) async {
    final builder = BytesBuilder()
      ..add(handshakeHash)
      ..add(data);
    handshakeHash =
        Uint8List.fromList((await Sha256().hash(builder.toBytes())).bytes);
  }

  Future<void> mixKey(List<int> inputKeyMaterial) async {
    final outputs = await _hkdf2(chainingKey, inputKeyMaterial);
    chainingKey = outputs.$1;
    cipherKey = outputs.$2;
    nonce = 0;
  }

  Future<Uint8List> encryptAndHash(List<int> plaintext) async {
    final key = cipherKey;
    if (key == null) {
      final cleartext = Uint8List.fromList(plaintext);
      await mixHash(cleartext);
      return cleartext;
    }
    final secretBox = await _cipher.encrypt(
      plaintext,
      secretKey: SecretKey(key),
      nonce: _noiseNonce(nonce++),
      aad: handshakeHash,
    );
    final ciphertext = secretBox.concatenation(nonce: false);
    await mixHash(ciphertext);
    return ciphertext;
  }

  Future<Uint8List> decryptAndHash(Uint8List ciphertext) async {
    final key = cipherKey;
    if (key == null) {
      await mixHash(ciphertext);
      return Uint8List.fromList(ciphertext);
    }
    if (ciphertext.length < _aeadMacLength) {
      throw const FormatException('Noise ciphertext is truncated');
    }
    final secretBox = _secretBoxWithoutNonce(
      ciphertext,
      nonce: _noiseNonce(nonce++),
    );
    final plaintext = await _cipher.decrypt(
      secretBox,
      secretKey: SecretKey(key),
      aad: handshakeHash,
    );
    await mixHash(ciphertext);
    return Uint8List.fromList(plaintext);
  }

  Future<_NoiseTransportSession> split({required bool initiator}) async {
    final outputs = await _hkdf2(chainingKey, const []);
    return _NoiseTransportSession(
      sendKey: initiator ? outputs.$1 : outputs.$2,
      receiveKey: initiator ? outputs.$2 : outputs.$1,
    );
  }
}

class _NoiseTransportSession {
  final Chacha20 _cipher = Chacha20.poly1305Aead();
  final Uint8List sendKey;
  final Uint8List receiveKey;
  final Set<int> _receivedNonces = {};
  final List<int> _receivedNonceOrder = [];
  int _sendNonce = 0;

  _NoiseTransportSession({
    required this.sendKey,
    required this.receiveKey,
  });

  Future<Uint8List> encryptPayload(GrassrootsPacket packet) async {
    final nonce = _sendNonce++;
    final nonceBytes = _nonceBytes(nonce);
    final secretBox = await _cipher.encrypt(
      packet.payload,
      secretKey: SecretKey(sendKey),
      nonce: _aeadNonce(nonce),
      aad: _applicationAad(packet.type),
    );
    return Uint8List.fromList([
      _applicationPayloadVersion,
      ...nonceBytes,
      ...secretBox.concatenation(nonce: false),
    ]);
  }

  Future<Uint8List> decryptPayload(
    GrassrootsPacket packet,
    PacketType clearType,
  ) async {
    final payload = packet.payload;
    if (payload.length < 1 + 8 + _aeadMacLength) {
      throw const FormatException('Secure payload is truncated');
    }
    if (payload[0] != _applicationPayloadVersion) {
      throw FormatException(
          'Unsupported secure payload version: ${payload[0]}');
    }

    final nonce = _nonceFromBytes(payload.sublist(1, 9));
    if (_receivedNonces.contains(nonce)) {
      throw StateError('Replay detected for secure payload nonce $nonce');
    }

    final ciphertext = payload.sublist(9);
    final secretBox = _secretBoxWithoutNonce(
      ciphertext,
      nonce: _aeadNonce(nonce),
    );
    final clear = await _cipher.decrypt(
      secretBox,
      secretKey: SecretKey(receiveKey),
      aad: _applicationAad(clearType),
    );
    _rememberReceivedNonce(nonce);
    return Uint8List.fromList(clear);
  }

  void _rememberReceivedNonce(int nonce) {
    _receivedNonces.add(nonce);
    _receivedNonceOrder.add(nonce);
    if (_receivedNonceOrder.length <= 2048) return;
    final removed = _receivedNonceOrder.removeAt(0);
    _receivedNonces.remove(removed);
  }
}

Future<(Uint8List, Uint8List)> _hkdf2(
    List<int> chainingKey, List<int> ikm) async {
  final hmac = Hmac.sha256();
  final tempKey =
      (await hmac.calculateMac(ikm, secretKey: SecretKey(chainingKey))).bytes;
  final output1 = (await hmac.calculateMac(
    const [1],
    secretKey: SecretKey(tempKey),
  ))
      .bytes;
  final output2Input = Uint8List.fromList([...output1, 2]);
  final output2 =
      (await hmac.calculateMac(output2Input, secretKey: SecretKey(tempKey)))
          .bytes;
  return (Uint8List.fromList(output1), Uint8List.fromList(output2));
}

Uint8List _encodeHandshakePayload(
  _NoiseHandshakeMessage message,
  Uint8List senderPubkey,
  Uint8List body, {
  Uint8List? signature,
}) {
  return Uint8List.fromList([
    _handshakePayloadVersion,
    message.value,
    ...senderPubkey,
    if (signature != null) ...signature,
    ...body,
  ]);
}

/// Decode a handshake envelope: (message, sender identity claim, message-1
/// claim signature or null, noise body). Must match the client's codec.
(_NoiseHandshakeMessage, Uint8List, Uint8List?, Uint8List)
    _decodeHandshakePayload(Uint8List payload) {
  if (payload.length < 2 + 32) {
    throw const FormatException('Noise handshake payload is truncated');
  }
  if (payload[0] != _handshakePayloadVersion) {
    throw FormatException('Unsupported Noise payload version: ${payload[0]}');
  }
  final message = _NoiseHandshakeMessage.fromValue(payload[1]);
  final senderPubkey = Uint8List.fromList(payload.sublist(2, 34));
  var offset = 34;
  Uint8List? signature;
  if (message == _NoiseHandshakeMessage.message1) {
    if (payload.length < offset + _signatureLength) {
      throw const FormatException('Noise message 1 signature is truncated');
    }
    signature =
        Uint8List.fromList(payload.sublist(offset, offset + _signatureLength));
    offset += _signatureLength;
  }
  return (
    message,
    senderPubkey,
    signature,
    Uint8List.fromList(payload.sublist(offset)),
  );
}

SecretBox _secretBoxWithoutNonce(Uint8List data, {required Uint8List nonce}) {
  final macOffset = data.length - _aeadMacLength;
  return SecretBox(
    Uint8List.fromList(data.sublist(0, macOffset)),
    nonce: nonce,
    mac: Mac(Uint8List.fromList(data.sublist(macOffset))),
  );
}

Uint8List _noiseNonce(int nonce) => _aeadNonce(nonce);

Uint8List _aeadNonce(int nonce) {
  final result = Uint8List(12);
  final view = ByteData.view(result.buffer);
  view.setUint64(4, nonce, Endian.little);
  return result;
}

Uint8List _nonceBytes(int nonce) {
  final result = Uint8List(8);
  ByteData.view(result.buffer).setUint64(0, nonce, Endian.little);
  return result;
}

int _nonceFromBytes(Uint8List nonceBytes) {
  if (nonceBytes.length != 8) {
    throw ArgumentError('Nonce must be 8 bytes');
  }
  return ByteData.view(
    nonceBytes.buffer,
    nonceBytes.offsetInBytes,
    nonceBytes.lengthInBytes,
  ).getUint64(0, Endian.little);
}

/// AAD binding the ciphertext to its clear packet type, so a ciphertext
/// cannot be replayed as a different packet type. Must match the client's
/// AAD; identity binding is inherent to the session.
Uint8List _applicationAad(PacketType clearType) {
  return Uint8List.fromList([clearType.value]);
}

String _hex(Uint8List bytes) {
  return bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
}
