import 'dart:async';
import 'dart:convert';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart' as libsodium;

import '../models/identity.dart';
import '../models/packet.dart';
import '../models/peer.dart';
import '../protocol/protocol_handler.dart';

const _xxProtocolName = 'Noise_XX_25519_ChaChaPoly_SHA256';
const _ikProtocolName = 'Noise_IK_25519_ChaChaPoly_SHA256';

/// The prologue both ends mix into the handshake hash: a label and the
/// protocol version, so that two ends speaking different versions derive
/// different keys even where an envelope's clear version field was altered.
const _prologueLabel = 'grassroots noise prologue';
const _applicationPayloadVersion = 1;
const _aeadMacLength = 16;
const _signatureLength = 64;
const _keyLength = 32;

/// The Noise pattern of a handshake (GNA §2.6, Session Establishment): IK
/// when the initiator expects a specific responder key, XX for a first
/// contact with an unrecognized presence under Open trust, after ANNOUNCE.
enum NoisePattern {
  /// `-> e, es, s, ss` / `<- e, ee, se`: the initiator sends its static key
  /// in the first message, encrypted to the expected key, and the responder
  /// never transmits its static key.
  ik,

  /// `-> e` / `<- e, ee, s, es` / `-> s, se`: each side transmits its static
  /// key, and each verifies the delivered key against the key ANNOUNCE
  /// claimed.
  xx,
}

enum NoiseHandshakeRole { initiator, responder }

/// What a handshake message is, by the envelope's kind byte.
enum _Kind {
  xx1(0x01, NoisePattern.xx),
  xx2(0x02, NoisePattern.xx),
  xx3(0x03, NoisePattern.xx),
  ik1(0x11, NoisePattern.ik),
  ik2(0x12, NoisePattern.ik);

  final int value;
  final NoisePattern pattern;
  const _Kind(this.value, this.pattern);

  static _Kind fromValue(int value) => _Kind.values.firstWhere(
        (kind) => kind.value == value,
        orElse: () =>
            throw FormatException('Unknown Noise handshake message: $value'),
      );

  /// Whether the envelope carries the sender's identity claim in clear: the
  /// XX messages, whose keys ANNOUNCE has already made public, and not the
  /// IK ones, whose initiator key travels encrypted and whose responder key
  /// does not travel at all (GNA §2.6).
  bool get carriesClaim => pattern == NoisePattern.xx;
}

class NoiseHandshakeResult {
  /// The peer this handshake message is from: for XX the identity claim of
  /// the envelope (message 1 verified by its signature, messages 2 and 3
  /// against the Noise-authenticated static); for IK the key the first
  /// message carried encrypted, checked against its static, or the key the
  /// initiator expected, which only its holder can answer for.
  final Uint8List remotePubkey;

  /// The pattern of the handshake the message belongs to.
  final NoisePattern pattern;

  final Uint8List? responsePayload;

  /// Whether this message established the session: true once per session.
  final bool sessionEstablished;

  /// Whether the response is this side's final handshake message, the one
  /// with no reply (IK message 2, XX message 3), which the caller
  /// retransmits until the other side's attestation message arrives (GNA
  /// §2.6).
  final bool sentFinal;

  /// Whether the handshake this message started replaced a session the pair
  /// held: the peer runs a new handshake, so the session it held is gone.
  final bool replacedSession;

  const NoiseHandshakeResult({
    required this.remotePubkey,
    required this.pattern,
    this.responsePayload,
    this.sessionEstablished = false,
    this.sentFinal = false,
    this.replacedSession = false,
  });
}

/// Owns Noise session state, keyed by transport medium plus peer identity,
/// and runs the two handshakes of GNA §2.6: Noise IK whenever the initiator
/// expects the responder's key, and Noise XX for a first contact with an
/// unrecognized presence under Open trust.
///
/// The Noise static key is the X25519 form of the local Ed25519 transport
/// identity, derived via the standard birational map (libsodium
/// `crypto_sign_ed25519_*_to_curve25519`): "the expected static is
/// computable from the peer's Ed25519 public key alone" (GNA §2.6).  An IK
/// initiator therefore takes the responder's expected static from its
/// Ed25519 key; an IK responder reads the initiator's static from the first
/// message, with the initiator's Ed25519 key as that message's encrypted
/// payload, and checks that the one is the image of the other.
///
/// Every handshake envelope opens with the protocol version (version 8, a
/// peer on another version refused at the handshake), and both
/// ends mix it into the handshake hash as the prologue.  The XX envelopes
/// carry the sender's identity claim in clear, message 1 signed over the
/// claim and its ephemeral; the IK envelopes carry no identity, so the
/// initiator's key is read only by the holder of the key it expects, and the
/// responder's key never travels.
class NoiseSessionManager {
  final GrassrootsIdentity identity;

  /// libsodium handle providing the Ed25519↔X25519 conversion used to derive
  /// and verify Noise static keys.
  final libsodium.SodiumSumo sodium;

  final Duration handshakeTimeout;

  final Map<_SessionKey, _SessionEntry> _entries = {};

  /// Connection → verified peer identity, learned at handshake completion,
  /// or by a session-encrypted packet decrypting on the connection. Lets
  /// [decryptPacket] resolve which identity-keyed session owns a packet that
  /// arrived on a given transport connection.
  final Map<_ConnectionKey, Uint8List> _connectionPeers = {};

  Future<SimpleKeyPair>? _staticKeyPairFuture;

  NoiseSessionManager({
    required this.identity,
    required this.sodium,
    this.handshakeTimeout = const Duration(seconds: 5),
  });

  /// The protocol version every handshake envelope opens with (GNA §2.6).
  static int get protocolVersion => ProtocolHandler.protocolVersion;

  bool hasSession(PeerTransport transport, Uint8List remotePubkey) {
    return _entries[_SessionKey(transport, _hex(remotePubkey))]?.session !=
        null;
  }

  /// The peers with an established session on [transport].
  Iterable<Uint8List> sessionPeers(PeerTransport transport) => [
        for (final entry in _entries.entries)
          if (entry.key.transport == transport && entry.value.session != null)
            _bytes(entry.key.pubkeyHex),
      ];

  /// The final handshake hash of the session with [remotePubkey] on
  /// [transport], or null when there is none.
  ///
  /// This is what the attestation exchange binds to (spec §Session
  /// Establishment): unique per session, identical at both ends, so an
  /// attestation over it cannot be replayed onto another channel. It is not
  /// cached anywhere — a fresh handshake yields a fresh hash and hence a fresh
  /// attestation.
  Uint8List? handshakeHashFor(
    PeerTransport transport,
    Uint8List remotePubkey,
  ) =>
      _entries[_SessionKey(transport, _hex(remotePubkey))]
          ?.session
          ?.handshakeHash;

  /// Starts a [pattern] initiator handshake with [remotePubkey] over the
  /// connection [peerId] if no session or in-flight handshake exists.
  /// Returns the encoded first handshake message to send, or null if the
  /// caller should simply wait for the existing session or handshake.
  ///
  /// For IK, [remotePubkey] is the key the initiator expects: the first
  /// message is encrypted to its X25519 image, so only its holder can read or
  /// answer it (GNA §2.6).
  Future<Uint8List?> startHandshake(
    PeerTransport transport,
    Uint8List remotePubkey, {
    required NoisePattern pattern,
    String? peerId,
  }) async {
    final key = _SessionKey(transport, _hex(remotePubkey));
    final entry = _entries.putIfAbsent(key, _SessionEntry.new);
    if (entry.session != null || entry.handshake != null) {
      return null;
    }

    final Uint8List encoded;
    final _NoiseHandshakeState handshake;
    switch (pattern) {
      case NoisePattern.ik:
        final Uint8List expectedStatic;
        try {
          expectedStatic = sodium.crypto.sign.pkToCurve25519(remotePubkey);
        } catch (_) {
          throw ArgumentError.value(
              remotePubkey, 'remotePubkey', 'not an Ed25519 public key');
        }
        handshake = await _NoiseHandshakeState.create(
          role: NoiseHandshakeRole.initiator,
          pattern: NoisePattern.ik,
          localStaticKeyPair: await _staticKeyPair(),
          remoteStatic: expectedStatic,
        );
        // The payload is our Ed25519 identity, encrypted after the static:
        // the responder checks that the static is its image.
        final body = await handshake.writeIk1(identity.publicKey);
        encoded = _encodeEnvelope(_Kind.ik1, body);
      case NoisePattern.xx:
        handshake = await _NoiseHandshakeState.create(
          role: NoiseHandshakeRole.initiator,
          pattern: NoisePattern.xx,
          localStaticKeyPair: await _staticKeyPair(),
        );
        final body = await handshake.writeXx1();
        encoded = _encodeEnvelope(
          _Kind.xx1,
          body,
          claim: identity.publicKey,
          signature: _signIdentityClaim(body),
        );
    }
    entry
      ..session = null
      ..handshake = handshake
      ..finalSent = null
      ..connectionId = peerId
      // Keep a pending waiter: the dialer may already be awaiting the
      // session (waitForSession registers before the handshake starts).
      ..completer ??= Completer<bool>();
    entry.lastSentEncoded = encoded;
    return encoded;
  }

  /// Wait for a session with [remotePubkey] to establish. When no handshake
  /// is in flight yet, registers a waiter anyway, so that a session the peer
  /// initiates releases it too.
  Future<bool> waitForSession(
    PeerTransport transport,
    Uint8List remotePubkey,
  ) async {
    final key = _SessionKey(transport, _hex(remotePubkey));
    final entry = _entries.putIfAbsent(key, _SessionEntry.new);
    if (entry.session != null) return true;

    final completer = entry.completer ??= Completer<bool>();

    try {
      return await completer.future.timeout(
        handshakeTimeout,
        onTimeout: () {
          reset(transport, remotePubkey);
          return false;
        },
      );
    } catch (_) {
      return false;
    }
  }

  /// The handshake message this side last sent for a still-in-flight
  /// handshake with [remotePubkey] — retransmitted on a timer over a lossy
  /// carrier until the next message arrives (GNA §2.6: "each handshake
  /// message is retransmitted on a timer until the next one arrives"). Null
  /// once the session is established or no handshake is in flight.
  Uint8List? handshakeRetransmitPayload(
    PeerTransport transport,
    Uint8List remotePubkey,
  ) {
    final entry = _entries[_SessionKey(transport, _hex(remotePubkey))];
    if (entry == null || entry.session != null || entry.handshake == null) {
      return null;
    }
    return entry.lastSentEncoded;
  }

  /// This side's final handshake message of the session with [remotePubkey]
  /// — IK message 2 as responder, XX message 3 as initiator — which "has no
  /// reply" and is retransmitted "until the other side's attestation message
  /// arrives" (GNA §2.6). Null when there is no session, or the other side
  /// sent the final message.
  Uint8List? finalHandshakePayload(
    PeerTransport transport,
    Uint8List remotePubkey,
  ) {
    final entry = _entries[_SessionKey(transport, _hex(remotePubkey))];
    if (entry == null || entry.session == null) return null;
    return entry.finalSent;
  }

  /// Process an inbound handshake packet arriving on the connection [peerId].
  ///
  /// [acceptFirstMessage] is the responder's trust check (GNA §2.6: "The
  /// responder enforces the trust level"): asked of the first message of a
  /// handshake, with its pattern and the key it is from, before any response
  /// or responder state exists; false refuses it, and [StateError] is
  /// thrown.
  ///
  /// A handshake envelope on another protocol version is refused with a
  /// [FormatException] (version 8; versions 7 and 6 refused at the
  /// handshake).
  ///
  /// When [sendResponse] is provided, the manager transmits any response
  /// payload through it BEFORE releasing [waitForSession] waiters, and
  /// returns a result whose [NoiseHandshakeResult.responsePayload] is null.
  /// The ordering matters: a released waiter can push an encrypted packet
  /// onto the wire ahead of the final handshake message, which the other
  /// side cannot yet decrypt. A caller that passes no [sendResponse] must
  /// transmit the returned payload itself and accepts that race.
  Future<NoiseHandshakeResult> handleHandshakePacket(
    GrassrootsPacket packet, {
    required PeerTransport transport,
    String? peerId,
    required bool Function(NoisePattern pattern, Uint8List remotePubkey)
        acceptFirstMessage,
    Future<void> Function(Uint8List responsePayload, Uint8List remotePubkey)?
        sendResponse,
  }) async {
    final envelope = _decodeEnvelope(packet.payload);

    var result = switch (envelope.kind) {
      _Kind.xx1 => await _handleXx1(envelope, transport, acceptFirstMessage),
      _Kind.xx2 => await _handleXx2(envelope, transport, peerId),
      _Kind.xx3 => await _handleXx3(envelope, transport, peerId),
      _Kind.ik1 =>
        await _handleIk1(envelope, transport, peerId, acceptFirstMessage),
      _Kind.ik2 => await _handleIk2(envelope, transport, peerId),
    };

    final response = result.responsePayload;
    if (sendResponse != null && response != null) {
      await sendResponse(response, result.remotePubkey);
      result = NoiseHandshakeResult(
        remotePubkey: result.remotePubkey,
        pattern: result.pattern,
        sessionEstablished: result.sessionEstablished,
        sentFinal: result.sentFinal,
        replacedSession: result.replacedSession,
      );
    }
    // Release waiters only after any response is on the wire.
    if (result.sessionEstablished) {
      _entries[_SessionKey(transport, _hex(result.remotePubkey))]
          ?.complete(true);
    }
    return result;
  }

  Future<GrassrootsPacket> encryptPacket(
    GrassrootsPacket packet, {
    required PeerTransport transport,
    required Uint8List remotePubkey,
  }) async {
    if (!packet.type.hasSecureVariant) return packet;

    final key = _SessionKey(transport, _hex(remotePubkey));
    final session = _entries[key]?.session;
    if (session == null) {
      throw StateError('No Noise session for $transport/${key.pubkeyHex}');
    }

    final encryptedPayload = await session.encryptPayload(packet);
    return packet.copyWith(
      type: packet.type.secureVariant,
      payload: encryptedPayload,
    );
  }

  /// Decrypt a session packet that arrived on [peerId], returning the clear
  /// packet together with the session-authenticated sender identity and the
  /// handshake hash of the session that decrypted it — the session an
  /// attestation it carries is judged against (spec §Session Establishment).
  ///
  /// The owning session is resolved by identity: first the caller-supplied
  /// [remotePubkey] (when the connection is already identified), then the
  /// connection→identity mapping learned at handshake completion.  Over the
  /// PAN a pair holds one session over two GATT legs, and a leg opened after
  /// the handshake is identified by no handshake: a packet on a connection
  /// neither names is tried against each GATT session, the one whose keys
  /// open it naming the peer, and the connection is recorded as that peer's.
  Future<(GrassrootsPacket, Uint8List, Uint8List)> decryptPacket(
    GrassrootsPacket packet, {
    required PeerTransport transport,
    String? peerId,
    Uint8List? remotePubkey,
  }) async {
    if (!packet.type.isSessionEncrypted) {
      throw ArgumentError('Not a session-encrypted packet: ${packet.type}');
    }

    final connectionPeer = peerId != null
        ? _connectionPeers[_ConnectionKey(transport, peerId)]
        : null;
    final clearType = packet.type.clearVariant;
    for (final candidate in [remotePubkey, connectionPeer]) {
      if (candidate == null) continue;
      final found = _entries[_SessionKey(transport, _hex(candidate))]?.session;
      if (found != null) {
        final clearPayload = await found.decryptPayload(packet, clearType);
        return (
          packet.copyWith(type: clearType, payload: clearPayload),
          candidate,
          found.handshakeHash,
        );
      }
    }

    if (transport == PeerTransport.bleDirect && peerId != null) {
      for (final entry in _entries.entries.toList()) {
        if (entry.key.transport != transport) continue;
        final session = entry.value.session;
        if (session == null) continue;
        final Uint8List clearPayload;
        try {
          clearPayload = await session.decryptPayload(packet, clearType);
        } catch (_) {
          continue;
        }
        final sender = _bytes(entry.key.pubkeyHex);
        _recordConnectionPeer(transport, peerId, sender);
        return (
          packet.copyWith(type: clearType, payload: clearPayload),
          sender,
          session.handshakeHash,
        );
      }
    }
    throw StateError('No Noise session for $transport/$peerId');
  }

  void reset(PeerTransport transport, Uint8List remotePubkey) {
    final removed = _entries.remove(_SessionKey(transport, _hex(remotePubkey)));
    removed?.complete(false);
    final pubkeyHex = _hex(remotePubkey);
    _connectionPeers.removeWhere(
      (key, value) => key.transport == transport && _hex(value) == pubkeyHex,
    );
  }

  void resetTransport(PeerTransport transport) {
    final keys = _entries.keys
        .where((key) => key.transport == transport)
        .toList(growable: false);
    for (final key in keys) {
      _entries.remove(key)?.complete(false);
    }
    _connectionPeers.removeWhere((key, _) => key.transport == transport);
  }

  void dispose() {
    for (final entry in _entries.values) {
      entry.complete(false);
    }
    _entries.clear();
    _connectionPeers.clear();
  }

  // ===== XX =====

  Future<NoiseHandshakeResult> _handleXx1(
    _Envelope envelope,
    PeerTransport transport,
    bool Function(NoisePattern, Uint8List) accept,
  ) async {
    final remotePubkey = envelope.claim!;
    final body = envelope.body;
    if (!_verifyIdentityClaim(remotePubkey, envelope.signature!, body)) {
      throw const FormatException(
          'Noise XX message 1 identity claim failed verification');
    }
    final key = _SessionKey(transport, _hex(remotePubkey));
    final entry = _entries.putIfAbsent(key, _SessionEntry.new);

    // A retransmitted message 1 (our message 2 was lost): resend the same
    // message 2 — a fresh responder state would break the initiator's
    // in-flight handshake.
    if (entry.handshake?.role == NoiseHandshakeRole.responder &&
        entry.lastInboundBody != null &&
        _bytesEqual(entry.lastInboundBody!, body)) {
      return NoiseHandshakeResult(
        remotePubkey: remotePubkey,
        pattern: NoisePattern.xx,
        responsePayload: entry.lastSentEncoded,
      );
    }

    if (!accept(NoisePattern.xx, remotePubkey)) {
      throw StateError('Noise XX message 1 from '
          '${_hex(remotePubkey).substring(0, 8)} refused by the trust level');
    }

    if (_yieldsToOwnInitiation(entry, key)) {
      return NoiseHandshakeResult(
          remotePubkey: remotePubkey, pattern: NoisePattern.xx);
    }

    final handshake = await _NoiseHandshakeState.create(
      role: NoiseHandshakeRole.responder,
      pattern: NoisePattern.xx,
      localStaticKeyPair: await _staticKeyPair(),
    );
    await handshake.readXx1(body);
    final responseBody = await handshake.writeXx2();
    final replaced = entry.session != null;
    entry
      ..session = null
      ..handshake = handshake
      ..finalSent = null
      ..lastInboundBody = body
      // Keep a pending waiter: the dialer may already be awaiting the
      // session (waitForSession registers before the handshake starts).
      ..completer ??= Completer<bool>();
    final encoded = _encodeEnvelope(
      _Kind.xx2,
      responseBody,
      claim: identity.publicKey,
    );
    entry.lastSentEncoded = encoded;
    return NoiseHandshakeResult(
      remotePubkey: remotePubkey,
      pattern: NoisePattern.xx,
      responsePayload: encoded,
      replacedSession: replaced,
    );
  }

  Future<NoiseHandshakeResult> _handleXx2(
    _Envelope envelope,
    PeerTransport transport,
    String? peerId,
  ) async {
    final remotePubkey = envelope.claim!;
    final body = envelope.body;
    final key = _SessionKey(transport, _hex(remotePubkey));
    final entry = _entries.putIfAbsent(key, _SessionEntry.new);
    final handshake = entry.handshake;
    if (handshake == null ||
        handshake.role != NoiseHandshakeRole.initiator ||
        handshake.pattern != NoisePattern.xx ||
        entry.session != null) {
      // A retransmitted message 2 after we completed (our message 3 was
      // lost): resend the same message 3.
      if (entry.session != null &&
          entry.finalSent != null &&
          entry.lastInboundBody != null &&
          _bytesEqual(entry.lastInboundBody!, body)) {
        return NoiseHandshakeResult(
          remotePubkey: remotePubkey,
          pattern: NoisePattern.xx,
          responsePayload: entry.finalSent,
        );
      }
      return NoiseHandshakeResult(
          remotePubkey: remotePubkey, pattern: NoisePattern.xx);
    }
    entry.lastInboundBody = body;

    await handshake.readXx2(body);
    if (!_verifyRemoteStatic(handshake, remotePubkey)) {
      _entries.remove(key);
      entry.complete(false);
      return NoiseHandshakeResult(
          remotePubkey: remotePubkey, pattern: NoisePattern.xx);
    }
    final responseBody = await handshake.writeXx3();
    final session = await handshake.splitForInitiator();
    final encoded = _encodeEnvelope(
      _Kind.xx3,
      responseBody,
      claim: identity.publicKey,
    );
    entry
      ..session = session
      ..handshake = null
      ..lastSentEncoded = encoded
      ..finalSent = encoded;
    // Waiters are released by handleHandshakePacket, after message 3 is sent.
    _recordConnectionPeer(transport, peerId, remotePubkey);
    return NoiseHandshakeResult(
      remotePubkey: remotePubkey,
      pattern: NoisePattern.xx,
      responsePayload: encoded,
      sessionEstablished: true,
      sentFinal: true,
    );
  }

  Future<NoiseHandshakeResult> _handleXx3(
    _Envelope envelope,
    PeerTransport transport,
    String? peerId,
  ) async {
    final remotePubkey = envelope.claim!;
    final body = envelope.body;
    final key = _SessionKey(transport, _hex(remotePubkey));
    final entry = _entries.putIfAbsent(key, _SessionEntry.new);
    final handshake = entry.handshake;
    if (handshake == null ||
        handshake.role != NoiseHandshakeRole.responder ||
        handshake.pattern != NoisePattern.xx ||
        entry.session != null) {
      // A retransmitted message 3 after we completed: nothing to answer.
      return NoiseHandshakeResult(
          remotePubkey: remotePubkey, pattern: NoisePattern.xx);
    }

    await handshake.readXx3(body);
    if (!_verifyRemoteStatic(handshake, remotePubkey)) {
      _entries.remove(key);
      entry.complete(false);
      return NoiseHandshakeResult(
          remotePubkey: remotePubkey, pattern: NoisePattern.xx);
    }
    final session = await handshake.splitForResponder();
    entry
      ..session = session
      ..handshake = null;
    // Waiters are released by handleHandshakePacket.
    _recordConnectionPeer(transport, peerId, remotePubkey);
    return NoiseHandshakeResult(
      remotePubkey: remotePubkey,
      pattern: NoisePattern.xx,
      sessionEstablished: true,
    );
  }

  // ===== IK =====

  Future<NoiseHandshakeResult> _handleIk1(
    _Envelope envelope,
    PeerTransport transport,
    String? peerId,
    bool Function(NoisePattern, Uint8List) accept,
  ) async {
    final body = envelope.body;
    // Only the holder of the key the initiator expects can read the message,
    // and reading it is what names the initiator.
    final handshake = await _NoiseHandshakeState.create(
      role: NoiseHandshakeRole.responder,
      pattern: NoisePattern.ik,
      localStaticKeyPair: await _staticKeyPair(),
    );
    final Uint8List remotePubkey;
    try {
      remotePubkey = await handshake.readIk1(body);
    } catch (e) {
      throw FormatException('Noise IK message 1 does not open: $e');
    }
    if (remotePubkey.length != _keyLength ||
        !_verifyRemoteStatic(handshake, remotePubkey)) {
      throw const FormatException(
          'Noise IK message 1: the static key is not the image of the '
          'identity key it carries');
    }
    final key = _SessionKey(transport, _hex(remotePubkey));
    final entry = _entries.putIfAbsent(key, _SessionEntry.new);

    // A retransmitted message 1 (our message 2 was lost): resend the same
    // message 2, the session it established standing.
    if (entry.lastInboundBody != null &&
        _bytesEqual(entry.lastInboundBody!, body) &&
        entry.session != null &&
        entry.finalSent != null) {
      return NoiseHandshakeResult(
        remotePubkey: remotePubkey,
        pattern: NoisePattern.ik,
        responsePayload: entry.finalSent,
      );
    }

    if (!accept(NoisePattern.ik, remotePubkey)) {
      throw StateError('Noise IK message 1 from '
          '${_hex(remotePubkey).substring(0, 8)} refused by the trust level');
    }

    if (_yieldsToOwnInitiation(entry, key)) {
      return NoiseHandshakeResult(
          remotePubkey: remotePubkey, pattern: NoisePattern.ik);
    }

    final responseBody = await handshake.writeIk2();
    final session = await handshake.splitForResponder();
    final replaced = entry.session != null;
    final encoded = _encodeEnvelope(_Kind.ik2, responseBody);
    entry
      ..session = session
      ..handshake = null
      ..lastInboundBody = body
      ..lastSentEncoded = encoded
      ..finalSent = encoded
      ..completer ??= Completer<bool>();
    _recordConnectionPeer(transport, peerId, remotePubkey);
    return NoiseHandshakeResult(
      remotePubkey: remotePubkey,
      pattern: NoisePattern.ik,
      responsePayload: encoded,
      sessionEstablished: true,
      sentFinal: true,
      replacedSession: replaced,
    );
  }

  Future<NoiseHandshakeResult> _handleIk2(
    _Envelope envelope,
    PeerTransport transport,
    String? peerId,
  ) async {
    final body = envelope.body;

    // The initiator's handshake in flight on this connection, then every IK
    // initiator handshake in flight on the medium: message 2 names no one,
    // and only the handshake it answers can open it.
    final candidates = <_SessionKey>[];
    for (final entry in _entries.entries) {
      if (entry.key.transport != transport) continue;
      final handshake = entry.value.handshake;
      if (handshake == null ||
          handshake.role != NoiseHandshakeRole.initiator ||
          handshake.pattern != NoisePattern.ik ||
          entry.value.session != null) {
        // A retransmitted message 2 after we completed: nothing to answer.
        if (entry.value.session != null &&
            entry.value.lastInboundBody != null &&
            _bytesEqual(entry.value.lastInboundBody!, body)) {
          return NoiseHandshakeResult(
            remotePubkey: _bytes(entry.key.pubkeyHex),
            pattern: NoisePattern.ik,
          );
        }
        continue;
      }
      if (peerId != null && entry.value.connectionId == peerId) {
        candidates.insert(0, entry.key);
      } else {
        candidates.add(entry.key);
      }
    }

    for (final key in candidates) {
      final entry = _entries[key];
      final handshake = entry?.handshake;
      if (entry == null || handshake == null) continue;
      final trial = handshake.copy();
      try {
        await trial.readIk2(body);
      } catch (_) {
        continue;
      }
      final remotePubkey = _bytes(key.pubkeyHex);
      final session = await trial.splitForInitiator();
      entry
        ..session = session
        ..handshake = null
        ..lastInboundBody = body
        ..finalSent = null;
      _recordConnectionPeer(transport, peerId, remotePubkey);
      return NoiseHandshakeResult(
        remotePubkey: remotePubkey,
        pattern: NoisePattern.ik,
        sessionEstablished: true,
      );
    }
    throw const FormatException(
        'Noise IK message 2 answers no handshake in flight');
  }

  /// Glare: the pair's first messages crossed. "The deterministic initiator
  /// is the endpoint with the lexicographically smaller public key" (GNA
  /// §2.7, §4.1): with the smaller key, keep our own handshake and leave
  /// theirs unanswered; with the larger, drop ours and answer theirs, a
  /// waiter on our session staying to be released by the one theirs makes.
  bool _yieldsToOwnInitiation(_SessionEntry entry, _SessionKey key) {
    final existing = entry.handshake;
    if (existing?.role != NoiseHandshakeRole.initiator) return false;
    return _hex(identity.publicKey).compareTo(key.pubkeyHex) < 0;
  }

  void _recordConnectionPeer(
    PeerTransport transport,
    String? peerId,
    Uint8List remotePubkey,
  ) {
    if (peerId == null) return;
    _connectionPeers[_ConnectionKey(transport, peerId)] =
        Uint8List.fromList(remotePubkey);
  }

  /// Sign the XX message-1 identity claim: our Ed25519 identity over
  /// (claim ‖ noise body), binding the claim to this handshake's ephemeral.
  Uint8List _signIdentityClaim(Uint8List body) {
    final message = Uint8List.fromList([...identity.publicKey, ...body]);
    final secretKey =
        libsodium.SecureKey.fromList(sodium, identity.privateKey);
    try {
      return sodium.crypto.sign.detached(
        message: message,
        secretKey: secretKey,
      );
    } finally {
      secretKey.dispose();
    }
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
      // public key by any peer (see [_verifyRemoteStatic]).
      final edSecret = libsodium.SecureKey.fromList(sodium, identity.privateKey);
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

  /// Verify the Noise-delivered remote static key equals the X25519 form of
  /// the sender's Ed25519 identity ([remotePubkey]): the XX claim, or the IK
  /// initiator's identity carried in its first message. A mismatch — a peer
  /// presenting a static that does not belong to the identity it claims, or a
  /// tampered handshake — aborts the handshake (GNA §2.6).
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
  Completer<bool>? completer;

  /// The last handshake message this side put on the wire — retransmitted on
  /// a timer over a lossy carrier while its handshake is in flight, and
  /// resent when the peer's retransmitted previous message shows the reply
  /// was lost (GNA §2.6: the handshake is self-ordering).
  Uint8List? lastSentEncoded;

  /// This side's final handshake message of the standing session, where this
  /// side sent it: retransmitted until the other side's attestation message
  /// arrives (GNA §2.6).
  Uint8List? finalSent;

  /// The last inbound handshake body, to tell a retransmitted duplicate
  /// (resend the stored reply) from a genuinely new handshake attempt.
  Uint8List? lastInboundBody;

  /// The connection this side's first message went out on, as initiator:
  /// where the answering message 2, which names no one, is looked for first.
  String? connectionId;

  void complete(bool value) {
    final pending = completer;
    if (pending != null && !pending.isCompleted) {
      pending.complete(value);
    }
    completer = null;
  }
}

class _SessionKey {
  final PeerTransport transport;
  final String pubkeyHex;

  const _SessionKey(this.transport, this.pubkeyHex);

  @override
  bool operator ==(Object other) {
    return other is _SessionKey &&
        other.transport == transport &&
        other.pubkeyHex == pubkeyHex;
  }

  @override
  int get hashCode => Object.hash(transport, pubkeyHex);
}

class _ConnectionKey {
  final PeerTransport transport;
  final String peerId;

  const _ConnectionKey(this.transport, this.peerId);

  @override
  bool operator ==(Object other) {
    return other is _ConnectionKey &&
        other.transport == transport &&
        other.peerId == peerId;
  }

  @override
  int get hashCode => Object.hash(transport, peerId);
}

class _NoiseHandshakeState {
  final NoiseHandshakeRole role;
  final NoisePattern pattern;
  final SimpleKeyPair localStaticKeyPair;
  final _NoiseSymmetricState symmetric;
  final X25519 _x25519 = X25519();

  SimpleKeyPair? _localEphemeralKeyPair;
  SimplePublicKey? _remoteEphemeralPublicKey;
  SimplePublicKey? _remoteStaticPublicKey;
  _NoiseTransportSession? _initiatorSession;
  _NoiseTransportSession? _responderSession;

  /// The remote peer's static public key: as delivered in the handshake (XX
  /// messages 2/3, IK message 1), verified against the expected identity by
  /// [NoiseSessionManager._verifyRemoteStatic]; or, for an IK initiator, the
  /// key it expects.
  SimplePublicKey? get remoteStaticPublicKey => _remoteStaticPublicKey;

  _NoiseHandshakeState._({
    required this.role,
    required this.pattern,
    required this.localStaticKeyPair,
    required this.symmetric,
  });

  static Future<_NoiseHandshakeState> create({
    required NoiseHandshakeRole role,
    required NoisePattern pattern,
    required SimpleKeyPair localStaticKeyPair,
    Uint8List? remoteStatic,
  }) async {
    final symmetric = await _NoiseSymmetricState.initialize(
      pattern == NoisePattern.ik ? _ikProtocolName : _xxProtocolName,
    );
    await symmetric.mixHash(_prologue());
    final state = _NoiseHandshakeState._(
      role: role,
      pattern: pattern,
      localStaticKeyPair: localStaticKeyPair,
      symmetric: symmetric,
    );
    if (pattern == NoisePattern.ik) {
      // The pre-message `<- s`: the responder's static, known to both.
      if (role == NoiseHandshakeRole.initiator) {
        if (remoteStatic == null) {
          throw ArgumentError('An IK initiator needs the expected key');
        }
        state._remoteStaticPublicKey = SimplePublicKey(
          Uint8List.fromList(remoteStatic),
          type: KeyPairType.x25519,
        );
        await symmetric.mixHash(remoteStatic);
      } else {
        final own = await localStaticKeyPair.extractPublicKey();
        await symmetric.mixHash(own.bytes);
      }
    }
    return state;
  }

  /// A copy to try a message on, leaving this state as it was if the message
  /// does not open.
  _NoiseHandshakeState copy() {
    final copied = _NoiseHandshakeState._(
      role: role,
      pattern: pattern,
      localStaticKeyPair: localStaticKeyPair,
      symmetric: symmetric.copy(),
    );
    copied._localEphemeralKeyPair = _localEphemeralKeyPair;
    copied._remoteEphemeralPublicKey = _remoteEphemeralPublicKey;
    copied._remoteStaticPublicKey = _remoteStaticPublicKey;
    return copied;
  }

  static Uint8List _prologue() {
    final version = ByteData(2)
      ..setUint16(0, NoiseSessionManager.protocolVersion, Endian.big);
    return Uint8List.fromList(
        [...utf8.encode(_prologueLabel), ...version.buffer.asUint8List()]);
  }

  // --- XX: -> e / <- e, ee, s, es / -> s, se ---

  Future<Uint8List> writeXx1() async {
    _localEphemeralKeyPair = await _x25519.newKeyPair();
    final ephemeral = await _localEphemeralKeyPair!.extractPublicKey();
    await symmetric.mixHash(ephemeral.bytes);
    return Uint8List.fromList(ephemeral.bytes);
  }

  Future<void> readXx1(Uint8List body) async {
    if (body.length != _keyLength) {
      throw const FormatException('Noise XX message 1 must be 32 bytes');
    }
    _remoteEphemeralPublicKey = SimplePublicKey(
      Uint8List.fromList(body),
      type: KeyPairType.x25519,
    );
    await symmetric.mixHash(body);
  }

  Future<Uint8List> writeXx2() async {
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

  Future<void> readXx2(Uint8List body) async {
    if (body.length != _keyLength + _keyLength + _aeadMacLength) {
      throw const FormatException('Noise XX message 2 must be 80 bytes');
    }
    final localEphemeral = _requireLocalEphemeral();
    final remoteEphemeralBytes = body.sublist(0, _keyLength);
    _remoteEphemeralPublicKey = SimplePublicKey(
      Uint8List.fromList(remoteEphemeralBytes),
      type: KeyPairType.x25519,
    );
    await symmetric.mixHash(remoteEphemeralBytes);
    await symmetric.mixKey(
      await _dh(localEphemeral, _remoteEphemeralPublicKey!),
    );

    final remoteStaticBytes =
        await symmetric.decryptAndHash(body.sublist(_keyLength));
    _remoteStaticPublicKey = SimplePublicKey(
      Uint8List.fromList(remoteStaticBytes),
      type: KeyPairType.x25519,
    );
    await symmetric.mixKey(await _dh(localEphemeral, _remoteStaticPublicKey!));
  }

  Future<Uint8List> writeXx3() async {
    final remoteEphemeral = _requireRemoteEphemeral();
    final staticPublic = await localStaticKeyPair.extractPublicKey();
    final encryptedStatic = await symmetric.encryptAndHash(staticPublic.bytes);
    await symmetric.mixKey(await _dh(localStaticKeyPair, remoteEphemeral));
    return Uint8List.fromList(encryptedStatic);
  }

  Future<void> readXx3(Uint8List body) async {
    if (body.length != _keyLength + _aeadMacLength) {
      throw const FormatException('Noise XX message 3 must be 48 bytes');
    }
    final localEphemeral = _requireLocalEphemeral();
    final remoteStaticBytes = await symmetric.decryptAndHash(body);
    _remoteStaticPublicKey = SimplePublicKey(
      Uint8List.fromList(remoteStaticBytes),
      type: KeyPairType.x25519,
    );
    await symmetric.mixKey(await _dh(localEphemeral, _remoteStaticPublicKey!));
  }

  // --- IK: <- s / ... / -> e, es, s, ss / <- e, ee, se ---

  /// Write IK message 1, carrying [payload] encrypted after the static.
  Future<Uint8List> writeIk1(Uint8List payload) async {
    final remoteStatic = _remoteStaticPublicKey!;
    _localEphemeralKeyPair = await _x25519.newKeyPair();
    final ephemeral = await _localEphemeralKeyPair!.extractPublicKey();
    await symmetric.mixHash(ephemeral.bytes);
    await symmetric.mixKey(await _dh(_localEphemeralKeyPair!, remoteStatic));
    final staticPublic = await localStaticKeyPair.extractPublicKey();
    final encryptedStatic = await symmetric.encryptAndHash(staticPublic.bytes);
    await symmetric.mixKey(await _dh(localStaticKeyPair, remoteStatic));
    final encryptedPayload = await symmetric.encryptAndHash(payload);
    return Uint8List.fromList(
        [...ephemeral.bytes, ...encryptedStatic, ...encryptedPayload]);
  }

  /// Read IK message 1 and answer its payload: the initiator's identity.
  Future<Uint8List> readIk1(Uint8List body) async {
    const length = _keyLength + (_keyLength + _aeadMacLength) * 2;
    if (body.length != length) {
      throw FormatException('Noise IK message 1 must be $length bytes');
    }
    final remoteEphemeralBytes = body.sublist(0, _keyLength);
    _remoteEphemeralPublicKey = SimplePublicKey(
      Uint8List.fromList(remoteEphemeralBytes),
      type: KeyPairType.x25519,
    );
    await symmetric.mixHash(remoteEphemeralBytes);
    await symmetric
        .mixKey(await _dh(localStaticKeyPair, _remoteEphemeralPublicKey!));
    const staticEnd = _keyLength * 2 + _aeadMacLength;
    final remoteStaticBytes =
        await symmetric.decryptAndHash(body.sublist(_keyLength, staticEnd));
    _remoteStaticPublicKey = SimplePublicKey(
      Uint8List.fromList(remoteStaticBytes),
      type: KeyPairType.x25519,
    );
    await symmetric.mixKey(await _dh(localStaticKeyPair, _remoteStaticPublicKey!));
    return symmetric.decryptAndHash(body.sublist(staticEnd));
  }

  Future<Uint8List> writeIk2() async {
    final remoteEphemeral = _requireRemoteEphemeral();
    final remoteStatic = _remoteStaticPublicKey!;
    _localEphemeralKeyPair = await _x25519.newKeyPair();
    final ephemeral = await _localEphemeralKeyPair!.extractPublicKey();
    await symmetric.mixHash(ephemeral.bytes);
    await symmetric.mixKey(await _dh(_localEphemeralKeyPair!, remoteEphemeral));
    await symmetric.mixKey(await _dh(_localEphemeralKeyPair!, remoteStatic));
    final tag = await symmetric.encryptAndHash(const <int>[]);
    return Uint8List.fromList([...ephemeral.bytes, ...tag]);
  }

  Future<void> readIk2(Uint8List body) async {
    if (body.length != _keyLength + _aeadMacLength) {
      throw const FormatException('Noise IK message 2 must be 48 bytes');
    }
    final localEphemeral = _requireLocalEphemeral();
    final remoteEphemeralBytes = body.sublist(0, _keyLength);
    _remoteEphemeralPublicKey = SimplePublicKey(
      Uint8List.fromList(remoteEphemeralBytes),
      type: KeyPairType.x25519,
    );
    await symmetric.mixHash(remoteEphemeralBytes);
    await symmetric.mixKey(
        await _dh(localEphemeral, _remoteEphemeralPublicKey!));
    await symmetric.mixKey(
        await _dh(localStaticKeyPair, _remoteEphemeralPublicKey!));
    await symmetric.decryptAndHash(body.sublist(_keyLength));
  }

  Future<_NoiseTransportSession> splitForInitiator() async {
    return _initiatorSession ??= await symmetric.split(initiator: true);
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

  static Future<_NoiseSymmetricState> initialize(String protocol) async {
    final protocolName = utf8.encode(protocol);
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

  _NoiseSymmetricState copy() => _NoiseSymmetricState._(
        chainingKey: Uint8List.fromList(chainingKey),
        handshakeHash: Uint8List.fromList(handshakeHash),
      )
        ..cipherKey = cipherKey == null ? null : Uint8List.fromList(cipherKey!)
        ..nonce = nonce;

  Future<void> mixHash(List<int> data) async {
    final builder = BytesBuilder()
      ..add(handshakeHash)
      ..add(data);
    handshakeHash = Uint8List.fromList((await Sha256().hash(
      builder.toBytes(),
    ))
        .bytes);
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
      handshakeHash: Uint8List.fromList(handshakeHash),
    );
  }
}

class _NoiseTransportSession {
  final Chacha20 _cipher = Chacha20.poly1305Aead();
  final Uint8List sendKey;
  final Uint8List receiveKey;

  /// The final handshake hash of the handshake that produced this session —
  /// identical at both ends, unique to this session, and chosen by neither
  /// side alone. The attestation exchange binds to it (spec §Session
  /// Establishment).
  final Uint8List handshakeHash;

  final Set<int> _receivedNonces = {};
  final List<int> _receivedNonceOrder = [];
  int _sendNonce = 0;

  _NoiseTransportSession({
    required this.sendKey,
    required this.receiveKey,
    required this.handshakeHash,
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
  final tempKey = (await hmac.calculateMac(
    ikm,
    secretKey: SecretKey(chainingKey),
  ))
      .bytes;
  final output1 = (await hmac.calculateMac(
    const [1],
    secretKey: SecretKey(tempKey),
  ))
      .bytes;
  final output2Input = Uint8List.fromList([...output1, 2]);
  final output2 = (await hmac.calculateMac(
    output2Input,
    secretKey: SecretKey(tempKey),
  ))
      .bytes;
  return (Uint8List.fromList(output1), Uint8List.fromList(output2));
}

/// A decoded handshake envelope.
class _Envelope {
  final _Kind kind;

  /// The sender's identity claim: the XX messages' alone.
  final Uint8List? claim;

  /// The signature over the claim and the ephemeral: XX message 1's alone.
  final Uint8List? signature;

  final Uint8List body;

  const _Envelope(this.kind, this.claim, this.signature, this.body);
}

/// The handshake envelope: the protocol version (2 bytes, big-endian), the
/// message kind (1 byte), for XX the sender's identity claim (32 bytes) and,
/// on message 1, its signature over the claim and the ephemeral (64 bytes),
/// and the Noise message.
Uint8List _encodeEnvelope(
  _Kind kind,
  Uint8List body, {
  Uint8List? claim,
  Uint8List? signature,
}) {
  final version = ByteData(2)
    ..setUint16(0, NoiseSessionManager.protocolVersion, Endian.big);
  return Uint8List.fromList([
    ...version.buffer.asUint8List(),
    kind.value,
    if (claim != null) ...claim,
    if (signature != null) ...signature,
    ...body,
  ]);
}

/// Decode a handshake envelope. A peer on another protocol version is
/// refused here, at the handshake (GNA §2.6): its envelope is not read
/// further.
_Envelope _decodeEnvelope(Uint8List payload) {
  if (payload.length < 3) {
    throw const FormatException('Noise handshake payload is truncated');
  }
  final version =
      ByteData.sublistView(payload, 0, 2).getUint16(0, Endian.big);
  if (version != NoiseSessionManager.protocolVersion) {
    throw FormatException('Noise handshake protocol version $version != '
        '${NoiseSessionManager.protocolVersion}; refused');
  }
  final kind = _Kind.fromValue(payload[2]);
  var offset = 3;
  Uint8List? claim;
  Uint8List? signature;
  if (kind.carriesClaim) {
    if (payload.length < offset + _keyLength) {
      throw const FormatException('Noise handshake claim is truncated');
    }
    claim = Uint8List.fromList(payload.sublist(offset, offset + _keyLength));
    offset += _keyLength;
    if (kind == _Kind.xx1) {
      if (payload.length < offset + _signatureLength) {
        throw const FormatException(
            'Noise XX message 1 signature is truncated');
      }
      signature = Uint8List.fromList(
          payload.sublist(offset, offset + _signatureLength));
      offset += _signatureLength;
    }
  }
  return _Envelope(
      kind, claim, signature, Uint8List.fromList(payload.sublist(offset)));
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
/// cannot be replayed as a different packet type. Sender and recipient
/// identities are inherent to the session (both statics are in the Noise
/// handshake transcript); message identity is payload content, encrypted
/// and authenticated with the rest of the payload.
Uint8List _applicationAad(PacketType clearType) {
  return Uint8List.fromList([clearType.value]);
}

String _hex(Uint8List bytes) {
  return bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
}

Uint8List _bytes(String hex) {
  final out = Uint8List(hex.length ~/ 2);
  for (var i = 0; i < out.length; i++) {
    out[i] = int.parse(hex.substring(2 * i, 2 * i + 2), radix: 16);
  }
  return out;
}
