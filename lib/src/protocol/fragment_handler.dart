import 'dart:async';
import 'dart:typed_data';
import 'package:flutter/foundation.dart';
import 'package:uuid/uuid.dart';
import '../models/packet.dart';

/// Result of fragmenting a large message
class FragmentedMessage {
  final String messageId;
  final List<GrassrootsPacket> fragments;

  FragmentedMessage({
    required this.messageId,
    required this.fragments,
  });
}

/// Result of reassembling a fragmented message: the final payload bytes plus
/// the originating message id (carried in `FRAGMENT_START` and matching the
/// sender's `MessageSendingAction.messageId`, so the receiver's ACK can be
/// addressed to the same id the sender is tracking).
class ReassembledMessage {
  final String messageId;
  final Uint8List payload;

  ReassembledMessage({required this.messageId, required this.payload});
}

/// State of a message being reassembled
class _ReassemblyState {
  final String messageId;
  final int totalFragments;
  final int totalSize;
  final Map<int, Uint8List> receivedChunks = {};
  final DateTime startedAt = DateTime.now();

  _ReassemblyState({
    required this.messageId,
    required this.totalFragments,
    required this.totalSize,
  });

  bool get isComplete => receivedChunks.length == totalFragments;

  void addChunk(int index, Uint8List data) {
    receivedChunks[index] = data;
  }

  Uint8List? reassemble() {
    if (!isComplete) return null;

    // Concatenate chunks in order
    final result = BytesBuilder();
    for (var i = 0; i < totalFragments; i++) {
      final chunk = receivedChunks[i];
      if (chunk == null) return null;
      result.add(chunk);
    }
    return result.toBytes();
  }
}

/// Handles fragmentation and reassembly of messages larger than BLE MTU.
///
/// Grassroots fragments payloads larger than [fragmentThreshold] bytes:
/// - fragmentStart: contains metadata + first chunk
/// - fragmentContinue: intermediate chunks
/// - fragmentEnd: final chunk, triggers reassembly
///
/// Inter-fragment delay: 20ms (to avoid overwhelming BLE buffer)
class FragmentHandler {
  static const _uuid = Uuid();

  /// Maximum chunk size per fragment.
  ///
  /// Session security adds 25 bytes of payload overhead (version + nonce +
  /// AEAD tag), so keep fragments below the BLE target after encryption.
  static const int maxFragmentPayload = 270;

  /// Threshold for fragmenting: max payload that fits in a single encrypted
  /// BLE packet.
  static const int fragmentThreshold = 320;

  /// Inter-fragment delay
  static const Duration fragmentDelay = Duration(milliseconds: 20);

  /// Timeout for incomplete reassembly. Sized to cover slow Android receivers
  /// where Ed25519 verification of every fragment runs in series on a worker
  /// isolate at ~150-200ms each — a 100 KB picture (~315 fragments) needs
  /// roughly a minute end-to-end. The worker keeps the main isolate
  /// responsive; this timeout just has to outlast the verify queue draining.
  static const Duration reassemblyTimeout = Duration(minutes: 2);

  /// Messages currently being reassembled, keyed by messageId
  final Map<String, _ReassemblyState> _reassemblyBuffer = {};

  /// Timer for cleaning up stale reassembly attempts
  Timer? _cleanupTimer;

  FragmentHandler() {
    _startCleanupTimer();
  }

  /// Check if a payload needs fragmentation
  bool needsFragmentation(Uint8List payload) =>
      payload.length > fragmentThreshold;

  /// Fragment a large payload into multiple packets.
  ///
  /// Returns a [FragmentedMessage] containing all the packets to send.
  /// Caller should send them with [fragmentDelay] between each.
  ///
  /// [messageId] is the application-level id under which the sender tracks
  /// delivery (`MessageSendingAction.messageId`). It travels in the
  /// `FRAGMENT_START` payload and is echoed back in the receiver's ACK so
  /// `MessageDeliveredAction` can find the right outgoing-message slot.
  FragmentedMessage fragment({
    required Uint8List payload,
    String? messageId,
  }) {
    if (!needsFragmentation(payload)) {
      throw ArgumentError('Payload does not need fragmentation');
    }

    final id = messageId ?? _uuid.v4();
    final fragments = <GrassrootsPacket>[];

    // Calculate number of fragments needed
    final totalFragments = (payload.length / maxFragmentPayload).ceil();

    for (var i = 0; i < totalFragments; i++) {
      final start = i * maxFragmentPayload;
      final end = (start + maxFragmentPayload).clamp(0, payload.length);
      final chunk = payload.sublist(start, end);

      final PacketType type;
      final Uint8List fragmentPayload;

      if (i == 0) {
        // First fragment: include metadata
        type = PacketType.fragmentStart;
        fragmentPayload = _encodeFragmentStart(
          messageId: id,
          totalFragments: totalFragments,
          totalSize: payload.length,
          chunk: chunk,
        );
      } else if (i == totalFragments - 1) {
        // Last fragment
        type = PacketType.fragmentEnd;
        fragmentPayload = _encodeFragmentEnd(
          messageId: id,
          fragmentIndex: i,
          chunk: chunk,
        );
      } else {
        // Middle fragments
        type = PacketType.fragmentContinue;
        fragmentPayload = _encodeFragmentContinue(
          messageId: id,
          fragmentIndex: i,
          chunk: chunk,
        );
      }

      fragments.add(GrassrootsPacket(
        type: type,
        payload: fragmentPayload,
      ));
    }

    return FragmentedMessage(messageId: id, fragments: fragments);
  }

  /// Process an incoming fragment packet.
  ///
  /// Returns the reassembled message (payload + originating messageId) if
  /// this was the final fragment and all fragments have been received.
  /// Otherwise returns null.
  ReassembledMessage? processFragment(GrassrootsPacket packet) {
    switch (packet.type) {
      case PacketType.fragmentStart:
        return _processFragmentStart(packet);
      case PacketType.fragmentContinue:
        return _processFragmentContinue(packet);
      case PacketType.fragmentEnd:
        return _processFragmentEnd(packet);
      default:
        throw ArgumentError('Not a fragment packet: ${packet.type}');
    }
  }

  ReassembledMessage? _processFragmentStart(GrassrootsPacket packet) {
    final (messageId, totalFragments, totalSize, chunk) =
        _decodeFragmentStart(packet.payload);

    debugPrint('[fragment] START msgId=${messageId.substring(0, 8)} '
        'totalFragments=$totalFragments totalSize=${totalSize}B '
        'firstChunk=${chunk.length}B');

    // Create reassembly state
    _reassemblyBuffer[messageId] = _ReassemblyState(
      messageId: messageId,
      totalFragments: totalFragments,
      totalSize: totalSize,
    )..addChunk(0, chunk);

    // Check if single-fragment message
    if (totalFragments == 1) {
      final state = _reassemblyBuffer.remove(messageId)!;
      debugPrint(
          '[fragment] reassembled single-fragment msgId=${messageId.substring(0, 8)}');
      final payload = state.reassemble();
      if (payload == null) return null;
      return ReassembledMessage(messageId: messageId, payload: payload);
    }

    return null;
  }

  ReassembledMessage? _processFragmentContinue(GrassrootsPacket packet) {
    final (messageId, fragmentIndex, chunk) =
        _decodeFragmentContinue(packet.payload);

    final state = _reassemblyBuffer[messageId];
    if (state == null) {
      // Missing start fragment (or buffer was GC'd by reassembly timeout
      // before this fragment landed). Drop with a log so the gap is visible.
      debugPrint('[fragment] CONTINUE msgId=${messageId.substring(0, 8)} '
          'index=$fragmentIndex DROPPED — no reassembly buffer '
          '(timeout or missing start)');
      return null;
    }

    state.addChunk(fragmentIndex, chunk);
    // Log every 25th to avoid spamming for big payloads.
    if (fragmentIndex % 25 == 0) {
      debugPrint('[fragment] CONTINUE msgId=${messageId.substring(0, 8)} '
          'index=$fragmentIndex received=${state.receivedChunks.length}/${state.totalFragments}');
    }
    return null;
  }

  ReassembledMessage? _processFragmentEnd(GrassrootsPacket packet) {
    final (messageId, fragmentIndex, chunk) =
        _decodeFragmentEnd(packet.payload);

    final state = _reassemblyBuffer[messageId];
    if (state == null) {
      debugPrint('[fragment] END msgId=${messageId.substring(0, 8)} '
          'index=$fragmentIndex DROPPED — no reassembly buffer '
          '(timeout or missing start)');
      return null;
    }

    state.addChunk(fragmentIndex, chunk);

    // Attempt reassembly
    final result = state.reassemble();
    if (result != null) {
      _reassemblyBuffer.remove(messageId);
      debugPrint(
          '[fragment] END msgId=${messageId.substring(0, 8)} reassembled '
          '${result.length}B from ${state.totalFragments} fragments');
      return ReassembledMessage(messageId: messageId, payload: result);
    }

    debugPrint('[fragment] END msgId=${messageId.substring(0, 8)} INCOMPLETE: '
        'have ${state.receivedChunks.length}/${state.totalFragments} fragments — '
        'missing fragments will never arrive, dropping at next cleanup');
    return null;
  }

  // ===== Encoding helpers =====

  Uint8List _encodeFragmentStart({
    required String messageId,
    required int totalFragments,
    required int totalSize,
    required Uint8List chunk,
  }) {
    // Format: [messageId:36][totalFragments:2][totalSize:4][chunk:...]
    final buffer = BytesBuilder();
    buffer.add(Uint8List.fromList(messageId.codeUnits));

    final header = ByteData(6);
    header.setUint16(0, totalFragments, Endian.big);
    header.setUint32(2, totalSize, Endian.big);
    buffer.add(header.buffer.asUint8List());

    buffer.add(chunk);
    return buffer.toBytes();
  }

  Uint8List _encodeFragmentContinue({
    required String messageId,
    required int fragmentIndex,
    required Uint8List chunk,
  }) {
    // Format: [messageId:36][fragmentIndex:2][chunk:...]
    final buffer = BytesBuilder();
    buffer.add(Uint8List.fromList(messageId.codeUnits));

    final header = ByteData(2);
    header.setUint16(0, fragmentIndex, Endian.big);
    buffer.add(header.buffer.asUint8List());

    buffer.add(chunk);
    return buffer.toBytes();
  }

  Uint8List _encodeFragmentEnd({
    required String messageId,
    required int fragmentIndex,
    required Uint8List chunk,
  }) {
    // Same format as continue
    return _encodeFragmentContinue(
      messageId: messageId,
      fragmentIndex: fragmentIndex,
      chunk: chunk,
    );
  }

  // ===== Decoding helpers =====

  (String, int, int, Uint8List) _decodeFragmentStart(Uint8List data) {
    final messageId = String.fromCharCodes(data.sublist(0, 36));
    final header = ByteData.view(data.buffer, data.offsetInBytes + 36, 6);
    final totalFragments = header.getUint16(0, Endian.big);
    final totalSize = header.getUint32(2, Endian.big);
    final chunk = data.sublist(42);
    return (messageId, totalFragments, totalSize, chunk);
  }

  (String, int, Uint8List) _decodeFragmentContinue(Uint8List data) {
    final messageId = String.fromCharCodes(data.sublist(0, 36));
    final header = ByteData.view(data.buffer, data.offsetInBytes + 36, 2);
    final fragmentIndex = header.getUint16(0, Endian.big);
    final chunk = data.sublist(38);
    return (messageId, fragmentIndex, chunk);
  }

  (String, int, Uint8List) _decodeFragmentEnd(Uint8List data) {
    return _decodeFragmentContinue(data);
  }

  // ===== Cleanup =====

  void _startCleanupTimer() {
    _cleanupTimer = Timer.periodic(const Duration(seconds: 10), (_) {
      _cleanupStaleReassemblies();
    });
  }

  void _cleanupStaleReassemblies() {
    final now = DateTime.now();
    _reassemblyBuffer.removeWhere((id, state) {
      final age = now.difference(state.startedAt);
      if (age > reassemblyTimeout) {
        debugPrint(
            '[fragment] DROPPING stale reassembly msgId=${id.substring(0, 8)} '
            'received=${state.receivedChunks.length}/${state.totalFragments} '
            'age=${age.inSeconds}s — fragments arrived too slowly to reassemble');
        return true;
      }
      return false;
    });
  }

  /// Clean up resources
  void dispose() {
    _cleanupTimer?.cancel();
    _reassemblyBuffer.clear();
  }
}
