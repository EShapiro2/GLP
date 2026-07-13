import 'dart:async';
import 'dart:typed_data';
import '../platform/compat.dart';
import '../models/packet.dart';

/// A message split for transmission per spec §Message Transport.
class FragmentedMessage {
  final String messageId;
  final List<GrassrootsPacket> fragments;

  FragmentedMessage({required this.messageId, required this.fragments});

  int get totalFragments => fragments.length;
}

/// A fully reassembled message ready for delivery.
class ReassembledMessage {
  final String messageId;
  final Uint8List payload;

  ReassembledMessage({required this.messageId, required this.payload});
}

/// One decoded fragment (spec §Message Transport: every fragment carries
/// the messageId, its index, and the fragment count, so any subset arriving
/// in any order — over a lossy carrier — reassembles without a
/// distinguished first fragment).
class Fragment {
  final String messageId;
  final int index;
  final int count;
  final Uint8List chunk;

  Fragment({
    required this.messageId,
    required this.index,
    required this.count,
    required this.chunk,
  });
}

class _ReassemblyState {
  final int count;
  final Map<int, Uint8List> receivedChunks = {};
  final DateTime startedAt = DateTime.now();

  _ReassemblyState({required this.count});

  bool get isComplete => receivedChunks.length == count;

  Uint8List assemble() {
    final builder = BytesBuilder(copy: false);
    for (var i = 0; i < count; i++) {
      builder.add(receivedChunks[i]!);
    }
    return builder.toBytes();
  }
}

/// Fragmentation and reassembly for the shared message transport (spec
/// §Message Transport): a message is assigned its messageId and split into
/// fragments sized to the carrier; each fragment is self-contained; the
/// receiver reassembles when all fragments have arrived and delivers the
/// message whole (atomic delivery).
class FragmentHandler {
  /// The messageId is a full 36-character UUID.
  static const int messageIdLength = 36;

  /// Per-fragment payload overhead: messageId + index(u32) + count(u32).
  static const int fragmentHeaderLength = messageIdLength + 8;

  /// Fragment chunk size for the BLE carrier: sized to the negotiated GATT
  /// MTU with room for the 5-byte frame and the session AEAD overhead
  /// (version + nonce + tag = 25 bytes).
  static const int bleMaxChunk = 270;

  /// Fragment chunk size for the IP carrier: one fragment rides one UDP
  /// datagram sized within the path MTU (spec §Message Transport). A
  /// conservative 1200-byte datagram budget minus the frame (5), the
  /// fragment header (44) and the session overhead (25) leaves ~1126;
  /// rounded down.
  static const int udpMaxChunk = 1100;

  /// Timeout for incomplete reassembly. Over a lossy carrier fragments
  /// retransmit with backoff, so this must outlast the sender giving up.
  static const Duration reassemblyTimeout = Duration(minutes: 2);

  /// Messages currently being reassembled, keyed by messageId.
  final Map<String, _ReassemblyState> _reassemblyBuffer = {};

  Timer? _cleanupTimer;

  FragmentHandler() {
    _startCleanupTimer();
  }

  /// Whether [payload] exceeds one [maxChunk]-sized carrier packet.
  bool needsFragmentation(Uint8List payload, {required int maxChunk}) =>
      payload.length > maxChunk;

  /// Split [payload] into self-contained fragments of at most [maxChunk]
  /// chunk bytes each.
  FragmentedMessage fragment({
    required Uint8List payload,
    required String messageId,
    required int maxChunk,
  }) {
    if (messageId.length != messageIdLength) {
      throw ArgumentError.value(
          messageId, 'messageId', 'must be a 36-character UUID');
    }
    final count = payload.isEmpty ? 1 : (payload.length / maxChunk).ceil();
    final fragments = <GrassrootsPacket>[];
    for (var i = 0; i < count; i++) {
      final start = i * maxChunk;
      final end = (start + maxChunk).clamp(0, payload.length);
      fragments.add(GrassrootsPacket(
        type: PacketType.fragment,
        payload: encodeFragment(
          messageId: messageId,
          index: i,
          count: count,
          chunk: payload.sublist(start, end),
        ),
      ));
    }
    return FragmentedMessage(messageId: messageId, fragments: fragments);
  }

  /// Payload layout: `[messageId(36)][index u32][count u32][chunk]`.
  static Uint8List encodeFragment({
    required String messageId,
    required int index,
    required int count,
    required Uint8List chunk,
  }) {
    final bytes = Uint8List(fragmentHeaderLength + chunk.length);
    bytes.setRange(0, messageIdLength, messageId.codeUnits);
    final view = ByteData.view(bytes.buffer);
    view.setUint32(messageIdLength, index, Endian.big);
    view.setUint32(messageIdLength + 4, count, Endian.big);
    bytes.setRange(fragmentHeaderLength, bytes.length, chunk);
    return bytes;
  }

  static Fragment decodeFragment(Uint8List payload) {
    if (payload.length < fragmentHeaderLength) {
      throw const FormatException('Fragment payload is truncated');
    }
    final messageId =
        String.fromCharCodes(payload.sublist(0, messageIdLength));
    final view =
        ByteData.view(payload.buffer, payload.offsetInBytes, payload.length);
    final index = view.getUint32(messageIdLength, Endian.big);
    final count = view.getUint32(messageIdLength + 4, Endian.big);
    if (count == 0 || index >= count) {
      throw FormatException('Fragment index $index out of range for $count');
    }
    return Fragment(
      messageId: messageId,
      index: index,
      count: count,
      chunk: Uint8List.fromList(payload.sublist(fragmentHeaderLength)),
    );
  }

  /// Per-fragment acknowledgment payload: `[messageId(36)][index u32]`.
  static Uint8List encodeFragmentAck({
    required String messageId,
    required int index,
  }) {
    final bytes = Uint8List(messageIdLength + 4);
    bytes.setRange(0, messageIdLength, messageId.codeUnits);
    ByteData.view(bytes.buffer).setUint32(messageIdLength, index, Endian.big);
    return bytes;
  }

  static (String, int) decodeFragmentAck(Uint8List payload) {
    if (payload.length < messageIdLength + 4) {
      throw const FormatException('Fragment ACK payload is truncated');
    }
    final messageId =
        String.fromCharCodes(payload.sublist(0, messageIdLength));
    final index =
        ByteData.view(payload.buffer, payload.offsetInBytes, payload.length)
            .getUint32(messageIdLength, Endian.big);
    return (messageId, index);
  }

  /// Store an incoming [fragment]. Returns the reassembled message when
  /// this fragment completes it, null otherwise (including duplicates).
  ReassembledMessage? addFragment(Fragment fragment) {
    final state = _reassemblyBuffer.putIfAbsent(
      fragment.messageId,
      () => _ReassemblyState(count: fragment.count),
    );
    if (state.count != fragment.count) {
      debugPrint('[fragment] Count mismatch for ${fragment.messageId}: '
          '${state.count} vs ${fragment.count}; dropping fragment');
      return null;
    }
    state.receivedChunks[fragment.index] = fragment.chunk;
    if (!state.isComplete) return null;
    _reassemblyBuffer.remove(fragment.messageId);
    return ReassembledMessage(
      messageId: fragment.messageId,
      payload: state.assemble(),
    );
  }

  void _startCleanupTimer() {
    _cleanupTimer = Timer.periodic(const Duration(seconds: 30), (_) {
      final now = DateTime.now();
      _reassemblyBuffer.removeWhere((messageId, state) {
        final stale = now.difference(state.startedAt) > reassemblyTimeout;
        if (stale) {
          debugPrint('[fragment] Reassembly timed out for $messageId '
              '(${state.receivedChunks.length}/${state.count})');
        }
        return stale;
      });
    });
  }

  void dispose() {
    _cleanupTimer?.cancel();
    _reassemblyBuffer.clear();
  }
}
