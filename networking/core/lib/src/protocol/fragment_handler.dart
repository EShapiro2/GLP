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

  /// The fragment type the message travelled as: [PacketType.fragment] for a
  /// `send` payload, [PacketType.attestationFragment] for the attestation
  /// exchange.
  final PacketType type;

  ReassembledMessage({
    required this.messageId,
    required this.payload,
    this.type = PacketType.fragment,
  });
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

  /// The packet type the fragment arrived as (one of
  /// [FragmentHandler.fragmentTypes]); a message's fragments all share it.
  final PacketType type;

  Fragment({
    required this.messageId,
    required this.index,
    required this.count,
    required this.chunk,
    this.type = PacketType.fragment,
  });
}

class _ReassemblyState {
  final int count;
  final PacketType type;
  final Map<int, Uint8List> receivedChunks = {};

  /// When a fragment of the message last arrived: the buffer is dropped only
  /// after [FragmentHandler.reassemblyTimeout] of silence since then.
  DateTime lastArrival;

  _ReassemblyState({
    required this.count,
    required this.type,
    required this.lastArrival,
  });

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

  /// The packet types a fragment travels as (spec §Message Protocol: one
  /// protocol carries `send` payloads and the attestation exchange). The two
  /// share the layout below, the acknowledgments and the retransmission; the
  /// type tells the receiver which the reassembled message is.
  static const Set<PacketType> fragmentTypes = {
    PacketType.fragment,
    PacketType.attestationFragment,
  };

  static void _checkFragmentType(PacketType type) {
    if (!fragmentTypes.contains(type)) {
      throw ArgumentError.value(type, 'type', 'not a fragment type');
    }
  }

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

  /// How long a reassembly buffer survives with no fragment of its message
  /// arriving. The clock restarts on every fragment's arrival, so a buffer is
  /// never dropped while fragments still arrive, however long the message
  /// takes. A sender retransmits each fragment with backoff for its attempt
  /// window (about 30 s over IP, 12 s over GATT) and then fails the message,
  /// which falls back to its queue and is sent whole again; this outlasts
  /// that window, so a buffer is dropped only once its sender has given up.
  /// Dropped earlier, the fragments that arrive afterwards would land in a
  /// fresh buffer and be acknowledged, and the sender would settle a message
  /// never reassembled.
  static const Duration reassemblyTimeout = Duration(minutes: 2);

  /// Messages currently being reassembled, keyed by the sender's public key
  /// (hex) and the messageId: two senders may each use the same id.
  final Map<({String senderHex, String messageId}), _ReassemblyState>
      _reassemblyBuffer = {};

  /// The messages released after they were saved, by sender, messageId and
  /// fragment type, with when each was released: kept for
  /// [reassemblyTimeout], so that a fragment of one its sender retransmits —
  /// the acknowledgment of its last fragment lost — is acknowledged at once
  /// rather than held back in a fresh buffer ([wasReleased]). An exact record
  /// keyed on the authenticated sender, bounded in time: it never takes a
  /// fresh message for another.
  final Map<({String senderHex, String messageId, PacketType type}), DateTime>
      _released = {};

  Timer? _cleanupTimer;

  /// The clock the buffers' silence is measured by; a test passes its own.
  final DateTime Function() _now;

  FragmentHandler({DateTime Function() now = DateTime.now}) : _now = now {
    _startCleanupTimer();
  }

  /// Whether [payload] exceeds one [maxChunk]-sized carrier packet.
  bool needsFragmentation(Uint8List payload, {required int maxChunk}) =>
      payload.length > maxChunk;

  /// Split [payload] into self-contained fragments of at most [maxChunk]
  /// chunk bytes each, travelling as [type] — [PacketType.fragment] for a
  /// `send` payload, [PacketType.attestationFragment] for the attestation
  /// exchange.
  FragmentedMessage fragment({
    required Uint8List payload,
    required String messageId,
    required int maxChunk,
    PacketType type = PacketType.fragment,
  }) {
    _checkFragmentType(type);
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
        type: type,
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

  /// Decode a fragment payload that arrived as [type].
  static Fragment decodeFragment(
    Uint8List payload, {
    PacketType type = PacketType.fragment,
  }) {
    _checkFragmentType(type);
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
      type: type,
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

  /// Store an incoming [fragment] sent by the peer whose public key (hex) is
  /// [senderHex]. Returns the reassembled message when this fragment
  /// completes it, or when the message is complete already and this is its
  /// last fragment ([heldIndex]); null otherwise.
  ///
  /// A completed message stays in the buffer until [release]: the receiver
  /// acknowledges the last fragment only once it has saved the message, and
  /// a save that fails leaves the buffer whole, so that the sender's
  /// retransmission of that fragment returns the message again and the save
  /// is retried.
  ///
  /// No fragment is dropped. A message's fragments share one count and one
  /// type; a fragment whose count or type differs from its buffer's belongs
  /// to another attempt at the message — sent again over another carrier, at
  /// another chunk size — and replaces the stale buffer with one it starts.
  /// So a payload's chunk can never complete an attestation or the reverse,
  /// and no attempt's fragment is mixed into another's.
  ReassembledMessage? addFragment(Fragment fragment,
      {required String senderHex}) {
    final key = (senderHex: senderHex, messageId: fragment.messageId);
    final arrival = _now();
    var state = _reassemblyBuffer[key];
    if (state != null &&
        (state.count != fragment.count || state.type != fragment.type)) {
      debugPrint('[fragment] ${fragment.messageId} arrives as '
          '${fragment.count} ${fragment.type.name} fragment(s), its buffer '
          'holds ${state.count} ${state.type.name}: another attempt, which '
          'replaces it');
      state = null;
    }
    state ??= _reassemblyBuffer[key] = _ReassemblyState(
      count: fragment.count,
      type: fragment.type,
      lastArrival: arrival,
    );
    state.lastArrival = arrival;
    if (state.isComplete) {
      if (fragment.index != heldIndex(fragment.count)) return null;
    } else {
      state.receivedChunks[fragment.index] = fragment.chunk;
      if (!state.isComplete) return null;
    }
    return ReassembledMessage(
      messageId: fragment.messageId,
      payload: state.assemble(),
      type: state.type,
    );
  }

  /// The index of the fragment of a [count]-fragment message whose
  /// acknowledgment the receiver holds back until the message is
  /// reassembled and saved: the last by index, whatever order the fragments
  /// arrive in. Every other fragment is acknowledged on arrival. A fixed
  /// fragment, so that a receiver that lost its buffer never acknowledges the
  /// held fragment alone into a fresh one: the sender retransmits it until it
  /// gives up, and sends the whole message again.
  static int heldIndex(int count) => count - 1;

  /// Drop the buffer of the message [messageId] from [senderHex], travelling
  /// as [type], once the receiver has saved it (or, for a message not saved,
  /// handled it), and record it as released for [reassemblyTimeout].
  void release({
    required String senderHex,
    required String messageId,
    required PacketType type,
  }) {
    _reassemblyBuffer.remove((senderHex: senderHex, messageId: messageId));
    _released[(senderHex: senderHex, messageId: messageId, type: type)] =
        _now();
  }

  /// Whether the message [messageId] from [senderHex], travelling as [type],
  /// was released less than [reassemblyTimeout] ago: saved, so that a
  /// fragment of it is acknowledged at once and the message not delivered
  /// again.
  bool wasReleased({
    required String senderHex,
    required String messageId,
    required PacketType type,
  }) =>
      _released.containsKey(
          (senderHex: senderHex, messageId: messageId, type: type));

  void _startCleanupTimer() {
    _cleanupTimer = Timer.periodic(
        const Duration(seconds: 30), (_) => _dropSilentBuffers());
  }

  /// Drop every buffer whose message has had no fragment arrive for longer
  /// than [reassemblyTimeout], and every record of a message released longer
  /// ago than that.
  void _dropSilentBuffers() {
    final now = _now();
    _released
        .removeWhere((_, releasedAt) => now.difference(releasedAt) >
            reassemblyTimeout);
    _reassemblyBuffer.removeWhere((key, state) {
      final silent = now.difference(state.lastArrival) > reassemblyTimeout;
      if (silent) {
        debugPrint('[fragment] Reassembly timed out for ${key.messageId} '
            '(${state.receivedChunks.length}/${state.count}): no fragment '
            'for $reassemblyTimeout');
      }
      return silent;
    });
  }

  /// The periodic sweep of silent buffers, as a test drives it with its own
  /// clock.
  @visibleForTesting
  void debugDropSilentBuffers() => _dropSilentBuffers();

  void dispose() {
    _cleanupTimer?.cancel();
    _reassemblyBuffer.clear();
    _released.clear();
  }
}
