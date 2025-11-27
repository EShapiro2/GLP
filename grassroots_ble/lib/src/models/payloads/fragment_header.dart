/// Fragment header for the Grassroots BLE protocol.
library;

import 'dart:typed_data';

/// Header for fragmented messages (types 0xF0, 0xF1, 0xF2).
///
/// Format:
/// - Message ID: 2 bytes (unique ID for reassembly)
/// - Fragment Index: 1 byte (0-based index)
/// - Total Fragments: 1 byte (total count)
class FragmentHeader {
  /// Size of the fragment header in bytes.
  static const int size = 4;

  /// Unique ID for reassembly (same across all fragments of a message).
  final int messageId;

  /// 0-based index of this fragment.
  final int fragmentIndex;

  /// Total number of fragments.
  final int totalFragments;

  /// Creates a fragment header.
  FragmentHeader({
    required this.messageId,
    required this.fragmentIndex,
    required this.totalFragments,
  }) {
    if (messageId < 0 || messageId > 65535) {
      throw ArgumentError('messageId must be 0-65535');
    }
    if (fragmentIndex < 0 || fragmentIndex > 255) {
      throw ArgumentError('fragmentIndex must be 0-255');
    }
    if (totalFragments < 1 || totalFragments > 255) {
      throw ArgumentError('totalFragments must be 1-255');
    }
    if (fragmentIndex >= totalFragments) {
      throw ArgumentError(
        'fragmentIndex ($fragmentIndex) must be < totalFragments ($totalFragments)',
      );
    }
  }

  /// Whether this is the first fragment.
  bool get isFirst => fragmentIndex == 0;

  /// Whether this is the last fragment.
  bool get isLast => fragmentIndex == totalFragments - 1;

  /// Whether this is a middle fragment (not first and not last).
  bool get isMiddle => !isFirst && !isLast;

  /// Encodes this header to bytes.
  Uint8List encode() {
    return Uint8List.fromList([
      (messageId >> 8) & 0xFF,
      messageId & 0xFF,
      fragmentIndex,
      totalFragments,
    ]);
  }

  /// Decodes a fragment header from bytes.
  ///
  /// Throws [FormatException] if the data is invalid.
  static FragmentHeader decode(Uint8List data) {
    if (data.length < size) {
      throw FormatException(
        'Fragment header too short: ${data.length} bytes (need $size)',
      );
    }

    final messageId = (data[0] << 8) | data[1];
    final fragmentIndex = data[2];
    final totalFragments = data[3];

    if (totalFragments == 0) {
      throw FormatException('totalFragments cannot be 0');
    }
    if (fragmentIndex >= totalFragments) {
      throw FormatException(
        'fragmentIndex ($fragmentIndex) >= totalFragments ($totalFragments)',
      );
    }

    return FragmentHeader(
      messageId: messageId,
      fragmentIndex: fragmentIndex,
      totalFragments: totalFragments,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is FragmentHeader &&
          runtimeType == other.runtimeType &&
          messageId == other.messageId &&
          fragmentIndex == other.fragmentIndex &&
          totalFragments == other.totalFragments;

  @override
  int get hashCode =>
      Object.hash(messageId, fragmentIndex, totalFragments);

  @override
  String toString() =>
      'FragmentHeader(id=$messageId, ${fragmentIndex + 1}/$totalFragments)';
}
