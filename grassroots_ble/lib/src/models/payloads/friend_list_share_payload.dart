/// Friend list share payload for the Grassroots BLE protocol.
library;

import 'dart:typed_data';

/// Payload for friend list sharing (type 0x31).
///
/// Used at Privacy Level 4 to share friend list as a Bloom filter.
///
/// Format:
/// - Filter Size: 2 bytes (big-endian, size in bytes)
/// - Hash Count: 1 byte (number of hash functions k)
/// - Bloom Filter: variable (the filter bits)
class FriendListSharePayload {
  /// The Bloom filter containing friend PeerIDs.
  final Uint8List bloomFilter;

  /// Number of hash functions used.
  final int hashCount;

  /// Creates a friend list share payload.
  FriendListSharePayload({
    required this.bloomFilter,
    required this.hashCount,
  }) {
    if (bloomFilter.isEmpty) {
      throw ArgumentError('bloomFilter cannot be empty');
    }
    if (bloomFilter.length > 65535) {
      throw ArgumentError(
        'bloomFilter too large: ${bloomFilter.length} bytes (max 65535)',
      );
    }
    if (hashCount < 1 || hashCount > 255) {
      throw ArgumentError('hashCount must be 1-255');
    }
  }

  /// The size of the Bloom filter in bits.
  int get filterSizeBits => bloomFilter.length * 8;

  /// Encodes this payload to bytes.
  Uint8List encode() {
    final buffer = BytesBuilder();
    final size = bloomFilter.length;
    buffer.addByte((size >> 8) & 0xFF);
    buffer.addByte(size & 0xFF);
    buffer.addByte(hashCount);
    buffer.add(bloomFilter);
    return buffer.toBytes();
  }

  /// Decodes a friend list share payload from bytes.
  ///
  /// Throws [FormatException] if the data is invalid.
  static FriendListSharePayload decode(Uint8List data) {
    if (data.length < 3) {
      throw FormatException(
        'Friend list share payload too short: ${data.length} bytes (min 3)',
      );
    }

    final filterSize = (data[0] << 8) | data[1];
    final hashCount = data[2];

    if (filterSize == 0) {
      throw FormatException('Filter size cannot be 0');
    }

    if (data.length < 3 + filterSize) {
      throw FormatException(
        'Friend list share payload truncated: expected ${3 + filterSize} bytes, got ${data.length}',
      );
    }

    final bloomFilter = Uint8List.fromList(data.sublist(3, 3 + filterSize));

    return FriendListSharePayload(
      bloomFilter: bloomFilter,
      hashCount: hashCount,
    );
  }

  /// Checks if a PeerID might be in this filter.
  ///
  /// Note: This is a simplified check. The actual Bloom filter lookup
  /// should use the same hash functions as the sender.
  /// See BloomFilter class for full implementation.
  bool mightContain(Uint8List peerIdBytes) {
    // Placeholder - actual implementation in BloomFilter class
    throw UnimplementedError('Use BloomFilter.mightContain instead');
  }

  @override
  String toString() =>
      'FriendListSharePayload(${bloomFilter.length} bytes, k=$hashCount)';
}
