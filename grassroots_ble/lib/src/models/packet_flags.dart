/// Packet flags for the Grassroots BLE protocol.
///
/// Flags are stored in a single byte in the packet header.
library;

/// Packet flags indicating presence of optional fields.
class PacketFlags {
  /// Bit 0: Has recipient ID (directed message).
  static const int hasRecipientBit = 0x01;

  /// Bit 1: Has signature (authenticated message).
  static const int hasSignatureBit = 0x02;

  /// Bit 2: Is compressed (zlib compression applied).
  static const int isCompressedBit = 0x04;

  /// Bit 3: Has route (source routing info included).
  static const int hasRouteBit = 0x08;

  /// The raw flags byte.
  final int value;

  /// Creates flags from a raw byte value.
  const PacketFlags(this.value);

  /// Creates flags with no bits set.
  const PacketFlags.none() : value = 0;

  /// Creates flags from individual boolean values.
  PacketFlags.from({
    bool hasRecipient = false,
    bool hasSignature = false,
    bool isCompressed = false,
    bool hasRoute = false,
  }) : value = (hasRecipient ? hasRecipientBit : 0) |
            (hasSignature ? hasSignatureBit : 0) |
            (isCompressed ? isCompressedBit : 0) |
            (hasRoute ? hasRouteBit : 0);

  /// Whether the packet has a recipient ID.
  bool get hasRecipient => (value & hasRecipientBit) != 0;

  /// Whether the packet has a signature.
  bool get hasSignature => (value & hasSignatureBit) != 0;

  /// Whether the payload is compressed.
  bool get isCompressed => (value & isCompressedBit) != 0;

  /// Whether the packet has routing information.
  bool get hasRoute => (value & hasRouteBit) != 0;

  /// Returns a new [PacketFlags] with the given flag set.
  PacketFlags withFlag(int bit) => PacketFlags(value | bit);

  /// Returns a new [PacketFlags] with the given flag cleared.
  PacketFlags withoutFlag(int bit) => PacketFlags(value & ~bit);

  /// Returns a new [PacketFlags] with hasRecipient set.
  PacketFlags withRecipient() => withFlag(hasRecipientBit);

  /// Returns a new [PacketFlags] with hasSignature set.
  PacketFlags withSignature() => withFlag(hasSignatureBit);

  /// Returns a new [PacketFlags] with isCompressed set.
  PacketFlags withCompressed() => withFlag(isCompressedBit);

  /// Returns a new [PacketFlags] with hasRoute set.
  PacketFlags withRoute() => withFlag(hasRouteBit);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PacketFlags &&
          runtimeType == other.runtimeType &&
          value == other.value;

  @override
  int get hashCode => value.hashCode;

  @override
  String toString() {
    final parts = <String>[];
    if (hasRecipient) parts.add('hasRecipient');
    if (hasSignature) parts.add('hasSignature');
    if (isCompressed) parts.add('isCompressed');
    if (hasRoute) parts.add('hasRoute');
    return 'PacketFlags(${parts.isEmpty ? 'none' : parts.join(', ')})';
  }
}
