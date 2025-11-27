/// Protocol constants for Grassroots BLE communication.
///
/// All values are from the protocol specification.
library;

/// Protocol version.
const int protocolVersion = 1;

// =============================================================================
// Timeouts (in seconds unless noted)
// =============================================================================

/// BLE connection establishment timeout.
const Duration connectionTimeout = Duration(seconds: 10);

/// Noise handshake completion timeout.
const Duration handshakeTimeout = Duration(seconds: 10);

/// Disconnect idle connections after this duration.
const Duration idleTimeout = Duration(seconds: 60);

/// Incomplete fragment reassembly timeout.
const Duration fragmentTimeout = Duration(seconds: 30);

/// First retry delay.
const Duration retryInitial = Duration(seconds: 2);

/// Maximum retry delay.
const Duration retryMax = Duration(seconds: 16);

/// Maximum retry attempts.
const int retryCount = 4;

/// Wait after friend rejection before retrying.
const Duration friendCooldown = Duration(seconds: 300);

/// Store-and-forward message max age.
const Duration messageExpiry = Duration(hours: 24);

/// Deduplication Bloom filter rotation interval.
const Duration bloomRotation = Duration(seconds: 300);

// =============================================================================
// Size Limits
// =============================================================================

/// Maximum message payload size in bytes.
const int maxMessageSize = 65535;

/// Maximum display name length in bytes.
const int maxDisplayName = 63;

/// Maximum friends list size.
const int maxFriends = 1000;

/// Store-and-forward queue size.
const int maxPendingMessages = 100;

/// Maximum fragments per message.
const int maxFragments = 255;

/// Default TTL for flooding.
const int defaultTtl = 7;

/// Assumed BLE MTU in bytes.
const int bleMtu = 512;

/// RSSI threshold for proximity friending (dBm).
const int rssiThreshold = -50;

// =============================================================================
// Packet Sizes
// =============================================================================

/// Fixed header size in bytes.
const int headerSize = 14;

/// Sender/Recipient ID size in bytes.
const int peerIdSize = 8;

/// Ed25519 signature size in bytes.
const int signatureSize = 64;

/// Curve25519 public key size in bytes.
const int curve25519KeySize = 32;

/// Ed25519 public key size in bytes.
const int ed25519KeySize = 32;

/// UUID size in bytes (for message IDs).
const int uuidSize = 16;

/// Service UUID size in bytes (BLE).
const int serviceUuidSize = 16;

/// SHA-256 fingerprint size in bytes.
const int fingerprintSize = 32;

// =============================================================================
// Bloom Filter Parameters
// =============================================================================

/// Deduplication filter size in bits.
const int dedupFilterSizeBits = 8192;

/// Deduplication filter hash function count.
const int dedupFilterHashCount = 7;

/// Friend list filter size in bits.
const int friendFilterSizeBits = 2048;

/// Friend list filter hash function count.
const int friendFilterHashCount = 5;

// =============================================================================
// Padding Block Sizes
// =============================================================================

/// Valid padding block sizes for traffic analysis resistance.
const List<int> paddingBlockSizes = [256, 512, 1024, 2048];

// =============================================================================
// BLE Duty Cycle
// =============================================================================

/// Advertising duration.
const Duration advertiseDuration = Duration(seconds: 2);

/// Scanning duration.
const Duration scanDuration = Duration(seconds: 5);

// =============================================================================
// Fixed UUIDs
// =============================================================================

/// Fixed characteristic UUID for GATT service.
const String characteristicUuid = 'A1B2C3D4-E5F6-4A5B-8C9D-0E1F2A3B4C5D';
