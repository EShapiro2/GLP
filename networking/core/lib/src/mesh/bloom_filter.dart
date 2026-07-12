import 'dart:typed_data';
import 'dart:math';

/// Optimized Bloom filter for packet ID deduplication.
/// 
/// Grassroots uses this to prevent infinite routing loops in the mesh.
/// 
/// Properties:
/// - No false negatives: if we say "not seen", it's definitely not seen
/// - Rare false positives: may occasionally say "seen" when not actually seen
/// - False positive rate ~1% with current parameters
/// 
/// The filter automatically rotates (clears old entries) to prevent filling up.
class BloomFilter {
  /// Number of bits in the filter
  final int size;
  
  /// Number of hash functions to use
  final int hashCount;
  
  /// The bit array
  late Uint8List _bits;
  
  /// Number of items added
  int _itemCount = 0;
  
  /// Maximum items before rotation (to maintain false positive rate)
  final int maxItems;
  
  /// Timestamp of last rotation
  DateTime _lastRotation = DateTime.now();
  
  /// How often to force rotation (even if not full)
  final Duration rotationInterval;
  
  /// Create a Bloom filter optimized for expected item count.
  /// 
  /// Default parameters target ~1% false positive rate for 10,000 items,
  /// which is suitable for a busy mesh over a few minutes.
  BloomFilter({
    this.size = 96000,          // ~12KB, good for mobile
    this.hashCount = 7,          // Optimal for 1% FP rate
    this.maxItems = 10000,
    this.rotationInterval = const Duration(minutes: 5),
  }) {
    _bits = Uint8List((size + 7) ~/ 8);
  }
  
  /// Check if an item might be in the filter.
  /// 
  /// Returns:
  /// - false: Definitely NOT in the filter
  /// - true: PROBABLY in the filter (may be false positive)
  bool mightContain(String item) {
    _maybeRotate();
    final hashes = _getHashes(item);
    for (final hash in hashes) {
      final index = hash % size;
      final byteIndex = index ~/ 8;
      final bitIndex = index % 8;
      if ((_bits[byteIndex] & (1 << bitIndex)) == 0) {
        return false;
      }
    }
    return true;
  }
  
  /// Add an item to the filter.
  void add(String item) {
    _maybeRotate();
    final hashes = _getHashes(item);
    for (final hash in hashes) {
      final index = hash % size;
      final byteIndex = index ~/ 8;
      final bitIndex = index % 8;
      _bits[byteIndex] |= (1 << bitIndex);
    }
    _itemCount++;
  }
  
  /// Check and add in one operation (common pattern).
  /// Returns true if the item was already (probably) present.
  bool checkAndAdd(String item) {
    final wasPresent = mightContain(item);
    if (!wasPresent) {
      add(item);
    }
    return wasPresent;
  }
  
  /// Clear the filter (rotate)
  void clear() {
    _bits = Uint8List((size + 7) ~/ 8);
    _itemCount = 0;
    _lastRotation = DateTime.now();
  }
  
  /// Current fill ratio (0.0 to 1.0)
  double get fillRatio => _itemCount / maxItems;
  
  /// Estimated false positive rate at current fill level
  double get estimatedFalsePositiveRate {
    // FP rate ≈ (1 - e^(-kn/m))^k
    // k = hash count, n = items, m = size
    final exponent = -hashCount * _itemCount / size;
    return pow(1 - exp(exponent), hashCount).toDouble();
  }
  
  /// Check if rotation is needed and perform it
  void _maybeRotate() {
    final now = DateTime.now();
    if (_itemCount >= maxItems || 
        now.difference(_lastRotation) > rotationInterval) {
      clear();
    }
  }
  
  /// Generate hash values for an item using double hashing technique.
  /// This gives us k independent hashes from just 2 hash computations.
  List<int> _getHashes(String item) {
    final bytes = Uint8List.fromList(item.codeUnits);
    
    // Use two different hash functions
    final h1 = _fnv1a(bytes);
    final h2 = _murmur3(bytes);
    
    // Generate k hashes using h1 + i*h2
    return List.generate(hashCount, (i) => (h1 + i * h2) & 0x7FFFFFFF);
  }
  
  /// FNV-1a hash function
  int _fnv1a(Uint8List data) {
    var hash = 2166136261;
    for (final byte in data) {
      hash ^= byte;
      hash = (hash * 16777619) & 0xFFFFFFFF;
    }
    return hash;
  }
  
  /// Simple Murmur3-like hash
  int _murmur3(Uint8List data) {
    var hash = 0;
    for (var i = 0; i < data.length; i++) {
      hash ^= data[i];
      hash = (hash * 0x5bd1e995) & 0xFFFFFFFF;
      hash ^= hash >> 15;
    }
    return hash;
  }
}
