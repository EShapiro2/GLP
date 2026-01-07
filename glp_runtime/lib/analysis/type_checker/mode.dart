// lib/analysis/type_checker/mode.dart
//
// Mode system for moded type checking.
// Specification: docs/modules/moded-term.md
// Paper Reference: Definition 4.2

/// Data flow mode annotation
/// - consume (↓): program reads/consumes value at this position
/// - produce (↑): program writes/produces value at this position
enum Mode {
  consume,  // ↓ - program consumes value
  produce;  // ↑ - program produces value

  /// Mode flip operation (involution)
  /// consume.flip = produce
  /// produce.flip = consume
  /// (T?)? = T
  Mode get flip => this == consume ? produce : consume;

  @override
  String toString() => this == consume ? '↓' : '↑';
}

/// Combine embedded mode with parent mode
/// Uses XOR semantics (involution property):
/// - produce ⊕ produce = produce (no flip)
/// - produce ⊕ consume = consume (one flip)
/// - consume ⊕ produce = consume (one flip)
/// - consume ⊕ consume = produce (two flips cancel)
Mode combineMode(Mode parent, Mode embedded) {
  if (parent == embedded) {
    return Mode.produce;
  } else {
    return Mode.consume;
  }
}
