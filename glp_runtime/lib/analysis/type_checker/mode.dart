// lib/analysis/type_checker/mode.dart
//
// Mode system for moded type checking.
// Modes represent data flow direction: consume (caller writes, callee reads)
// vs produce (callee writes, caller reads).

/// Data flow mode for procedure arguments, TGLP's ↓ and ↑:
/// - consume (↓): caller writes, callee reads (the `Type?` syntax)
/// - produce (↑): callee writes, caller reads (the default, no `?` marker)
enum Mode {
  produce,  // Callee writes, caller reads (default, no ? marker) = ↑
  consume;  // Caller writes, callee reads (Type? syntax) = ↓

  /// The mode inverted: produce ↔ consume (TGLP, the dual of a mode).
  /// Used at call boundaries: caller's produce = callee's consume
  Mode get flip {
    switch (this) {
      case Mode.produce:
        return Mode.consume;
      case Mode.consume:
        return Mode.produce;
    }
  }

  /// The words the diagnostics print a mode as: a path's step is written
  /// `(symbol, index, output)` or `(symbol, index, input)`
  /// (moded_term.dart, [ModedPath.toString]).
  @override
  String toString() {
    switch (this) {
      case Mode.produce:
        return 'output';
      case Mode.consume:
        return 'input';
    }
  }
}

/// Combine embedded mode with parent mode (involution property)
///
/// When a type appears inside another type's definition, the mode
/// is determined by combining the parent's mode with the embedded mode:
/// - produce ⊕ produce = produce (no inversions)
/// - produce ⊕ consume = consume (one inversion)
/// - consume ⊕ produce = consume (one inversion)
/// - consume ⊕ consume = produce (two inversions cancel)
///
/// This is XOR on the boolean representation (produce=false, consume=true).
Mode combineMode(Mode parent, Mode embedded) {
  if (parent == embedded) {
    return Mode.produce;  // Same modes cancel out
  } else {
    return Mode.consume;  // Different modes combine to consume
  }
}
