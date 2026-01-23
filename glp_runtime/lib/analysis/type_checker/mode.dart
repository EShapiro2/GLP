// lib/analysis/type_checker/mode.dart
//
// Mode system for moded type checking.
// Modes represent data flow direction: input (caller writes, callee reads)
// vs output (callee writes, caller reads).

/// Data flow mode for procedure arguments
///
/// Two naming conventions are supported:
/// - input/output: Traditional GLP naming
/// - consume/produce: Paper Definition 4.2 naming (↓/↑)
///
/// Mapping:
/// - consume (↓) = input = caller writes, callee reads
/// - produce (↑) = output = callee writes, caller reads
enum Mode {
  output,  // Callee writes, caller reads (default, no ? marker) = produce (↑)
  input;   // Caller writes, callee reads (Type? syntax) = consume (↓)

  /// Alias for input mode (↓) per paper Definition 4.2
  static const consume = Mode.input;

  /// Alias for output mode (↑) per paper Definition 4.2
  static const produce = Mode.output;

  /// Mode dual operator per Definition 5.1
  /// Inverts mode: output ↔ input, produce ↔ consume
  /// Used at call boundaries: caller's output = callee's input
  Mode get dual {
    switch (this) {
      case Mode.output:
        return Mode.input;
      case Mode.input:
        return Mode.output;
    }
  }

  /// Alias for dual (per paper notation)
  Mode get flip => dual;

  @override
  String toString() {
    switch (this) {
      case Mode.output:
        return 'output';
      case Mode.input:
        return 'input';
    }
  }
}

/// Combine embedded mode with parent mode (involution property)
///
/// When a type appears inside another type's definition, the mode
/// is determined by combining the parent's mode with the embedded mode:
/// - output ⊕ output = output (no inversions)
/// - output ⊕ input  = input  (one inversion)
/// - input  ⊕ output = input  (one inversion)
/// - input  ⊕ input  = output (two inversions cancel)
///
/// This is XOR on the boolean representation (output=false, input=true).
Mode combineMode(Mode parent, Mode embedded) {
  if (parent == embedded) {
    return Mode.output;  // Same modes cancel out
  } else {
    return Mode.input;   // Different modes combine to input
  }
}
