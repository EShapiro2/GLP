// lib/analysis/type_checker/moded_label.dart
//
// Moded transition labels for type automata.
// Corresponds to paper Definition 7.5 (Moded Transition Label).

import 'mode.dart';

/// A moded transition label: pair (π, m) where π is a path element
/// and m ∈ {output, input, ⊥} is a mode annotation.
///
/// Mode is non-null only for TypeRef positions (T or T?).
/// For PrimitiveModeAlt positions (_ or _?), mode is null (tracked via primitiveStateModes).
class ModedLabel {
  final String pathElement;  // e.g., "ch(2,1)", "[|](2,1)", "[]", "0"
  final Mode? mode;          // null represents ⊥ (unmoded)

  const ModedLabel(this.pathElement, {this.mode});

  /// Create label for functor argument position
  factory ModedLabel.functor(String name, int arity, int argIndex, {Mode? mode}) {
    return ModedLabel('$name($arity,$argIndex)', mode: mode);
  }

  /// Create label for list head position [|](2,1)
  factory ModedLabel.listHead({Mode? mode}) => ModedLabel('[|](2,1)', mode: mode);

  /// Create label for list tail position [|](2,2)
  factory ModedLabel.listTail({Mode? mode}) => ModedLabel('[|](2,2)', mode: mode);

  /// Create label for empty list constant
  factory ModedLabel.nil() => const ModedLabel('[]');

  /// Create label for constant value
  factory ModedLabel.constant(Object value) => ModedLabel(value.toString());

  /// Create label from structural symbol with optional mode
  factory ModedLabel.fromSymbol(String symbol, {Mode? mode}) =>
      ModedLabel(symbol, mode: mode);

  /// The structural symbol without mode (for matching)
  String get symbol => pathElement;

  @override
  bool operator ==(Object other) =>
      other is ModedLabel &&
      pathElement == other.pathElement &&
      mode == other.mode;

  @override
  int get hashCode => Object.hash(pathElement, mode);

  @override
  String toString() {
    if (mode == null) return pathElement;
    return '$pathElement${mode == Mode.consume ? "?" : ""}';
  }
}
