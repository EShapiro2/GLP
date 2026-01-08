// lib/analysis/type_checker/moded_label.dart
//
// Moded transition labels for type DFA traversal.
// Specification: docs/modules/well-typed-term.md v0.3
//
// A moded label combines a path element (structural position) with a mode
// annotation (consume/produce) for DFA transition matching.

import 'mode.dart';
import 'type_dfa.dart';

/// A transition label with mode annotation.
///
/// Used for matching moded term paths against type DFA transitions.
/// The label combines:
/// - pathElement: structural position (functor/arity/argIndex or constant)
/// - mode: data flow direction (consume ↓ or produce ↑)
class ModedLabel {
  /// The underlying path element (structural information)
  final String pathElement;

  /// The mode annotation (null for unmoded labels)
  final Mode? mode;

  ModedLabel(this.pathElement, {this.mode});

  /// Create moded label for functor argument position
  ///
  /// Format: "functor(arity,argIndex)"
  factory ModedLabel.functor(String name, int arity, int argIndex,
      {Mode? mode}) {
    return ModedLabel('$name($arity,$argIndex)', mode: mode);
  }

  /// Create moded label for constant
  factory ModedLabel.constant(Object value, {Mode? mode}) {
    return ModedLabel(value.toString(), mode: mode);
  }

  /// Create moded label for list cons head [H|T] position 1
  factory ModedLabel.listHead({Mode? mode}) {
    return ModedLabel('[|](2,1)', mode: mode);
  }

  /// Create moded label for list cons tail [H|T] position 2
  factory ModedLabel.listTail({Mode? mode}) {
    return ModedLabel('[|](2,2)', mode: mode);
  }

  /// Create moded label for empty list []
  factory ModedLabel.nil({Mode? mode}) {
    return ModedLabel('[]', mode: mode);
  }

  /// Convert to PathElement (drops mode information)
  PathElement toPathElement() => PathElement(pathElement);

  @override
  String toString() {
    if (mode == null) return pathElement;
    final modeStr = mode == Mode.consume ? '↓' : '↑';
    return '$pathElement:$modeStr';
  }

  @override
  bool operator ==(Object other) =>
      other is ModedLabel &&
      pathElement == other.pathElement &&
      mode == other.mode;

  @override
  int get hashCode => Object.hash(pathElement, mode);
}
