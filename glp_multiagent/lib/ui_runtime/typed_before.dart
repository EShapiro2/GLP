/// The values a person typed in each text field before, offered in it again,
/// and what the device is told a field takes (vGLP, Section 6.2, Remark
/// "Construct Families": "A family may offer in a text field the values its
/// person typed in it before, and may tell the device what a field takes, by
/// the name of its type, as an email address or a person's name, so that the
/// device fills it; neither changes the answer a submission forms"; Section
/// 5.4: a text field is typed "by the type its variable carries in the schema
/// where it carries one", vGLP 37fe038).
///
/// A field is known by a key its widget gives it, the same each time the
/// field is drawn again; the values are kept for the app's session, the most
/// recent first, at most [kept] of them.  Offering one fills the field and
/// submits nothing: the person still submits the form.
library;

import 'package:flutter/material.dart';

class TypedBefore {
  TypedBefore._();

  /// The most values kept for a field.
  static const int kept = 5;

  static final Map<String, List<String>> _values = {};

  /// Record [value], typed in the field [field] and submitted.
  static void record(String field, String value) {
    final v = value.trim();
    if (v.isEmpty) return;
    final l = _values.putIfAbsent(field, () => <String>[]);
    l.remove(v);
    l.insert(0, v);
    if (l.length > kept) l.removeRange(kept, l.length);
  }

  /// The values typed in [field] before, the most recent first.
  static List<String> of(String field) =>
      List.unmodifiable(_values[field] ?? const <String>[]);

  /// Forget every value, as a test starts afresh.
  static void clear() => _values.clear();
}

/// The hints the device is given for a field of the type [type]: an email
/// address or a person's name, which the device can fill; none for another.
List<String>? autofillHintsFor(String type) => switch (type) {
      'email' => const [AutofillHints.email],
      'name' => const [AutofillHints.name],
      _ => null,
    };

/// The keyboard of a field of the type [type].
TextInputType keyboardFor(String type) => switch (type) {
      'number' => TextInputType.number,
      'email' => TextInputType.emailAddress,
      'name' => TextInputType.name,
      _ => TextInputType.text,
    };

/// The values typed in [field] before, each a chip that puts it into
/// [controller]; nothing where there are none.  [keyOf] keys each chip.
Widget typedBeforeChips(String field, TextEditingController controller,
    {required Key Function(String value) keyOf, VoidCallback? onPicked}) {
  final vs = TypedBefore.of(field);
  if (vs.isEmpty) return const SizedBox.shrink();
  return Wrap(spacing: 6, runSpacing: 4, children: [
    for (final v in vs)
      ActionChip(
        key: keyOf(v),
        label: Text(v),
        onPressed: () {
          controller.text = v;
          onPicked?.call();
        },
      ),
  ]);
}
