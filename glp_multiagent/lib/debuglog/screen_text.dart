/// The text of what the bridge draws: each construct on the person's screen,
/// by its identifier, with the texts it shows in order, as the screenshot
/// harness reads a construct's texts (test/apps_screens_test.dart, `textsOf`:
/// the [Text] widgets under the construct's key, [constructKey]); a field
/// shows the text typed into it.  Texts outside every construct are under
/// the identifier null (GLP/phones/docs/debugging.md, "The phone side", item
/// 3, `screen`).
library;

import 'package:flutter/widgets.dart';

import '../ui_runtime/construct_family.dart' show constructKey;

/// One construct's texts on the screen.
class ConstructText {
  /// The construct's identifier, or null for the texts outside every
  /// construct.
  final int? id;

  /// Its texts, in the order they are drawn.
  final List<String> texts;

  ConstructText(this.id, this.texts);

  Map<String, Object?> toJson() => {'id': id, 'texts': texts};
}

/// The construct whose key is [key], or null.
int? _constructOf(Key? key) {
  if (key is! ValueKey<String>) return null;
  const prefix = 'construct:';
  final v = key.value;
  if (!v.startsWith(prefix)) return null;
  final id = int.tryParse(v.substring(prefix.length));
  // The key is constructKey's, and no other key of this form.
  return id != null && constructKey(id) == key ? id : null;
}

/// The texts under [root], construct by construct, in the order they are
/// drawn.
List<ConstructText> screenText(Element root) {
  final byId = <int?, List<String>>{};
  void visit(Element e, int? construct) {
    final w = e.widget;
    final here = _constructOf(w.key) ?? construct;
    if (w is Text) {
      final t = w.data ?? w.textSpan?.toPlainText();
      if (t != null) byId.putIfAbsent(here, () => []).add(t);
    } else if (w is EditableText) {
      byId.putIfAbsent(here, () => []).add(w.controller.text);
    }
    e.visitChildElements((c) => visit(c, here));
  }

  visit(root, null);
  return [for (final e in byId.entries) ConstructText(e.key, e.value)];
}
