// The values a person typed in a text field before, offered in it again, and
// what the device is told a field of a type takes (lib/ui_runtime/
// typed_before.dart; vGLP, Section 6.2, Remark "Construct Families"; vGLP
// 37fe038, Section 5.4).
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/typed_before.dart';

void main() {
  setUp(TypedBefore.clear);

  test('the values typed in a field are kept, the most recent first, each once, at most five', () {
    for (final v in ['a@x', 'b@x', 'a@x', '', 'c@x', 'd@x', 'e@x', 'f@x']) {
      TypedBefore.record('f', v);
    }
    expect(TypedBefore.of('f'), ['f@x', 'e@x', 'd@x', 'c@x', 'a@x']);
    expect(TypedBefore.of('g'), isEmpty);
  });

  test('a field of type email or name tells the device what it takes; another tells it nothing', () {
    expect(autofillHintsFor('email'), [AutofillHints.email]);
    expect(autofillHintsFor('name'), [AutofillHints.name]);
    expect(autofillHintsFor('text'), isNull);
    expect(keyboardFor('email'), TextInputType.emailAddress);
    expect(keyboardFor('number'), TextInputType.number);
  });

  testWidgets('a value typed before is offered under the field and, picked, fills it, submitting nothing', (tester) async {
    TypedBefore.record('f', 'alice@example.org');
    final c = TextEditingController();
    var picked = 0;
    await tester.pumpWidget(MaterialApp(
        home: Scaffold(
            body: typedBeforeChips('f', c,
                keyOf: (v) => ValueKey('before:$v'), onPicked: () => picked++))));
    await tester.tap(find.byKey(const ValueKey('before:alice@example.org')));
    expect(c.text, 'alice@example.org');
    expect(picked, 1);
  });
}
