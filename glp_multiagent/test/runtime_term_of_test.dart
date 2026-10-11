// The runtime term of a ground term from the screen (isolate_protocol.dart,
// runtimeTermOf).  GLP-Spec appendix-lp.tex, Definition "Logic Programs
// Syntax": one kind of constant besides numbers and [], the string, so a
// screen string and a screen atom of the same text are one value (GLP #3
// Cowork, 2026-10-04 15:19 UTC, FAULT 2 (b)); and [] is the runtime's empty
// list, no string (FAULT 2 (a)).  Until 2026-10-10 a screen string was the
// constant of its text in double quotes, and until 2026-10-07 a list ended in
// the string 'nil'.

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

void main() {
  test('a screen string is the string of its text, an atom of it the same',
      () {
    final s = runtimeTermOf(const GString('a b')) as rt.ConstTerm;
    final a = runtimeTermOf(const GAtom('a b')) as rt.ConstTerm;
    expect(s.value, 'a b');
    expect(s.value, a.value);
  });

  test('a screen list ends in the empty list, not the string nil', () {
    final l = runtimeTermOf(const GList([GAtom('x')])) as rt.StructTerm;
    expect(l.functor, '.');
    expect((l.args[1] as rt.ConstTerm).value, same(rt.nil));
    final empty = runtimeTermOf(const GList([])) as rt.ConstTerm;
    expect(empty.value, same(rt.nil));
    expect(empty.value == 'nil', isFalse);
  });
}
