// A refusal that names no atom, as one for a condition on an act's values (Library's Lend with a day not later
// than today), says so, and is not left with no reason (GLP #179).
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

void main() {
  const alice = GAtom('alice'), bob = GAtom('bob');
  test('a refusal for a condition on values says so', () {
    final n = SocialNotice.of(const GStruct('refused', [
      GStruct('lend', [alice, bob, GAtom('tome'), GInt(3)]),
      alice,
      GStruct('unable', [GList([]), GList([])]),
    ]))!;
    expect(n.sentence(alice), endsWith(': a condition on its values does not hold.'));
  });
  test('a refusal that names atoms says them, as before', () {
    final n = SocialNotice.of(const GStruct('refused', [
      GStruct('lend', [alice, bob, GAtom('tome'), GInt(3)]),
      alice,
      GStruct('unable', [GList([GStruct('knows', [bob])]), GList([])]),
    ]))!;
    expect(n.sentence(alice), isNot(contains('a condition on its values')));
  });
}
