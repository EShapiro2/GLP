// A refusal that names no atom, as one for a condition on an act's values (Library's Lend with a day not later
// than today), says so, and is not left with no reason (GLP #179).
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

void main() {
  const alice = GAtom('alice'), bob = GAtom('bob');
  // A reason naming neither an atom nor a conjunct gives no cause (VEUI 42f53d1, Section 6.2; Audit PR 181).
  for (final d in const [
    GAtom('unable'),
    GStruct('unable', [GList([]), GList([])]),
    GStruct('unable', [GList([]), GList([]), GList([])]),
  ]) {
    test('a refusal naming no atom and no conjunct says only who refused: ${formatTerm(d)}', () {
      final n = SocialNotice.of(GStruct('refused', [
        const GStruct('lend', [alice, bob, GAtom('tome'), GInt(3)]),
        alice,
        d,
      ]))!;
      expect(n.sentence(alice), matches(RegExp(r'^Your machine refused [^:]*\.$')));
    });
  }
  test('a refusal that names atoms says them, as before', () {
    final n = SocialNotice.of(const GStruct('refused', [
      GStruct('lend', [alice, bob, GAtom('tome'), GInt(3)]),
      alice,
      GStruct('unable', [GList([GStruct('knows', [bob])]), GList([])]),
    ]))!;
    expect(n.sentence(alice), isNot(contains('a condition on its values')));
  });
  test('a refusal with unmet conjuncts says each in the contract\'s words (GLP #179, step 2)', () {
    final n = SocialNotice.of(const GStruct('refused', [
      GStruct('lend', [alice, bob, GAtom('tome'), GInt(3)]),
      alice,
      GStruct('unable', [GList([]), GList([]), GList([GString("gt(num('Day'), num('Today'))")])]),
    ]))!;
    expect(n.sentence(alice),
        endsWith(': a condition on its values does not hold: Day is more than Today.'));
  });
  test('conjuncts in the vocabulary: le, and, not, plus, val', () {
    expect(SocialNotice.conjunctWords("le(num(d), num(t))"), 'd is at most t');
    expect(SocialNotice.conjunctWords("and(ge(num('A'), num(1)), not(eq(val('B'), num('C'))))"),
        'A is at least 1 and not B is C');
    expect(SocialNotice.conjunctWords("eq(num('X'), plus(num('Y'), num(2)))"), 'X is Y plus 2');
    expect(SocialNotice.conjunctWords('no term ('), 'no term (');
  });
}
