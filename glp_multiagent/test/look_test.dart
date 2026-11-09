// The look of an app (lib/ui_runtime/look.dart): each contract's icon and
// family and each family's colours, from programs/apps/look.json (GSA #2,
// 2026-11-09, Udi approving: "each contract's colour and icon come from the
// contract ... the app's green everywhere goes").
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/look.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';

import 'programs_dir.dart';

void main() {
  late Looks looks;
  setUpAll(() => looks = Looks.readSync('${programsDir()}/self.glp'));

  test('the repository\'s look.json gives every contract it names a family it names, and the family other', () {
    expect(looks.families.keys, contains(Looks.otherFamily));
    for (final e in looks.contracts.entries) {
      expect(looks.families.keys, contains(e.value.family), reason: e.key);
      expect(e.value.icon, isNotEmpty, reason: e.key);
    }
    expect(looks.contracts.keys, containsAll(['GKK', 'GSG', 'GSN', 'GC', 'GAS']));
  });

  test('a contract\'s look is its icon and its family\'s colours, by its name in any case', () {
    final gc = looks.of('gc');
    expect(gc.contract, 'GC');
    expect(gc.family, looks.contracts['GC']!.family);
    expect(gc.colours, same(looks.families[gc.family]));
    expect(looks.of('GKK').icon, looks.contracts['GKK']!.icon);
  });

  test('a contract the file does not name, or of a family it does not name, is of the family other', () {
    final none = looks.of('NO-SUCH-CONTRACT');
    expect(none.family, Looks.otherFamily);
    expect(none.icon, Looks.otherIcon);
    final odd = Looks.parse('{"families": {"other": {"name": "Other", "colours": ["#b0b0b8", "#7a7a85"]}},'
        ' "contracts": {"X": {"icon": "x", "family": "nowhere"}}}');
    expect(odd.of('X').family, Looks.otherFamily);
    expect(odd.of('X').icon, 'x');
    expect(odd.of('X').colours.dark, const Color(0xFF7A7A85));
  });

  test('a file with no family other, or a colour that is not one, is refused', () {
    expect(() => Looks.parse('{"families": {}, "contracts": {}}'), throwsFormatException);
    expect(() => Looks.parse('{"families": {"other": {"name": "O", "colours": ["#12", "#7a7a85"]}}, "contracts": {}}'),
        throwsFormatException);
  });

  test('the theme\'s primary colour is the family\'s dark colour, its buttons filled with it', () {
    final look = looks.of('GSG');
    final theme = look.theme(fontFamily: 'Roboto');
    expect(theme.colorScheme.primary, look.colours.dark);
    final style = theme.elevatedButtonTheme.style!;
    expect(style.backgroundColor!.resolve({}), look.colours.dark);
  });

  testWidgets('a surface in a contract\'s look draws its bar in the family\'s colour, with the icon, and no green', (tester) async {
    final look = looks.of(superappContract);
    final ui = UiRuntime(manifest: superappManifest, onSend: (_) {});
    await tester.pumpWidget(MaterialApp(
        home: Theme(
            data: look.theme(),
            child: AgentSurface(agentId: 'alice', runtime: ui, icon: look.icon))));
    final bar = tester.widget<AppBar>(find.byType(AppBar));
    expect(bar.backgroundColor, look.colours.dark);
    expect(find.textContaining(look.icon), findsOneWidget);
    for (final m in tester.widgetList<Material>(find.byType(Material))) {
      expect(m.color, isNot(Colors.green), reason: 'no green on the surface');
    }
  });
}
