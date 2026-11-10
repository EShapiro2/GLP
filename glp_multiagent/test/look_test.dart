// The look of an app (lib/ui_runtime/look.dart): each family's colours from
// programs/apps/look.json, a contract's icon and family from its Look line,
// 'Look: <icon>, <family>' (GSC's Section 2), and the site's about.json until
// its owner writes one (GSA #2, 2026-11-09, Udi approving: "each contract's
// colour and icon come from the contract ... the app's green everywhere goes").
import 'dart:convert';
import 'dart:io';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart' show lookOfExport;
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/look.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';

import 'programs_dir.dart';

const _families = '{"families": {'
    '"social": {"from": "#34c3ae", "to": "#169482", "name": "Social"},'
    '"os": {"from": "#9aa3ad", "to": "#5f6b78", "name": "Operating System"},'
    '"other": {"from": "#b0b0b8", "to": "#7a7a85", "name": "Other"}}}';

String _contract(String name, List<String> pragmas, {String heading = 'Pragmas'}) =>
    ['% Owner: GSC, by Udi\'s ruling', '$name: A Contract', 'Alice and Bob are persons.',
     '1. ■ Something.  Alice may do it.', heading, ...pragmas].join('\n');

void main() {
  late Looks looks;
  setUpAll(() => looks = Looks.readSync('${programsDir()}/self.glp'));

  test('the repository\'s looks: every family two colours, the family other, and every contract with a Look line taken from it', () {
    expect(looks.families.keys, contains(Looks.otherFamily));
    final programs = Directory(programsDir());
    var lines = 0;
    for (final f in programs.listSync(recursive: true).whereType<File>()) {
      if (!f.path.endsWith('.txt')) continue;
      final c = Looks.contractLook(f.readAsStringSync());
      if (c == null) continue;
      lines++;
      expect(looks.of(c.name).icon, c.icon, reason: f.path);
      expect(looks.of(c.name).family, c.family, reason: f.path);
      expect(looks.families.keys, contains(c.family), reason: f.path);
    }
    expect(lines, greaterThan(0), reason: 'no contract of the repository writes a Look line');
  });

  test('a contract with no Look line takes about.json\'s icon and family', () {
    final about = jsonDecode(File('${Directory(programsDir()).parent.path}/servers/peoplesweb/web/about.json')
        .readAsStringSync()) as Map<String, dynamic>;
    final gkk = (about['GKK'] as List).cast<String>();
    expect(looks.of('gkk').icon, gkk[0]);
    expect(looks.of('GKK').family, gkk[1]);
  });

  test('the Look line opens the interface pragmas, under either heading, and wins over about.json', () {
    final about = '{"X": ["x", "social", "a line"], "Y": ["y", "social", ""]}';
    final l = Looks.parse(_families, about: about, contractTexts: [
      _contract('X', ['Look: 🏪, os', 'Things: the things of Alice.']),
      _contract('Y', ['Things: the things of Alice.', 'Look: 🏪, os'], heading: 'Interface pragmas'),
      'Z: not a contract\nPragmas\nLook: z, os',
    ]);
    expect(l.of('x').icon, '🏪');
    expect(l.of('x').family, 'os');
    expect(l.of('Y').icon, 'y', reason: 'a Look line not first is none');
    expect(l.of('Z').family, Looks.otherFamily, reason: 'a text with no % Owner: line is no contract');
    expect(Looks.contractLook(_contract('W', ['Look: w, social'], heading: 'Interface pragmas'))!.name, 'W');
  });

  test('a contract with no look, or of a family the file does not name, is of the family other', () {
    final l = Looks.parse(_families, contractTexts: [_contract('Q', ['Look: q, nowhere'])]);
    expect(l.of('NO-SUCH').family, Looks.otherFamily);
    expect(l.of('NO-SUCH').icon, Looks.otherIcon);
    expect(l.of('Q').family, Looks.otherFamily);
    expect(l.of('Q').icon, 'q');
    expect(l.of('Q').colours.to, const Color(0xFF7A7A85));
  });

  test('a file with no family other, or a colour that is not one, is refused', () {
    expect(() => Looks.parse('{"families": {}}'), throwsFormatException);
    expect(() => Looks.parse('{"families": {"other": {"from": "#12", "to": "#7a7a85", "name": "O"}}}'),
        throwsFormatException);
  });

  test('the look a platform exports, look(Icon, Family) among the pragmas of contract(Text, Schemas), read off the term, and nolook or another term giving none (vGLP b413fe1, Section 5.4)', () {
    rt.Term s(String f, List<rt.Term> args) => rt.StructTerm(f, args);
    rt.Term c(Object v) => rt.ConstTerm(v);
    final nil = c(rt.nil);
    final pragmas = s('pragmas', [nil, nil, nil, s('look', [c('📦'), c('os')]), s('version', [c(1), c(1)])]);
    expect(lookOfExport(s('with', [nil, pragmas])), (icon: '📦', family: 'os'));
    final none = s('pragmas', [nil, nil, nil, c('nolook'), s('version', [c(1), c(1)])]);
    expect(lookOfExport(s('with', [nil, none])), isNull);
    expect(lookOfExport(c('other')), isNull);
    expect(lookOfExport(s('with', [nil, nil])), isNull);
  });

  test('a contract\'s look from the icon and the family its platform exports, the colours the family\'s, a family not in the file the family other', () {
    final l = Looks.parse(_families);
    final x = l.withLook('xpd', '📦', 'os');
    expect(x.contract, 'XPD');
    expect(x.icon, '📦');
    expect(x.family, 'os');
    expect(x.colours.name, 'Operating System');
    expect(l.withLook('X', '🏪', 'nosuch').family, Looks.otherFamily);
  });

  test('the theme\'s primary colour is the family\'s dark colour, its buttons filled with it', () {
    final look = looks.of('GSG');
    final theme = look.theme(fontFamily: 'Roboto');
    expect(theme.colorScheme.primary, look.colours.to);
    expect(theme.elevatedButtonTheme.style!.backgroundColor!.resolve({}), look.colours.to);
  });

  testWidgets('a surface in a contract\'s look draws its bar in the family\'s colour, with the icon, and no green', (tester) async {
    final look = looks.of(superappContract);
    final ui = UiRuntime(manifest: superappManifest, onSend: (_) {});
    await tester.pumpWidget(MaterialApp(
        home: Theme(
            data: look.theme(),
            child: AgentSurface(agentId: 'alice', runtime: ui, icon: look.icon))));
    final bar = tester.widget<AppBar>(find.byType(AppBar));
    expect(bar.backgroundColor, look.colours.to);
    expect(find.textContaining(look.icon), findsOneWidget);
    for (final m in tester.widgetList<Material>(find.byType(Material))) {
      expect(m.color, isNot(Colors.green), reason: 'no green on the surface');
    }
  });
}
