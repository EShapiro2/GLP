/// A UI structure's landmarks and the order of its lists, as the app reads them from a compiled description
/// (`programs/gce/ui/App.ui.json`, the Grassroots App's own screens, whose More is a landmark): every screen
/// the person can reach includes each landmark from the start, and a list the UI style orders is drawn in its
/// order (lib/ui_runtime/described.dart).
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/described.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

void main() {
  final app = UiDescription.parse(File('../programs/gce/ui/App.ui.json').readAsStringSync());

  test('App: More is a landmark, reached from the start with no list', () {
    expect(app.start, 'Home');
    expect(app.landmarks, ['More']);
    final core = DescribedCore(app, const SocialState(GAtom('alice'), [], [], []), null);
    expect(core.screens().map((x) => x.$1), containsAll(['Home', 'More']));
  });

  test('a list in its order: People alphabetical; none as held; newest first reversed', () {
    final core = DescribedCore(app, const SocialState(GAtom('alice'), [], [], []), null);
    final people = app.screens['Home']!.components.firstWhere((c) => c.kind == 'list' && c.j['title'] == 'People');
    expect(people.j['order'], 'alpha');
    const xs = [GAtom('carol'), GAtom('bob'), GAtom('dave')];
    expect(core.ordered(people, xs).map(formatTerm), ['bob', 'carol', 'dave']);
    expect(core.ordered(UiComponent('list', {'order': 'newest'}), xs).map(formatTerm), ['dave', 'bob', 'carol']);
    expect(core.ordered(UiComponent('list', {}), xs).map(formatTerm), ['carol', 'bob', 'dave']);
  });
}
