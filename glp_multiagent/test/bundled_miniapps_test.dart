/// The mini-apps that reach the phone are named in two places, both in
/// `tool/sync_glp_assets.sh`, and this test is the only thing that makes the
/// two one list.
///
/// A certified mini-app reaches a sandboxed platform as an artefact: the
/// super-app's agent resolves a name only within its own directory, so
/// `tool/sync_glp_assets.sh` compiles each mini-app with `:artefact` into the
/// super-app's directory in the bundle, and `lib/glp_sources.dart` copies it
/// out of the bundle at first run.  Two lists say which mini-apps those are
/// --- the script's `for prog in` program directories and the script's
/// `for a in` assertion that each artefact was written --- and a mini-app
/// added to one and not the other is either built and never checked or
/// checked and never built.  That is what happened to the denominated mini-app
/// (Currencies Code, 2026-09-16): it was in none of the lists, the suite
/// proved it in the repo because Section SG builds its artefact straight into
/// the repo with `:artefact`, and on the simulator there was no mini-app to
/// install.
///
/// Until 2026-10-07 there was a third list, the `.glpw` entries of
/// `bundledGlp` in `lib/glp_sources.dart`, which named what the loader copied.
/// The loader now copies what the bundle's manifest names, which the script
/// generates from what it built (GLP, 2026-10-04 13:21 UTC: "the two bundle
/// lists go"), so it keeps no list of artefacts, and this test holds it to
/// keeping none.
///
/// The files are read as text on purpose.  A test that runs the script and
/// inspects what it wrote proves the script against itself: the `for a in`
/// assertion would pass on exactly the artefacts the `for prog in` list built,
/// whatever either list says.
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';

/// `flutter test` runs with `glp_multiagent/` as its working directory.
const _script = 'tool/sync_glp_assets.sh';
const _sources = 'lib/glp_sources.dart';

String _basename(String path) => path.split('/').last;

/// The program directories the `:artefact` block compiles, by mini-app name.
/// The list is one shell word per line with `\` continuations, ending `; do`.
Set<String> _artefactPrograms(String script) {
  final m = RegExp(r'for prog in\s+(.*?);\s*do', dotAll: true).firstMatch(script);
  expect(m, isNotNull, reason: 'no `for prog in ...; do` list in $_script');
  return m!
      .group(1)!
      .replaceAll('\\\n', ' ')
      .split(RegExp(r'\s+'))
      .where((w) => w.startsWith('../programs/'))
      .map(_basename)
      .toSet();
}

/// The names the script asserts an artefact was written for.
Set<String> _writtenAssertions(String script) {
  final m = RegExp(r'for a in\s+([^;]*);\s*do').firstMatch(script);
  expect(m, isNotNull, reason: 'no `for a in ...; do` assertion list in $_script');
  return m!.group(1)!.trim().split(RegExp(r'\s+')).where((w) => w.isNotEmpty).toSet();
}

/// The artefacts `lib/glp_sources.dart` names, by mini-app name: none, since
/// the loader copies what the bundle's manifest names.
Set<String> _namedArtefacts(String sources) => RegExp(r"'([^']*\.glpw)'")
    .allMatches(sources)
    .map((m) => _basename(m.group(1)!).replaceAll('.glpw', ''))
    .toSet();

void main() {
  test('the mini-app lists that reach the phone name one set, and the loader '
      'keeps none', () {
    final script = File(_script).readAsStringSync();
    final sources = File(_sources).readAsStringSync();

    final built = _artefactPrograms(script);
    final asserted = _writtenAssertions(script);
    final named = _namedArtefacts(sources);

    // An empty list would make the two agree by naming nothing at all.
    expect(built, isNotEmpty, reason: '$_script builds no artefact');
    expect(asserted, isNotEmpty, reason: '$_script asserts no artefact');

    expect(asserted, equals(built),
        reason: 'the `for a in` assertion of $_script does not name the '
            'mini-apps its `for prog in` list builds: built $built, '
            'asserted $asserted');
    expect(named, isEmpty,
        reason: '$_sources names artefacts, $named: the loader copies what '
            'the bundle\'s manifest names, and a list of its own drifts');
  });
}
