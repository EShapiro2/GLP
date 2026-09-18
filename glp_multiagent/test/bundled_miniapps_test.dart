/// The mini-apps that reach the phone are named in three places, and this test
/// is the only thing that makes the three one list.
///
/// A certified mini-app reaches a sandboxed platform as an artefact: the
/// super-app's agent resolves a name only within its own directory, so
/// `tool/sync_glp_assets.sh` compiles each mini-app with `:artefact` into the
/// super-app's directory in the bundle, and `lib/glp_sources.dart` copies it
/// out of the bundle at first run.  Three lists say which mini-apps those are
/// --- the script's `for prog in` program directories, the script's `for a in`
/// assertion that each artefact was written, and the `.glpw` entries of
/// `bundledGlp` --- and a mini-app added to one and not the others is either
/// built and never shipped or shipped and never built.  That is what happened
/// to the denominated mini-app (Currencies Code, 2026-09-16): it was in none of
/// the three, the suite proved it in the repo because Section SG builds its
/// artefact straight into the repo with `:artefact`, and on the simulator there
/// was no mini-app to install.
///
/// The three files are read as text on purpose.  A test that runs the script
/// and inspects what it wrote proves the script against itself: the `for a in`
/// assertion would pass on exactly the artefacts the `for prog in` list built,
/// whatever either list says, and `bundledGlp` would not be consulted at all.
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

/// The artefacts `bundledGlp` copies out of the bundle, by mini-app name.
Set<String> _bundledArtefacts(String sources) => RegExp(r"'([^']*\.glpw)'")
    .allMatches(sources)
    .map((m) => _basename(m.group(1)!).replaceAll('.glpw', ''))
    .toSet();

void main() {
  test('the three mini-app lists that reach the phone name one set', () {
    final script = File(_script).readAsStringSync();
    final sources = File(_sources).readAsStringSync();

    final built = _artefactPrograms(script);
    final asserted = _writtenAssertions(script);
    final bundled = _bundledArtefacts(sources);

    // An empty list would make the three agree by naming nothing at all.
    expect(built, isNotEmpty, reason: '$_script builds no artefact');
    expect(asserted, isNotEmpty, reason: '$_script asserts no artefact');
    expect(bundled, isNotEmpty, reason: '$_sources bundles no artefact');

    expect(asserted, equals(built),
        reason: 'the `for a in` assertion of $_script does not name the '
            'mini-apps its `for prog in` list builds: built $built, '
            'asserted $asserted');
    expect(bundled, equals(built),
        reason: 'the `.glpw` entries of bundledGlp in $_sources do not name '
            'the mini-apps $_script builds: built $built, bundled $bundled');
  });
}
