/// `resolveGlpPaths()` must choose its source tree by the platform, and this
/// test is what holds it to that.
///
/// The loader used to take the first of three branches whose directory
/// existed: the repo beside the package, then the hard-coded clone under
/// `/Users/udi/Grassroots/GLP/programs`, then the bundle.  An iOS Simulator
/// build runs on the Mac's own filesystem, so the second directory is there
/// and the sandboxed app read the repo and never opened its bundle.  What it
/// then ran was whichever artefacts a suite run had last left in the clone ---
/// `denominated.glpw` not among them --- and the super-app's `load_file/2`
/// aborted, with the home agent dead after it (Currencies, 2026-09-17).
///
/// So the branch is asserted, not the desktop behaviour: a test that only
/// checks that a desktop host reads the repo passes on the defect.  The
/// platform is injected because that is the one thing a host running
/// `flutter test` cannot be --- a phone --- and the sandboxed branch is the
/// branch that was never taken.
///
/// `getApplicationDocumentsDirectory()` reaches the platform through the
/// `path_provider` method channel, which no plugin answers under
/// `flutter test`; the mock handler below answers it with a fresh temporary
/// directory, which stands in for the app's Documents directory.
library;

import 'dart:io';

import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/glp_sources.dart';

const _pathProvider = MethodChannel('plugins.flutter.io/path_provider');

/// The artefact whose absence killed the simulator run.
const _denominated = 'programs/social/graph/core/denominated.glpw';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late Directory docs;

  setUp(() {
    docs = Directory.systemTemp.createTempSync('glp_docs_');
    TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger
        .setMockMethodCallHandler(
      _pathProvider,
      (call) async =>
          call.method == 'getApplicationDocumentsDirectory' ? docs.path : null,
    );
  });

  tearDown(() {
    TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger
        .setMockMethodCallHandler(_pathProvider, null);
    if (docs.existsSync()) docs.deleteSync(recursive: true);
  });

  test('a sandboxed build reads its bundle, never a desktop path', () async {
    final glp = await resolveGlpPaths(sandboxed: true);

    // Every path the loader hands the engine, the derived ones included: the
    // super-app's own directory is where a mini-app artefact is read from, and
    // the currency's directory is derived from the root self.glp.
    final returned = <String, String>{
      'grassappDir': glp.grassappDir,
      'graphDir': glp.graphDir,
      'cssnDir': glp.cssnDir,
      'rootSelfGlp': glp.rootSelfGlp,
      'coreDir': glp.coreDir,
      'pingappDir': glp.pingappDir,
      'coinsDir': glp.coinsDir,
    };
    returned.forEach((name, p) {
      expect(p, startsWith('${docs.path}/'),
          reason: '$name is not under the documents directory: $p');
      expect(p.contains('/Users/udi/'), isFalse,
          reason: '$name reaches the developer\'s filesystem: $p');
      expect(p.contains('../programs'), isFalse,
          reason: '$name reaches the repo beside the package: $p');
    });

    // The bundle is the source, so everything it carries must have landed.
    expect(bundledGlp, isNotEmpty, reason: 'the bundled list names nothing');
    expect(bundledGlp, contains(_denominated),
        reason: 'the denominated mini-app is not in the bundled list');
    for (final a in bundledGlp) {
      final f = File('${docs.path}/glp/$a');
      expect(f.existsSync(), isTrue,
          reason: '$a was not written under the documents directory');
      expect(f.lengthSync(), greaterThan(0), reason: '$a landed empty');
    }
  });

  test('a desktop build still reads the repo beside the package', () async {
    final glp = await resolveGlpPaths(sandboxed: false);

    expect(glp.rootSelfGlp, endsWith('/programs/self.glp'));
    expect(glp.rootSelfGlp.startsWith(docs.path), isFalse,
        reason: 'the desktop branch fell through to the bundle: '
            '${glp.rootSelfGlp}');
    expect(File(glp.rootSelfGlp).existsSync(), isTrue,
        reason: 'the desktop branch names no root self.glp on disc: '
            '${glp.rootSelfGlp}');
  });
}
