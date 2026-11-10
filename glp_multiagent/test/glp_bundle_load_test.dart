/// The phone loads the social graph from its bundle, and the program it loads
/// is the one the repository's is: the same source identity h(M).
///
/// A sandboxed build reads only its bundle (glp_sources_platform_test.dart),
/// which tool/sync_glp_assets.sh stages from the trees the app loads, the
/// root self.glp among them.  Until 2026-11-08 the root self.glp exposed
/// sglp#monitor and sglp#sglp, and the bundle stages nothing of
/// programs/sglp, so the program the phone loads, programs/social/graph/core,
/// could not be compiled from its bundle at all (GLP #13, Code_inbox
/// 2026-11-07 21:07 and 21:19 UTC, Udi approving: the two exposes leave the
/// root, and a test loads the phone bundle as the phone does).
///
/// So this test lays the bundle out as the app does on the phone ---
/// resolveGlpPaths(sandboxed: true), which copies every file the bundle's
/// manifest names into the Documents directory (_fromBundle) --- and loads
/// GlpPaths.coreDir as AgentRuntime._setUp does: GlpEngine with the person's
/// identity, madGLP enabled, loadProgram of the directory.  The h(M) it
/// computes must equal the one glp_runtime/bin/source_identity.dart prints for
/// ../programs/social/graph/core, the repository's: the bundle is the
/// repository's program, not another.
///
/// `getApplicationDocumentsDirectory()` is answered with a fresh temporary
/// directory, as in glp_sources_platform_test.dart.
library;

import 'dart:io';

import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/glp_sources.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/body_kernels.dart' show bytesToHex;
import 'package:glp_runtime/wire/artefact.dart' show Artefact;

const _pathProvider = MethodChannel('plugins.flutter.io/path_provider');

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late Directory docs;

  setUp(() {
    docs = Directory.systemTemp.createTempSync('glp_bundle_docs_');
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

  test('the bundle holds nothing of sGLP', () async {
    final bundled = await bundledGlp();
    expect(bundled.keys.where((p) => p.startsWith('programs/sglp/')), isEmpty);
    expect(bundled.keys.where((p) => p.endsWith('_sglp.glp')), isEmpty);
  });

  test('the phone loads programs/social/graph/core from its bundle, at the '
      "repository's h(M)", () async {
    final glp = await resolveGlpPaths(sandboxed: true);
    expect(glp.coreDir, startsWith('${docs.path}/'));

    // As AgentRuntime._setUp makes the agent's engine and loads its program.
    final engine = GlpEngine(rootSelfGlpPath: glp.rootSelfGlp);
    engine.enableMadGLP(agentId: 'bundle_load_test');
    expect(engine.loadProgram(glp.coreDir), isTrue,
        reason: 'the bundle\'s core did not load');
    final artefact = engine.appModule?.artefact;
    expect(artefact, isA<Artefact>(),
        reason: 'the bundle\'s core loaded with no module value');
    final bundleHM = bytesToHex((artefact as Artefact).hM);

    // The repository's h(M), as source_identity prints it.
    final run = await Process.run(
        'dart', ['run', 'bin/source_identity.dart', '../programs/social/graph/core'],
        workingDirectory: '../glp_runtime');
    expect(run.exitCode, 0, reason: '${run.stderr}');
    final repoHM = (run.stdout as String).trim().split('\n').last.trim();
    expect(repoHM, matches(RegExp(r'^[0-9a-f]{64}$')));
    expect(bundleHM, repoHM);
  }, timeout: const Timeout(Duration(minutes: 5)));
}
