/// One loader for the GLP scenario sources, working on both desktop and
/// sandboxed (iOS) platforms.
///
/// The GLP engine reads `.glp` by real filesystem path — the root `self.glp`
/// directly, ancestor `self.glp` files by walking parent directories, and the
/// `lib/` modules the root `self.glp` exposes. On a desktop dev checkout it
/// reads the repo under `programs/`. iOS is sandboxed and cannot reach the
/// repo, so the same files are bundled as assets and copied, once, into the
/// app's Documents directory (a real path) preserving the `programs/.../`
/// tree — and the engine is pointed there. Same files, one loader, no forked
/// program.
///
/// Which of the two the loader takes is decided by the platform and by nothing
/// else. It used to be decided by which directory happened to exist, and an
/// iOS Simulator build runs on the Mac's own filesystem: the repo path is
/// there, so the simulator read the repo and never opened its bundle. What it
/// then ran was whichever artefacts a suite run had last left in the clone,
/// which did not include the denominated mini-app, and the super-app's
/// `load_file/2` aborted with the home agent dead after it (Currencies,
/// 2026-09-17). A filesystem test cannot tell a sandboxed build from a desktop
/// one, so it is not asked: on iOS and Android the bundle branch is taken
/// outright, and neither desktop path is consulted.
library;

import 'dart:io';

import 'package:flutter/foundation.dart' show visibleForTesting;
import 'package:flutter/services.dart' show rootBundle;
import 'package:path_provider/path_provider.dart';

/// Resolved locations of the GLP source tree.
class GlpPaths {
  final String grassappDir; // .../programs/grassapp
  final String graphDir; // .../programs/social/graph
  final String cssnDir; // .../programs/cssn
  final String rootSelfGlp; // .../programs/self.glp
  const GlpPaths(
      this.grassappDir, this.graphDir, this.cssnDir, this.rootSelfGlp);

  /// The Grassroots Super-App's program, and the mini-app it installs. A
  /// mini-app's certified artefact is read by `load_file/2` from the super-app's
  /// own directory, so `.glpw` files belong in [coreDir].
  String get coreDir => '$graphDir/core';
  String get pingappDir => '$graphDir/pingapp';

  /// The currency's program (Currencies): programs/currencies/coins, which holds the
  /// certified mini-app currency/ and the harness that runs it for a live
  /// person. Derived from the root self.glp so it cannot drift from the tree
  /// the engine is actually reading.
  String get coinsDir => '${File(rootSelfGlp).parent.path}/currencies/coins';

  /// The sovereign currency's program (Currencies): programs/currencies/sovereign,
  /// which holds the certified mini-app denominated/ — the compiled
  /// denominated bond agent with its mediator — and, beside it, the plays that
  /// run it. Derived from the root self.glp, as [coinsDir] is.
  String get sovereignDir =>
      '${File(rootSelfGlp).parent.path}/currencies/sovereign';
}

/// The bundle's manifest, generated with the bundle by tool/sync_glp_assets.sh
/// from the repository: one line per file, its name under `assets/glp/bundle/`
/// and its path in the tree the engine reads, a tab between.  The bundle holds
/// every `.glp` of the trees the app loads --- the directories [GlpPaths]
/// names --- read from the repository as discovery reads a program (TGLP
/// modules.tex, Compilation, first step), and the certified mini-apps'
/// artefacts; no list of them is kept here.  Until 2026-10-07 one was, kept
/// by hand beside the script's, and the two lacked what the trees had gained
/// since: `programs/social/graph/ui/self.glp` (GSG, gap abe6081f), among
/// others.
const _manifest = 'assets/glp/manifest.txt';

/// The files the bundle holds, by their paths in the tree the engine reads
/// (`programs/...`, relative to `glp/` in the Documents directory), each with
/// its asset key: read from the bundle's manifest ([_manifest]).
///
/// Public so that the sandboxed-branch test asserts over what the loader
/// actually copies rather than over a transcription of it.
@visibleForTesting
Future<Map<String, String>> bundledGlp() async {
  final manifest = await rootBundle.loadString(_manifest, cache: false);
  final files = <String, String>{};
  for (final line in manifest.split('\n')) {
    final tab = line.indexOf('\t');
    if (tab < 0) continue;
    files[line.substring(tab + 1)] =
        'assets/glp/bundle/${line.substring(0, tab)}';
  }
  return files;
}

/// The platforms whose filesystem is sandboxed away from the repo, so that the
/// bundle is the only source there. It holds for the simulator as much as for
/// the device — the simulator's app is the sandboxed build, whatever the host
/// it runs on — and any later sandboxed platform is added here.
bool get _platformIsSandboxed => Platform.isIOS || Platform.isAndroid;

/// Where the engine is to read the GLP sources.
///
/// [sandboxed] overrides the platform test, so that a test running on a
/// desktop host can exercise the branch the phone takes; omitted, the real
/// platform decides.
Future<GlpPaths> resolveGlpPaths({bool? sandboxed}) async {
  if (sandboxed ?? _platformIsSandboxed) return _fromBundle();

  // Desktop dev: the repo is reachable on disk — read it directly.
  final rel = Directory('../programs/grassapp');
  if (rel.existsSync()) {
    final base = Directory('../programs').absolute.path;
    return GlpPaths('$base/grassapp', '$base/social/graph', '$base/cssn',
        '$base/self.glp');
  }
  const repo = '/Users/udi/Grassroots/GLP/programs';
  if (Directory('$repo/grassapp').existsSync()) {
    return GlpPaths('$repo/grassapp', '$repo/social/graph', '$repo/cssn',
        '$repo/self.glp');
  }

  // A desktop build with no repo on disk — a released macOS app — carries the
  // same bundle and reads it.
  return _fromBundle();
}

/// Copy the bundled assets into Documents and point the engine at that tree.
Future<GlpPaths> _fromBundle() async {
  final docs = await getApplicationDocumentsDirectory();
  final base = '${docs.path}/glp/programs';
  for (final e in (await bundledGlp()).entries) {
    // Byte-for-byte, not as text: a `.glpw` opens with the magic `GLPW` and
    // fixed-width little-endian fields and is not UTF-8, so loadString fails
    // on it (currency.glpw at byte 24). Bytes carry the `.glp` sources
    // unchanged too, so one loop serves both.
    final data = await rootBundle.load(e.value);
    final out = File('${docs.path}/glp/${e.key}');
    await out.parent.create(recursive: true);
    await out.writeAsBytes(data.buffer
        .asUint8List(data.offsetInBytes, data.lengthInBytes));
  }
  return GlpPaths('$base/grassapp', '$base/social/graph', '$base/cssn',
      '$base/self.glp');
}
