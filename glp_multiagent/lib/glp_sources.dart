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
library;

import 'dart:io';

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
}

/// The bundled assets (relative to `assets/glp/`), in the tree the engine's
/// ancestor-scope walk expects. Keep in sync with tool/sync_glp_assets.sh and
/// pubspec.yaml.
const _bundledGlp = [
  'programs/self.glp',
  // lib modules the root self.glp exposes (-expose(social#graph#routing#...)).
  'programs/social/graph/routing/output.glp',
  'programs/social/graph/routing/inject.glp',
  'programs/social/graph/routing/intro.glp',
  'programs/social/graph/routing/befriend.glp',
  // GrassApp (coins among friends).
  'programs/grassapp/self.glp',
  'programs/grassapp/currency_txn.glp',
  'programs/grassapp/grassapp_agent.glp',
  'programs/grassapp/grassapp_mediator.glp',
  'programs/grassapp/play_grassapp_boot.glp',
  'programs/grassapp/play_village_headless.glp',
  // Social graph (the canonical platform program).
  'programs/social/graph/self.glp',
  'programs/social/graph/agent.glp',
  'programs/social/graph/boot.glp',
  'programs/social/graph/play_ui_boot.glp',
  'programs/social/graph/ui/mediator.glp',
  'programs/social/graph/ui/actors.glp',
  // CSSN social network (groups): the whole program is statically linked, so
  // every module the root self.glp reaches must be bundled.
  'programs/cssn/self.glp',
  'programs/cssn/agent.glp',
  'programs/cssn/child_agent.glp',
  'programs/cssn/boot.glp',
  'programs/cssn/play_ui_boot.glp',
  'programs/cssn/ui/mediator.glp',
  'programs/cssn/ui/actors.glp',
];

Future<GlpPaths> resolveGlpPaths() async {
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

  // Sandboxed (iOS): copy the bundled assets into Documents and use that tree.
  final docs = await getApplicationDocumentsDirectory();
  final base = '${docs.path}/glp/programs';
  for (final a in _bundledGlp) {
    final source = await rootBundle.loadString('assets/glp/$a');
    final out = File('${docs.path}/glp/$a');
    await out.parent.create(recursive: true);
    await out.writeAsString(source);
  }
  return GlpPaths('$base/grassapp', '$base/social/graph', '$base/cssn',
      '$base/self.glp');
}
