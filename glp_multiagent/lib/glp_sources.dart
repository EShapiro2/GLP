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
  final String grassappDir; // .../programs/book/grassapp
  final String graphDir; // .../programs/social/graph
  final String rootSelfGlp; // .../programs/self.glp
  const GlpPaths(this.grassappDir, this.graphDir, this.rootSelfGlp);
}

/// The bundled assets (relative to `assets/glp/`), in the tree the engine's
/// ancestor-scope walk expects. Keep in sync with tool/sync_glp_assets.sh and
/// pubspec.yaml.
const _bundledGlp = [
  'programs/self.glp',
  // lib modules the root self.glp exposes (-expose(lib#routing#...)).
  'programs/lib/routing/output.glp',
  'programs/lib/routing/inject.glp',
  'programs/lib/routing/intro.glp',
  'programs/lib/routing/befriend.glp',
  // GrassApp (coins among friends).
  'programs/book/grassapp/self.glp',
  'programs/book/grassapp/grassapp_agent.glp',
  'programs/book/grassapp/grassapp_mediator.glp',
  'programs/book/grassapp/play_grassapp_boot.glp',
  // Social graph (the canonical platform program).
  'programs/social/graph/self.glp',
  'programs/social/graph/agent.glp',
  'programs/social/graph/boot.glp',
  'programs/social/graph/play_ui_boot.glp',
  'programs/social/graph/ui/mediator.glp',
  'programs/social/graph/ui/actors.glp',
];

Future<GlpPaths> resolveGlpPaths() async {
  // Desktop dev: the repo is reachable on disk — read it directly.
  final rel = Directory('../programs/book/grassapp');
  if (rel.existsSync()) {
    final base = Directory('../programs').absolute.path;
    return GlpPaths(
        '$base/book/grassapp', '$base/social/graph', '$base/self.glp');
  }
  const repo = '/Users/udi/Grassroots/GLP/programs';
  if (Directory('$repo/book/grassapp').existsSync()) {
    return GlpPaths(
        '$repo/book/grassapp', '$repo/social/graph', '$repo/self.glp');
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
  return GlpPaths('$base/book/grassapp', '$base/social/graph', '$base/self.glp');
}
