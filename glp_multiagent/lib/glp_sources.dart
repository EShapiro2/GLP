/// One loader for the GLP scenario sources, working on both desktop and
/// sandboxed (iOS) platforms.
///
/// The GLP engine reads `.glp` by real filesystem path — the root `self.glp`
/// directly, and ancestor `self.glp` files by walking parent directories. On a
/// desktop dev checkout it reads the repo under `programs/`. iOS is sandboxed
/// and cannot reach the repo, so the same files are bundled as assets and
/// copied, once, into the app's Documents directory (a real path) preserving
/// the `programs/.../` tree — and the engine is pointed there. Same files, one
/// loader, no forked program.
library;

import 'dart:io';

import 'package:flutter/services.dart' show rootBundle;
import 'package:path_provider/path_provider.dart';

/// Resolved locations of the GLP source tree.
class GlpPaths {
  final String grassappDir; // .../programs/book/grassapp
  final String rootSelfGlp; // .../programs/self.glp
  const GlpPaths(this.grassappDir, this.rootSelfGlp);
}

/// The bundled assets (relative to `assets/glp/`), in the tree the engine's
/// ancestor-scope walk expects.
const _bundledGlp = [
  'programs/self.glp',
  'programs/book/grassapp/self.glp',
  'programs/book/grassapp/grassapp_agent.glp',
  'programs/book/grassapp/grassapp_mediator.glp',
  'programs/book/grassapp/play_grassapp_boot.glp',
];

Future<GlpPaths> resolveGlpPaths() async {
  // Desktop dev: the repo is reachable on disk — read it directly.
  final rel = Directory('../programs/book/grassapp');
  if (rel.existsSync()) {
    final base = Directory('../programs').absolute.path;
    return GlpPaths('$base/book/grassapp', '$base/self.glp');
  }
  const repo = '/Users/udi/Grassroots/GLP/programs';
  if (Directory('$repo/book/grassapp').existsSync()) {
    return GlpPaths('$repo/book/grassapp', '$repo/self.glp');
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
  return GlpPaths('$base/book/grassapp', '$base/self.glp');
}
