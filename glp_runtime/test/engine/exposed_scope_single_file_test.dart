/// A module loaded alone is checked in its scope, the exposes included.
///
/// TGLP (modules.tex, "The -expose directive"): a self.glp's `-expose(M)` lifts
/// the exported procedures of M into that directory's scope, as if defined in
/// its self.glp, callable by name inside the directory and its descendants.
/// The linker discovered them so, and resolved the call; the engine's own check
/// of a real-file load built the self.glp chain alone, without the exposes,
/// until 2026-09-18, and refused as undefined what the linker resolved ---
/// `agent/4` of programs/tests/agent_roundtrip, `send_to_net/1` of
/// system/mad_predicates, which the root self.glp exposes into every program.
/// The check now uses the module's discovered scope.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  test('a single-file load sees what its directory exposes', () async {
    final engine =
        GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
    // in_subtree.glp calls twice/2, which expose/basic/self.glp exposes from
    // util/strutil.glp; loaded alone, it typechecks and runs.
    final path =
        File('../programs/tests/expose/basic/in_subtree.glp').absolute.path;
    expect(engine.loadFile(path), isTrue);

    final result = await engine.runGoal('use_exposed(R)');
    expect(result.succeeded, isTrue, reason: 'Error: ${result.error}');
    expect(result.bindings['R'].toString(), contains('100'));
  });
}
