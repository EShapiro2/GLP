/// A boot over a program directory, for the tests that start agents from an
/// inline source: TGLP 6ce148a, modules.tex, "Entry and the absence of a boot
/// module": a file that starts several agents carries its G@p clause "and the
/// persons scripted in GLP that the clause names, and nothing else: the
/// agents' program is a program directory, which the agents load"; the
/// loader refuses a boot file that defines anything else
/// (BootLoader._personsOnly; GLP #14, Code_inbox 2026-11-10 13:08 UTC).
/// [program] is written as the `self.glp` of a fresh directory under
/// programs/tests, so that its scope holds the root self.glp, and is removed
/// when the test ends.
library;

import 'dart:io';

import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:test/test.dart';

/// The root self.glp, absolute.
String get rootSelfGlp => File('../programs/self.glp').absolute.path;

/// The boot of [boot], its clause and its persons, over the program
/// [program], written as a program directory's self.glp.
BootConfig bootWithProgram(String boot, String program) {
  final dir = Directory('../programs/tests').createTempSync('glp_boot_prog_');
  addTearDown(() {
    if (dir.existsSync()) dir.deleteSync(recursive: true);
  });
  File('${dir.path}/self.glp').writeAsStringSync(program);
  return BootLoader().load(boot)
    ..rootSelfGlpPath = rootSelfGlp
    ..programDir = dir.absolute.path;
}
