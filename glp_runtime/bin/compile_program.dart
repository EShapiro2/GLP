/// Compile a program once, outside any person's agent, into the bytes of the
/// compiled program a host gives its agents at their start (IGLP,
/// Implementation Notes, "Boot": "A host may give an agent, at its start, the
/// compiled program it runs in place of the program's source: a module value,
/// which the loader verifies as it verifies a module the agent adopts, and
/// whose certificate's key the host accepts; the agent runs it and compiles
/// nothing").
///
///   dart run bin/compile_program.dart <program> <out> <seed>
///
/// <program> is a directory with a self.glp or a self-contained module file;
/// <out> is the file the bytes are written to ([StoredProgram.toBytes]),
/// which a host reads with `StoredProgram.fromBytes` and gives to
/// `AgentRuntime(compiled: ...)`; <seed> is a file of the 32 bytes of the
/// host's Ed25519 seed, the key the certificate is written under, made with
/// fresh random bytes where the file does not exist.  The root self.glp is
/// `programs/self.glp`, found from the working directory, `glp_runtime`, or
/// given by GLP_ROOT_SELF.  A program refused a certificate is refused here,
/// the compiler naming the call that refuses it, and nothing is written.
library;

import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/multiagent/stored_program.dart';

void main(List<String> args) {
  if (args.length != 3) {
    stderr.writeln('usage: dart run bin/compile_program.dart '
        '<program> <out> <seed>');
    exit(2);
  }
  final [program, out, seedPath] = args;
  final root = Platform.environment['GLP_ROOT_SELF'] ??
      File('../programs/self.glp').absolute.path;
  final seedFile = File(seedPath);
  if (!seedFile.existsSync()) {
    final r = Random.secure();
    seedFile.writeAsBytesSync(
        Uint8List.fromList([for (var i = 0; i < 32; i++) r.nextInt(256)]));
  }
  final seed = seedFile.readAsBytesSync();
  if (seed.length != 32) {
    stderr.writeln('$seedPath holds ${seed.length} bytes, not a 32-byte seed');
    exit(2);
  }
  final watch = Stopwatch()..start();
  final StoredProgram compiled;
  try {
    compiled = StoredProgram.compile(program,
        rootSelfGlpPath: root, host: PersonIdentity.fromSeed(seed));
  } on StateError catch (e) {
    stderr.writeln(e.message);
    exit(1);
  } catch (e) {
    stderr.writeln('$program does not compile: $e');
    exit(1);
  }
  final bytes = compiled.toBytes();
  File(out).writeAsBytesSync(bytes);
  stdout.writeln('$out: ${bytes.length} bytes, compiled in '
      '${watch.elapsedMilliseconds} ms');
}
