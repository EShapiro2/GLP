/// Print the source identity h(M) of a program, as an agent that loads it
/// signs under it.
///
///     dart run bin/source_identity.dart <programDir> [<rootSelfGlp>]
///
/// The program is loaded as an agent's host loads it (AgentRuntime._setUp:
/// GlpEngine with the person's identity, madGLP enabled, `loadProgram` of the
/// directory), and the line printed is the h(M) of the program's module value,
/// in lowercase hex: the Hash that `signature/2` reports in signed(Key, Hash,
/// Term) for a signature that `sign/3` made in a goal of that program
/// (GLP-Spec appendix-guards, "Identity and signature"; IGLP code format,
/// "Deterministic Flattening" and "Signed content"; body_kernels.dart,
/// signKernel and _signatureOf).  h(M) is the SHA-256 of the canonical print
/// of the linked, pruned program, so it changes with any change to a module
/// the program links or a type definition it reaches, and with nothing else:
/// the person's key does not enter it.
///
/// [rootSelfGlp] defaults to programs/self.glp of this repository.  For the
/// social graph as the GrassApp phone runs it, <programDir> is
/// programs/social/graph/core (glp_multiagent/lib/main_superapp.dart,
/// GlpPaths.coreDir).
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';

import 'package:glp_runtime/runtime/body_kernels.dart' show bytesToHex;
import 'package:glp_runtime/wire/artefact.dart' show Artefact;

void main(List<String> args) {
  if (args.isEmpty || args.length > 2) {
    stderr.writeln('usage: source_identity <programDir> [<rootSelfGlp>]');
    exit(64);
  }
  final programDir = Directory(args[0]).absolute.path;
  final rootSelfGlp = args.length == 2
      ? File(args[1]).absolute.path
      : Platform.script.resolve('../../programs/self.glp').toFilePath();

  // As AgentRuntime._setUp makes the agent's engine, with no layer to sign
  // by: the key does not enter h(M), and the certificate is not read.
  final engine = GlpEngine(rootSelfGlpPath: rootSelfGlp);
  engine.enableMadGLP(agentId: 'source_identity');
  engine.loadProgram(programDir);
  final module = engine.appModule;
  final artefact = module?.artefact;
  if (artefact is! Artefact) {
    stderr.writeln('source_identity: $programDir loaded with no module value');
    exit(1);
  }
  print(bytesToHex(artefact.hM));
}
