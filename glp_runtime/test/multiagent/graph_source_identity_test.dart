/// The social graph's source identity: the Hash signature/2 reports for the
/// social graph's reconnection attestation.
///
/// The phone's agent loads programs/social/graph/core as its one program
/// (glp_multiagent/lib/main_superapp.dart, GlpPaths.coreDir;
/// AgentRuntime._setUp, GlpEngine.loadProgram), and signs attest(PkA, PkB)
/// in a goal of it (core/agent.glp).  sign/3 puts into the signed term the
/// source identity of the calling goal's module (GLP-Spec appendix-guards,
/// "Identity and signature"; IGLP code format, "Signed content"), and every
/// goal of the program carries the program's module value, a spawned goal its
/// parent's.  So the Hash signature/2 reports for the agent's attestation is
/// the h(M) of the loaded core program, and bin/source_identity.dart prints
/// it: this test signs attest(a, b) under that program, as the agent does,
/// and holds signature/2's Hash to the module's h(M), to the flattening's and
/// to what the tool prints.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show SimulationNetworkClient;
import 'package:glp_runtime/runtime/body_kernels.dart' show bytesToHex;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/flattening.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;
final String _core = Directory('../programs/social/graph/core').absolute.path;

void main() {
  test('signature/2 reports, for attest/2 signed under the social graph, the '
      'h(M) that source_identity prints', () async {
    // The simulation's layer holds alice's key pair and signs; the engine
    // signs by it (the GLP paper f265fec, "Identity and signature").
    final a = PersonIdentity.generate();
    final layer = SimulationNetworkClient.alone(a.keyPair, selfId: 'alice');
    final engine =
        GlpEngine(rootSelfGlpPath: _rootSelf, identity: LayerSigner(layer));
    engine.enableMadGLP(agentId: 'alice');
    engine.madContext!.network = layer;
    engine.loadProgram(_core);
    final hM = bytesToHex((engine.appModule!.artefact as Artefact).hM);

    final result = await engine.runGoal(
        'self_key(K), sign(attest(a, b), K?, S), signature(S?, Sig)');
    expect(result.succeeded, isTrue);
    final sig = result.bindings['Sig'];
    expect(sig, isA<StructTerm>());
    final s = sig as StructTerm;
    expect(s.functor, 'signed');
    expect((s.args[0] as ConstTerm).value, a.pub.hex);
    expect((s.args[1] as ConstTerm).value, hM,
        reason: 'the Hash is the signing program\'s h(M)');
    final t = s.args[2] as StructTerm;
    expect(t.functor, 'attest');
    expect([for (final x in t.args) (x as ConstTerm).value], ['a', 'b']);

    // The flattening of the program on disc gives the same identity.
    final flat = flattenProject(_core, rootSelfGlpPath: _rootSelf);
    expect(bytesToHex(flat.hM), hM);

    // And the tool prints it, 64 lowercase hex characters on its last line.
    final run = await Process.run(
        Platform.resolvedExecutable, ['run', 'bin/source_identity.dart', _core]);
    expect(run.exitCode, 0, reason: '${run.stderr}');
    final last = (run.stdout as String).trim().split('\n').last;
    expect(last, matches(RegExp(r'^[0-9a-f]{64}$')));
    expect(last, hM);
  }, timeout: const Timeout(Duration(minutes: 5)));
}
