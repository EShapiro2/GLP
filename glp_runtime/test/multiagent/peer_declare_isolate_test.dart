/// peer_declare/1 is honoured by the simulation's router under a closed trust
/// level: a declared peer's first contact is answered, an undeclared one's is
/// not.
///
/// IGLP appendix-implementation-notes.tex, Simulation realisation: "The PAN's
/// cold-call trust level is enforced as trust_declare sets it: under closed,
/// first contact from an agent not declared by peer_declare is not answered."
/// IGLP, Definition "Seam Predicates": "peer_declare gives the layer the
/// public key P of a peer the program knows"; GLP-Networking-API, "Known
/// Peers": the known-peer set "is what Closed trust ... recognizes".  The
/// simulation names agents symbolically, "resolved to keys at the seam".
///
/// The declarations run from GLP in bob's isolate.  The order is made by the
/// program, not by timing: bob declares, then tells dave, and only dave's word
/// sets alice and carol calling; bob's declarations reach the router over the
/// port his send to dave follows them on, so both are in force before either
/// call is routed.  Bob contacts neither caller, so neither call is answered
/// for his having contacted its sender.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Bob closes his PAN, declares alice, and reports each caller he hears;
/// dave relays bob's go to alice and carol, who each call bob.
const _boot = '''
procedure boot.
boot :- agent_init(bob, _)@bob, agent_init(dave, _)@dave,
    agent_init(alice, _)@alice, agent_init(carol, _)@carol.

procedure agent_init(_?, _?).
agent_init(bob, In) :-
    trust_declare(pan, closed), peer_declare(alice),
    send_to_net([msg(dave, go)]), heard(In?).
agent_init(dave, [msg(dave, go)|_]) :-
    send_to_net([msg(alice, go), msg(carol, go)]).
agent_init(alice, [msg(alice, go)|_]) :-
    send_to_net([msg(bob, hello(alice))]).
agent_init(carol, [msg(carol, go)|_]) :-
    send_to_net([msg(bob, hello(carol))]).

procedure heard(_?).
heard([msg(bob, hello(Caller))|In]) :- ground(Caller?) |
    send_to_user([Caller?]), heard(In?).
''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  test('under closed, a peer declared by peer_declare is answered and an '
      'undeclared one is not', () async {
    final config = BootLoader().load(_boot);
    config.rootSelfGlpPath = _rootSelf;

    await manager.boot(config);
    manager.start();
    await manager.settle();

    expect(manager.faults, isEmpty);
    expect(manager.trustLevelOf('bob', ProximityUnderlay.pan), TrustLevel.closed,
        reason: 'bob declared pan closed');
    expect(manager.outputOf('bob'), ['alice'],
        reason: 'alice, whom bob declared, is answered; carol, whom he did '
            'not, is not');
  }, timeout: Timeout(Duration(seconds: 60)));
}
