/// A trust declaration, declare(trust(U, L)), is enforced by the simulation's
/// router, per underlay.
///
/// IGLP appendix-implementation-notes.tex, Simulation realisation: "The PAN's
/// cold-call trust level is enforced as the trust declaration sets it";
/// GLP-Networking-API, Trust levels: "A level is held per ProximityUnderlay,
/// and is set with setTrustLevel(underlay, level); both levels default to
/// Closed".  IGLP, Definition "Seam Predicates" (3b736bb): "declare(trust(U,
/// L)) sets the cold-call trust level of the proximity underlay U (pan or lan)
/// to L (open or closed)".  Until 2026-10-09 the underlay was a medium, ble or
/// lan, and until 2026-11-03 the predicate trust_declare/2.
///
/// Until 2026-10-02 the agent's layer recorded the level it was given and the
/// router never learnt of it: the router enforced one level per agent, the one
/// the boot harness set (isolate_manager.dart), so a level an agent declared
/// governed nothing.  These tests run the declaration from GLP in an agent's
/// isolate and read the level the router then holds.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// Two agents: bob declares PAN closed and LAN open, alice declares nothing.
const _boot = '''
procedure boot.
boot :- agent(ch(_?, _), ch(_?, _))@alice, say(alice, ch(_?, _))@alice, agent(ch(_?, _), ch(_?, _))@bob, say(bob, ch(_?, _))@bob.
exported procedure(X) agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(X)), Stream(NetMsg(X)))?).
agent(ch(In, []), ch(N, [])) :- start(In?, N?).
procedure(X) start(Stream(Constant)?, Stream(NetMsg(X))?).
start([alice | _], N) :- agent_init(alice, N?).
start([bob | _], N) :- agent_init(bob, N?).
start(_, _) :- otherwise | true.
exported procedure say(Constant?, Channel(Stream(Constant), Stream(Constant))?).
say(P, ch(_, [P?])).

procedure agent_init(_?, _?).
agent_init(bob, _) :- declare(trust(pan, closed)), declare(trust(lan, open)).
agent_init(alice, _).
''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  test("an agent's trust declaration sets the level the router enforces",
      () async {
    final config = BootLoader().load(_boot);
    config.rootSelfGlpPath = _rootSelf;

    await manager.boot(config);
    // Before the agents run: bob's key, made at his spawn (IGLP,
    // Implementation Notes, "Boot": "When the runtime spawns an agent, before
    // anything is compiled or sent, it makes the agent's key pair"), the boot
    // harness's PAN level Open for the plays, and LAN Closed, as no level is
    // set for it.
    expect(manager.keyOf('bob'), isNotNull);
    expect(manager.trustLevelOf('bob', ProximityUnderlay.pan), TrustLevel.open);
    expect(manager.trustLevelOf('bob', ProximityUnderlay.lan), TrustLevel.closed);

    manager.start();
    await manager.settle();

    expect(manager.faults, isEmpty);
    expect(manager.trustLevelOf('bob', ProximityUnderlay.pan), TrustLevel.closed,
        reason: 'bob declared pan closed');
    expect(manager.trustLevelOf('bob', ProximityUnderlay.lan), TrustLevel.open,
        reason: 'bob declared lan open, the two underlays independently');
    expect(manager.trustLevelOf('alice', ProximityUnderlay.pan), TrustLevel.open,
        reason: 'alice declared nothing: the boot level stands');
    expect(manager.trustLevelOf('alice', ProximityUnderlay.lan),
        TrustLevel.closed);
  }, timeout: Timeout(Duration(seconds: 60)));
}
