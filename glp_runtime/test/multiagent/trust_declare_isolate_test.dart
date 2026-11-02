/// trust_declare/2 is enforced by the simulation's router, per underlay.
///
/// IGLP appendix-implementation-notes.tex, Simulation realisation: "The PAN's
/// cold-call trust level is enforced as trust_declare sets it";
/// GLP-Networking-API, Trust levels: "A level is held per ProximityUnderlay,
/// and is set with setTrustLevel(underlay, level); both levels default to
/// Closed".  IGLP, Definition "Seam Predicates" (477c586): trust_declare "sets
/// the cold-call trust level of the proximity underlay U (pan or lan) to L
/// (open or closed)".  Until 2026-10-09 the underlay was a medium, ble or lan.
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
agent(ch(In, []), ch(N, [])) :- self_key(_), start(In?, N?).
procedure(X) start(Stream(Constant)?, Stream(NetMsg(X))?).
start([alice | _], N) :- agent_init(alice, N?).
start([bob | _], N) :- agent_init(bob, N?).
start(_, _) :- otherwise | true.
exported procedure say(Constant?, Channel(Stream(Constant), Stream(Constant))?).
say(P, ch(_, [P?])).

procedure agent_init(_?, _?).
agent_init(bob, _) :- trust_declare(pan, closed), trust_declare(lan, open).
agent_init(alice, _).
''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  test("an agent's trust_declare/2 sets the level the router enforces", () async {
    final config = BootLoader().load(_boot);
    config.rootSelfGlpPath = _rootSelf;

    await manager.boot(config);
    // Before the agents run: bob has no key (IGLP, Implementation Notes,
    // "Boot": "An agent has no key at boot"), and the router holds no level
    // for him.  Once his first call of self_key/1 makes his key, the boot
    // harness sets the PAN level Open for the plays, and he then declares his
    // own.  Until 2026-11-02 the harness made his key and set the level at boot.
    expect(manager.keyOf('bob'), isNull);
    expect(manager.trustLevelOf('bob', ProximityUnderlay.pan), TrustLevel.closed);
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
