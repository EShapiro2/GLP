/// trust_declare/2 is enforced by the simulation's router, per medium.
///
/// GLP-Networking-API, Simulation Realization, "Discovery and trust":
/// "setTrustLevel(medium, level) is enforced as specified for BLE: under Closed,
/// first contact from an unknown agent is not answered"; Trust levels: "A level
/// is held per ProximityMedium, and GLP sets one medium's level with
/// setTrustLevel(medium, level); until set, both levels are Closed".  IGLP,
/// Definition "Seam Predicates": trust_declare(M, L) "sets the cold-call trust
/// level of the proximity medium M (ble or lan) to L (open or closed)".
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

/// Two agents: bob declares BLE closed and LAN open, alice declares nothing.
const _boot = '''
procedure boot.
boot :- agent_init(alice, _)@alice, agent_init(bob, _)@bob.

procedure agent_init(_?, _?).
agent_init(bob, _) :- trust_declare(ble, closed), trust_declare(lan, open).
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
    // Before the agents run: the boot harness's BLE level Open for the plays,
    // and LAN Closed, as no level is set for it.
    expect(manager.trustLevelOf('bob', ProximityMedium.ble), TrustLevel.open);
    expect(manager.trustLevelOf('bob', ProximityMedium.lan), TrustLevel.closed);

    manager.start();
    await manager.settle();

    expect(manager.faults, isEmpty);
    expect(manager.trustLevelOf('bob', ProximityMedium.ble), TrustLevel.closed,
        reason: 'bob declared ble closed');
    expect(manager.trustLevelOf('bob', ProximityMedium.lan), TrustLevel.open,
        reason: 'bob declared lan open, the two media independently');
    expect(manager.trustLevelOf('alice', ProximityMedium.ble), TrustLevel.open,
        reason: 'alice declared nothing: the boot level stands');
    expect(manager.trustLevelOf('alice', ProximityMedium.lan),
        TrustLevel.closed);
  }, timeout: Timeout(Duration(seconds: 60)));
}
