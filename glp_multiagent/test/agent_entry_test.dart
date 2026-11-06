/// The entry of a deployed agent (`lib/agent_entry.dart`): at its first start
/// it makes the key pair and saves it in the store, at every start it reads
/// it from the store and instantiates the layer with it (IGLP, Implementation
/// Notes, "Boot" and "Persistence"), and the agent, in its isolate, runs over
/// the layer the application holds: here two agents over the headless layer
/// on UDP loopback, the same path as the Mac's and the phone's
/// (helpers/lan_agents.dart).
///
/// Needs a native libsodium, as GNA's headless tests do; self-skips when it
/// is absent.
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/agent_entry.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart'
    show
        FileOutboundQueueStore,
        GrassrootsIdentity,
        HeadlessGrassrootsNetwork,
        initHeadlessSodium;
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

import 'helpers/lan_agents.dart';
import 'programs_dir.dart';

void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;
  late Directory dir;
  late String agents;
  late String play;
  late String root;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping: $e';
    }
    dir = Directory('${programsDir()}/tests').createTempSync('glp_entry_');
    agents = '${dir.path}/agents';
    play = '${dir.path}/play.glp';
    File(play).writeAsStringSync(lanPlay);
    root = '${programsDir()}/self.glp';
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  Future<AgentLayer> headless(GrassrootsIdentity identity, String queue) async {
    final layer = HeadlessGrassrootsNetwork(
      identity: identity,
      sodium: sodium!,
      outboundQueueStore: FileOutboundQueueStore(queue),
    );
    return AgentLayer(HeadlessGlpNetwork(layer),
        start: layer.start, dispose: layer.dispose);
  }

  int portOf(AgentEntry entry) =>
      (entry.layer.network as HeadlessGlpNetwork).layer.boundPort!;

  test('a first start makes and saves a key; a second start of the same agent '
      'has the same key and the layer\'s identity equals it', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final directory = '$agents/keys';
    final store = FileAgentStateStore('$directory/alice.state');
    expect(await store.loadKeyPair(), isNull);

    final first = await AgentEntry.open(
        persona: 'alice', directory: directory, makeLayer: headless);
    final saved = await store.loadKeyPair();
    expect(saved?.pub, first.keyPair.pub);
    expect(saved?.priv, first.keyPair.priv);
    expect(first.layer.network.getIdentity(), first.keyPair.pub);

    expect(
        await secondStart(first,
            persona: 'alice', directory: directory, makeLayer: headless),
        isNull);

    // Another agent's key is its own.
    final other = await AgentEntry.open(
        persona: 'bob', directory: directory, makeLayer: headless);
    expect(other.keyPair.pub, isNot(first.keyPair.pub));
    await other.close();
  }, timeout: Timeout.none);

  test('the declarations of the text a build gives', () {
    final key = 'ab' * 32;
    final d = AgentDeclarations.parse(
        rendezvous: '$key@203.0.113.7:9516', peers: '$key,$key@10.0.0.2:7');
    expect(d.rendezvous.single.key, PubKey.fromHex(key));
    expect(d.rendezvous.single.address, '203.0.113.7:9516');
    expect([for (final p in d.peers) p.address], [null, '10.0.0.2:7']);
    expect(AgentDeclarations.parse().peers, isEmpty);
  });

  test('two deployed agents in isolates: a cold call and an assignment cross '
      'between them over their layers', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final caller = await AgentEntry.open(
        persona: 'caller', directory: agents, makeLayer: headless);
    final callee = await AgentEntry.open(
        persona: 'callee', directory: agents, makeLayer: headless);
    addTearDown(caller.close);
    addTearDown(callee.close);

    callee.declare(AgentDeclarations(
        peers: [(key: caller.keyPair.pub, address: null)]));
    caller.declare(AgentDeclarations(peers: [
      (key: callee.keyPair.pub, address: '127.0.0.1:${portOf(callee)}'),
    ]));

    final answering = await AgentRun.spawn(callee,
        persona: 'callee',
        program: play,
        rootSelf: root,
        person: calleePerson());
    final calling = await AgentRun.spawn(caller,
        persona: 'caller',
        program: play,
        rootSelf: root,
        person: callerPerson(callee.keyPair.pub.hex));

    // The cold call crosses, addressed to the callee's name, its key; the
    // assignment crosses back; each agent is named by its key.
    expect(await answering.first((l) => l.startsWith('< called(')),
        contains(callee.keyPair.pub.hex));
    expect(await calling.first((l) => l.startsWith('< answered(')),
        contains('forty_two'));
    final receipt = await calling.first((l) => l.startsWith('MAD_RECV from '));
    expect(receipt, contains(callee.keyPair.pub.hex));
    expect(calling.underlay(receipt), Underlay.wan.name);

    await calling.stop();
    await answering.stop();
  }, timeout: Timeout.none);
}
