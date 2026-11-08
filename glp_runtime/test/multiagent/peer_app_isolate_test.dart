/// peer_app/2 under the isolate simulation, the path of the REPL's `:boot`:
/// the router reports each agent's connections and disconnections to its
/// isolate, which records them, so peer_app answers `app(D, N, V)`, the app
/// the harness names, for a connected peer and `none` for one the harness has
/// cut off.
///
/// IGLP 4a9ad28, Implementation Notes, "Networking Seam", The contract: "the
/// layer reports peer discovery, connection, disconnection, and reachability
/// ... the runtime records the app the layer reports attested in its session
/// with each peer, for peer_app"; "Simulation realisation": "The adjacency
/// relation is total by default, every agent a PAN peer of every other, and
/// the harness cuts and restores it", and "The router reports each connection
/// and disconnection to the agents, and reports every peer attested with the
/// app the harness names, its developer, name and version".  IGLP, Definition
/// "Seam Predicates": peer_app assigns "app(D, N, V) ... unattested where that
/// session is unattested, or none where the layer has no session with P".
/// The harness names the app by IsolateManager.boot's `application`, the
/// REPL's `:app`; where it names none, the paper does not say, and a
/// connection is reported unattested, as before 2026-11-07.  Until 2026-11-07
/// the isolate simulation delivered no connectivity event to any agent, and
/// peer_app answered `none` for every peer under `:boot`.
///
/// Alice asks peer_app for bob each time a poke reaches her.  The three pokes
/// come from carol, dave and erin, each pair held from the start and released
/// by the harness in its phase: connected, the alice--bob adjacency cut, and
/// restored.  So each answer is asked in a phase the harness has settled, by
/// the program's events and on no clock.
@Timeout.none
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show Application;
import 'package:glp_runtime/multiagent/isolate_manager.dart';

const _boot = '''
procedure boot.
boot :- agent(ch(_?, _), ch(_?, _))@alice, say(alice, ch(_?, _))@alice, agent(ch(_?, _), ch(_?, _))@bob, say(bob, ch(_?, _))@bob, agent(ch(_?, _), ch(_?, _))@carol, say(carol, ch(_?, _))@carol, agent(ch(_?, _), ch(_?, _))@dave, say(dave, ch(_?, _))@dave, agent(ch(_?, _), ch(_?, _))@erin, say(erin, ch(_?, _))@erin.

PaMsg ::= poke.

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(PaMsg)), Stream(NetMsg(PaMsg)))?).
agent(ch(In, []), ch(N, [])) :- start(In?, N?).
procedure start(Stream(Constant)?, Stream(NetMsg(PaMsg))?).
start([P | _], N) :- agent_init(P?, N?).
start([], _).
exported procedure say(Constant?, Channel(Stream(Constant), Stream(Constant))?).
say(P, ch(_, [P?])).

procedure agent_init(Constant?, Stream(NetMsg(PaMsg))?).
agent_init(alice, In) :- probe(In?).
agent_init(bob, _).
agent_init(_, _) :- otherwise | send_to_net([msg(alice, poke)]).

procedure probe(Stream(NetMsg(PaMsg))?).
probe([msg(alice, poke) | In]) :- peer_app(bob, A), send_to_user([A?]), probe(In?).
probe([_ | In]) :- otherwise | probe(In?).
probe([]).
''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  /// Boot the five agents, the harness naming [application], and hold
  /// alice's three answers to [connected], none and [connected]: connected,
  /// cut, restored.
  Future<void> phases(Application? application, String connected) async {
    final config = BootLoader().load(_boot);
    config.rootSelfGlpPath = _rootSelf;

    await manager.boot(config, application: application);
    for (final poker in ['carol', 'dave', 'erin']) {
      manager.holdDelivery(poker, 'alice');
    }
    manager.start();
    await manager.settle();
    expect(manager.outputOf('alice'), isEmpty,
        reason: 'every poke is held until its phase');

    manager.releaseDelivery('carol', 'alice');
    await manager.settle();
    expect(manager.outputOf('alice'), [connected],
        reason: 'alice and bob are connected from the boot');

    manager.cut('alice', 'bob');
    manager.releaseDelivery('dave', 'alice');
    await manager.settle();
    expect(manager.outputOf('alice'), [connected, 'none'],
        reason: 'the cut is reported to alice as bob disconnected');

    manager.restore('alice', 'bob');
    manager.releaseDelivery('erin', 'alice');
    await manager.settle();
    expect(manager.outputOf('alice'), [connected, 'none', connected],
        reason: 'the restore is reported to alice as bob connected, '
            'attested again');

    expect(manager.faults, isEmpty);
  }

  test('peer_app answers app(D, N, V), the app the harness names, for a '
      'connected peer, none once the adjacency is cut, and app(D, N, V) '
      'again once it is restored', () async {
    await phases(
        const Application(
            developer: 'TEAM', name: 'app.grassroots', version: 7),
        "app('TEAM', 'app.grassroots', 7)");
  }, timeout: Timeout.none);

  test('where the harness names no app, a connected peer is unattested',
      () async {
    await phases(null, 'unattested');
  }, timeout: Timeout.none);
}
