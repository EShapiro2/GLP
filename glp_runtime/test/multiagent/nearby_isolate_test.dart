/// declare(nearby(pan, S)) under the isolate simulation, the path of the
/// REPL's `:boot`: the router reports each agent's discoveries and losses
/// to its isolate, onPeerDiscovered and onPeerLost (GNA cfbfeac, Section
/// 3.1), which feeds them to the nearby declarations, so a declaration on
/// the PAN is given seen(K) for an adjacent agent the declaring agent
/// declared a peer, and for any other adjacent agent once the PAN trust
/// levels of both are open, gone(K) when the harness cuts the adjacency, and
/// seen(K) again when it restores it.
///
/// GLP-Spec 210cee1, appendix-guards, "Networking seam": "nearby(U, S)
/// assigns S a stream of the agents nearby over the proximity underlay U:
/// seen(K) when the layer discovers on U an agent of key K that it recognises
/// or, under open trust, contacts, and gone(K) when that agent can no longer
/// be reached over U".  IGLP 45fabfc, Implementation Notes, "Simulation
/// realisation": "The adjacency relation is total by default, every agent a
/// PAN peer of every other, and the harness cuts and restores it"; "A nearby
/// declaration on the PAN is given seen for an adjacent agent the declaring
/// agent has declared a peer, and for any other adjacent agent once the PAN
/// trust levels of both are open, at the moment the second opens; it is
/// given gone when the harness cuts the adjacency.  The harness sets no
/// trust level at spawn."  In the simulation an agent is named by the
/// readable name standing for its key.
///
/// Each presence is an event of its own, followed by a run, so alice tells
/// her person each one in the phase the harness has settled, on no clock.
@Timeout.none
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

const _boot = '''
procedure boot.
boot :- agent(ch(_?, _), ch(_?, _))@alice, say(alice, ch(_?, _))@alice, agent(ch(_?, _), ch(_?, _))@bob, say(bob, ch(_?, _))@bob, agent(ch(_?, _), ch(_?, _))@carol, say(carol, ch(_?, _))@carol.

NbMsg ::= poke.

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(NbMsg)), Stream(NetMsg(NbMsg)))?).
agent(ch(In, []), ch(_, [])) :- start(In?).

procedure start(Stream(Constant)?).
start([alice | _]) :- declare(peer(bob)), declare(trust(pan, open)), declare(nearby(pan, S)), watch(S?).
start([bob | _]) :- declare(nearby(pan, S)), watch(S?).
start([carol | _]) :- declare(trust(pan, open)).
start([]).

procedure watch(Stream(Presence)?).
watch([P | S]) :- ground(P?) | send_to_user([P?]), watch(S?).
watch([]) :- send_to_user([closed]).

exported procedure say(Constant?, Channel(Stream(Constant), Stream(Constant))?).
say(P, ch(_, [P?])).
''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  test('a nearby declaration on the PAN is given seen for an adjacent agent '
      'declared a peer and for one whose PAN is open as the declaring '
      "agent's is, gone when the harness cuts the adjacency, and seen again "
      'when it restores it', () async {
    final config = BootLoader().load(_boot);
    config.rootSelfGlpPath = _rootSelf;

    await manager.boot(config);
    manager.start();
    await manager.settle();
    expect(manager.outputOf('alice').toSet(), {'seen(bob)', 'seen(carol)'},
        reason: 'bob a peer alice declared, carol open as alice is');
    expect(manager.outputOf('alice'), hasLength(2),
        reason: 'a discovery is one seen, and a connection none');

    manager.cut('alice', 'bob');
    await manager.settle();
    expect(manager.outputOf('alice').skip(2), ['gone(bob)']);

    manager.restore('alice', 'bob');
    await manager.settle();
    expect(manager.outputOf('alice').skip(2), ['gone(bob)', 'seen(bob)']);

    manager.cut('bob', 'carol');
    await manager.settle();
    expect(manager.outputOf('alice'), hasLength(4),
        reason: 'a cut between two other agents is no presence of alice\'s');

    expect(manager.outputOf('bob'), isEmpty,
        reason: 'bob declares no peer and his trust stays Closed, which the '
            'harness sets at spawn, so his declaration is given no seen');
    expect(manager.faults, isEmpty);
  }, timeout: Timeout.none);
}
