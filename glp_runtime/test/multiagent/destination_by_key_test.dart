/// In a simulation, a message reaches the agent its destination names, whether
/// the destination is the agent's key or a readable name standing for it.
///
/// IGLP, Implementation Notes, "Simulation realisation" (4f84612): "a readable
/// name stands for a key (Remark Agent Names), so a destination, in a global
/// name or in a message, is a key or a readable name standing for one, and the
/// seam resolves both to the one agent; a program runs unchanged in a
/// simulation and over the network."  Remark "Agent Names": "An agent p is its
/// public key ... A readable name such as alice or bob ... is a variable
/// standing for a public key."
///
/// Bob tells alice his key, the one self_key/1 answers him, in a message to
/// alice by her readable name.  Alice sends msg(K, ask(R)), K that key, so the
/// message's destination and its global name '_w'(K, 0) carry the key; bob
/// assigns done to R, the writer it carries, and the value reaches alice on
/// the link, where she reports it.  The link is the one alice exported to bob:
/// were the key not resolved to bob at the seam, the message would not reach
/// him, and his assignment would come from an agent other than the one the
/// link was exported to and be held (IGLP, "Held links").
///
/// The waits are on events only: boot and settle end when the agents are
/// ready and quiescent, or on a fault, and the test has no clock.
@Timeout.none
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

const _boot = '''
procedure boot.
boot :- agent(ch(_?, _), ch(_?, _))@alice, say(alice, ch(_?, _))@alice, agent(ch(_?, _), ch(_?, _))@bob, say(bob, ch(_?, _))@bob.

KeyMsg ::= key(Key) ; ask(Done?).

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(KeyMsg)), Stream(NetMsg(KeyMsg)))?).
agent(ch(In, []), ch(N, [])) :- declare(trust(pan, open)), start(In?, N?).
procedure start(Stream(Constant)?, Stream(NetMsg(KeyMsg))?).
start([P | _], N) :- agent_init(P?, N?).
start([], _).
exported procedure say(Constant?, Channel(Stream(Constant), Stream(Constant))?).
say(P, ch(_, [P?])).

procedure agent_init(Constant?, Stream(NetMsg(KeyMsg))?).
agent_init(bob, In) :-
    self_key(K), send_to_net([msg(alice, key(K?))]), bob_in(In?).
agent_init(alice, In) :- alice_in(In?).

procedure alice_in(Stream(NetMsg(KeyMsg))?).
alice_in([msg(_, key(K)) | _]) :- ground(K?) |
    send_to_net([msg(K?, ask(R))]), answered(R?).
alice_in([_ | In]) :- otherwise | alice_in(In?).
alice_in([]).

procedure answered(Done?).
answered(done) :- send_to_user([answered]).

procedure bob_in(Stream(NetMsg(KeyMsg))?).
bob_in([msg(_, ask(R?)) | _]) :- reply(R).
bob_in([_ | In]) :- otherwise | bob_in(In?).
bob_in([]).

procedure reply(Done).
reply(done).
''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  test('a message sent to the key a peer\'s self_key/1 answered reaches the '
      'peer, and the value on its link comes back', () async {
    final config = BootLoader().load(_boot);
    config.rootSelfGlpPath = _rootSelf;

    await manager.boot(config);
    manager.start();
    await manager.settle();

    expect(manager.faults, isEmpty);
    expect(manager.outputOf('alice'), ['answered'],
        reason: 'bob received ask(R), sent to his key, and his assignment to '
            'R reached alice on the link she exported to him');
  }, timeout: Timeout.none);
}
