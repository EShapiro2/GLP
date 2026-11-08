/// declare(peer(P)) is honoured by the simulation's router under a closed
/// trust level: a declared peer's first contact is answered, an undeclared
/// one's is not.
///
/// IGLP appendix-implementation-notes.tex, Simulation realisation: "The PAN's
/// cold-call trust level is enforced as the trust declaration sets it: under
/// closed, first contact from an agent not declared a peer is not answered."
/// IGLP, Definition "Seam Predicates": "declare(peer(P)) gives the layer the
/// public key P of a peer the program knows"; GLP-Networking-API, "Known
/// Peers": the known-peer set "is what Closed trust ... recognizes".  In the
/// simulation a key or a readable name standing for one names the one agent,
/// "the seam resolves both to the one agent" (IGLP 4f84612, "Simulation
/// realisation").
///
/// The declarations run from GLP in bob's isolate.  The order is made by the
/// program, not by timing: bob closes his PAN and then tells dave he is ready,
/// so dave, whom bob has contacted, is answered; dave asks bob to declare
/// alice's key only once he has both bob's word and alice's key, in either
/// order; bob declares, then tells dave, and only dave's word sets alice and
/// carol calling.  Bob's declarations reach the router over the port his sends
/// to dave follow them on, so each is in force before the message that follows
/// it is routed.  Bob contacts neither caller, so neither call is answered for
/// his having contacted its sender.  Until 2026-11-03 dave asked bob as soon
/// as alice's key came, and under load the request could reach the router
/// after bob closed his PAN and before his word to dave, a first contact from
/// an agent he had not contacted, and was not answered (IGLP-coop).
@Timeout.none
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';

/// The harness sets no trust level at spawn (IGLP 45fabfc, "Simulation
/// realisation"), so dave, alice and carol open their PANs at their start,
/// and bob, Closed, declares his closed.  Bob reports each caller he hears; alice tells dave her
/// key, dave hands it to bob, who declares it by declare(peer(K)), and dave then
/// relays the go to alice and carol, who each call bob.  Bob declares alice by
/// the key self_key/1 assigns her, made at her spawn (IGLP, Implementation
/// Notes, "Boot"), which reaches him in a message.  Until 2026-11-02 bob
/// declared alice by her name at his start.
const _boot = '''
procedure boot.
boot :- agent(ch(_?, _), ch(_?, _))@bob, say(bob, ch(_?, _))@bob, agent(ch(_?, _), ch(_?, _))@dave, say(dave, ch(_?, _))@dave, agent(ch(_?, _), ch(_?, _))@alice, say(alice, ch(_?, _))@alice, agent(ch(_?, _), ch(_?, _))@carol, say(carol, ch(_?, _))@carol.

PdMsg ::= go ; ready ; declared ; key(Key) ; declare(Key) ; hello(Constant).
DaveKey ::= waiting ; got(Key).

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(PdMsg)), Stream(NetMsg(PdMsg)))?).
agent(ch(In, []), ch(N, [])) :- start(In?, N?).
procedure start(Stream(Constant)?, Stream(NetMsg(PdMsg))?).
start([P | _], N) :- agent_init(P?, N?).
start([], _).
exported procedure say(Constant?, Channel(Stream(Constant), Stream(Constant))?).
say(P, ch(_, [P?])).

procedure agent_init(Constant?, Stream(NetMsg(PdMsg))?).
agent_init(bob, In) :-
    declare(trust(pan, closed)),
    send_to_net([msg(dave, ready)]), bob_in(In?).
agent_init(dave, In) :- declare(trust(pan, open)), dave_key(In?).
agent_init(alice, In) :-
    declare(trust(pan, open)), self_key(K), send_to_net([msg(dave, key(K?))]), called(alice, In?).
agent_init(carol, In) :- declare(trust(pan, open)), called(carol, In?).

procedure bob_in(Stream(NetMsg(PdMsg))?).
bob_in([msg(bob, declare(K)) | In]) :- ground(K?) |
    declare(peer(K?)), send_to_net([msg(dave, declared)]), heard(In?).
bob_in([_ | In]) :- otherwise | bob_in(In?).
bob_in([]).

procedure dave_key(Stream(NetMsg(PdMsg))?).
dave_key(In) :- dave_wait(In?, no, waiting).

procedure dave_wait(Stream(NetMsg(PdMsg))?, Constant?, DaveKey?).
dave_wait([msg(dave, ready) | In], _, got(K)) :- ground(K?) |
    send_to_net([msg(bob, declare(K?))]), dave_go(In?).
dave_wait([msg(dave, ready) | In], _, waiting) :- dave_wait(In?, yes, waiting).
dave_wait([msg(dave, key(K)) | In], yes, _) :- ground(K?) |
    send_to_net([msg(bob, declare(K?))]), dave_go(In?).
dave_wait([msg(dave, key(K)) | In], no, _) :- ground(K?) |
    dave_wait(In?, no, got(K?)).
dave_wait([_ | In], R, S) :- otherwise | dave_wait(In?, R?, S?).
dave_wait([], _, _).

procedure dave_go(Stream(NetMsg(PdMsg))?).
dave_go([msg(dave, declared) | _]) :-
    send_to_net([msg(alice, go), msg(carol, go)]).
dave_go([_ | In]) :- otherwise | dave_go(In?).
dave_go([]).

procedure called(Constant?, Stream(NetMsg(PdMsg))?).
called(P, [msg(_, go) | _]) :- send_to_net([msg(bob, hello(P?))]).
called(P, [_ | In]) :- otherwise | called(P?, In?).
called(_, []).

procedure heard(Stream(NetMsg(PdMsg))?).
heard([msg(bob, hello(Caller)) | In]) :- ground(Caller?) |
    send_to_user([Caller?]), heard(In?).
heard([_ | In]) :- otherwise | heard(In?).
heard([]).
''';

String get _rootSelf => File('../programs/self.glp').absolute.path;

void main() {
  late IsolateManager manager;

  setUp(() => manager = IsolateManager());
  tearDown(() async => manager.shutdown());

  test('under closed, a peer declared by declare(peer(P)) is answered and an '
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
  }, timeout: Timeout.none);
}
