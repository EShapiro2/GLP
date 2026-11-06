/// Two deployed agents exchanging a cold call and an assignment: the agent of
/// the app's entry ([AgentEntry], `lib/agent_entry.dart`), in an isolate of
/// its own, over the layer the application holds.  The same run on UDP
/// loopback in one process (`test/agent_entry_test.dart`), and over the LAN
/// between two machines, the Mac (`lan_mac_agent.dart`) and the iOS Simulator
/// (`integration_test/lan_agent_test.dart`), which `tool/simctl_lan_agents.sh`
/// runs.
///
/// The harness gives each agent the other's key and address (GLP #12,
/// 2026-11-06 07:13 UTC: "Rung 4's two agents get each other's keys and
/// addresses from the harness, the person goal's ground arguments carrying
/// the keys the person knows"): the entry declares them to the layer
/// ([AgentDeclarations]), and the persons of [lanPlay] carry the key.  Between
/// machines the blackboard, GNA's `tool/e2e_blackboard.dart`, carries them.
///
/// Every wait is on an event, a line of the agent's or a key on the
/// blackboard, and no clock decides anything (IGLP, Implementation Notes,
/// "Event-driven execution"); a run that never ends is the harness's to stop.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';

import 'package:glp_multiagent/agent_entry.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart' show PersonGoal;
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;

/// The play of GNA's adapter test with its persons scripted (IGLP,
/// Implementation Notes, "Boot"): the caller's person names the callee by its
/// key, its ground argument, and the callee's tells its agent to answer; the
/// caller cold-calls the callee with a writer, and the callee assigns the
/// writer.
const String lanPlay = r'''
GnIn  ::= call(Key) ; answer.
GnOut ::= answered(Constant) ; called(Key).
GnMsg ::= ask(Constant?).

exported procedure agent(Channel(Stream(GnIn), Stream(GnOut))?, Channel(Stream(NetMsg(GnMsg)), Stream(NetMsg(GnMsg)))?).
agent(ch(In, []), ch(N, [])) :- start(In?, N?).

procedure start(Stream(GnIn)?, Stream(NetMsg(GnMsg))?).
start([call(Callee) | _], _) :- ground(Callee?) |
    send_to_net([msg(Callee?, ask(Answer))]),
    answered(Answer?).
start([answer | _], [msg(Me, ask(A?)) | _]) :-
    send_to_user([called(Me?)]),
    answer(A).
start([], _).

procedure answered(Constant?).
answered(forty_two) :- send_to_user([answered(forty_two)]).

procedure answer(Constant).
answer(forty_two).

exported procedure caller(Key?, Channel(Stream(GnOut), Stream(GnIn))?).
caller(Callee, ch(_, [call(Callee?)])).

exported procedure callee(Channel(Stream(GnOut), Stream(GnIn))?).
callee(ch(_, [answer])).
''';

/// The caller's person, who knows the callee's key.
PersonGoal callerPerson(String calleeKey) =>
    PersonGoal('caller', ["'$calleeKey'", 'ch(_?,_)'], 1);

/// The callee's person.
PersonGoal calleePerson() => PersonGoal('callee', ['ch(_?,_)'], 0);

/// One deployed agent in its isolate: what it reports, and waits on it.
class AgentRun {
  final ReceivePort _reply = ReceivePort();
  final ReceivePort _ended = ReceivePort();
  final ReceivePort _exit = ReceivePort();
  late final Isolate _isolate;
  final List<String> lines = [];
  final List<(bool Function(String), Completer<String>)> _waiting = [];
  SendPort? _commands;
  Object? _fault;

  AgentRun._() {
    _reply.listen(_take);
    _ended.listen((e) => _end(
        e is List ? 'its isolate threw: ${e.first}' : 'its isolate exited'));
  }

  /// Spawn the agent of [persona], running [program] under the root
  /// [rootSelf], on [entry]'s key pair and layer, its person [person].
  static Future<AgentRun> spawn(AgentEntry entry,
      {required String persona,
      required String program,
      required String rootSelf,
      required PersonGoal person}) async {
    final run = AgentRun._();
    run._isolate = await Isolate.spawn(
      agentIsolateEntry,
      InitAgent(
        agentId: persona,
        program: program,
        rootSelfGlpPath: rootSelf,
        replyPort: run._reply.sendPort,
        deployment: entry.deployment,
        person: person,
      ),
      onError: run._ended.sendPort,
      onExit: run._ended.sendPort,
    );
    return run;
  }

  void _take(Object? msg) {
    switch (msg) {
      case AgentReady(:final commandPort):
        _commands = commandPort;
      case AgentOutput(:final line):
        _add(line);
      case AgentLog(:final message):
        _add(message);
      case AgentError(:final error):
        _end('it reported $error');
    }
  }

  void _add(String line) {
    lines.add(line);
    for (final w in List.of(_waiting)) {
      if (w.$1(line)) {
        _waiting.remove(w);
        w.$2.complete(line);
      }
    }
  }

  void _end(String why) {
    _fault ??= why;
    for (final w in _waiting) {
      w.$2.completeError(StateError('the agent stopped: $why'));
    }
    _waiting.clear();
  }

  /// The first line of the agent's transcript or log that [test] accepts.
  Future<String> first(bool Function(String) test) {
    for (final l in lines) {
      if (test(l)) return Future.value(l);
    }
    if (_fault != null) {
      return Future.error(StateError('the agent stopped: $_fault'));
    }
    final c = Completer<String>();
    _waiting.add((test, c));
    return c.future;
  }

  /// The underlay of the message the agent received, as its runtime logs it.
  String? underlay(String receipt) =>
      RegExp(r'^MAD_RECV from [0-9a-f]+ over (\w+) ')
          .firstMatch(receipt)
          ?.group(1);

  /// Stop the agent; [stopped] waits for its isolate's end.
  Future<void> stop() {
    final stopped = Completer<void>();
    _ended.close();
    final exit = _exit..listen((_) => stopped.complete());
    _isolate.addOnExitListener(exit.sendPort);
    _commands?.send(DisposeAgent());
    _isolate.kill(priority: Isolate.beforeNextEvent);
    return stopped.future.whenComplete(() {
      exit.close();
      _reply.close();
    });
  }
}

/// The host's blackboard, as GNA's end-to-end agents read it, at [url], the
/// run's keys under [run].
class Blackboard {
  final String url;
  final String run;
  final HttpClient _http = HttpClient();
  Blackboard(this.url, this.run);

  Future<void> put(String key, String value) async {
    final req = await _http.putUrl(Uri.parse('$url/$run-$key'));
    req.write(value);
    await (await req.close()).drain<void>();
  }

  /// The value of [key], once it is set: the blackboard answers a long wait
  /// with it, or with 408, and it is asked again.
  Future<String> wait(String key) async {
    while (true) {
      final req = await _http.getUrl(Uri.parse('$url/wait/$run-$key?t=60'));
      final res = await req.close();
      final body = await utf8.decodeStream(res);
      if (res.statusCode == HttpStatus.ok) return body;
    }
  }
}

/// The callee of a run between two machines: it gives the blackboard its key
/// and [address], null where its peer finds it by DNS-SD; takes the caller's
/// key; and answers the cold call.  It returns the underlay the call came
/// over.
Future<String> runCallee(Blackboard board, AgentEntry entry,
    {required String persona,
    required String program,
    required String rootSelf,
    required String? address}) async {
  await board.put('callee_key', entry.keyPair.pub.hex);
  await board.put('callee_address', address ?? '');
  final callerKey = PubKey.fromHex(await board.wait('caller_key'));
  entry.declare(AgentDeclarations(peers: [(key: callerKey, address: null)]));
  final run = await AgentRun.spawn(entry,
      persona: persona,
      program: program,
      rootSelf: rootSelf,
      person: calleePerson());
  await board.put('callee_ready', 'ready');
  final receipt = await run.first((l) => l.startsWith('MAD_RECV from '));
  await run.first((l) => l.startsWith('< called('));
  final underlay = run.underlay(receipt) ?? 'none';
  await board.wait('caller_done');
  await run.stop();
  return underlay;
}

/// The caller of a run between two machines: it gives the blackboard its key,
/// takes the callee's key and address, cold-calls it once it is ready, and
/// waits for the assignment.  It returns the underlay the assignment came
/// over.
Future<String> runCaller(Blackboard board, AgentEntry entry,
    {required String persona,
    required String program,
    required String rootSelf}) async {
  await board.put('caller_key', entry.keyPair.pub.hex);
  final calleeHex = await board.wait('callee_key');
  final address = await board.wait('callee_address');
  await board.wait('callee_ready');
  entry.declare(AgentDeclarations(peers: [
    (key: PubKey.fromHex(calleeHex), address: address.isEmpty ? null : address),
  ]));
  final run = await AgentRun.spawn(entry,
      persona: persona,
      program: program,
      rootSelf: rootSelf,
      person: callerPerson(calleeHex));
  await run.first((l) => l.startsWith('< answered(forty_two'));
  final receipt = await run.first((l) => l.startsWith('MAD_RECV from '));
  await board.put('caller_done', 'done');
  await run.stop();
  return run.underlay(receipt) ?? 'none';
}

/// The second start of a run's agent: the entry opens again on the agent's
/// directory, and the key is the first start's and the layer's identity.
/// Returns what differs, or null.
Future<String?> secondStart(AgentEntry first,
    {required String persona,
    required String directory,
    required AgentLayerMaker makeLayer}) async {
  final key = first.keyPair.pub;
  await first.close();
  final second = await AgentEntry.open(
      persona: persona, directory: directory, makeLayer: makeLayer);
  final identity = second.layer.network.getIdentity();
  await second.close();
  if (second.keyPair.pub != key) {
    return 'the second start has key ${second.keyPair.pub.hex}, and the '
        'first had ${key.hex}';
  }
  if (identity != key) {
    return 'the layer\'s identity is ${identity.hex}, and the key ${key.hex}';
  }
  return null;
}
