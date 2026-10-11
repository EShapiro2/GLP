/// A madGLP agent hosted over a `GlpNetwork`: the runtime (a `GlpEngine` in
/// madGLP mode) bound to the network as the runtime's own hosts bind the
/// simulation's (glp_runtime `AgentRuntime`, `IsolateManager`), for the
/// adapter's tests.
///
/// The runtime's hosts construct the simulation's network themselves and name
/// agents symbolically, resolving the names to keys through the simulation's
/// directory; neither takes a network from outside.  Over a real network "the
/// name of an agent p is its public key" (IGLP, Remark "Agent Names"), so this
/// host names its agent by its key, presented as 64 lowercase hex characters
/// as `peer_address/2` presents a peer, and resolves a destination's name to
/// its key by reading the hex.  The runtime itself is used unchanged.
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';

class GlpAdapterHost {
  /// The agent's network, the adapter under test.
  final glp.GlpNetwork network;

  /// The agent's name: its public key's hex.
  final String name;

  final GlpEngine _engine;
  late final Scheduler _scheduler;

  /// Every line the agent sent its person (`send_to_user/1`), in order.
  final List<String> outputs = [];
  final StreamController<String> _lines = StreamController.broadcast();

  /// The agent's person's lines as they come.
  Stream<String> get lines => _lines.stream;

  /// Each payload the runtime handed the network ([glp.GlpNetwork.send]) and
  /// each the network delivered ([glp.GlpNetwork.onMessageReceived]), by the
  /// peer's name.
  final List<String> sentTo = [];
  final List<String> receivedFrom = [];

  GlpAdapterHost._(this.network, this.name, this._engine);

  /// Boot the agent: its person is [identity], the layer's own key pair, so
  /// the layer, the certificate and `self_key` agree (glp_runtime
  /// `PersonIdentity`); its one program is the module file [program], under
  /// the root of [rootSelfGlpPath]; its entry goal is [entry] applied to the
  /// agent's name, [constants], and the network input, as the runtime's hosts
  /// post it.
  static GlpAdapterHost boot({
    required String rootSelfGlpPath,
    required String program,
    required GrassrootsIdentity identity,
    required glp.GlpNetwork network,
    required String entry,
    List<String> constants = const [],
  }) {
    final pub = glp.PubKey.fromList(identity.publicKey);
    final engine = GlpEngine(
      rootSelfGlpPath: rootSelfGlpPath,
      identity: PersonIdentity(pub, Uint8List.fromList(identity.privateKey)),
    );
    final host = GlpAdapterHost._(network, pub.hex, engine);
    engine.enableMadGLP(agentId: host.name);
    if (!engine.loadFile(program)) {
      throw StateError('the program $program did not load');
    }
    final ctx = engine.madContext!;
    ctx.network = network;
    engine.runtime.outputCallback = (line) {
      host.outputs.add(line);
      host._lines.add(line);
    };

    // Outgoing: the madGLP Send places each message on the channel to its
    // destination, the agent the name denotes, whose key the name is.
    ctx.onMessageReady = (destination, message) {
      host.sentTo.add(destination);
      network.send(glp.PubKey.fromHex(destination),
          Uint8List.fromList(message.payload));
    };

    // Incoming: the Receive transaction, then a drain and the Sends it makes.
    network.onMessageReceived = (sender, payload, messageId, underlay) {
      host.receivedFrom.add(sender.hex);
      ctx.handleIncomingPayload(payload: payload, fromAgent: sender.hex);
      host._run();
    };

    final goal = '$entry(${[
      glpConstantText(host.name),
      for (final c in constants) glpConstantText(c),
      'NetIn?',
    ].join(', ')})';
    final posted = engine.postGoal(goal, inputs: const ['NetIn']);
    ctx.wp.initializeSerializerEntry(posted.inputs['NetIn']!.currentWriterId);
    host._scheduler = posted.scheduler;
    host._run();
    return host;
  }

  /// One drain to quiescence and the Sends it makes (glp_runtime
  /// `Scheduler.drainAndSend`, as the runtime's hosts run an event).
  void _run() {
    _scheduler.drainAndSend(() => _engine.madContext!.flushMessages());
  }

  /// The first line the person is sent that [test] accepts, waiting up to
  /// [timeout].
  Future<String> line(bool Function(String) test,
      {Duration timeout = const Duration(seconds: 60)}) {
    for (final l in outputs) {
      if (test(l)) return Future.value(l);
    }
    return lines.firstWhere(test).timeout(timeout);
  }
}
