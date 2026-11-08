// The directory's agent and its host (GNA Section 7, "Hosting": "A directory
// is hosted as a rendezvous server is, a headless layer under its own key,
// with a web server for its page and a mail server for its verification
// messages").
//
// The layer is the headless layer under the directory's own key, with its
// server address and the mark of a rendezvous server, so that it answers
// unsolicited WAN contact from any key (GNA Section 2.7), as
// networking/glue/bin/rendezvous_server.dart instantiates it; it declares the
// rendezvous servers its configuration names.  The agent is
// programs/examples/rendezvous/gds_agent.glp, run over the layer through the
// glue's adapter by the runtime's host, AgentRuntime.  What the agent admits,
// and on which stream it asks a person's app for links, is the agent's GLP;
// this host keeps the listings and their links in the store, which it shares
// with the web server, and sends the verification mail.
//
// Between the two, over the agent's person channel (gds_agent.glp, "The
// agent and its host"): the agent reports listing(N, A, Email, Name,
// Visibility), links(A, Links) and unlisting(A); the host answers a listing
// with mailed(N) or refused(N), and asks for links with ask(A, K) whenever
// the store says how many to ask for.
import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';

import 'server.dart';
import 'store.dart';

/// A rendezvous server the directory's layer declares: its key and address.
typedef RendezvousServerEntry = ({Uint8List key, String address});

class DirectoryAgent {
  final DirectoryService service;
  final HeadlessGrassrootsNetwork layer;
  final AgentRuntime agent;

  /// The directory's key.
  final glp.PubKey key;

  /// Called with each line the runtime writes for a terminal, for a log.
  void Function(String line)? onLine;

  Future<void> _inputs = Future.value();

  DirectoryAgent._(this.service, this.layer, this.agent, this.key);

  Store get store => service.store;

  /// Builds the layer and the agent and starts them: [identity] the
  /// directory's key pair, [port] and [publicAddress] its server address,
  /// [queueFile] the layer's fair-delivery queue, [program] gds_agent.glp and
  /// [rootSelf] the root self.glp.  Returns null where the UDP port cannot be
  /// bound.
  static Future<DirectoryAgent?> start({
    required DirectoryService service,
    required GrassrootsIdentity identity,
    required int port,
    required String publicAddress,
    required OutboundQueueStore queue,
    required String program,
    required String rootSelf,
    List<RendezvousServerEntry> rendezvous = const [],
  }) async {
    final sodium = await initHeadlessSodium();
    final layer = HeadlessGrassrootsNetwork(
      identity: identity,
      serverAddress: ServerAddress(listenPort: port, publicAddress: publicAddress),
      rendezvousServer: true,
      sodium: sodium,
      outboundQueueStore: queue,
    );
    final key = glp.PubKey.fromList(identity.publicKey);
    final agent = AgentRuntime(
      agentId: 'directory',
      program: program,
      rootSelfGlpPath: rootSelf,
      keyPair: (pub: key, priv: Uint8List.fromList(identity.privateKey)),
      network: HeadlessGlpNetwork(layer),
    );
    final d = DirectoryAgent._(service, layer, agent, key);
    agent.onUserOutput = d._report;
    agent.onOutput = (line) => d.onLine?.call(line);
    service.store.onAsk = d._ask;
    // The agent is bound to its layer before the layer starts, so that no
    // message the layer delivers finds no agent.
    await agent.initialize();
    for (final r in rendezvous) {
      layer.wan.putRendezvousServer(r.key, r.address);
    }
    if (!await layer.start()) {
      service.store.onAsk = null;
      agent.dispose();
      await layer.dispose();
      return null;
    }
    return d;
  }

  /// The UDP port the layer is bound to.
  int? get boundPort => layer.boundPort;

  /// The host's commands reach the agent in the order they are made.
  void _input(rt.Term t) {
    _inputs = _inputs.then((_) => agent.injectUserInput(t));
  }

  void _ask(String person, int count) =>
      _input(rt.StructTerm('ask', [rt.ConstTerm(person), rt.ConstTerm(count)]));

  /// A report of the agent's.
  void _report(rt.Term t) {
    if (t is! rt.StructTerm) return;
    final a = t.args;
    switch ((t.functor, a.length)) {
      case ('listing', 5):
        _listing(_int(a[0]), _str(a[1]), _str(a[2]), a[3], _str(a[4]));
      case ('links', 2):
        store.supply(_str(a[0]), [
          for (final l in _list(a[1]))
            if (l is rt.StructTerm && l.functor == 'link' && l.args.length == 2)
              Link(_str(l.args[0]), _int(l.args[1])),
        ]);
      case ('unlisting', 1):
        store.unlist(_str(a[0]));
    }
  }

  /// GDS:2's verification mail for list request [n] of [person]: mailed(N)
  /// once it is sent, refused(N) where the store refuses it or it cannot be
  /// sent.
  Future<void> _listing(int n, String person, String email, rt.Term name, String visibility) async {
    String? refusal;
    try {
      refusal = await service.list(person, email, name: _name(name), listed: visibility == 'listed');
    } catch (e) {
      refusal = 'the mail could not be sent: $e';
    }
    onLine?.call('listing $n of $person, $email: ${refusal ?? 'mailed'}');
    _input(rt.StructTerm(refusal == null ? 'mailed' : 'refused', [rt.ConstTerm(n)]));
  }

  static String? _name(rt.Term t) =>
      (t is rt.StructTerm && t.functor == 'name' && t.args.length == 1) ? _str(t.args[0]) : null;

  static String _str(rt.Term t) => (t as rt.ConstTerm).value as String;

  static int _int(rt.Term t) => (t as rt.ConstTerm).value as int;

  static List<rt.Term> _list(rt.Term t) {
    final out = <rt.Term>[];
    var c = t;
    while (c is rt.StructTerm && c.functor == '.' && c.args.length == 2) {
      out.add(c.args[0]);
      c = c.args[1];
    }
    return out;
  }

  Future<void> dispose() async {
    service.store.onAsk = null;
    await _inputs;
    agent.dispose();
    await layer.dispose();
  }
}
