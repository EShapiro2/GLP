/// Isolate Manager for madGLP
///
/// Spawns agent isolates based on BootConfig and routes messages between them.
/// Execution is event-driven: agents drain+flush on Start and on each incoming
/// NetworkMsg. There is no tick loop or external clock.
///
/// Termination is external: the caller shuts down isolates when done.
///
/// The network is the simulation's --- its router and a client in each
/// isolate: "A boot is for a simulation; a deployed agent is booted by its
/// application" (IGLP, Implementation Notes, "Boot"), as AgentRuntime boots
/// one over the layer it is given.  When the manager spawns an agent, before
/// anything is compiled or sent, it makes the agent's key pair from the
/// SHA-256 of the clock, the device's hardware identifiers and the persona's
/// name, the agent's name p in the boot clause, combined by exclusive or; the
/// router knows the agent by it from the spawn, and the agent's client and
/// its compiler hold it.  On 2026-11-02 the key was for some hours made at
/// the agent's first call of self_key/1, and messages held until then; until
/// 2026-11-02 the manager also ran agents over given layers, each named in
/// the boot clause by a key made before the boot.
///
/// See: IGLP app:in-execution (Agent Execution and Boot).

import 'dart:async';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart' show TypeEnvironment;
import 'package:glp_runtime/runtime/body_kernels.dart' show formatTerm;
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/runtime.dart' show GlpRuntime;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart'
    show LayerSigner, PersonIdentity;
import 'package:glp_runtime/multiagent/mad_context.dart' show MadContext;
import 'package:glp_runtime/multiagent/simulation_network.dart';

/// Message types for inter-isolate communication
sealed class IsolateMessage {}

/// Agent is ready, provides its SendPort
class Ready extends IsolateMessage {
  final String agentId;
  final SendPort sendPort;
  Ready(this.agentId, this.sendPort);
}

/// Signal to start execution
class Start extends IsolateMessage {}

/// Agent → router: an outbound send. The wire carries opaque payload bytes
/// only, no MessageType (IGLP app:in-networking, The contract; Payloads), and
/// the id the agent's `send` returned for the message.
class RouterSend extends IsolateMessage {
  final String fromId;
  final String toId;
  final List<int> payload;
  final String senderMessageId;

  RouterSend(this.fromId, this.toId, this.payload, this.senderMessageId);

  @override
  String toString() => 'RouterSend($fromId->$toId, ${payload.length}B)';
}

/// Agent → router: the agent's layer sets the cold-call trust level of a
/// proximity underlay, as `declare(trust(U, L))` asks (IGLP, Definition "Seam
/// Predicates"); the router enforces it ("The PAN's cold-call trust level is
/// enforced as the trust declaration sets it", IGLP appendix-implementation-notes.tex,
/// Simulation realisation).
class RouterTrust extends IsolateMessage {
  final String agentId;
  final ProximityUnderlay underlay;
  final TrustLevel level;

  RouterTrust(this.agentId, this.underlay, this.level);

  @override
  String toString() => 'RouterTrust($agentId, $underlay, $level)';
}

/// Agent → router: the agent's layer adds a key to its known-peer set
/// ([known] true, putKnownPeer) or removes it ([known] false,
/// removeKnownPeer) (GNA, "Known Peers"); the router consults the set in the
/// Closed first-contact rule ("under closed, first contact from an agent not
/// declared a peer is not answered", IGLP, Simulation realisation).
class RouterKnownPeer extends IsolateMessage {
  final String agentId;
  final PubKey pk;
  final bool known;

  RouterKnownPeer(this.agentId, this.pk, this.known);

  @override
  String toString() => 'RouterKnownPeer($agentId, $pk, $known)';
}

/// Router → agent: a delivered message, with the authenticated sender id, the
/// router-assigned messageId, and the senderMessageId the sender's `send`
/// returned.
class Deliver extends IsolateMessage {
  final String fromId;
  final List<int> payload;
  final String messageId;
  final String senderMessageId;

  Deliver(this.fromId, this.payload, this.messageId, this.senderMessageId);

  @override
  String toString() => 'Deliver(from=$fromId, id=$messageId, ${payload.length}B)';
}

/// Router → agent: what the layer reports of a peer, its discovery, its
/// connection, or its disconnection, over [underlay], the PAN throughout
/// (IGLP, Implementation Notes, "Networking Seam", The contract: "the layer
/// reports peer discovery, connection, disconnection, and reachability";
/// "Simulation realisation": "the harness cuts and restores" the adjacency
/// relation, "the reported underlay is the PAN throughout").  The peer is
/// named by its id in the directory.  One unit of work, answered with
/// [AgentIdle].  Until 2026-11-07 no connectivity event reached an agent
/// isolate, and peer_app/2 answered `none` for every peer under `:boot`.
///
/// A connection carries the [application] the session with the peer is
/// attested with: the app the harness names ([IsolateManager.boot]), "The
/// router ... reports every peer attested with the app the harness names, its
/// developer, name and version" (IGLP 4a9ad28, "Simulation realisation");
/// null, the session unattested, where the harness names none.
class PeerConnectivity extends IsolateMessage {
  final String peerId;
  final Underlay underlay;
  final ConnectivityEvent event;
  final Application? application;

  PeerConnectivity(this.peerId, this.underlay, this.event, [this.application]);

  @override
  String toString() => 'PeerConnectivity($peerId, ${underlay.name}, '
      '${event.name}${application == null ? '' : ', $application'})';
}

/// An agent reports that it has finished one unit of work — a `Start`, a
/// `Deliver` or a [PeerConnectivity] — having drained its queue and flushed everything the drain
/// produced. It is sent after the flush, so the traffic that work generated is
/// already on its way to the manager when this arrives: a port delivers in
/// order, so the manager never sees an agent idle while its own output is still
/// unaccounted for.
class AgentIdle extends IsolateMessage {
  final String agentId;
  AgentIdle(this.agentId);
}

/// One line an agent sent to its person (`send_to_user/1`), forwarded to the
/// manager so a harness can assert what a play produced rather than that it
/// settled. Until 2026-09-18 the line was only printed inside the isolate.
class AgentOutput extends IsolateMessage {
  final String agentId;
  final String line;
  AgentOutput(this.agentId, this.line);
}

/// Sent by an agent isolate when handling a message throws.
///
/// Without this an uncaught exception ends the isolate silently: the agent stops
/// answering, no AgentIdle ever arrives, and the only symptom is [settle]
/// timing out — which is indistinguishable from a system that is merely slow.
/// A throw and a timing change then look identical, and a diagnosis cannot
/// separate them. This makes the throw visible at the moment it happens.
class AgentFaulted extends IsolateMessage {
  final String agentId;
  final String error;
  AgentFaulted(this.agentId, this.error);
}

/// Sent by an agent isolate when initialization fails (e.g. a load/type-check
/// error or a missing goal label) so the manager fails fast instead of hanging
/// forever waiting for a `Ready` that will never arrive (known-issues Issue 19).
class AgentInitFailed extends IsolateMessage {
  final String agentId;
  final String error;
  AgentInitFailed(this.agentId, this.error);
}

/// Trace configuration for multi-agent tracing
class TraceConfig {
  /// Enable GLP-level trace (reductions, suspensions, failures)
  final bool glp;
  /// Enable MAD infrastructure trace (send, globalize, localize, message routing)
  final bool mad;
  /// Only trace these agents (null = all agents)
  final Set<String>? agents;

  const TraceConfig({this.glp = false, this.mad = false, this.agents});
  static const off = TraceConfig();
}

/// Configuration passed to agent isolate
class AgentConfig {
  /// The agent's name p in the boot clause, the persona's name its key is
  /// made from (IGLP, Implementation Notes, "Boot").
  final String agentId;

  /// The person scripted in GLP at the agent, or null.
  final PersonGoal? person;

  /// The agent's key pair, made when the manager spawns it (IGLP,
  /// Implementation Notes, "Boot").
  final ({PubKey pub, Uint8List priv}) keyPair;

  /// The simulation's directory, every agent of the boot in it from the spawn.
  final NetworkDirectory directory;
  final String programSource;
  final String? programDir; // Optional program directory for static linking
  final String rootSelfGlpPath; // Absolute path to programs/self.glp
  final String? bootPath; // The boot file's path, for its own self.glp chain
  final SendPort mainPort;
  final TraceConfig traceConfig;

  AgentConfig({
    required this.agentId,
    this.person,
    required this.keyPair,
    required this.directory,
    required this.programSource,
    this.programDir,
    required this.rootSelfGlpPath,
    this.bootPath,
    required this.mainPort,
    this.traceConfig = const TraceConfig(),
  });
}

/// Manages agent isolates and message routing.
///
/// Event-driven: agents execute on Start and on each incoming NetworkMsg.
/// Termination is external — the caller calls shutdown() when done.
class IsolateManager {
  final Map<String, SendPort> _agentPorts = {};
  final ReceivePort _mainPort = ReceivePort();

  /// The simulation router: owns the directory, adjacency, trust, queues, and
  /// messageId assignment, and routes all inter-agent traffic (IGLP
  /// app:in-networking, Simulation realisation).
  final SimulationRouter _router = SimulationRouter();

  /// Each agent's public key, made when the manager spawns it.
  final Map<String, PubKey> _keys = {};

  /// [agentId]'s public key, made at its spawn (IGLP, Implementation Notes,
  /// "Boot"), or null where the boot declares no such agent.
  PubKey? keyOf(String agentId) => _keys[agentId];

  SimulationRouter _simulation(String control) => _router;

  /// Trace configuration (set via boot)
  TraceConfig _traceConfig = TraceConfig.off;

  /// Units of work handed to each agent — one per `Start`, one per `Deliver`.
  final Map<String, int> _workSent = {};

  /// Units of work each agent has reported finished, via [AgentIdle].
  final Map<String, int> _workDone = {};

  /// Awaiting [settle]; completed when the system is quiescent.
  Completer<void>? _settle;

  /// Awaiting [boot]; completed when every agent is ready, and failed when one
  /// cannot be: it reports that it could not initialise, it faults, or its
  /// isolate exits before it is ready.  So a boot ends on an event in every
  /// case and never needs a clock to end it (IGLP, Implementation Notes,
  /// "Event-driven execution": "there is no clock and no polling").
  Completer<void>? _booting;

  /// Faults reported by agents, newest last. A non-empty list means at least one
  /// agent threw while handling a message.
  final List<String> _faults = [];

  /// The agent isolates, killed at [shutdown]: a settled system's agents wait
  /// on their ports for ever, and would otherwise keep a long-lived process —
  /// the REPL's `:boot` — alive after the harness is done with them.
  final Map<String, Isolate> _isolates = {};

  /// One per agent isolate, carrying that isolate's `onError` and `onExit`.
  ///
  /// [AgentFaulted] covers a throw the isolate's own handler catches, which is
  /// every synchronous one. It cannot cover an asynchronous uncaught error or an
  /// isolate that exits: both kill the isolate without running that handler, and
  /// the work already counted for it is then never reported. These ports are how
  /// that becomes a named failure instead of a thirty-second silence.
  final Map<String, ReceivePort> _isolateEvents = {};

  /// What the agents have thrown, if anything.
  List<String> get faults => List.unmodifiable(_faults);

  /// What each agent has sent to its person, in order of arrival.
  final Map<String, List<String>> _outputs = {};

  /// The lines [agentId] sent to its person so far (see [AgentOutput]).
  List<String> outputOf(String agentId) =>
      List.unmodifiable(_outputs[agentId] ?? const []);

  /// The messages the router has delivered, by sender and destination.
  final Map<(String, String), int> _delivered = {};

  /// How many messages the router has delivered from [fromId] to [toId], the
  /// sender being the agent the router authenticated.
  int delivered(String fromId, String toId) => _delivered[(fromId, toId)] ?? 0;

  /// Every pair of agents between which the router has delivered a message,
  /// sender first, with the number delivered, in order of the first delivery.
  Map<(String, String), int> get deliveries => Map.unmodifiable(_delivered);

  /// Whether every agent has finished every unit of work handed to it.
  ///
  /// A message parked in the router (a pair under [cut] or [holdDelivery]) was
  /// never handed to anyone, so it does not keep the system busy — parking is
  /// the harness's own doing and releasing it is the harness's to time.
  bool get _quiescent {
    for (final id in _workSent.keys) {
      if ((_workDone[id] ?? 0) < _workSent[id]!) return false;
    }
    return true;
  }

  void _noteWorkSent(String agentId) {
    _workSent[agentId] = (_workSent[agentId] ?? 0) + 1;
  }

  void _noteWorkDone(String agentId) {
    _workDone[agentId] = (_workDone[agentId] ?? 0) + 1;
    final s = _settle;
    if (s != null && !s.isCompleted && _quiescent) {
      _settle = null;
      s.complete();
    }
  }

  /// Wait until every agent has drained and flushed everything it was given,
  /// and the cascade of traffic that produced has itself settled.
  ///
  /// This is the condition the plays actually wait for. It replaces a fixed
  /// wall-clock delay, which is a guess: too short and the play is judged before
  /// it has run, too long and every run pays for the worst machine. [timeout] is
  /// a backstop a caller may give for a system that never settles, which then
  /// throws rather than passing quietly.  Given none, the default, settle waits
  /// for quiescence and for nothing else: every way the wait can end without
  /// it is an event --- an agent's fault, its isolate's uncaught error or exit
  /// owing work --- so its verdict does not depend on how loaded the machine
  /// is (IGLP-coop, 2026-11-02: under load the tests' plays outran their wall
  /// clocks while their agents were still being booted).  A run that never
  /// settles is the suite's watchdog's to report (test/run_all_tests.sh,
  /// Section Q).
  Future<void> settle(
      {Duration? timeout}) async {
    if (_faults.isNotEmpty) {
      throw StateError('an agent faulted before settling: ${_faults.first}');
    }
    if (_quiescent) return;
    final c = Completer<void>();
    _settle = c;
    if (timeout == null) {
      await c.future;
      return;
    }
    await c.future.timeout(timeout, onTimeout: () {
      _settle = null;
      final outstanding = <String>[];
      for (final id in _workSent.keys) {
        final owed = _workSent[id]! - (_workDone[id] ?? 0);
        if (owed > 0) outstanding.add('$id owes $owed');
      }
      throw StateError(
          'agents did not settle within $timeout: ${outstanding.join(', ')}');
    });
  }

  /// Record that [agentId] faulted, and fail any pending [settle] and any
  /// pending [boot] with it.
  ///
  /// Surfaced at once rather than left to time out: the fault is the diagnosis,
  /// and a timeout thirty seconds later is not.
  void _recordFault(String agentId, String error) {
    _faults.add('$agentId: $error');
    print('[IsolateManager] agent $agentId faulted: $error');
    final s = _settle;
    if (s != null && !s.isCompleted) {
      _settle = null;
      s.completeError(StateError('agent $agentId faulted: $error'));
    }
    _failBoot(StateError('agent $agentId faulted: $error'));
  }

  /// Fail a pending [boot] with [error].
  void _failBoot(Object error) {
    final b = _booting;
    if (b != null && !b.isCompleted) b.completeError(error);
  }

  /// [agentId]'s isolate has exited. An exit while the agent still owes work is
  /// the hang: that work can no longer be reported, so it is a fault; so is an
  /// exit during a boot before the agent is ready, since its Ready can no
  /// longer come. An exit owing nothing after the boot is teardown and is
  /// ignored.
  void _noteIsolateExit(String agentId) {
    final b = _booting;
    if (b != null && !b.isCompleted && !_agentPorts.containsKey(agentId)) {
      _recordFault(agentId, 'isolate exited before it was ready');
      return;
    }
    final owed = (_workSent[agentId] ?? 0) - (_workDone[agentId] ?? 0);
    if (owed <= 0) return;
    _recordFault(agentId, 'isolate exited owing $owed unit(s) of work');
  }

  /// Infrastructure log: only prints when MAD tracing is on.
  void _log(String msg) {
    if (_traceConfig.mad) print('[IsolateManager] $msg');
  }

  /// Boot all agents from configuration.
  ///
  /// [application] is the app the harness names: the router reports every
  /// peer connected attested with it, its developer, name and version (IGLP
  /// 4a9ad28, "Simulation realisation"), so peer_app/2 assigns app(D, N, V).
  /// Where the harness names none, a connection is reported unattested.
  ///
  /// Returns when all agents are ready (but not yet started).
  Future<void> boot(BootConfig config,
      {TraceConfig traceConfig = TraceConfig.off,
      Application? application}) async {
    _traceConfig = traceConfig;
    final readyCompleter = Completer<void>();
    _booting = readyCompleter;
    // A failure can be reported while a later agent's isolate is still being
    // spawned, before this boot awaits the completer below; marked handled
    // here, it reaches the await and not the zone as an uncaught error.
    readyCompleter.future.ignore();
    var readyCount = 0;
    final expectedCount = config.directives.length;

    final router = _router;
    // Each agent's key pair, made at its spawn, before anything is compiled
    // or sent, from the SHA-256 of the clock, the device's hardware
    // identifiers and the persona's name, the agent's name p in the boot
    // clause, combined by exclusive or; it is the key the networking layer
    // holds for the agent (IGLP, Implementation Notes, "Boot"), here the
    // router's directory and the agent's client.  "The harness sets no trust
    // level at spawn" (IGLP 45fabfc, "Simulation realisation"): both levels
    // are Closed until the agent declares one ([RouterTrust]).  Until
    // 2026-11-08 the harness set the PAN level Open at every spawn.
    final keyPairs = <String, PersonIdentity>{};
    for (final directive in config.directives) {
      final identity = PersonIdentity.forPersona(directive.agentId);
      keyPairs[directive.agentId] = identity;
      _keys[directive.agentId] = identity.pub;
      router.register(directive.agentId, identity.pub);
      _log('${directive.agentId} spawned with key ${identity.pub.hex}');
    }
    //
    // The router delivers to the destination agent's isolate port --- the
    // seam's simulation transport.  The sender is the agent the directory
    // names by the key the router authenticated; a key it names no agent by
    // is a fault --- the layer authenticates every sender --- and the message
    // is dropped and reported (GLP #3 Cowork, 2026-10-11 12:59 UTC, "01:05").
    router.onDeliver = (toId, fromPk, payload, messageId, senderMessageId, t) {
      final port = _agentPorts[toId];
      if (port == null) {
        print('[IsolateManager] WARNING: Unknown destination $toId');
        return;
      }
      final fromId = router.directory.idOf(fromPk);
      if (fromId == null) {
        _recordFault(toId, 'a message from ${fromPk.hex}, a key the '
            'directory names no agent by, is dropped');
        return;
      }
      _noteWorkSent(toId);
      _delivered.update((fromId, toId), (n) => n + 1, ifAbsent: () => 1);
      port.send(Deliver(fromId, payload, messageId, senderMessageId));
    };

    // The router reports each agent's connections and disconnections to its
    // isolate, as the layer does (IGLP, Implementation Notes, "Networking
    // Seam", The contract: "the layer reports peer discovery, connection,
    // disconnection, and reachability"): every adjacent pair connected once
    // every agent is ready ([SimulationRouter.bootComplete]), and a pair the
    // harness cuts and restores ([cut], [restore]).  Each report is one unit
    // of work handed to the agent, so a settle waits until it is recorded.
    // A connection is reported attested with the app the harness names,
    // [application] (IGLP 4a9ad28, "Simulation realisation"), unattested
    // where it names none.
    router.onConnectivity = (toId, peerPk, underlay, event) {
      final port = _agentPorts[toId];
      if (port == null) return; // not ready: no isolate to report to yet
      final peerId = router.directory.idOf(peerPk);
      if (peerId == null) return; // the router reports only agents it holds
      _noteWorkSent(toId);
      port.send(PeerConnectivity(peerId, underlay, event,
          event == ConnectivityEvent.connected ? application : null));
    };

    // Single listener for all messages
    _mainPort.listen((msg) {
      // Count Ready messages for boot completion; _handleMessage records
      // the port.
      if (msg is Ready && !readyCompleter.isCompleted) {
        readyCount++;
        if (readyCount == expectedCount) {
          readyCompleter.complete();
        }
      }
      // An isolate that died during init reports it here so boot() fails fast
      // instead of awaiting a Ready that will never come (Issue 19).
      if (msg is AgentInitFailed && !readyCompleter.isCompleted) {
        readyCompleter.completeError(
          StateError('Agent ${msg.agentId} failed to initialize: ${msg.error}'),
        );
        return;
      }
      // Always handle messages via _handleMessage
      _handleMessage(msg);
    });

    // Spawn an isolate per agent, with its key pair and the directory.
    for (final directive in config.directives) {
      final agentConfig = AgentConfig(
        agentId: directive.agentId,
        person: directive.person,
        keyPair: keyPairs[directive.agentId]!.keyPair,
        directory: router.directory,
        programSource: config.source,
        programDir: config.programDir,
        rootSelfGlpPath: config.rootSelfGlpPath,
        bootPath: config.bootPath,
        mainPort: _mainPort.sendPort,
        traceConfig: traceConfig,
      );

      // An uncaught asynchronous error, and an exit, both bypass the isolate's
      // own try/catch. One port carries each: `onError` sends a two-element
      // list, `onExit` the message given here, which is null.
      final events = ReceivePort();
      _isolateEvents[directive.agentId] = events;
      events.listen((event) {
        if (event is List) {
          _recordFault(directive.agentId, 'uncaught async error: ${event.first}');
        } else {
          _noteIsolateExit(directive.agentId);
        }
      });

      _isolates[directive.agentId] = await Isolate.spawn(
          _agentIsolateEntry, agentConfig,
          onError: events.sendPort, onExit: events.sendPort);
    }

    // Wait for all agents to be ready
    try {
      await readyCompleter.future;
    } finally {
      _booting = null;
    }

    // Every agent is ready: "The adjacency relation is total by default,
    // every agent a PAN peer of every other" (IGLP, "Simulation
    // realisation"), so each agent is told of every peer adjacent to it,
    // discovered and connected, before the run starts.
    router.bootComplete();
  }

  /// Harness control: visible disconnection of a pair (IGLP app:in-networking,
  /// Simulation realisation).  The simulation's only.
  void cut(String a, String b) => _simulation('cut').cut(a, b);

  /// Harness control: reverse a [cut], flushing queued messages in order.
  void restore(String a, String b) => _simulation('restore').restore(a, b);

  /// Harness control: invisible delay of a pair's delivery (IGLP
  /// app:in-networking, Simulation realisation).
  void holdDelivery(String a, String b) =>
      _simulation('holdDelivery').holdDelivery(a, b);

  /// Harness control: release a [holdDelivery], flushing in reverse order.
  void releaseDelivery(String a, String b) =>
      _simulation('releaseDelivery').releaseDelivery(a, b);

  /// The cold-call trust level of [underlay] the router holds for [agentId] and
  /// enforces on the first contacts it routes to it: the one the agent
  /// declared with declare(trust(U, L)), Closed until it declares one.  The
  /// simulation's only.
  TrustLevel trustLevelOf(String agentId, ProximityUnderlay underlay) =>
      _simulation('trustLevelOf').trustLevelOf(agentId, underlay);

  /// Start all agents.
  void start() {
    for (final entry in _agentPorts.entries) {
      _noteWorkSent(entry.key);
      entry.value.send(Start());
    }
  }

  /// Shutdown all isolates: close the ports, then kill the isolates, whose
  /// exits then reach no port and are not faults.
  Future<void> shutdown() async {
    _mainPort.close();
    for (final p in _isolateEvents.values) {
      p.close();
    }
    _isolateEvents.clear();
    _agentPorts.clear();
    for (final i in _isolates.values) {
      i.kill(priority: Isolate.immediate);
    }
    _isolates.clear();
  }

  /// Handle messages from agent isolates.
  void _handleMessage(dynamic msg) {
    if (msg is Ready) {
      _log('${msg.agentId} ready');
      _agentPorts[msg.agentId] = msg.sendPort;

    } else if (msg is AgentIdle) {
      _noteWorkDone(msg.agentId);

    } else if (msg is AgentFaulted) {
      _recordFault(msg.agentId, msg.error);

    } else if (msg is AgentOutput) {
      _outputs.putIfAbsent(msg.agentId, () => []).add(msg.line);

    } else if (msg is RouterSend || msg is RouterTrust ||
        msg is RouterKnownPeer) {
      _handleRouterMessage(_router, msg);
    }
  }

  /// A simulation client's traffic to the router: a send, a trust level, a
  /// change to its known-peer set.
  void _handleRouterMessage(SimulationRouter router, IsolateMessage msg) {
    if (msg is RouterSend) {
      if (_traceConfig.glp && _isTracingAgent(msg.fromId)) {
        print('[${msg.fromId}] → send to ${msg.toId}');
      }
      router.routeSend(msg.fromId, msg.toId, Uint8List.fromList(msg.payload),
          msg.senderMessageId);

    } else if (msg is RouterTrust) {
      _log('${msg.agentId} sets trust ${msg.underlay.name} ${msg.level.name}');
      router.setTrustLevel(msg.agentId, msg.underlay, msg.level);

    } else if (msg is RouterKnownPeer) {
      _log('${msg.agentId} ${msg.known ? 'puts' : 'removes'} known peer '
          '${msg.pk}');
      if (msg.known) {
        router.putKnownPeer(msg.agentId, msg.pk);
      } else {
        router.removeKnownPeer(msg.agentId, msg.pk);
      }
    }
  }

  /// Check if an agent should be traced.
  bool _isTracingAgent(String agentId) {
    if (_traceConfig.agents != null && !_traceConfig.agents!.contains(agentId)) return false;
    return true;
  }
}

/// Agent isolate entry point.
///
/// This runs in a separate isolate for each agent.
/// Uses GlpEngine - the ONE way to run GLP programs.
///
/// Event-driven execution: drain+flush on Start and on each incoming NetworkMsg.
/// Its steps, each a function below: the engine made, holding the agent's key
/// pair made at its spawn, and the simulation's client bound with the same
/// pair, before anything is compiled or sent; the program loaded; the agent's
/// goal posted with its scripted person or the runtime's rendering of its
/// output to the person; the serializer entry made; the output and the
/// tracing wired; and the event loop.  A step that fails reports the failure to the manager
/// ([AgentInitFailed]) and the isolate returns.
void _agentIsolateEntry(AgentConfig config) async {
  final agentId = config.agentId;
  final receivePort = ReceivePort();
  final tc = config.traceConfig;

  // Infrastructure log: only prints when MAD tracing is on
  void log(String msg) {
    if (tc.mad) print('[$agentId] $msg');
  }

  log('Starting isolate');

  // The networking seam (IGLP, Implementation Notes, "Networking Seam"): the
  // agent talks to a GlpNetwork, the simulation's client, made with the
  // agent's key pair --- the key the networking layer holds for the agent
  // (IGLP, Implementation Notes, "Boot") --- over the directory, which holds
  // every agent of the boot from the spawn.  The client is made before the
  // engine, which signs by it ([LayerSigner]): the layer holds the private
  // key, the runtime none (the GLP paper f265fec, "Identity and signature";
  // IGLP 07a2b00, "The contract").
  final directory = config.directory;
  final simulation = _simulationClient(config, directory);
  final engine = _makeEngine(config, simulation);
  final ctx = engine.madContext!;
  final runtime = engine.runtime;
  final network = _bindSimulation(simulation, config, ctx, directory, log);

  if (!_loadProgram(engine, config, log)) return;
  engine.debugTrace = tc.glp;  // Enable GLP trace only when requested

  final posted = _postGoal(engine, config, log);
  if (posted == null) return;

  final netInWriter = _initializeSerializerEntry(ctx, posted, log);

  // The scheduler over the code image the goal was posted on, and one event's
  // work on it: reduce until quiescent, then perform the Sends.
  final scheduler = posted.scheduler;
  Future<void> runEvent() => _drainAndSend(scheduler, () {
        ctx.flushMessages();
        return ctx.answered();
      }, agentId, engine.debugTrace);

  log('Network input ready: writer=$netInWriter');

  _wireOutput(runtime, config);
  _wireTracing(tc, agentId, scheduler, ctx);

  // Signal ready
  config.mainPort.send(Ready(agentId, receivePort.sendPort));

  await _eventLoop(
      receivePort, config, ctx, network, directory, runEvent, log);
}

/// The agent's engine, in madGLP mode, signing by [simulation], the
/// simulation's layer, which holds the agent's key pair, made at its spawn
/// from the clock, the device's hardware identifiers and the persona's name
/// (IGLP 07a2b00, Implementation Notes, "Boot"; "The contract": the layer
/// holds the key pair; the GLP paper f265fec, "Identity and signature"):
/// self_key/1 answers its public key, sign/3 signs by the layer under it, and
/// the certificate of every module compiled for the agent names it (IGLP
/// code-format-fragment.tex, the certificate: "the public key of the agent
/// for which the program was compiled").  The load refuses a program that
/// does not typecheck: this isolate ran with the checker's errors printed as
/// warnings until 2026-09-18.
GlpEngine _makeEngine(AgentConfig config, SimulationNetworkClient simulation) {
  final engine = GlpEngine(
      rootSelfGlpPath: config.rootSelfGlpPath,
      identity: LayerSigner(simulation));

  // Enable madGLP mode (creates the MadContext)
  engine.enableMadGLP(agentId: config.agentId);
  return engine;
}

/// Load the agent's program into [engine]; false where the load fails, the
/// failure reported to the manager.
bool _loadProgram(
    GlpEngine engine, AgentConfig config, void Function(String) log) {
  final agentId = config.agentId;
  // Load program code: either via program linking or individual file loading.
  // A load/type-check failure here (e.g. UnknownTypeError) must be reported to
  // the manager, not left to kill the isolate silently — otherwise boot() hangs
  // forever waiting for Ready (Issue 19).
  // Every source handed over here is loaded on top of what the engine already
  // holds and is checked in the scope it is handed over in --- the linked
  // program's entry points and the boot file's ancestor chain, the root among
  // them, the root alone where its path is not known (IGLP, Implementation
  // Notes, "The scope a boot source is checked in", 8aafd09), so a call to a
  // procedure the program does not export is refused here, naming the call.
  // Under the synthetic names alone the check saw the bare root scope and
  // refused agent/7 and ui_mediator/5, which the engine resolves, and
  // send_to_net/1 while enableMadGLP loaded it.
  TypeEnvironment bootScope() => config.bootPath != null
      ? engine.scopeFor(config.bootPath!)
      : engine.scope;
  try {
    if (config.programDir != null) {
      // Program-directory mode: static-link the program, then load boot source on top.
      engine.loadProgram(config.programDir!);
    }
    // The boot source, its boot clause stripped: on top of the linked program,
    // or, with no program directory, the program itself.
    engine.loadSource(config.programSource,
        filename: 'program', scope: bootScope());
    log(config.programDir != null
        ? 'Program loaded via program linking (${config.programDir}) + boot source'
        : 'Program loaded via GlpEngine (root self.glp + user code)');
  } catch (e, st) {
    print('[$agentId] ERROR: init failed during load: $e');
    config.mainPort.send(AgentInitFailed(agentId, '$e\n$st'));
    return false;
  }
  return true;
}

/// Post the agent's goal on [engine]; null where it is refused, the refusal
/// reported to the manager.
///
/// Every agent's initial goal is `agent(ch(_?,_),ch(_?,_))` (GLP-Spec
/// maglp.tex, Definition "Multiagent GLP"), the first channel to the person
/// and the second to the network.  "The runtime holds the writer of the
/// first channel's reader for the person's input, renders the first
/// channel's output to the person, holds the writer of the second channel's
/// reader as the index-0 serializer entry, and takes the second channel's
/// output as the agent's network output" (IGLP, Implementation Notes,
/// "Boot").  So the goal is posted as
///
///   agent(ch(PIn?, POut), ch(NetIn?, NetOut)), send_to_user(POut?),
///   send_to_net(NetOut?)
///
/// the runtime holding PIn's writer and NetIn's: send_to_user/1 renders the
/// output to the person, and send_to_net/1 sends each message of the network
/// output to the agent it names (IGLP, Remark "Network Output Processing":
/// "Each agent spawns a send_to_net goal to process its network output
/// stream").  Where the boot clause names a scripted person P at the agent,
/// "the runtime runs P on p's person channel in the person's place, its writes
/// being the person's input": P is posted with the channel ch(POut?, PIn) in
/// place of send_to_user(POut?), and the runtime holds no writer of the
/// person's input.  The goal is posted by the engine's one posting call,
/// which checks it as a body goal before it runs (TGLP modules.tex,
/// "Type-Compatible Attestation Between Agents") and refuses it where it does
/// not check or names no entry point; the agent then fails to initialise,
/// naming the refusal.
PostedGoal? _postGoal(
    GlpEngine engine, AgentConfig config, void Function(String) log) {
  final agentId = config.agentId;
  final person = config.person;
  final goals = [
    'agent(ch(PIn?, POut), ch(NetIn?, NetOut))',
    person == null ? 'send_to_user(POut?)' : person.text('POut', 'PIn'),
    'send_to_net(NetOut?)',
  ];
  final PostedGoal posted;
  try {
    posted = engine.postGoal(goals.join(', '),
        inputs: [if (person == null) 'PIn', 'NetIn']);
  } catch (e) {
    print('[$agentId] ERROR: Goal agent/2 refused: $e');  // Always print errors
    config.mainPort.send(AgentInitFailed(agentId, 'Goal agent/2 refused: $e'));
    return null;
  }
  log('Posted ${goals.join(', ')}');
  return posted;
}

/// Make the permanent index-0 serializer entry over the network input of
/// [posted]; the writer of the input.
HeapCell _initializeSerializerEntry(
    MadContext ctx, PostedGoal posted, void Function(String) log) {
  // Initialize the permanent index-0 serializer entry for network input
  // IGLP Definition Index-0 Serializer: "At boot time, each agent p creates a permanent entry
  // at index 0 mapping `_r(p, 0)` to the local writer N_p for p's network
  // input stream." --- the writer of the goal's network input, whose reader
  // the goal holds.
  final netInWriter = posted.inputs['NetIn']!.currentWriterId;
  ctx.wp.initializeSerializerEntry(netInWriter);
  log('Serializer entry initialized at index 0, netIn=$netInWriter');
  return netInWriter;
}

/// The simulation's client over [directory], made with the agent's key pair,
/// which it holds and signs with, as the networking layer does (IGLP 07a2b00,
/// "The contract" and "Simulation realisation").  The client forwards sends to
/// the router (main isolate) over the existing port; deliveries arrive as
/// Deliver messages and fire onMessageReceived.
SimulationNetworkClient _simulationClient(
    AgentConfig config, NetworkDirectory directory) {
  final agentId = config.agentId;
  return SimulationNetworkClient(
    selfId: agentId,
    directory: directory,
    keyPair: config.keyPair,
    sendToRouter: (toId, payload, senderMessageId) => config.mainPort
        .send(RouterSend(agentId, toId, payload, senderMessageId)),
    // The agent's trust declarations and known peers reach the router,
    // which enforces them, over the same port as its sends, so that a send
    // after a declaration is routed under it.
    trustToRouter: (underlay, level) =>
        config.mainPort.send(RouterTrust(agentId, underlay, level)),
    knownPeerToRouter: (pk, known) =>
        config.mainPort.send(RouterKnownPeer(agentId, pk, known)),
  );
}

/// Bind [simulation], the simulation's client: the seam predicates, the
/// outgoing messages, and the incoming messages, whose run the event loop
/// makes ([_eventLoop], Deliver).
GlpNetwork _bindSimulation(SimulationNetworkClient simulation,
    AgentConfig config, MadContext ctx, NetworkDirectory directory,
    void Function(String) log) {
  final agentId = config.agentId;
  final GlpNetwork network = simulation;
  // Back the seam predicates (IGLP Definition Seam Predicates), peer_app/2
  // by what the layer reports of each peer's session.
  ctx.network = network;
  network.onPeerConnected = ctx.peerConnected;
  network.onPeerDisconnected = ctx.peerDisconnected;
  // And the nearby declarations by what the layer reports of each peer's
  // discovery and loss over the PAN, onPeerDiscovered and onPeerLost (GNA
  // cfbfeac, Section 3.1): "A nearby declaration on the PAN is given seen for
  // an adjacent agent the declaring agent has declared a peer, and for any
  // other adjacent agent once the PAN trust levels of both are open, at the
  // moment the second opens; it is given gone when the harness cuts the
  // adjacency" (IGLP 45fabfc, "Simulation realisation"), which the router
  // decides ([SimulationRouter]).
  network.onPeerDiscovered = ctx.peerDiscovered;
  network.onPeerLost = ctx.peerLost;
  // And the public-address and network declarations by what the layer
  // reports of the agent's own address and local network, none in the
  // simulation (the GLP paper f265fec, "Networking seam"; IGLP 07a2b00,
  // "Simulation realisation").
  network.onConnectivityStatusChanged =
      (oldAddress, newAddress) => ctx.publicAddressChanged(newAddress);
  network.onNetworkChanged = (oldId, newId) => ctx.networkChanged(newId);

  // A destination, in a global name or in a message, is a key or a readable
  // name standing for one, and the seam resolves both to the one agent (IGLP
  // 4f84612, Implementation Notes, "Simulation realisation"): here by the
  // directory, to the agent's id, before the Send records anything.
  ctx.resolveAgent = directory.agentNamed;

  // Outgoing: ctx.onMessageReady(destId, msg) → network.send.
  ctx.onMessageReady = (destId, msg) {
    final pk = directory.pkOf(destId);
    if (pk == null) {
      print('[$agentId] ERROR: unknown destination $destId');  // Always print errors
      return null;
    }
    log('Sending to $destId (${msg.payload.length}B)');
    // The layer's id for the message is of no use to the runtime.
    return network.send(pk, Uint8List.fromList(msg.payload)).then((_) {});
  };

  // Incoming: dispatch on the message kind byte (value/request/
  // acknowledgement) via the MadContext receive entry point.  The sender is
  // the agent the directory names by the key the router authenticated; a
  // key it names no agent by is a fault --- the layer authenticates every
  // sender --- and the message is dropped and reported (GLP #3 Cowork,
  // 2026-10-11 12:59 UTC, "01:05").
  network.onMessageReceived =
      (senderPk, payload, messageId, senderMessageId, underlay) {
    final fromId = directory.idOf(senderPk);
    if (fromId == null) {
      print('[$agentId] ERROR: a message from ${senderPk.hex}, a key the '
          'directory names no agent by, is dropped');
      config.mainPort.send(AgentFaulted(agentId,
          'a message from ${senderPk.hex}, a key the directory names no '
          'agent by, is dropped'));
      return;
    }
    try {
      log('Incoming payload from $fromId (${payload.length}B)');
      ctx.handleIncomingPayload(payload: payload, fromAgent: fromId);
    } catch (e) {
      print('[$agentId] ERROR handling delivery: $e');  // Always print errors
    }
  };
  return network;
}

/// Wire what the agent sends to its person: printed under the agent's name
/// and forwarded to the manager.
void _wireOutput(GlpRuntime runtime, AgentConfig config) {
  final agentId = config.agentId;
  // What the agent sends to its person is printed under the agent's name, so
  // a harness reading the process's output can tell whose line it is, and is
  // forwarded to the manager, so a harness can assert it (IsolateManager.outputOf).
  // Each element is printed in GLP syntax, as the REPL prints a binding, a
  // quoted constant quoted, so that the line reads back as the term
  // ([formatTerm]; GLP #9 Cowork, 2026-10-20 09:22 UTC, item 4.2): the
  // runtime hands it the term whole ('_output'/1, outputKernel).  Until
  // 2026-10-20 it was handed the text send_to_user/1 shows on a terminal, a
  // constant unquoted ([formatGroundTerm]).
  runtime.outputTermCallback = (term) {
    final text = formatTerm(term);
    print('[$agentId] $text');
    config.mainPort.send(AgentOutput(agentId, text));
  };
}

/// Wire the tracing [tc] asks for: the GLP trace under the agent's name, the
/// MAD trace as it is.
void _wireTracing(
    TraceConfig tc, String agentId, Scheduler scheduler, MadContext ctx) {
  // Set up tracing: lines print directly (no buffering needed without ticks)
  if (tc.glp) {
    scheduler.traceSink = (String line) {
      print('[$agentId] $line');
    };
  }

  if (tc.mad) {
    ctx.traceSink = (String line) {
      // MAD traces already include [MAD agentId] prefix
      print(line);
    };
  }
}

/// The agent's event loop over [receivePort]: [Start], and the simulation
/// router's [Deliver]s and [PeerConnectivity] reports, each one unit of work,
/// answered with [AgentIdle].
Future<void> _eventLoop(
    ReceivePort receivePort,
    AgentConfig config,
    MadContext ctx,
    GlpNetwork network,
    NetworkDirectory directory,
    Future<void> Function() runEvent,
    void Function(String) log) async {
  final agentId = config.agentId;
  // Event-driven message handling loop
  await for (final msg in receivePort) {
    try {
    if (msg is Start) {
      // Initial drain+flush: kicks off the agent's goal.
      await runEvent();
      config.mainPort.send(AgentIdle(agentId));

    } else if (msg is Deliver) {
      // The simulation router's delivery.
      log('Received delivery from ${msg.fromId} (id=${msg.messageId})');
      final senderPk = directory.pkOf(msg.fromId);
      if (senderPk != null) {
        network.onMessageReceived?.call(
          senderPk,
          Uint8List.fromList(msg.payload),
          msg.messageId,
          msg.senderMessageId,
          Underlay.pan,
        );
      } else {
        print('[$agentId] ERROR: delivery from unknown ${msg.fromId}');  // Always print errors
      }

      // Drain activated goals and flush any response messages
      await runEvent();
      // The cycle is complete, and the agent keeps no state, so the message
      // is acknowledged to the layer (IGLP, Implementation Notes,
      // "Persistence"; GNA, Section 2.9, acknowledge(messageId)).
      network.acknowledge(msg.messageId);
      config.mainPort.send(AgentIdle(agentId));

    } else if (msg is PeerConnectivity) {
      // What the layer reports of a peer, surfaced through the network's
      // callbacks as the layer surfaces it, and so recorded for peer_app/2
      // ([MadContext.peerConnected], [MadContext.peerDisconnected]).  A
      // connection carries the app the harness names, the session attested
      // with it, or null, unattested, where the harness names none (IGLP
      // 4a9ad28, "Simulation realisation": the router "reports every peer
      // attested with the app the harness names, its developer, name and
      // version"), as AgentRuntime.onConnectivityEvent reports it.  Until
      // 2026-11-07 every connection was reported unattested.  A connection is also
      // when a message the layer refused is sent again, where M_p holds one
      // (IGLP, The contract: "on a refusal, keeps the message and sends it
      // again with the agent's next Sends, or when the layer reports a peer
      // connected").
      final peerPk = directory.pkOf(msg.peerId);
      if (peerPk == null) {
        print('[$agentId] ERROR: connectivity report about unknown '
            '${msg.peerId}');  // Always print errors
      } else {
        log('Peer ${msg.peerId} ${msg.event.name} over ${msg.underlay.name}');
        // A presence the report puts on a nearby stream is followed by a run,
        // as a place event is (IGLP b20951d, after Definition "Seam
        // Predicates": a presence on a nearby stream is bridged below the
        // madGLP layer, reactivating whatever suspended on its reader).
        final presences = ctx.presencesDelivered;
        final reaches = ctx.reachesDelivered;
        var run = false;
        switch (msg.event) {
          case ConnectivityEvent.connected:
            network.onPeerConnected?.call(peerPk, msg.underlay, msg.application);
            run = ctx.mp.hasSendable;
          case ConnectivityEvent.disconnected:
            network.onPeerDisconnected?.call(peerPk, msg.underlay);
          case ConnectivityEvent.discovered:
            network.onPeerDiscovered?.call(
                peerPk, ProximityUnderlay.values.byName(msg.underlay.name));
          case ConnectivityEvent.lost:
            network.onPeerLost?.call(
                peerPk, ProximityUnderlay.values.byName(msg.underlay.name));
        }
        if (run ||
            ctx.presencesDelivered != presences ||
            ctx.reachesDelivered != reaches) {
          await runEvent();
        }
      }
      config.mainPort.send(AgentIdle(agentId));
    }
    } catch (e, st) {
      // An uncaught throw used to end the isolate here, silently. Report it, and
      // still account the work unit, so a waiter fails on the fault rather than
      // on a timeout that says nothing about what went wrong.
      print('[$agentId] ERROR: uncaught while handling ${msg.runtimeType}: $e');
      print(st);
      config.mainPort.send(AgentFaulted(agentId, '$e'));
      config.mainPort.send(AgentIdle(agentId));
    }
  }
}

/// One event's work in an agent isolate: reduce until quiescent, the queue
/// empty and nothing runnable, then perform the Sends, which [send] makes ---
/// and again while a goal waits on when_idle with the machine idle after them
/// (IGLP eadadcd, Implementation Notes, "The when_idle Guard";
/// [Scheduler.drainAndSend], as the single-isolate runtime's is). One
/// [Scheduler.drainWithStatus] stops at its cycle cap with goals still queued,
/// so each drain is [Scheduler.drainToQuiescence]. Its cap is a safety net
/// against a program that never quiesces: reaching it is reported, never
/// passed over, because everything after a half-run means something other
/// than what it says.
Future<void> _drainAndSend(Scheduler scheduler, Future<void> Function() send,
    String agentId, bool debug) async {
  final result = await scheduler.drainAndSend(send, debug: debug);
  if (result.status == ExecutionStatus.capped) {
    print('[$agentId] ERROR: the program did not quiesce: stopped after '
        '${result.goalsRun} goals with ${scheduler.rt.gq.length} '
        'still queued');
  }
}
