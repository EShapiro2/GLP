/// Isolate Manager for madGLP
///
/// Spawns agent isolates based on BootConfig and routes messages between them.
/// Execution is event-driven: agents drain+flush on Start and on each incoming
/// NetworkMsg. There is no tick loop or external clock.
///
/// Termination is external: the caller shuts down isolates when done.
///
/// The network: the manager runs its agents over the layers it is given, one
/// per agent ([AgentLayer]), and builds the simulation's --- its router and a
/// client in each isolate --- only when it is given none (IGLP, Implementation
/// Notes, "Networking Seam", One interface: "transport integration is a
/// backend swap").  Over given layers an agent is named by its key (IGLP,
/// Remark "Agent Names"), so the boot program names it so.  Until 2026-10-12
/// the manager built the simulation's, always.
///
/// See: IGLP app:in-execution (Agent Execution and Boot).

import 'dart:async';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart' show TypeEnvironment;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/wire/payload_codec.dart' show PayloadCodec;
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
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
/// only, no MessageType (IGLP app:in-networking, The contract; Payloads).
class RouterSend extends IsolateMessage {
  final String fromId;
  final String toId;
  final List<int> payload;

  RouterSend(this.fromId, this.toId, this.payload);

  @override
  String toString() => 'RouterSend($fromId->$toId, ${payload.length}B)';
}

/// Agent → router: the agent's layer sets the cold-call trust level of a
/// proximity underlay, as `trust_declare/2` asks (IGLP, Definition "Seam
/// Predicates"); the router enforces it ("The PAN's cold-call trust level is
/// enforced as trust_declare sets it", IGLP appendix-implementation-notes.tex,
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
/// declared by peer_declare is not answered", IGLP, Simulation realisation).
class RouterKnownPeer extends IsolateMessage {
  final String agentId;
  final PubKey pk;
  final bool known;

  RouterKnownPeer(this.agentId, this.pk, this.known);

  @override
  String toString() => 'RouterKnownPeer($agentId, $pk, $known)';
}

/// Router → agent: a delivered message, with the authenticated sender id and the
/// router-assigned messageId.
class Deliver extends IsolateMessage {
  final String fromId;
  final List<int> payload;
  final String messageId;

  Deliver(this.fromId, this.payload, this.messageId);

  @override
  String toString() => 'Deliver(from=$fromId, id=$messageId, ${payload.length}B)';
}

/// UI event from window to agent
class UIEvent extends IsolateMessage {
  final String agentId;
  final List<int> payload;
  UIEvent(this.agentId, this.payload);
}

/// An agent reports that it has finished one unit of work — a `Start` or a
/// `Deliver` — having drained its queue and flushed everything the drain
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

/// An agent's networking layer, given to an [IsolateManager] to run the agent
/// over: the agent's key pair, and how the agent's isolate opens the layer.
///
/// The layer is the agent's (GNA, "Instantiation": "The layer is created once
/// per agent"), and it is opened in the agent's own isolate, where its events
/// are the agent's (IGLP, Implementation Notes, "Event-driven execution":
/// "Each agent runs in its own isolate and is event-driven"): an instance
/// holds sockets and callbacks, which no port carries to another isolate, so
/// the manager is given how to open it rather than the instance.
class AgentLayer {
  /// The agent's Ed25519 key pair, the layer's.  Its public key is the agent's
  /// name (IGLP, Remark "Agent Names"); the engine holds the pair as the
  /// person's identity, so the certificate its compiler writes, self_key/1
  /// and sign/3 are under it.
  final ({PubKey pub, Uint8List priv}) keyPair;

  /// Opens the agent's layer, instantiated with [keyPair] (GNA, "Identity").
  /// Called once, in the agent's isolate, before the agent is reported ready;
  /// sent there, so it must be sendable.
  final Future<GlpNetwork> Function() open;

  const AgentLayer({required this.keyPair, required this.open});

  /// The agent's name: its key, as 64 lowercase hex characters.
  String get name => keyPair.pub.hex;
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
  final String agentId;
  final String goalFunctor;
  final int goalArity; // Arity of the goal (e.g., 2, 3, 4)
  final List<String> goalConstantArgs; // Constant args between agentId and netIn
  final String programSource;
  final String? programDir; // Optional program directory for static linking
  final String rootSelfGlpPath; // Absolute path to programs/self.glp
  final String? bootPath; // The boot file's path, for its own self.glp chain
  final SendPort mainPort;
  final SendPort? uiPort; // null for headless
  final TraceConfig traceConfig;

  /// This agent's Ed25519 key pair: its identity is its public key, held with
  /// its private key by the networking layer (IGLP app:in-networking, The
  /// contract). The agent's GlpNetwork is instantiated with it (GNA,
  /// "Identity").
  final ({PubKey pub, Uint8List priv}) keyPair;

  /// The simulation's shared identifier–key directory, published to every
  /// adapter (§4); null over a given layer, which names agents by key.
  final NetworkDirectory? directory;

  /// Opens the agent's given layer ([AgentLayer.open]); null in the
  /// simulation, whose client the isolate builds over [directory].
  final Future<GlpNetwork> Function()? openNetwork;

  AgentConfig({
    required this.agentId,
    required this.goalFunctor,
    this.goalArity = 2,
    this.goalConstantArgs = const [],
    required this.programSource,
    this.programDir,
    required this.rootSelfGlpPath,
    this.bootPath,
    required this.mainPort,
    required this.keyPair,
    required this.directory,
    this.openNetwork,
    this.uiPort,
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

  /// The layers the manager was given, by agent name, or null, and the
  /// manager builds the simulation's.
  final Map<String, AgentLayer>? _layers;

  /// The simulation router: owns the directory, adjacency, trust, queues, and
  /// messageId assignment, and routes all inter-agent traffic (IGLP
  /// app:in-networking, Simulation realisation).  Built only where the manager
  /// was given no layers; over given layers each agent's traffic is its
  /// layer's.
  final SimulationRouter? _router;

  /// A manager over the layers [layers], one per agent it is to boot, each
  /// agent named by its layer's key; or, given none, over the simulation's.
  IsolateManager({List<AgentLayer>? layers})
      : _layers = layers == null ? null : {for (final l in layers) l.name: l},
        _router = layers == null ? SimulationRouter() : null;

  /// The simulation's router, for its harness controls; refused over given
  /// layers, where no router routes.
  SimulationRouter _simulation(String control) {
    final router = _router;
    if (router == null) {
      throw StateError('$control is a control of the simulation\'s router, '
          'and this manager runs over given layers');
    }
    return router;
  }

  /// Trace configuration (set via boot)
  TraceConfig _traceConfig = TraceConfig.off;

  /// Callback for UI output from agents (for Flutter integration)
  void Function(String agentId, Term message)? onUIOutput;

  /// Units of work handed to each agent — one per `Start`, one per `Deliver`.
  final Map<String, int> _workSent = {};

  /// Units of work each agent has reported finished, via [AgentIdle].
  final Map<String, int> _workDone = {};

  /// Awaiting [settle]; completed when the system is quiescent.
  Completer<void>? _settle;

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
  /// a backstop for a system that never settles — it throws rather than passing
  /// quietly, since a play that cannot settle is a failure and not a slow pass.
  ///
  /// Over given layers it waits for the work the manager handed out, each
  /// agent's [Start]: the layers deliver the agents' traffic themselves, and a
  /// host does not see a message in flight on a real network (IGLP,
  /// "Event-driven execution": "Agents do not detect or report termination").
  Future<void> settle(
      {Duration timeout = const Duration(seconds: 30)}) async {
    if (_faults.isNotEmpty) {
      throw StateError('an agent faulted before settling: ${_faults.first}');
    }
    if (_quiescent) return;
    final c = Completer<void>();
    _settle = c;
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

  /// Record that [agentId] faulted, and fail any pending [settle] with it.
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
  }

  /// [agentId]'s isolate has exited. An exit while the agent still owes work is
  /// the hang: that work can no longer be reported, so it is a fault. An exit
  /// owing nothing is teardown and is ignored.
  void _noteIsolateExit(String agentId) {
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
  /// Returns when all agents are ready (but not yet started).
  Future<void> boot(BootConfig config, {TraceConfig traceConfig = TraceConfig.off}) async {
    _traceConfig = traceConfig;
    final readyCompleter = Completer<void>();
    var readyCount = 0;
    final expectedCount = config.directives.length;

    final keyPairs = <String, ({PubKey pub, Uint8List priv})>{};
    final router = _router;
    final layers = _layers;
    if (layers != null) {
      // 1. Over given layers each agent is the layer's: its key pair is the
      //    layer's, and its name the key, which the boot program names it by
      //    (IGLP, Remark "Agent Names"; "Boot": "each body goal G@p directs the
      //    runtime to spawn an isolate for agent p").  No key is made up for
      //    an agent the manager holds no layer for.
      for (final directive in config.directives) {
        final layer = layers[directive.agentId];
        if (layer == null) {
          throw StateError('agent ${directive.agentId} has no layer: over a '
              'given network an agent is named by its key (IGLP, Remark '
              '"Agent Names"), and the manager was given the layers of '
              '${layers.keys.join(', ')}');
        }
        keyPairs[directive.agentId] = layer.keyPair;
      }
    } else {
      // 1. Generate an Ed25519 key pair per agent and populate the directory
      //    (IGLP app:in-networking, Simulation realisation). The boot harness
      //    sets the PAN level Open for the plays, the underlay of every
      //    simulated encounter; an agent's own trust_declare/2 sets its levels
      //    after ([RouterTrust]).
      for (final directive in config.directives) {
        final kp = generateKeyPair();
        keyPairs[directive.agentId] = kp;
        router!.register(directive.agentId, kp.pub);
        router.setTrustLevel(
            directive.agentId, ProximityUnderlay.pan, TrustLevel.open);
      }

      // 2. The router delivers to the destination agent's isolate port — the
      //    seam's simulation transport.  The sender is the agent the directory
      //    names by the key the router authenticated; a key it names no agent
      //    by is a fault --- the layer authenticates every sender --- and the
      //    message is dropped and reported (GLP #3 Cowork, 2026-10-11 12:59
      //    UTC, "01:05").  Until 2026-10-12 such a sender was named '?', and
      //    the message delivered.
      router!.onDeliver = (toId, fromPk, payload, messageId, t) {
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
        port.send(Deliver(fromId, payload, messageId));
      };
    }

    // Single listener for all messages
    _mainPort.listen((msg) {
      // Handle Ready messages for boot completion
      if (msg is Ready && !readyCompleter.isCompleted) {
        _agentPorts[msg.agentId] = msg.sendPort;
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

    // 3. Spawn isolates with the key pair and the complete directory.
    for (final directive in config.directives) {
      final agentConfig = AgentConfig(
        agentId: directive.agentId,
        goalFunctor: directive.goalFunctor,
        goalArity: directive.goalArity,
        goalConstantArgs: directive.constantArgs,
        programSource: config.source,
        programDir: config.programDir,
        rootSelfGlpPath: config.rootSelfGlpPath,
        bootPath: config.bootPath,
        mainPort: _mainPort.sendPort,
        keyPair: keyPairs[directive.agentId]!,
        directory: router?.directory,
        openNetwork: layers?[directive.agentId]!.open,
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
    await readyCompleter.future;
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
  /// enforces on the first contacts it routes to it: the boot harness's, or
  /// the one the agent declared with trust_declare/2.  The simulation's only.
  TrustLevel trustLevelOf(String agentId, ProximityUnderlay underlay) =>
      _simulation('trustLevelOf').trustLevelOf(agentId, underlay);

  /// Start all agents.
  void start() {
    for (final entry in _agentPorts.entries) {
      _noteWorkSent(entry.key);
      entry.value.send(Start());
    }
  }

  /// Inject a UI event to an agent (for testing).
  void injectUIEvent(String agentId, Term message) {
    final port = _agentPorts[agentId];
    if (port == null) {
      print('[IsolateManager] WARNING: Unknown agent $agentId');
      return;
    }

    // Serialize the message
    final payload = PayloadCodec.serializeAgentMessage(message);
    port.send(UIEvent(agentId, payload));
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
      // The simulation client's traffic to its router; an agent over a given
      // layer sends none.
      final router = _router;
      if (router == null) {
        print('[IsolateManager] ERROR: $msg reached a manager over given '
            'layers, which has no router');
        return;
      }
      _handleRouterMessage(router, msg);
    }
  }

  /// A simulation client's traffic to the router: a send, a trust level, a
  /// change to its known-peer set.
  void _handleRouterMessage(SimulationRouter router, IsolateMessage msg) {
    if (msg is RouterSend) {
      if (_traceConfig.glp && _isTracingAgent(msg.fromId)) {
        print('[${msg.fromId}] → send to ${msg.toId}');
      }
      router.routeSend(msg.fromId, msg.toId, Uint8List.fromList(msg.payload));

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
    if (!_traceConfig.glp && !_traceConfig.mad) return false;
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
void _agentIsolateEntry(AgentConfig config) async {
  final agentId = config.agentId;
  final receivePort = ReceivePort();
  final tc = config.traceConfig;

  // Infrastructure log: only prints when MAD tracing is on
  void log(String msg) {
    if (tc.mad) print('[$agentId] $msg');
  }

  log('Starting isolate');

  // Create GlpEngine — the ONE way to run GLP programs.
  // The engine holds the agent's key pair as the person's identity from
  // construction, so the certificate its compiler writes, self_key/1 and
  // sign/3 are under the key the networking layer below is given.
  // The load refuses a program that does not typecheck: this isolate ran with
  // the checker's errors printed as warnings until 2026-09-18.
  final engine = GlpEngine(
      rootSelfGlpPath: config.rootSelfGlpPath,
      identity: PersonIdentity(config.keyPair.pub, config.keyPair.priv));

  // Enable madGLP mode (creates the MadContext)
  engine.enableMadGLP(agentId: agentId);

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
      engine.loadSource(config.programSource,
          filename: 'program', scope: bootScope());
      log('Program loaded via program linking (${config.programDir}) + boot source');
    } else {
      // No program directory: the boot source, its boot clause stripped, is
      // the program.
      engine.loadSource(config.programSource,
          filename: 'program', scope: bootScope());
      log('Program loaded via GlpEngine (root self.glp + user code)');
    }
  } catch (e, st) {
    print('[$agentId] ERROR: init failed during load: $e');
    config.mainPort.send(AgentInitFailed(agentId, '$e\n$st'));
    return;
  }
  engine.debugTrace = tc.glp;  // Enable GLP trace only when requested
  final ctx = engine.madContext!;
  final runtime = engine.runtime;

  // The agent's goal G, posted with the network-input reader as its last
  // argument (IGLP, Implementation Notes, "Boot"), the agent's id first and
  // the spawn directive's constants between them:
  //   Arity 2: agent_init(agentId, netIn)
  //   Arity 3: child_init(agentId, playNum, netIn)
  //   Arity 4: parent_init(agentId, childName, playNum, netIn)
  // It is posted by the engine's one posting call, which checks it as a body
  // goal before it runs (TGLP modules.tex, "Type-Compatible Attestation
  // Between Agents": the initial goal posted to the runtime, "at boot or
  // interactively", "is type-checked before execution as a body goal") and
  // refuses it where it does not check or names no entry point; the agent
  // then fails to initialise, naming the refusal.  Until 2026-10-04 it was
  // put on the queue here, by its label, unchecked.  The goal carries the
  // program's module value, as every posted goal does: self_module/1 returns
  // it, sign/3 puts its source identity into a signed term, and every goal
  // spawned from it inherits it.
  final arity = config.goalArity;
  final goalLabel = '${config.goalFunctor}/$arity';
  final goalArgs = [
    glpConstantText(agentId),
    for (final c in config.goalConstantArgs)
      int.tryParse(c)?.toString() ?? glpConstantText(c),
    'NetIn?',
  ];
  final PostedGoal posted;
  try {
    if (goalArgs.length != arity) {
      throw GoalRefused('$goalLabel takes $arity arguments, and the spawn '
          'directive gives ${goalArgs.length}');
    }
    posted = engine.postGoal('${config.goalFunctor}(${goalArgs.join(', ')})',
        inputs: const ['NetIn']);
  } catch (e) {
    print('[$agentId] ERROR: Goal $goalLabel refused: $e');  // Always print errors
    config.mainPort.send(AgentInitFailed(agentId, 'Goal $goalLabel refused: $e'));
    return;
  }
  log('Posted ${config.goalFunctor}/$arity');

  // Initialize the permanent index-0 serializer entry for network input
  // Spec Section 4.1: "At boot time, each agent p creates a permanent entry
  // at index 0 mapping `_r(p, 0)` to the local writer N_p for p's network
  // input stream." --- the writer of the goal's network input, whose reader
  // the goal holds.
  final netInWriter = posted.inputs['NetIn']!.currentWriterId;
  ctx.wp.initializeSerializerEntry(netInWriter);
  log('Serializer entry initialized at index 0, netIn=$netInWriter');

  // The scheduler over the code image the goal was posted on, and one event's
  // work on it: reduce until quiescent, then perform the Sends.
  final scheduler = posted.scheduler;
  void runEvent() =>
      _drainAndSend(scheduler, ctx.flushMessages, agentId, engine.debugTrace);

  // Networking seam (spec §3–4): the agent talks to a GlpNetwork, not the
  // mainPort directly --- the layer it was given, or, where it was given none,
  // the simulation's client.
  final open = config.openNetwork;
  final directory = config.directory;

  // An event a given layer delivers --- a message, a place event --- is the
  // agent's own, and is followed by a run and the Sends it makes (IGLP,
  // Implementation Notes, "Event-driven execution").  It is taken once the code
  // that delivered it has returned, since a layer may deliver from inside a
  // call the runtime made of it and a transaction or a run is never started
  // inside another; and one delivered before the manager starts the agent is
  // held until it does, so that the agent runs nothing before [Start].  A throw
  // is reported as the agent's fault.
  var started = false;
  final heldEvents = <void Function()>[];
  void takeEvent(void Function() event) {
    try {
      event();
    } catch (e, st) {
      print('[$agentId] ERROR: uncaught while taking a layer event: $e');
      print(st);
      config.mainPort.send(AgentFaulted(agentId, '$e'));
    }
  }

  void layerEvent(void Function() event) {
    if (!started) {
      heldEvents.add(event);
      return;
    }
    scheduleMicrotask(() => takeEvent(event));
  }

  final GlpNetwork network;
  if (open != null) {
    // A given layer, opened here, in the agent's isolate, where its events
    // are the agent's.  Its key is the agent's: the one the agent is named by
    // and the engine holds as the person's identity.
    try {
      network = await open();
      if (network.getIdentity() != config.keyPair.pub) {
        throw StateError('the layer opened for $agentId has the key '
            '${network.getIdentity().hex}: an agent has one key, its name');
      }
    } catch (e, st) {
      print('[$agentId] ERROR: init failed opening its layer: $e');
      config.mainPort.send(AgentInitFailed(agentId, '$e\n$st'));
      return;
    }
    // Back the seam predicates (IGLP Definition Seam Predicates).
    ctx.network = network;

    // Outgoing: the destination's name is its key (IGLP, Remark "Agent
    // Names").  A destination that is no key names no agent over the layer,
    // and its message is not sent.
    ctx.onMessageReady = (destId, msg) {
      final PubKey pk;
      try {
        pk = PubKey.fromHex(destId);
      } on Object catch (e) {
        print('[$agentId] ERROR: a message to $destId, which is no key and '
            'so names no agent over the network, is not sent: $e');
        return;
      }
      log('Sending to $destId (${msg.payload.length}B)');
      network.send(pk, Uint8List.fromList(msg.payload));
    };

    // Incoming: the Receive transaction, from the sender the layer
    // authenticated, named by its key (IGLP, The contract: "the sender
    // authenticated by the layer"), then a run and the Sends it makes.
    network.onMessageReceived = (senderPk, payload, messageId, underlay) {
      layerEvent(() {
        log('Incoming payload from ${senderPk.hex} over ${underlay.name} '
            '(${payload.length}B)');
        ctx.handleIncomingPayload(payload: payload, fromAgent: senderPk.hex);
        runEvent();
      });
    };

    // A place event the layer reports binds the place's stream
    // (MadContext.deliverPlaceEvent); the run follows it.
    ctx.onPlaceEventDelivered = () => layerEvent(runEvent);
  } else {
    // The simulation: the client forwards sends to the router (main isolate)
    // over the existing port; deliveries arrive as Deliver messages and fire
    // onMessageReceived.
    final simulation = SimulationNetworkClient(
      selfId: agentId,
      directory: directory!,
      keyPair: config.keyPair,
      sendToRouter: (toId, payload) =>
          config.mainPort.send(RouterSend(agentId, toId, payload)),
      // The agent's trust declarations and known peers reach the router,
      // which enforces them, over the same port as its sends, so that a send
      // after a declaration is routed under it.
      trustToRouter: (underlay, level) =>
          config.mainPort.send(RouterTrust(agentId, underlay, level)),
      knownPeerToRouter: (pk, known) =>
          config.mainPort.send(RouterKnownPeer(agentId, pk, known)),
    );
    network = simulation;
    // Back the seam predicates (IGLP Definition Seam Predicates).
    ctx.network = network;

    // Outgoing (spec §4): ctx.onMessageReady(destId, msg) → network.send.
    ctx.onMessageReady = (destId, msg) {
      final pk = directory.pkOf(destId);
      if (pk == null) {
        print('[$agentId] ERROR: unknown destination $destId');  // Always print errors
        return;
      }
      log('Sending to $destId (${msg.payload.length}B)');
      network.send(pk, Uint8List.fromList(msg.payload));
    };

    // Incoming (spec §4): dispatch on the message kind byte (value/request/
    // acknowledgement) via the MadContext receive entry point.  The sender is
    // the agent the directory names by the key the router authenticated; a
    // key it names no agent by is a fault --- the layer authenticates every
    // sender --- and the message is dropped and reported (GLP #3 Cowork,
    // 2026-10-11 12:59 UTC, "01:05").  Until 2026-10-12 such a sender was
    // named '?', and the message received.
    network.onMessageReceived = (senderPk, payload, messageId, transport) {
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
  }

  log('Network input ready: writer=$netInWriter');

  // What the agent sends to its person is printed under the agent's name, so
  // a harness reading the process's output can tell whose line it is, and is
  // forwarded to the manager, so a harness can assert it (IsolateManager.outputOf).
  runtime.outputCallback = (text) {
    print('[$agentId] $text');
    config.mainPort.send(AgentOutput(agentId, text));
  };

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

  // Signal ready
  config.mainPort.send(Ready(agentId, receivePort.sendPort));

  // Event-driven message handling loop
  await for (final msg in receivePort) {
    try {
    if (msg is Start) {
      // Initial drain+flush: kicks off the agent's goal; then the events a
      // given layer delivered before it, in order.
      runEvent();
      started = true;
      for (final event in heldEvents) {
        takeEvent(event);
      }
      heldEvents.clear();
      config.mainPort.send(AgentIdle(agentId));

    } else if (msg is Deliver) {
      // The simulation router's delivery.
      log('Received delivery from ${msg.fromId} (id=${msg.messageId})');
      final senderPk = directory?.pkOf(msg.fromId);
      if (senderPk != null) {
        network.onMessageReceived?.call(
          senderPk,
          Uint8List.fromList(msg.payload),
          msg.messageId,
          Underlay.pan,
        );
      } else {
        print('[$agentId] ERROR: delivery from unknown ${msg.fromId}');  // Always print errors
      }

      // Drain activated goals and flush any response messages
      runEvent();
      config.mainPort.send(AgentIdle(agentId));

    } else if (msg is UIEvent) {
      // UIEvent handling is for external Flutter UI integration.
      // With the current architecture (actors internal to GLP), this is not used.
      // The actor communicates directly with the agent via channels in GLP code.
      log('Received UI event (not processed - actors are internal)');
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
void _drainAndSend(
    Scheduler scheduler, void Function() send, String agentId, bool debug) {
  final result = scheduler.drainAndSend(send, debug: debug);
  if (result.status == ExecutionStatus.capped) {
    print('[$agentId] ERROR: the program did not quiesce: stopped after '
        '${result.goalsRun} goals with ${scheduler.rt.gq.length} '
        'still queued');
  }
}
