/// AgentRuntime — the host of one agent for the UI.
///
/// Uses GlpEngine (the ONE way to run GLP programs) for compilation,
/// MadContext for madGLP messaging, and Scheduler for execution.
///
/// Boot: GlpEngine loads the root scope, enableMadGLP creates the MadContext,
/// and the agent's one program is loaded --- a program is one
/// compiled value (GSG, Section 4, "Compiled programs as values"), and several
/// sources are not co-loaded into one engine.  The entry goal [goalLabel] is
/// then posted with the agent's id, the person's input stream where the entry
/// takes one, and the network input stream, by the engine's one posting call,
/// which checks it ([GlpEngine.postGoal]).  The person's acts arrive as
/// ground terms and never as text (GSG, Appendix "The Prototype's Screens",
/// and Section 3, "What the super-app grants a mini-app", (ib)).  Network
/// output goes through send_to_net → the _send/3 kernel → MadContext; output to
/// the person through send_to_user → the _output/1 kernel → [onUserOutput],
/// each element a ground term, whole, and in the agent's transcript
/// ([onOutput]) as a terminal shows it.
///
/// The network: the host runs over a [GlpNetwork] it is given, and builds the
/// simulation's only when it is given none (IGLP, Implementation Notes,
/// "Networking Seam", One interface: "A simulation realisation of the same
/// interface backs the tests and plays, so transport integration is a backend
/// swap").  Over a given network the agent is named by its key (IGLP, Remark
/// "Agent Names": "Over a real network, the name of an agent p is its public
/// key, held by the networking layer"), presented as 64 lowercase hex
/// characters, as self_key/1 presents it.  Until 2026-10-12 the host built the
/// simulation's network itself, always.
library;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/body_kernels.dart' show formatGroundTerm;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/multiagent/simulation_network.dart';

/// Agent runtime encapsulating GLP execution, madGLP context, and I/O.
///
/// Usage:
/// 1. Create with the agent's id and the path of its one program, and with
///    the agent's [GlpNetwork] and key pair where it runs over a given one
/// 2. Set callbacks: onUserOutput, onOutput, onLog, onSendMadMessage
/// 3. Call initialize() to compile and start
/// 4. Call injectUserInput(act) with each of the person's acts, a ground term
/// 5. In the simulation, call onMadMessageReceived(from, payload) for network
///    messages; a given network delivers its own
class AgentRuntime {
  /// The host's handle on the agent: its name in the simulation, which names
  /// agents symbolically, and its tag in the log.  Over a given network the
  /// agent's name is its key ([name]).
  final String agentId;

  /// The path of the one program the agent runs: a directory with a self.glp,
  /// linked whole, or a self-contained module file (TGLP, def:program).
  final String program;

  final String rootSelfGlpPath;

  /// Entry-point goal label, e.g. 'agent_init/3', 'agent_init_play/3',
  /// 'parent_init/4', 'child_init/3'.
  final String goalLabel;

  /// Extra arguments inserted between Id (arg 0) and NetIn (last arg).
  /// For example, ['carol', '4'] for parent_init(alice, carol, 4, NetIn).
  final List<String> extraArgs;

  // Callbacks set by UI layer
  /// The agent's transcript, line by line: its own notes and, marked `< `,
  /// each element its program emits to the person, as a terminal shows it
  /// ([formatGroundTerm]).  No screen reads it: a screen is handed the term
  /// itself, by [onUserOutput].
  void Function(String line)? onOutput;

  /// The person's bridge: each element the agent's program emits to its
  /// person by send_to_user/1, handed over whole, a ground term (GLP-Spec
  /// appendix-guards.tex, "Output to the person": send_to_user(Stream) "emits
  /// each element of Stream to the person operating the agent").  Until
  /// 2026-10-11 a screen had only [onOutput]'s line, and parsed it.
  void Function(rt.Term term)? onUserOutput;
  void Function(String tag, String message)? onLog;
  Future<void> Function(String destination, Uint8List payload)? onSendMadMessage;

  /// Connectivity callbacks surfaced from the networking seam, with the
  /// parameters of [GlpNetwork]'s (GNA, "Connection and Reachability" and
  /// "Concepts and Principles", Discovery). The coordinator forwards router
  /// events via [onConnectivityEvent].
  void Function(PubKey pk, Underlay underlay, Object? identity)?
      onPeerConnected;
  void Function(PubKey pk, Underlay underlay)? onPeerDisconnected;
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerDiscovered;

  /// This agent's Ed25519 key pair. Provided by the coordinator, or generated
  /// --- the runtime without a layer generates the person's key itself (IGLP,
  /// "One interface").  Over a given network it is the layer's, which holds
  /// it (IGLP, The contract: "an agent's identity is its public key, held with
  /// its private key by the networking layer"), and must be provided.
  final ({PubKey pub, Uint8List priv})? keyPair;

  /// The shared identifier–key directory of the simulation. Provided by the
  /// coordinator, or built lazily (deterministic routing keys) when absent.
  /// Unused over a given network, which names agents by key.
  final NetworkDirectory directory;

  /// The networking layer the host was given, or null, and the host builds
  /// the simulation's.
  final GlpNetwork? givenNetwork;

  // Runtime state
  /// The agent's networking layer (seam spec §3-4): outgoing via [send],
  /// incoming via [GlpNetwork.onMessageReceived]: the one given, or the
  /// simulation's, which forwards to the coordinator through
  /// [onSendMadMessage].
  GlpNetwork? _network;
  GlpRuntime? _runtime;
  MadContext? _ctx;
  Scheduler? _scheduler;
  InputInjector? _userInput;
  InputInjector? _netInput;
  bool _initialized = false;

  // Stats
  int goalCount = 0;
  int heapVars = 0;
  int wpSize = 0;
  int mpSize = 0;

  // Enable GLP trace output
  bool glpTraceEnabled = true;

  /// The safety net on one event's reduction: how many goals it may run before
  /// the runtime declares the program non-quiescent and stops. It is not a
  /// budget an event may quietly exceed — reaching it is reported as an error
  /// and leaves the queue non-empty — and no program that quiesces reaches it.
  int maxQuiescenceCycles = 200000;

  AgentRuntime({
    required this.agentId,
    required this.program,
    required this.rootSelfGlpPath,
    this.goalLabel = 'agent_init/3',
    this.extraArgs = const [],
    this.keyPair,
    NetworkDirectory? directory,
    GlpNetwork? network,
  })  : directory = directory ?? NetworkDirectory(),
        givenNetwork = network {
    if (network == null) return;
    // The person's key is the layer's: the engine holds it from construction,
    // so the certificate its compiler writes, self_key/1 and sign/3 are under
    // the key the agent is named by.  The layer gives out its public key and
    // keeps the private half, so the host is given the pair as well.
    final kp = keyPair;
    if (kp == null) {
      throw ArgumentError('agent $agentId runs over a given network, whose '
          'key pair names it, and was given none');
    }
    if (kp.pub != network.getIdentity()) {
      throw ArgumentError('agent $agentId was given the key pair of '
          '${kp.pub.hex} and a network whose key is '
          '${network.getIdentity().hex}: an agent has one key, its name');
    }
  }

  /// The agent's name in madGLP: its key, as 64 lowercase hex characters,
  /// over a given network (IGLP, Remark "Agent Names"), and [agentId] in the
  /// simulation, whose agent names are symbolic --- "symbolic agent names are
  /// the simulation realisation's choice".  It is the anchor of every global
  /// name the agent creates and the first argument of its entry goal.
  String get name =>
      givenNetwork?.getIdentity().hex ?? agentId.toLowerCase();

  /// Deterministic routing key for [id], in the simulation only: returns the
  /// directory entry if present, otherwise derives a stable 32-byte key and
  /// registers it. Routing-only — the signing identity is the agent's real
  /// [keyPair].  Over a given network no key is made up: the agent's name is
  /// its key, and a destination is the key it names.
  PubKey _pkFor(String id) {
    final existing = directory.pkOf(id);
    if (existing != null) return existing;
    final src = id.codeUnits;
    final bytes = Uint8List(32);
    for (var i = 0; i < 32; i++) {
      bytes[i] = src.isEmpty ? 0 : (src[i % src.length] + i * 7) & 0xff;
    }
    final pk = PubKey(bytes);
    directory.register(id, pk);
    return pk;
  }

  /// Forward a router connectivity event to this client's callbacks.  The
  /// simulation attests nothing, so a connection carries no identity; a
  /// discovery is over a proximity underlay, the PAN throughout (IGLP,
  /// "Simulation realisation").
  void onConnectivityEvent(
      PubKey peer, Underlay underlay, ConnectivityEvent event) {
    switch (event) {
      case ConnectivityEvent.connected:
        onPeerConnected?.call(peer, underlay, null);
      case ConnectivityEvent.disconnected:
        onPeerDisconnected?.call(peer, underlay);
      case ConnectivityEvent.discovered:
        onPeerDiscovered?.call(
            peer, ProximityUnderlay.values.byName(underlay.name));
    }
  }

  bool get initialized => _initialized;
  GlpRuntime? get runtime => _runtime;
  MadContext? get ctx => _ctx;

  String get _tag => agentId.toUpperCase();

  void _log(String message) {
    onLog?.call(_tag, message);
  }

  void _output(String text) {
    onOutput?.call(text);
  }

  void updateStats() {
    if (_runtime != null && _ctx != null) {
      heapVars = _runtime!.heap.HP;
      wpSize = _ctx!.wp.globalizeEntryCount + _ctx!.wp.localizeEntryCount;
      mpSize = _ctx!.mp.totalLength;
    }
  }

  // =========================================================================
  // INITIALIZATION
  // =========================================================================

  Future<void> initialize() async {
    final selfName = name; // its key over a given network ([name])
    _log('INIT: Starting');
    _output('[INIT] Creating MadContext...');

    // The agent's key pair: provided by the coordinator, or generated. The
    // engine holds it as the person's identity from construction — so the
    // certificate its compiler writes, self_key/1 and sign/3 are all under it —
    // and the networking layer below is given the same pair.
    final kp = keyPair ?? generateKeyPair();

    // Use GlpEngine — the ONE way to run GLP programs.
    final engine = GlpEngine(
        rootSelfGlpPath: rootSelfGlpPath,
        identity: PersonIdentity(kp.pub, kp.priv));

    // Enable madGLP mode (creates the MadContext)
    engine.enableMadGLP(agentId: selfName);

    // Load the one program (TGLP, def:program).  A directory with a self.glp
    // is linked whole.  A self-contained module file is a program too (TGLP
    // modules.tex, "Hierarchy mirrors the file system"), linked the same way:
    // its scope is the one discovery gives it, the self.glp chain from the
    // root with the modules the chain exposes (Compilation, first step; "The
    // -expose directive").  It is no boot source, and is not checked in the
    // scope a boot source handed to the engine is (IGLP, Implementation
    // Notes, "The scope a boot source is checked in"): until 2026-10-07 it
    // was, and on this fresh engine that scope holds the chain without the
    // procedures it exposes (programs/tests/post_goal/exposed).
    if (FileSystemEntity.isDirectorySync(program)) {
      _log('INIT: Loading program from $program');
      engine.loadProgram(program);
      // Diagnostic: check key labels
      final linked = engine.combinedProgram;
      final keyLabels = ['parent_init/4', 'child_init/3', 'agent/4', 'ui_mediator/5', 'merge/3', 'tee/3'];
      for (final key in keyLabels) {
        final pc = linked.labels[key];
        _log('INIT: Label $key -> ${pc != null ? "PC=$pc" : "NOT FOUND"}');
      }
      _log('INIT: Program loaded via program linking ($program), ${linked.labels.length} labels');
    } else {
      engine.loadFile(program);
      _log('INIT: Program loaded from the module $program');
    }

    _runtime = engine.runtime;
    _ctx = engine.madContext;
    _log('INIT: MadContext created');

    // The _output/1 kernel's person: the bridge is handed each element whole
    // ([onUserOutput]), and the transcript shows it as a terminal does.
    _runtime!.outputTermCallback = (term) {
      onUserOutput?.call(term);
      _output('< ${formatGroundTerm(term)}');
    };

    // Networking seam (spec §3-4): route outgoing/incoming through the
    // agent's GlpNetwork instead of serializing OutboundMessages directly ---
    // the one given, or, where none is, the simulation's.  The wire carries
    // the opaque payload bytes only (no MessageType).
    final given = givenNetwork;
    if (given != null) {
      _bindGivenNetwork(given);
    } else {
      _bindSimulationNetwork(selfName, kp);
    }

    _output('[INIT] Loaded GLP program');

    // The entry goal, posted by the engine's one posting call, which checks
    // it as a body goal against the program's declarations before it runs
    // (TGLP modules.tex, "Type-Compatible Attestation Between Agents": the
    // initial goal posted to the runtime "is type-checked before execution
    // as a body goal") and refuses it, throwing, where it does not check or
    // names no entry point.  Until 2026-10-04 it was put on the queue here,
    // by its label, unchecked.
    //
    // Its arguments: the agent's id first and the network input last
    // (IGLP, Implementation Notes, "Boot": the runtime provides "only that
    // reader"), the extra constants between them, and before them the
    // person's input stream where the entry takes one --- an arity-3 entry
    // with no extra constants (agent_init/3, scenario_init/3: Id, UserIn,
    // NetIn).  The plays' actors (actor_init/3: Id, Target, NetIn) and the
    // parent/child entries take constants there instead.  Decided by the
    // goal's shape, not its name.  The goal holds the readers of the two
    // input streams, and the host their writers: the person's, to inject
    // the person's acts, and the network's, the index-0 serializer entry
    // (Spec Section 4.1: permanent entry mapping _r(p, 0) to local writer).
    final goalName = goalLabel.split('/').first;
    final goalArity = int.parse(goalLabel.split('/').last);
    final takesUserIn = goalArity == extraArgs.length + 3;
    final goalArgs = [
      glpConstantText(selfName),
      if (takesUserIn) 'UserIn?',
      for (final extra in extraArgs)
        int.tryParse(extra)?.toString() ?? glpConstantText(extra),
      'NetIn?',
    ];
    if (goalArgs.length != goalArity) {
      throw StateError('$goalLabel takes $goalArity arguments, and the agent '
          'has ${goalArgs.length} for it: its id, '
          '${extraArgs.length} extra and the network input');
    }
    final goalText = '$goalName(${goalArgs.join(', ')})';
    final posted = engine.postGoal(goalText,
        inputs: [if (takesUserIn) 'UserIn', 'NetIn']);
    _log('INIT: posted $goalText');

    final netInWriter = posted.inputs['NetIn']!.currentWriterId;
    _ctx!.wp.initializeSerializerEntry(netInWriter);
    _log('INIT: Serializer entry initialized, netIn=$netInWriter');

    // The person's input stream (Dart injects ground terms), and the network
    // input stream (receives from MadContext).  An entry that takes no person
    // stream is given none, and the person's acts have nowhere to go.
    _userInput = posted.inputs['UserIn'];
    _netInput = posted.inputs['NetIn'];

    // The scheduler over the code image the goal was posted on.
    _scheduler = posted.scheduler..traceSink = (line) => _log('GLP: $line');

    final argsDesc = [selfName, ...extraArgs, 'NetIn'].join(', ');
    _output('[GOAL] Started $goalName($argsDesc)');
    _log('INIT: GQ length before initial run: ${_runtime!.gq.length}');

    // Initial run
    final initStatus = await _runUntilQuiescent();
    _log('INIT: Initial run status: $initStatus, GQ after: ${_runtime!.gq.length}');

    _initialized = true;
    updateStats();
  }

  /// Bind the network the host was given: the agent is named by its key, a
  /// destination is the key it names, and the sender of an incoming message
  /// is named by the key the layer authenticated (IGLP, Remark "Agent Names";
  /// The contract: "the sender authenticated by the layer").  Each event the
  /// layer delivers --- a message, a place event --- is followed by a run
  /// until quiescent and the Sends it makes (IGLP, Implementation Notes,
  /// "Event-driven execution": "On each event the agent binds what the event
  /// delivers, reduces until quiescent ... and then performs its Sends").
  void _bindGivenNetwork(GlpNetwork network) {
    _network = network;
    _ctx!.network = network; // backs the seam predicates (IGLP Definition Seam Predicates)

    // Outgoing: the destination's name is its key.  A destination that is no
    // key names no agent over the layer, and its message is not sent.
    _ctx!.onMessageReady = (destination, msg) async {
      final PubKey pk;
      try {
        pk = PubKey.fromHex(destination);
      } on Object catch (e) {
        _log('MAD_ERROR: a message to $destination, which is no key and so '
            'names no agent over the network, is not sent: $e');
        _output('[ERROR] a message to $destination, which names no agent '
            'over the network, is not sent');
        return;
      }
      network.send(pk, Uint8List.fromList(msg.payload));
    };

    // Incoming: the Receive transaction, from the sender the layer
    // authenticated, then a run and the Sends it makes --- taken once the
    // code that delivered the message has returned, since a layer may deliver
    // from inside a call the runtime made of it, and a transaction or a run
    // is never started inside another.
    network.onMessageReceived = (sender, payload, messageId, underlay) {
      _log('MAD_RECV from ${sender.hex} over ${underlay.name} '
          '(${payload.length} bytes)');
      scheduleMicrotask(() {
        try {
          _ctx!.handleIncomingPayload(payload: payload, fromAgent: sender.hex);
        } catch (e) {
          _log('MAD_ERROR: $e');
        }
        updateStats();
        unawaited(_runUntilQuiescent());
      });
    };

    // A place event the layer reports binds the place's stream (MadContext
    // .deliverPlaceEvent); the run follows it, once the code that delivered
    // it has returned.
    _ctx!.onPlaceEventDelivered =
        () => scheduleMicrotask(() => unawaited(_runUntilQuiescent()));

    // Connectivity, surfaced as the layer reports it, its attested identity
    // with it.
    network.onPeerConnected =
        (pk, underlay, identity) => onPeerConnected?.call(pk, underlay, identity);
    network.onPeerDisconnected =
        (pk, underlay) => onPeerDisconnected?.call(pk, underlay);
    network.onPeerDiscovered =
        (pk, underlay) => onPeerDiscovered?.call(pk, underlay);
  }

  /// Build and bind the simulation's network, where the host was given none:
  /// a [SimulationNetworkClient] over [directory], whose sends go to the
  /// coordinator by [onSendMadMessage] and whose deliveries the coordinator
  /// makes by [onMadMessageReceived].  Agent names are symbolic and resolved
  /// to keys at the seam (IGLP, "Simulation realisation").
  void _bindSimulationNetwork(
      String selfName, ({PubKey pub, Uint8List priv}) kp) {
    directory.register(selfName, kp.pub); // self identity (for sign/verify)
    final network = SimulationNetworkClient(
      selfId: selfName,
      directory: directory,
      keyPair: kp,
      sendToRouter: (toId, payload) =>
          _sendMadPayload(toId, Uint8List.fromList(payload)),
    );
    _network = network;
    _ctx!.network = network; // backs the seam predicates (IGLP Definition Seam Predicates)

    // Outgoing (spec §4): ctx.onMessageReady(destId, msg) → network.send.
    _ctx!.onMessageReady = (destination, msg) async {
      network.send(_pkFor(destination), Uint8List.fromList(msg.payload));
    };

    // Incoming (spec §4): dispatch on the message kind byte (value/request/
    // acknowledgement) via the MadContext receive entry point.  The sender is
    // the agent the directory names by the key the layer authenticated; a key
    // it names no agent by is a fault --- the layer authenticates every
    // sender --- and the message is dropped and reported (GLP #3 Cowork,
    // 2026-10-11 12:59 UTC, "01:05").  Until 2026-10-12 such a sender was
    // named '?', and the message received.
    network.onMessageReceived = (senderPk, payload, messageId, underlay) {
      final fromId = directory.idOf(senderPk);
      if (fromId == null) {
        _log('MAD_ERROR: a message from $senderPk, a key the directory '
            'names no agent by, is dropped');
        _output('[ERROR] a message from ${senderPk.hex}, which names no '
            'agent, is dropped');
        return;
      }
      try {
        _ctx!.handleIncomingPayload(payload: payload, fromAgent: fromId);
      } catch (e) {
        _log('MAD_ERROR: $e');
      }
    };
  }

  // =========================================================================
  // USER INPUT
  // =========================================================================

  /// Inject the person's act, a ground term, into the person's input stream.
  /// The person acts through the user interface, and the acts arrive as
  /// ground terms, never as text (GSG, Appendix "The Prototype's Screens",
  /// and Section 3, "What the super-app grants a mini-app", (ib)).
  Future<void> injectUserInput(rt.Term act) async {
    final shown = formatTerm(act);
    _log('USER_INPUT: $shown');
    if (_runtime == null) {
      _log('USER_INPUT: early return (not initialized)');
      return;
    }
    if (_userInput == null) {
      _log('USER_INPUT: $goalLabel takes no person stream; the act is dropped');
      return;
    }

    _output('> $shown');

    try {
      final activations = _userInput!.inject(act);
      _log('USER_INPUT: ${activations.length} activations');
      for (final goal in activations) {
        _runtime!.gq.enqueue(goal);
      }

      await _runUntilQuiescent();
    } catch (e, st) {
      _log('USER_INPUT ERROR: $e\n$st');
      _output('[ERROR] $e');
    }
  }

  // =========================================================================
  // NETWORK MESSAGES
  // =========================================================================

  /// Handle an incoming madGLP message (seam §4): the opaque payload bytes are
  /// surfaced to the networking layer, which deserializes and dispatches them.
  /// The simulation's delivery, which its coordinator makes; a given network
  /// delivers its own messages, and a delivery here is refused.
  Future<void> onMadMessageReceived(String from, Uint8List payload) async {
    _log('MAD_RECV from $from (${payload.length} bytes)');

    if (givenNetwork != null) {
      throw StateError('agent $agentId runs over a given network, which '
          'delivers its messages; a coordinator delivers none');
    }

    final network = _network;
    if (_runtime == null || _ctx == null || network == null) {
      _log('MAD_RECV: ERROR - runtime/ctx/network is null');
      return;
    }

    network.onMessageReceived
        ?.call(_pkFor(from.toLowerCase()), payload, '', Underlay.pan);

    updateStats();
    await _runUntilQuiescent();
  }

  /// Handle legacy JSON message (backwards compatibility).
  Future<void> onLegacyMessageReceived(String from, dynamic payload) async {
    _output('[RECV from $from] $payload');
    if (_netInput == null || _runtime == null) return;

    final msgTerm = rt.StructTerm('msg', [
      rt.ConstTerm(from.toLowerCase()),
      rt.ConstTerm(agentId.toLowerCase()),
      rt.ConstTerm(payload),
    ]);

    final activations = _netInput!.inject(msgTerm);
    for (final goal in activations) {
      _runtime!.gq.enqueue(goal);
    }
    await _runUntilQuiescent();
  }

  Future<void> _sendMadPayload(String to, Uint8List payload) async {
    _log('SEND_MAD to $to (${payload.length} bytes)');
    await onSendMadMessage?.call(to, payload);
  }

  // =========================================================================
  // EXECUTION
  // =========================================================================

  /// Run the scheduler until quiescent.
  /// Returns the execution status name, or null if not initialized.
  ///
  /// Per agent-runtime-spec.md Section 3: one drain (run all runnable goals
  /// until quiescent), one flush (send all queued outbound messages) --- and
  /// again while a goal waits on when_idle and the flush has left the machine
  /// idle (IGLP eadadcd, Implementation Notes, "The when_idle Guard";
  /// [Scheduler.drainAndSend]).
  ///
  /// Quiescent is the queue empty and nothing runnable. One
  /// [Scheduler.drainWithStatus] does not reach it — it stops at its cycle cap
  /// with goals still queued — so the drain is [Scheduler.drainToQuiescence],
  /// which repeats it until the queue is empty. The cap left behind is
  /// [maxQuiescenceCycles], a net and not a budget, over the whole cycle.
  Future<String?> runUntilQuiescent() async {
    return _runUntilQuiescent();
  }

  Future<String?> _runUntilQuiescent() async {
    _log('RUN: start (GQ=${_runtime?.gq.length ?? 0})');
    if (_scheduler == null || _runtime == null) {
      _log('RUN: early return (not initialized)');
      return null;
    }

    try {
      // Per spec: drain all runnable goals, then flush outbound messages, and
      // again while a goal waits on when_idle.
      var messagesFlushed = 0;
      final result = _scheduler!.drainAndSend(
          () => messagesFlushed += _ctx!.flushMessages(),
          maxCycles: maxQuiescenceCycles,
          debug: glpTraceEnabled);
      _log('RUN: status=${result.status}, goals=${result.goalsRun}');
      goalCount += result.goalsRun;

      if (result.status == ExecutionStatus.capped) {
        // The net caught something, which for a program that quiesces it never
        // does. Say so where the person and the log both see it, and say what
        // is left standing: a run stopped here is half a run, and nothing that
        // follows it means what it says.
        _output('[ERROR] The program did not quiesce: stopped after '
            '${result.goalsRun} goals with ${_runtime!.gq.length} '
            'still queued (the limit is $maxQuiescenceCycles).');
        _log('RUN: CAPPED after ${result.goalsRun} goals, '
            'GQ=${_runtime!.gq.length}');
      }

      if (messagesFlushed > 0) {
        _log('RUN: flushed $messagesFlushed messages');
      }

      updateStats();
      _log('RUN: done (status=${result.status.name})');
      return result.status.name;
    } catch (e, st) {
      _log('RUN ERROR: $e\n$st');
      return 'error';
    }
  }

  // =========================================================================
  // TERM UTILITIES
  // =========================================================================

  rt.Term derefTerm(rt.Term term) {
    if (_runtime == null) return term;

    if (term is rt.VarRef) {
      final value = _runtime!.heap.getValue(term.addr);
      if (value != null && value is! rt.VarRef) {
        return derefTerm(value);
      }
      return term;
    }
    if (term is rt.StructTerm) {
      final derefArgs = term.args.map(derefTerm).toList();
      return rt.StructTerm(term.functor, derefArgs);
    }
    return term;
  }

  String formatTerm(rt.Term term) {
    if (term is rt.ConstTerm) {
      if (term.value == rt.nil || term.value == null) return '[]';
      return term.value.toString();
    }
    if (term is rt.VarRef) {
      final isReader = _runtime?.heap.isReader(term.addr) ?? false;
      return isReader ? 'X${term.addr}?' : 'X${term.addr}';
    }
    if (term is rt.StructTerm) {
      if (term.functor == '.' && term.args.length == 2) {
        final elements = <String>[];
        rt.Term current = term;
        while (current is rt.StructTerm && current.functor == '.' && current.args.length == 2) {
          elements.add(formatTerm(current.args[0]));
          current = current.args[1];
        }
        if (current is rt.ConstTerm && (current.value == rt.nil || current.value == null)) {
          return '[${elements.join(', ')}]';
        }
        return '[${elements.join(', ')} | ${formatTerm(current)}]';
      }
      final args = term.args.map(formatTerm).join(', ');
      return '${term.functor}($args)';
    }
    return term.toString();
  }

  // =========================================================================
  // CLEANUP
  // =========================================================================

  void dispose() {
    // No OutputObservers to dispose — output goes through _output/1 kernel
  }
}
