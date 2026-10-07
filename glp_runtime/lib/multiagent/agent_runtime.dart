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
/// the person through send_to_user → the _output/1 kernel → [onOutput].
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/runtime.dart';
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
/// 1. Create with the agent's id and the path of its one program
/// 2. Set callbacks: onOutput, onLog, onSendMadMessage
/// 3. Call initialize() to compile and start
/// 4. Call injectUserInput(act) with each of the person's acts, a ground term
/// 5. Call onMadMessageReceived(from, payload) for network messages
class AgentRuntime {
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
  void Function(String line)? onOutput;
  void Function(String tag, String message)? onLog;
  Future<void> Function(String destination, Uint8List payload)? onSendMadMessage;

  /// Connectivity callbacks surfaced from the networking seam (spec §2/§3). The
  /// coordinator forwards router events via [onConnectivityEvent].
  void Function(PubKey pk, Transport t)? onPeerConnected;
  void Function(PubKey pk, Transport t)? onPeerDisconnected;
  void Function(DiscoveredPeer p)? onPeerDiscovered;

  /// This agent's Ed25519 key pair. Provided by the coordinator, or generated.
  final ({PubKey pub, Uint8List priv})? keyPair;

  /// The shared identifier–key directory. Provided by the coordinator, or built
  /// lazily (deterministic routing keys) when absent.
  final NetworkDirectory directory;

  // Runtime state
  /// The agent's networking layer (seam spec §3-4): outgoing via [send],
  /// incoming via [GlpNetwork.onMessageReceived]. In simulation it forwards to
  /// the coordinator through [onSendMadMessage].
  SimulationNetworkClient? _network;
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
  }) : directory = directory ?? NetworkDirectory();

  /// Deterministic routing key for [id]: returns the directory entry if present,
  /// otherwise derives a stable 32-byte key and registers it. Routing-only — the
  /// signing identity is the agent's real [keyPair].
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

  /// Forward a router connectivity event to this client's callbacks (seam §3).
  void onConnectivityEvent(PubKey peer, Transport t, ConnectivityEvent event) {
    switch (event) {
      case ConnectivityEvent.connected:
        onPeerConnected?.call(peer, t);
      case ConnectivityEvent.disconnected:
        onPeerDisconnected?.call(peer, t);
      case ConnectivityEvent.discovered:
        onPeerDiscovered?.call(DiscoveredPeer(peer, t));
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
    final agentIdLower = agentId.toLowerCase();
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
    engine.enableMadGLP(agentId: agentIdLower);

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

    // Wire _output/1 kernel to our output callback
    _runtime!.outputCallback = (text) {
      _output('< $text');
    };

    // Networking seam (spec §3-4): route outgoing/incoming through a
    // SimulationNetworkClient instead of serializing OutboundMessages directly.
    // The wire carries the opaque payload bytes only (no MessageType).
    directory.register(agentIdLower, kp.pub); // self identity (for sign/verify)
    final network = SimulationNetworkClient(
      selfId: agentIdLower,
      directory: directory,
      sendToRouter: (toId, payload) =>
          _sendMadPayload(toId, Uint8List.fromList(payload)),
    );
    network.putIdentity(kp.pub, kp.priv);
    _network = network;
    _ctx!.network = network; // backs the seam predicates and valid_attestation/4 (§4)

    // Outgoing (spec §4): ctx.onMessageReady(destId, msg) → network.send.
    _ctx!.onMessageReady = (destination, msg) async {
      network.send(_pkFor(destination), Uint8List.fromList(msg.payload));
    };

    // Incoming (spec §4): dispatch on the message kind byte (value/request/
    // acknowledgement) via the MadContext receive entry point.
    network.onMessageReceived = (senderPk, payload, messageId, transport) {
      try {
        final fromId = directory.idOf(senderPk) ?? '?';
        _ctx!.handleIncomingPayload(payload: payload, fromAgent: fromId);
      } catch (e) {
        _log('MAD_ERROR: $e');
      }
    };

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
      glpConstantText(agentIdLower),
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

    final argsDesc = [agentIdLower, ...extraArgs, 'NetIn'].join(', ');
    _output('[GOAL] Started $goalName($argsDesc)');
    _log('INIT: GQ length before initial run: ${_runtime!.gq.length}');

    // Initial run
    final initStatus = await _runUntilQuiescent();
    _log('INIT: Initial run status: $initStatus, GQ after: ${_runtime!.gq.length}');

    _initialized = true;
    updateStats();
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
  Future<void> onMadMessageReceived(String from, Uint8List payload) async {
    _log('MAD_RECV from $from (${payload.length} bytes)');

    final network = _network;
    if (_runtime == null || _ctx == null || network == null) {
      _log('MAD_RECV: ERROR - runtime/ctx/network is null');
      return;
    }

    network.onMessageReceived
        ?.call(_pkFor(from.toLowerCase()), payload, '', Transport.ble);

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
      if (term.value == 'nil' || term.value == null) return '[]';
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
        if (current is rt.ConstTerm && (current.value == 'nil' || current.value == null)) {
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
