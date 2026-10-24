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
///
/// Termination and restart (IGLP, Definition "Restart", madglp-spec.tex; Implementation Notes, "Persistence"): where the host is given an [AgentStateStore], the agent's local state survives the termination of its application, kept by the store, and "a message is handed to the layer, and a delivered message acknowledged to the layer, only from a state the store holds, and a restart resumes from the state the store holds".  The agent is event-driven (Implementation Notes, "Event-driven execution"): each event --- the initial start, a message the layer delivers, the person's act, a place event --- is one cycle, which applies the event, reduces to quiescence, saves the local state with the cycle's outgoing messages undelivered in M_p, and only then hands them to the layer; the cycles are taken one at a time, in order of arrival.  At restart the agent goes on from the state the store holds, with no entry goal posted, and hands to the layer the messages that state holds undelivered.  A cycle whose save fails goes back to the state saved before it and applies the event again.  The layer is told that a message is delivered once the cycle that applied it is saved ([onDelivered]).  Until 2026-10-23 the runtime saved nothing (Code #13 to GNA, 2026-10-20 23:56 UTC), and until 2026-10-24 a cycle handed its messages to the layer before its save.
library;

import 'dart:async';
import 'dart:collection';
import 'dart:io';
import 'dart:typed_data';

import 'package:crypto/crypto.dart' as crypto;

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/code_image.dart' show CodeImage;
import 'package:glp_runtime/engine_v2/interp.dart'
    show ByteRunner, codeImageFromProgram;
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/body_kernels.dart' show formatGroundTerm;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/runtime/saved_state.dart';
import 'package:glp_runtime/wire/artefact.dart' show Artefact;
import 'package:glp_runtime/wire/codec.dart' show WireWriter;
import 'package:glp_runtime/multiagent/agent_state_store.dart';
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
///
/// Given a [stateStore], the agent saves its local state in each event cycle before its outgoing messages are handed to the layer, and a host made again over the same store goes on from it.
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

  /// Where the agent's local state is kept across the termination of its
  /// application, or null, and the agent keeps none (IGLP, Implementation
  /// Notes, "Persistence").  The embedding application gives it, as it gives
  /// the layer the file of its queue.
  final AgentStateStore? stateStore;

  /// The runtime tells the layer that the message of [messageId] is delivered: called once the cycle that applied it is saved, or, where there is no [stateStore], once the cycle is complete (IGLP, Implementation Notes, "Persistence": "a delivered message acknowledged to the layer, only from a state the store holds").  [GlpNetwork] has no function for it, GNA's API giving none yet, so the host binds it to the layer's.
  void Function(String messageId)? onDelivered;

  // Runtime state
  /// The engine the agent runs in: made at [initialize], and made again where a cycle whose save failed goes back to the state saved before it.
  GlpEngine? _engine;

  /// The key pair the agent runs under, fixed at [initialize].
  ({PubKey pub, Uint8List priv})? _kp;

  /// The identity of the code the agent's goals run, which a saved state is
  /// held to at restart: its program counters index this code.
  Uint8List? _programIdentity;

  /// The state last saved, which a cycle whose save fails goes back to.  Null until the first is saved.
  Uint8List? _lastSaved;

  /// The messages [_lastSaved] holds undelivered have been handed to the layer since it was saved ([_handOver]), so that a cycle going back to it takes them out of M_p and hands none of them over twice ([_rollBack]).
  bool _lastSavedHandedOver = false;

  /// The events waiting for their cycle, in order of arrival, each with the
  /// completer of whoever submitted it.
  final Queue<(_AgentEvent, Completer<void>)> _events = Queue();

  /// A cycle is being taken, or one is scheduled to be.
  bool _pumping = false;

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
    this.stateStore,
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

  /// Start the agent: where its [stateStore] holds a saved state, go on from it (IGLP, Definition "Restart": "the local state s_p of p at restart is its local state at termination"; Implementation Notes, "Persistence": "a restart resumes from the state the store holds"), the restart's cycle handing to the layer the messages that state holds undelivered ([_Resume]); otherwise boot it, the initial start being its first event cycle.
  Future<void> initialize() async {
    _log('INIT: Starting');
    // The agent's key pair: provided by the coordinator, or generated. The
    // engine holds it as the person's identity from construction — so the
    // certificate its compiler writes, self_key/1 and sign/3 are all under it —
    // and the networking layer below is given the same pair.
    _kp = keyPair ?? generateKeyPair();

    final saved = await stateStore?.load();
    _setUp();
    if (saved != null) {
      _restore(saved);
      _lastSaved = saved;
      _lastSavedHandedOver = false;
      _log('INIT: restored the state saved at termination '
          '(${saved.length} bytes), GQ=${_runtime!.gq.length}');
      _output('[INIT] Restored the agent\'s saved state');
      await _submit(const _Resume());
      _initialized = true;
      updateStats();
      return;
    }
    await _submit(const _Boot());
    _initialized = true;
    updateStats();
  }

  /// Make the engine, load the agent's one program into it, and bind the
  /// network: everything of the agent but its local state, which the boot
  /// creates ([_boot]) or a saved state restores ([_restore]).
  void _setUp() {
    final selfName = name; // its key over a given network ([name])
    final kp = _kp!;
    _output('[INIT] Creating MadContext...');

    // Use GlpEngine — the ONE way to run GLP programs.
    final engine = GlpEngine(
        rootSelfGlpPath: rootSelfGlpPath,
        identity: PersonIdentity(kp.pub, kp.priv));
    _engine = engine;

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

    // A place event the layer reports is an event of its own: its cycle
    // delivers it onto the place's stream, reduces, saves and sends ([_cycle]).
    _ctx!.placeEventSink = (place, event, declaration) => unawaited(
        _submit(_PlaceEvent(place, event, declaration))
            .catchError((Object _) {}));

    _output('[INIT] Loaded GLP program');
  }

  /// The initial start: post the entry goal and reduce.
  void _boot() {
    final engine = _engine!;
    final selfName = name;

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
    // NetIn).  The plays' actors (actor_init/4: Id, Role, Target, NetIn) and the
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
    final main = posted.scheduler.runners['main'];
    if (main is! ByteRunner) {
      throw StateError('the entry goal was posted on no code image');
    }
    _programIdentity = _codeIdentity(main.image);

    final argsDesc = [selfName, ...extraArgs, 'NetIn'].join(', ');
    _output('[GOAL] Started $goalName($argsDesc)');
    _log('INIT: GQ length before initial run: ${_runtime!.gq.length}');
  }

  /// Go on from the saved state [bytes]: the code compiled again by
  /// [_setUp], held to the identity the state was saved under; the local
  /// state read back into the engine's runtime and context; the host's ends
  /// of the person's and the network's input streams; and a runner for each
  /// module a goal runs.  Throws where the state is another agent's, of
  /// another key, or of another program.
  void _restore(Uint8List bytes) {
    final engine = _engine!;
    final r = StateReader(bytes);
    if (r.agentName != name) {
      throw StateError('the saved state is agent ${r.agentName}\'s, and this '
          'agent is $name');
    }
    if (!_sameBytes(r.agentKey, _kp!.pub.bytes)) {
      throw StateError('the saved state is of the agent of key '
          '${PubKey(r.agentKey).hex}, and this agent runs under '
          '${_kp!.pub.hex}: an agent has one key, which survives the '
          'termination with the layer, and the host is given it');
    }
    final image = codeImageFromProgram(engine.combinedProgram);
    final identity = _codeIdentity(image);
    if (!_sameBytes(r.programIdentity, identity)) {
      throw StateError('the saved state is of another program than $program '
          'compiles to: the program counters of its goals index other code');
    }
    _programIdentity = identity;
    r.readTables(appModule: engine.appModule);
    _runtime!.restoreState(r);
    _ctx!.restoreState(r);
    final userWriter = r.optCell();
    final netWriter = r.optCell();
    r.expectEnd();

    final heap = _runtime!.heap;
    _userInput =
        userWriter == null ? null : InputInjector(heap, 'UserIn', userWriter);
    _netInput =
        netWriter == null ? null : InputInjector(heap, 'NetIn', netWriter);
    _scheduler = Scheduler(rt: _runtime!, runners: {'main': ByteRunner(image)})
      ..traceSink = (line) => _log('GLP: $line');

    // A goal launched by run/3 runs the code of the module it was launched
    // on, by its compiled identity (module_kernels.dart): made again from the
    // module constant the goal carries.
    for (final MapEntry(key: goal, value: key)
        in _runtime!.goalPrograms.entries) {
      if (key is! String || !key.startsWith('module:')) continue;
      final module = _runtime!.getGoalModule(goal);
      if (module is! rt.ModuleTerm) {
        throw StateError('goal $goal runs module code $key and carries no '
            'module');
      }
      _runtime!.runners.putIfAbsent(
          key,
          () => ByteRunner(CodeImage.fromArtefactBytes(
              (module.artefact as Artefact).toBytes())));
    }
    _ctx!.resumePlaceEvents();
  }

  /// The identity of a code image: SHA-256 over its instruction-set version,
  /// its symbols and its code, which the goals' program counters index.
  static Uint8List _codeIdentity(CodeImage image) {
    final w = WireWriter()
      ..string(image.isaVersion)
      ..string(image.moduleName)
      ..clen(image.symbols.length);
    for (final sym in image.symbols) {
      w
        ..string(sym.signature)
        ..u8(sym.compiled ? 1 : 0)
        ..clen(sym.codeOffset)
        ..clen(sym.codeLength);
    }
    w.bytes(image.code);
    return Uint8List.fromList(crypto.sha256.convert(w.toBytes()).bytes);
  }

  static bool _sameBytes(List<int> a, List<int> b) {
    if (a.length != b.length) return false;
    for (var i = 0; i < a.length; i++) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }

  // =========================================================================
  // EVENT CYCLES
  // =========================================================================

  /// Submit [event] for its cycle, after the cycles of the events before it;
  /// completes when its cycle is complete.  The first cycle is taken once the
  /// code that submitted it has returned, since a layer may deliver from
  /// inside a call the runtime made of it, and a cycle is never started
  /// inside another.
  Future<void> _submit(_AgentEvent event) {
    final done = Completer<void>();
    _events.add((event, done));
    if (!_pumping) {
      _pumping = true;
      scheduleMicrotask(() => unawaited(_pump()));
    }
    return done.future;
  }

  Future<void> _pump() async {
    while (_events.isNotEmpty) {
      final (event, done) = _events.removeFirst();
      try {
        await _cycle(event);
        done.complete();
      } catch (e, st) {
        _log('CYCLE ERROR: $e\n$st');
        done.completeError(e, st);
      }
    }
    _pumping = false;
  }

  /// One event cycle (IGLP, Implementation Notes, "Event-driven execution": "On each event the agent binds what the event delivers, reduces until quiescent ... and then performs its Sends").  With no [stateStore], the agent applies [event], reduces and performs its Sends ([_apply], [Scheduler.drainAndSend]).  With one, it saves before it sends ([_cycleSaving]).  Once the cycle is complete --- saved, where there is a store --- the layer is told the message it applied is delivered ([onDelivered]).
  Future<void> _cycle(_AgentEvent event) async {
    final store = stateStore;
    if (store == null) {
      _apply(event, send: true);
    } else {
      await _cycleSaving(event, store);
    }
    if (event is _Message && event.messageId.isNotEmpty) {
      onDelivered?.call(event.messageId);
    }
  }

  /// The cycle of [event] over [store] (IGLP, Implementation Notes, "Persistence": "a message is handed to the layer, and a delivered message acknowledged to the layer, only from a state the store holds, and a restart resumes from the state the store holds").  The agent applies the event and reduces to quiescence, the Sends it makes left undelivered in M_p; saves the state, which holds them undelivered; and then hands them to the layer ([_handOver]).  Where the hand-over leaves the machine idle with a goal waiting on when_idle, the agent reduces again, which re-tries the goal, saves and hands over again, as [Scheduler.drainAndSend] does with no store; [maxQuiescenceCycles] bounds the goals of the whole cycle.  Once the last hand-over has handed messages to the layer, the state is saved again, holding them delivered, so that a restart hands over again only what the store holds undelivered.  The restart's cycle ([_Resume]) applies nothing, and begins with the hand-over of what the restored state holds undelivered.
  Future<void> _cycleSaving(_AgentEvent event, AgentStateStore store) async {
    var goals = 0;
    String? status;
    if (event is! _Resume) {
      (status, goals) = await _stepAndSave(store,
          () => _apply(event, send: false), 'the event is applied again');
    }
    while (true) {
      final handed = _handOver();
      // A run that stopped at the net, or on an error of the runtime, is not
      // run again; the cycle ends with its hand-over.
      final stopped =
          status == ExecutionStatus.capped.name || status == 'error';
      final idle = _runtime!.hasIdleWaits && _runtime!.isIdle;
      if (stopped || !idle || goals >= maxQuiescenceCycles) {
        if (!stopped && idle) _reportCapped(goals);
        if (handed > 0) await _saveHandedOver(store);
        return;
      }
      final (runStatus, ran) = await _stepAndSave(
          store,
          () => _runSync(send: false, maxCycles: maxQuiescenceCycles - goals),
          'the run after the hand-over is made again');
      status = runStatus;
      goals += ran;
    }
  }

  /// Take [step] --- the event applied, or a run after a hand-over --- and save the state it leaves: the status of the step's run, and the number of goals it ran.  Where the save fails, the agent goes back to the state saved before the step ([_rollBack]) and takes it again, and says so in [again].  A state that cannot be written is a fault of the runtime, not a failed save, and ends the cycle with the error.
  Future<(String?, int)> _stepAndSave(
      AgentStateStore store, String? Function() step, String again) async {
    while (true) {
      final before = goalCount;
      final status = step();
      final goals = goalCount - before;
      final state = _snapshot();
      try {
        await store.save(state);
        _lastSaved = state;
        _lastSavedHandedOver = false;
        return (status, goals);
      } catch (e) {
        _log('SAVE FAILED: $e; $again from the state saved before it');
        _output('[ERROR] the agent\'s state was not saved: $e; $again');
        _rollBack();
      }
    }
  }

  /// Hand to the layer every message M_p holds undelivered and not held (IGLP, Definition "madGLP Send Transaction": Send "is enabled when (m, q) ∈ M_p is undelivered and not held"), from the state the store holds: the state just saved, or the state a restart resumes from.  The number of messages handed over.
  int _handOver() {
    final handed = _ctx!.flushMessages();
    _lastSavedHandedOver = true;
    if (handed > 0) _log('RUN: handed $handed messages to the layer');
    return handed;
  }

  /// Save the state after the last hand-over of a cycle, which holds the messages handed over delivered, so that a restart from it does not hand them over again.  Where the save fails it is made again: the state is the one the agent holds, and nothing of the cycle is to be taken again.
  Future<void> _saveHandedOver(AgentStateStore store) async {
    final state = _snapshot();
    while (true) {
      try {
        await store.save(state);
        _lastSaved = state;
        _lastSavedHandedOver = false;
        return;
      } catch (e) {
        _log('SAVE FAILED: $e; the state after the hand-over is saved again');
        _output('[ERROR] the agent\'s state was not saved: $e; it is saved '
            'again');
      }
    }
  }

  /// Go back to the state saved before the step in progress: the engine made again and the state read back, or, before any state was saved, a fresh engine for the boot.  Where the messages that state holds undelivered have been handed to the layer since it was saved, they are taken out of M_p, so that none is handed over twice.
  void _rollBack() {
    _setUp();
    final saved = _lastSaved;
    if (saved == null) return;
    _restore(saved);
    if (_lastSavedHandedOver) {
      final removed = _ctx!.mp.removeSendable();
      if (removed > 0) {
        _log('ROLLBACK: $removed messages the saved state holds undelivered '
            'were handed to the layer after it was saved, and are taken out '
            'of M_p');
      }
    }
  }

  /// Apply [event] and reduce: the event's binding and a run until quiescent, and, with [send], the Sends it makes ([_runSync]).  The status of the run, or null where none was made.
  String? _apply(_AgentEvent event, {required bool send}) {
    switch (event) {
      case _Boot():
        _boot();
      case _Resume():
        // The restart binds nothing.
        break;
      case _Message(:final from, :final payload):
        try {
          _ctx!.handleIncomingPayload(payload: payload, fromAgent: from);
        } catch (e) {
          _log('MAD_ERROR: $e');
        }
        updateStats();
      case _Input(:final act):
        final input = _userInput;
        if (input == null) {
          _log('USER_INPUT: $goalLabel takes no person stream; the act is '
              'dropped');
          return null;
        }
        try {
          final activations = input.inject(act);
          _log('USER_INPUT: ${activations.length} activations');
          for (final goal in activations) {
            _runtime!.gq.enqueue(goal);
          }
        } catch (e, st) {
          _log('USER_INPUT ERROR: $e\n$st');
          _output('[ERROR] $e');
          return null;
        }
      case _PlaceEvent(:final place, :final event, :final declaration):
        _ctx!.deliverPlaceEvent(place, event, declaration: declaration);
    }
    final status = _runSync(send: send);
    if (event is _Boot) {
      _log('INIT: Initial run status: $status, GQ after: '
          '${_runtime!.gq.length}');
    }
    return status;
  }

  /// The agent's local state as the bytes saved in a cycle.
  Uint8List _snapshot() {
    final w = StateWriter(
      programIdentity: _programIdentity!,
      agentName: name,
      agentKey: _kp!.pub.bytes,
      appModule: _engine!.appModule,
    );
    _runtime!.saveState(w);
    _ctx!.saveState(w);
    w.optCell(_userInput?.currentWriterId);
    w.optCell(_netInput?.currentWriterId);
    return w.finish();
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

    // Incoming: the Receive transaction, from the sender the layer authenticated, then a run, and the Sends it makes, after the save where there is a store: the message's event cycle ([_cycle]), taken once the code that delivered the message has returned, since a layer may deliver from inside a call the runtime made of it, and a transaction or a run is never started inside another.  A place event the layer reports is a cycle of its own likewise ([MadContext.placeEventSink], set by [_setUp]).
    network.onMessageReceived = (sender, payload, messageId, underlay) {
      _log('MAD_RECV from ${sender.hex} over ${underlay.name} '
          '(${payload.length} bytes)');
      unawaited(_submit(_Message(sender.hex, payload, messageId))
          .catchError((Object _) {}));
    };

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
    network.onMessageReceived = (senderPk, payload, messageId, underlay) =>
        unawaited(_receiveSimulated(senderPk, payload, messageId)
            .catchError((Object _) {}));
  }

  /// A message the simulation delivers, from the agent the directory names by
  /// the key [senderPk]: its event cycle ([_cycle]).  A key the directory
  /// names no agent by is a fault --- the layer authenticates every sender ---
  /// and the message is dropped and reported at once, no cycle taken.
  Future<void> _receiveSimulated(
      PubKey senderPk, Uint8List payload, String messageId) {
    final fromId = directory.idOf(senderPk);
    if (fromId == null) {
      _log('MAD_ERROR: a message from $senderPk, a key the directory '
          'names no agent by, is dropped');
      _output('[ERROR] a message from ${senderPk.hex}, which names no '
          'agent, is dropped');
      return Future.value();
    }
    return _submit(_Message(fromId, payload, messageId));
  }

  // =========================================================================
  // USER INPUT
  // =========================================================================

  /// Inject the person's act, a ground term, into the person's input stream.
  /// The person acts through the user interface, and the acts arrive as
  /// ground terms, never as text (GSG, Appendix "The Prototype's Screens",
  /// and Section 3, "What the super-app grants a mini-app", (ib)).  The act is
  /// an event, and its cycle is taken in turn ([_cycle]).  An act whose cycle
  /// is not saved before the application terminates is not in the state the
  /// store holds, and no message of its cycle has reached the layer.
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
    await _submit(_Input(act));
  }

  // =========================================================================
  // NETWORK MESSAGES
  // =========================================================================

  /// Handle an incoming madGLP message (seam §4): the opaque payload bytes are
  /// surfaced to the networking layer, which deserializes and dispatches them.
  /// The simulation's delivery, which its coordinator makes; a given network
  /// delivers its own messages, and a delivery here is refused.  Completes
  /// when the message's cycle is complete --- saved, where the agent has a
  /// [stateStore] --- and then [onDelivered] has been told [messageId], where
  /// the coordinator gives one.
  Future<void> onMadMessageReceived(String from, Uint8List payload,
      {String messageId = ''}) async {
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

    await _receiveSimulated(_pkFor(from.toLowerCase()), payload, messageId);
    updateStats();
  }

  Future<void> _sendMadPayload(String to, Uint8List payload) async {
    _log('SEND_MAD to $to (${payload.length} bytes)');
    await onSendMadMessage?.call(to, payload);
  }

  // =========================================================================
  // EXECUTION
  // =========================================================================

  /// Run the scheduler until quiescent, within an event cycle, and return the
  /// execution status name, or null if not initialized.
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
  ///
  /// It is run only within an event cycle: "a message is handed to the layer
  /// ... only from a state the store holds" (IGLP fcd6128, Implementation
  /// Notes, "Persistence").  Until 2026-10-24 two public methods ran it
  /// outside one, `runUntilQuiescent` and `onLegacyMessageReceived`, which
  /// nothing called.
  ///
  /// With [send] false, the run makes no Sends: its messages stay undelivered in M_p, for a cycle over a store to hand to the layer once the state holding them is saved ([_cycleSaving]).  [maxCycles], where given, bounds the goals of this run in place of [maxQuiescenceCycles].
  String? _runSync({bool send = true, int? maxCycles}) {
    _log('RUN: start (GQ=${_runtime?.gq.length ?? 0})');
    if (_scheduler == null || _runtime == null) {
      _log('RUN: early return (not initialized)');
      return null;
    }

    try {
      // Per spec: drain all runnable goals, then flush outbound messages, and
      // again while a goal waits on when_idle; or, with no Sends, drain only.
      var messagesFlushed = 0;
      final limit = maxCycles ?? maxQuiescenceCycles;
      final result = send
          ? _scheduler!.drainAndSend(
              () => messagesFlushed += _ctx!.flushMessages(),
              maxCycles: limit,
              debug: glpTraceEnabled)
          : _scheduler!
              .drainToQuiescence(maxCycles: limit, debug: glpTraceEnabled);
      _log('RUN: status=${result.status}, goals=${result.goalsRun}');
      goalCount += result.goalsRun;

      if (result.status == ExecutionStatus.capped) {
        _reportCapped(result.goalsRun);
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

  /// The net caught something, which for a program that quiesces it never does: the run stopped after [goalsRun] goals.  Say so where the person and the log both see it, and say what is left standing: a run stopped here is half a run, and nothing that follows it means what it says.
  void _reportCapped(int goalsRun) {
    _output('[ERROR] The program did not quiesce: stopped after '
        '$goalsRun goals with ${_runtime!.gq.length} '
        'still queued (the limit is $maxQuiescenceCycles).');
    _log('RUN: CAPPED after $goalsRun goals, GQ=${_runtime!.gq.length}');
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

/// An event of the agent: what one cycle applies (IGLP, Implementation Notes,
/// "Event-driven execution": "an event is the initial start, an incoming
/// assignment message, the person's input on the UI channel, or a place event
/// from the networking layer"), and the restart.  Each is data, so a cycle
/// whose save failed is taken again on an engine made again.
sealed class _AgentEvent {
  const _AgentEvent();
}

/// The initial start.
final class _Boot extends _AgentEvent {
  const _Boot();
  @override
  String toString() => 'the initial start';
}

/// The restart from the state the store holds (IGLP, Implementation Notes, "Persistence": "a restart resumes from the state the store holds"): its cycle applies nothing, and hands to the layer the messages that state holds undelivered, which were saved before their hand-over and may not have reached the layer before the termination ([AgentRuntime._cycleSaving]).
final class _Resume extends _AgentEvent {
  const _Resume();
  @override
  String toString() => 'the restart';
}

/// A message the layer delivers, from the agent named [from], under the
/// layer's [messageId].
final class _Message extends _AgentEvent {
  final String from;
  final Uint8List payload;
  final String messageId;
  const _Message(this.from, this.payload, this.messageId);
  @override
  String toString() => 'a message from $from';
}

/// The person's act.
final class _Input extends _AgentEvent {
  final rt.Term act;
  const _Input(this.act);
  @override
  String toString() => 'the person\'s act';
}

/// A place event the layer reports, or its refusal of [declaration].
final class _PlaceEvent extends _AgentEvent {
  final String place;
  final PlaceEvent event;
  final int? declaration;
  const _PlaceEvent(this.place, this.event, this.declaration);
  @override
  String toString() => 'the place event ${event.name} of $place';
}
