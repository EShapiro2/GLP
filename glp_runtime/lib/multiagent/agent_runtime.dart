/// AgentRuntime — the host of one agent for the UI.
///
/// Uses GlpEngine (the ONE way to run GLP programs) for compilation,
/// MadContext for madGLP messaging, and Scheduler for execution.
///
/// Boot: GlpEngine loads the root scope, enableMadGLP creates the MadContext,
/// and the agent's one program is loaded --- a program is one
/// compiled value (GSG, Section 4, "Compiled programs as values"), and several
/// sources are not co-loaded into one engine.  The agent's initial goal is
/// then posted, `agent(ch(_?,_),ch(_?,_))`, every agent's, the first channel
/// to the person and the second to the network (GLP-Spec maglp.tex,
/// Definition "Multiagent GLP"; IGLP, Implementation Notes, "Boot": "a
/// deployed agent is spawned by its application with the same initial goal
/// and its key made the same way, the person being the person"), by the
/// engine's one posting call, which checks it ([GlpEngine.postGoal]).  Until
/// 2026-11-03 the host posted an entry goal its caller named, with the
/// agent's id and constants of the caller's.  The person's acts arrive as
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
/// "Agent Names": "An agent p is its public key, which the runtime makes and
/// supplies to the networking layer ... and has no other name"), presented as
/// 64 lowercase hex characters, as self_key/1 presents it.  Until 2026-10-12 the host built the
/// simulation's network itself, always.
///
/// Termination and restart (IGLP, Definition "Restart", madglp-spec.tex; Implementation Notes, "Persistence"): where the host is given an [AgentStateStore], the agent's local state survives the termination of its application, kept by the store, and "a message is handed to the layer, and a delivered message acknowledged to the layer, only from a state the store holds, and a restart resumes from the state the store holds".  The agent is event-driven (Implementation Notes, "Event-driven execution"): each event --- the initial start, a message the layer delivers, the person's act, a place event --- is one cycle, which applies the event, reduces to quiescence, saves the local state with the cycle's outgoing messages undelivered in M_p, and only then hands them to the layer; the cycles are taken one at a time, in order of arrival.  At restart the agent goes on from the state the store holds, with no entry goal posted, and hands to the layer the messages that state holds undelivered.  A cycle whose save fails leaves the agent running on its state in memory, reported once, and hands nothing to the layer from it until the next cycle that changes the state saves it ([_save]).  The layer is told that a message is delivered once a state that holds it applied is saved, by its `acknowledge(messageId)` ([GlpNetwork.acknowledge]).  Until 2026-10-23 the runtime saved nothing (Code #13 to GNA, 2026-10-20 23:56 UTC), and until 2026-10-24 a cycle handed its messages to the layer before its save.  The store keeps the compiled program the agent runs, and a restart may load a new version of it: the state is restored under the new version when every procedure called by a goal of the resolvent is defined in it with the same type identity, and otherwise the update is refused and the agent restarts under the version it terminated under (IGLP 61f3283, Termination and Restart; [StoredProgram]).  Until 2026-11-08 a state saved under other code than the program compiled to was refused, and the restart threw.
library;

import 'dart:async';
import 'dart:collection';
import 'dart:io';
import 'dart:typed_data';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/code_image.dart' show CodeImage;
import 'package:glp_runtime/engine_v2/interp.dart'
    show ByteRunner, codeArtefactFromProgram;
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/body_kernels.dart' show formatGroundTerm;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/runtime/saved_state.dart';
import 'package:glp_runtime/runtime/machine_state.dart' show GoalId, GoalRef;
import 'package:glp_runtime/wire/artefact.dart' show Artefact;
import 'package:glp_runtime/wire/codec.dart' show bytesEqual;
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/stored_program.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart'
    show TypeIdentityTables;
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart' show OutboundMessage;
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart' show PersonGoal;

/// Agent runtime encapsulating GLP execution, madGLP context, and I/O.
///
/// Usage:
/// 1. Create with the agent's id and the path of its one program, which
///    defines agent/2, and with the agent's [GlpNetwork] and key pair where it
///    runs over a given one
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

  /// The person scripted in GLP at this agent, where the agent is booted by a
  /// boot file: the person goal is posted on the agent's person channel, in
  /// place of the host's injector and send_to_user/1, beside the one initial
  /// goal `agent(ch(_?,_), ch(_?,_))` (IGLP, Implementation Notes, "Boot":
  /// "the runtime runs P there in the person's place, its writes being the
  /// person's input"), as the simulation's isolate posts it.  Null where the
  /// person is the person, whose acts the host injects.
  final PersonGoal? person;

  /// The boot file's own module, in the boot form: the boot source with its
  /// boot clause stripped, loaded on top of the program as the simulation's
  /// isolate loads it (IGLP, Implementation Notes, "Boot": "what remains of
  /// the file is an ordinary module"), checked in the scope of [bootPath]
  /// ("The scope a boot source is checked in").  Null where the program is
  /// the whole.
  final String? bootSource;
  final String? bootPath;

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
  void Function(PubKey pk, Underlay underlay, Application? application)?
      onPeerConnected;
  void Function(PubKey pk, Underlay underlay)? onPeerDisconnected;
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerDiscovered;
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerLost;

  /// This agent's Ed25519 key pair.  Provided by the coordinator or the
  /// application, or, where it is not, read from [stateStore], or made when
  /// the host spawns the agent, before anything is compiled or sent, from the
  /// SHA-256 of the clock, the device's hardware identifiers and the persona's
  /// name, [agentId], combined by exclusive or, and saved in [stateStore]
  /// (IGLP, Implementation Notes, "Boot").  Over a given network it must be
  /// provided, the layer having been instantiated with it (IGLP, The
  /// contract: "the runtime supplies the key pair to the layer when it
  /// instantiates it, the layer keeping it unchanged for its lifetime").
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

  /// The app installed on the device, its developer, its name and its version
  /// number, as the host gives it; null where it gives none, as the
  /// simulation does.  The store keeps it with the compiled program, and
  /// `self_app/2` assigns it, and, as the app whose program the agent runs,
  /// the one the store keeps where a restart refused an update (GLP-Spec
  /// cce13ce, appendix-guards, "Networking seam"; IGLP 59c40e2).
  final Application? installedApp;

  /// The simulation's coordinator is told that the message of [messageId] is acknowledged: the simulation's layer, which the host builds where it is given none, hands its `acknowledge(messageId)` here ([SimulationNetworkClient.acknowledgeToRouter]).  The runtime acknowledges a message once the cycle that applied it is saved, or, where there is no [stateStore], once the cycle is complete (IGLP, Implementation Notes, "Persistence": "a delivered message acknowledged to the layer, only from a state the store holds"), by the layer's [GlpNetwork.acknowledge] (GNA, Section 2.9); over a given network the layer is told, and this is not called.  Until 2026-11-03 [GlpNetwork] had no `acknowledge`, and this was the runtime's one call, which a host was to bind to the layer's and none did.
  void Function(String messageId)? onDelivered;

  // Runtime state
  /// The engine the agent runs in: made at [initialize].
  GlpEngine? _engine;

  /// The key pair the agent runs under, fixed at [initialize].
  ({PubKey pub, Uint8List priv})? _kp;

  /// The identity of the code the agent's goals run, which a saved state is
  /// held to at restart: its program counters index this code.
  Uint8List? _programIdentity;

  /// The code the agent's goals run ([_image]), as the bytes of its artefact
  /// ([_code]), and the program's module value, which its goals carry and
  /// `self_module` returns: [program]'s compilation, or, after a refused
  /// update, the program the agent terminated under, which its store keeps.
  CodeImage? _image;
  Uint8List? _code;
  rt.ModuleTerm? _appModule;


  /// The state in memory is not the state the store holds: a save failed, and no save has been kept since ([_save]).  Nothing is handed to the layer, and no message acknowledged to it, from such a state (IGLP, Implementation Notes, "Persistence").
  bool _unsaved = false;

  /// The failure of the save that made the state [_unsaved] has been reported, once.
  bool _saveFailureReported = false;

  /// The messages applied whose cycles' states the store does not yet hold, by the messageIds the layer gave them: acknowledged to the layer once a save is kept ([_cycle]).
  final List<String> _unacknowledged = [];

  /// The agent is disposed: it takes no event, saves nothing and hands nothing to the layer ([dispose]).
  bool _disposed = false;

  /// The events waiting for their cycle, in order of arrival, each with the
  /// completer of whoever submitted it.
  final Queue<(_AgentEvent, Completer<void>)> _events = Queue();

  /// A cycle is being taken, or one is scheduled to be.
  bool _pumping = false;

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

  /// The safety net on one event's reduction: how many goals it may run before
  /// the runtime declares the program non-quiescent and stops. It is not a
  /// budget an event may quietly exceed — reaching it is reported as an error
  /// and leaves the queue non-empty — and no program that quiesces reaches it.
  int maxQuiescenceCycles = 200000;

  AgentRuntime({
    required this.agentId,
    required this.program,
    required this.rootSelfGlpPath,
    this.person,
    this.bootSource,
    this.bootPath,
    this.keyPair,
    NetworkDirectory? directory,
    GlpNetwork? network,
    this.stateStore,
    this.installedApp,
  })  : directory = directory ?? NetworkDirectory(),
        givenNetwork = network {
    if (network == null) return;
    // The layer was instantiated with the agent's key pair (IGLP, The
    // contract), and gives out only its public key, so the host is given the
    // pair as well: the engine holds it from construction, so the certificate
    // its compiler writes, self_key/1 and sign/3 are under the key the agent
    // is named by.
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
  /// name the agent creates.
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

  /// Forward a router connectivity event to this client's callbacks.  A
  /// connection is attested with [application], the app the harness names
  /// (IGLP 4a9ad28, "Simulation realisation": the router "reports every peer
  /// attested with the app the harness names, its developer, name and
  /// version"), so peer_app/2 assigns app(D, N, V); where the harness names
  /// none the connection is unattested.  A discovery is over a proximity
  /// underlay, the PAN throughout (IGLP, "Simulation realisation").  Each is
  /// A discovery and a loss are recorded for the nearby declarations
  /// ([MadContext.peerDiscovered], [MadContext.peerLost]; GNA cfbfeac,
  /// Section 3.1, onPeerDiscovered and onPeerLost), as the router decides
  /// them (IGLP 45fabfc, "Simulation realisation"), each presence an event
  /// cycle of its own ([MadContext.presenceSink]).
  void onConnectivityEvent(
      PubKey peer, Underlay underlay, ConnectivityEvent event,
      {Application? application}) {
    switch (event) {
      case ConnectivityEvent.connected:
        _ctx?.peerConnected(peer, underlay, application);
        onPeerConnected?.call(peer, underlay, application);
      case ConnectivityEvent.disconnected:
        _ctx?.peerDisconnected(peer, underlay);
        onPeerDisconnected?.call(peer, underlay);
      case ConnectivityEvent.discovered:
        final proximity = ProximityUnderlay.values.byName(underlay.name);
        _ctx?.peerDiscovered(peer, proximity);
        onPeerDiscovered?.call(peer, proximity);
      case ConnectivityEvent.lost:
        final proximity = ProximityUnderlay.values.byName(underlay.name);
        _ctx?.peerLost(peer, proximity);
        onPeerLost?.call(peer, proximity);
    }
  }

  bool get initialized => _initialized;

  /// The agent's public key, once [initialize] has made or been given it.
  PubKey? get publicKey => _kp?.pub;

  /// Whether [initialize] booted the agent afresh, its first start, and did
  /// not restore it from the state its [stateStore] holds (IGLP,
  /// Implementation Notes, "Boot": "At the agent's first start the
  /// application sends on the person channel the act that starts its
  /// program").
  bool get firstStart => _firstStart;
  bool _firstStart = false;

  /// Where [initialize] restored the agent's state under a new version of its
  /// program: "A restart may load a new version of the agent's program ...
  /// The local state is then restored under the new version when every
  /// procedure called by a goal of the resolvent is defined in the new
  /// version with the same type identity" (IGLP 61f3283, madglp-spec.tex,
  /// Termination and Restart).
  bool get updated => _updated;
  bool _updated = false;

  /// Where [initialize] refused an update, the refusal: "otherwise the update
  /// is refused, and the agent restarts under the version it terminated
  /// under" (IGLP 61f3283, Termination and Restart).  The agent then runs the
  /// program its store keeps.  Null where no update was refused.
  UpdateRefusal? get updateRefusal => _updateRefusal;
  UpdateRefusal? _updateRefusal;
  GlpRuntime? get runtime => _runtime;

  /// The app whose program the agent runs, which the layer attests to the
  /// agent's peers: the installed app, or, where a restart refused an update,
  /// the app the store keeps with the program the agent runs (IGLP 59c40e2,
  /// Implementation Notes, "The contract": "the version it attests being the
  /// one the runtime gives it, that of the program the agent runs").  Null
  /// before [initialize], and where the host gave no app.
  Application? get runningApp => _runtime?.selfApps?.running;
  MadContext? get ctx => _ctx;

  String get _tag => agentId.toUpperCase();

  void _log(String message) {
    onLog?.call(_tag, message);
  }

  /// Whether the run is traced: only where its trace is read, by [onLog].
  /// The trace writes every goal reduced as text, which costs most of a run's
  /// time; an agent no one logs runs untraced.
  bool get _tracing => onLog != null;

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
    // The agent's key pair: provided, or the one its store keeps, or made
    // now, at the agent's spawn, before anything is compiled or sent, from the
    // SHA-256 of the clock, the device's hardware identifiers and the persona's
    // name, and kept (IGLP, Implementation Notes, "Boot").  The engine holds it
    // as the person's identity from construction --- so the certificate its
    // compiler writes, self_key/1 and sign/3 are all under it --- and the
    // networking layer below is given the same pair.
    final store = stateStore;
    _kp = keyPair ??
        (store != null
            ? await keptKeyPair(store, agentId)
            : PersonIdentity.forPersona(agentId).keyPair);

    final saved = await store?.load();
    _setUp();
    if (saved != null) {
      await _restoreSaved(store!, saved);
      _log('INIT: restored the state saved at termination '
          '(${saved.length} bytes), GQ=${_runtime!.gq.length}');
      // The layer starts empty at every launch, and at a restart the runtime
      // makes every declaration the store holds to it again before any goal
      // of the agent is reduced; the program does not declare again (IGLP
      // 0c77e47, Implementation Notes, "Persistence").  Until 2026-11-08 the
      // store kept the places and nearby declarations alone and told the new
      // layer none, so a restarted agent's layer knew no peer, trust level
      // or place.
      final redeclared = _ctx!.declareStandingToLayer();
      _log('INIT: ${redeclared.length} standing declarations made to the '
          'layer again');
      _output('[INIT] Restored the agent\'s saved state');
      await _submit(const _Resume());
      // The person's screen is not part of the local state: at a restart the
      // runtime grants refresh, an Input addressed to no construct, in the
      // person's place, once, and the dispatcher has every construct draw
      // again what it last drew, so the screen is drawn from the restored
      // state (IGLP 7a25ff8, madglp-spec.tex, the paragraph after Definition
      // "Restart"; vGLP d433f71, Section 6.3: "Input(T) ::= input(Integer,
      // Path, T) ; refresh", "a gesture grants refresh, as does the runtime
      // when it restarts the agent from its saved state").  The runtime keeps
      // no draw.  It grants it where it holds the person writer, the person
      // being the person; a person scripted in GLP holds it in the runtime's
      // place.
      if (person == null && _userInput != null) {
        _log('USER_INPUT: refresh, granted at the restart');
        await _submit(_Input(refreshGrant));
      }
      _initialized = true;
      updateStats();
      return;
    }
    // The store keeps the compiled program the agent runs (IGLP 61f3283,
    // "Persistence"), the version a later restart refusing an update
    // restarts it under.
    if (store != null) await _keepProgram(store);
    await _submit(const _Boot());
    _firstStart = true;
    _initialized = true;
    updateStats();
  }

  /// Make the engine, load the agent's one program into it, and bind the
  /// network: everything of the agent but its local state, which the boot
  /// creates ([_boot]) or a saved state restores ([_restore]).  Given
  /// [stored], the program the store keeps, the agent runs it in place of
  /// [program]'s compilation, and nothing is compiled: the version it
  /// terminated under, where a restart refused an update (IGLP 61f3283,
  /// Termination and Restart).
  void _setUp({StoredProgram? stored}) {
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
    if (stored != null) {
      _code = stored.code;
      _image = stored.image;
      _appModule = stored.module;
      _log('INIT: running the program the store keeps, the version the agent '
          'terminated under, in place of $program\'s compilation');
    } else {
      if (FileSystemEntity.isDirectorySync(program)) {
        _log('INIT: Loading program from $program');
        engine.loadProgram(program);
        _log('INIT: Program loaded via program linking ($program)');
      } else {
        engine.loadFile(program);
        _log('INIT: Program loaded from the module $program');
      }
      final boot = bootSource;
      if (boot != null) {
        final path = bootPath;
        engine.loadSource(boot,
            filename: 'program',
            scope: path != null ? engine.scopeFor(path) : engine.scope);
        _log('INIT: the boot source loaded on top'
            '${path != null ? ', in the scope of $path' : ''}');
      }
      // The code the goals run, the program and the root linked as one
      // ([GlpEngine.combinedProgram]), as the engine's posting call runs it.
      _code = codeArtefactFromProgram(engine.combinedProgram);
      _image = CodeImage.fromArtefactBytes(_code!);
      _appModule = engine.appModule;
    }
    _programIdentity = codeIdentityOf(_image!);

    _runtime = engine.runtime;
    _runtime!.selfApps = (
      running: stored != null ? stored.app : installedApp,
      installed: installedApp,
    );
    _ctx = engine.madContext;
    _log('INIT: MadContext created');

    // The _output/1 kernel's person: the bridge is handed each element whole
    // ([onUserOutput]), and the transcript shows it as a terminal does.
    _runtime!.outputTermCallback = (term) {
      onUserOutput?.call(term);
      _output('< ${formatGroundTerm(term)}');
    };

    // Networking seam (IGLP, Implementation Notes, "Networking Seam"): route
    // outgoing/incoming through the
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

    // A presence on a nearby stream likewise: its cycle delivers it onto the
    // stream, reduces, saves and sends (IGLP b20951d, after Definition "Seam
    // Predicates": "A place event and a presence on a nearby stream are
    // bridged below the madGLP layer").
    _ctx!.presenceSink = (underlay, presence, declaration) => unawaited(
        _submit(_Presence(underlay, presence, declaration))
            .catchError((Object _) {}));

    // A reach on the reachable stream likewise (IGLP 0c77e47, after
    // Definition "Seam Predicates": "A place event and a presence or a reach
    // on a nearby or reachable stream are bridged below the madGLP layer").
    _ctx!.reachSink = (reach, declaration) => unawaited(
        _submit(_Reach(reach, declaration)).catchError((Object _) {}));

    _output('[INIT] Loaded GLP program');
  }

  /// The initial start: post the entry goal and reduce.
  void _boot() {
    final engine = _engine!;
    final selfName = name;

    // The agent's initial goal, every agent's, agent(ch(_?,_),ch(_?,_))
    // (GLP-Spec maglp.tex, Definition "Multiagent GLP"), posted as
    //
    //   agent(ch(UserIn?, UserOut), ch(NetIn?, NetOut)), send_to_user(UserOut?),
    //   send_to_net(NetOut?)
    //
    // "The runtime holds the writer of the first channel's reader for the
    // person's input, renders the first channel's output to the person, holds
    // the writer of the second channel's reader as the index-0 serializer
    // entry, and takes the second channel's output as the agent's network
    // output" (IGLP, Implementation Notes, "Boot"; "a deployed agent is
    // spawned by its application with the same initial goal").  The host
    // holds UserIn's writer, to inject the person's acts, and NetIn's, the
    // index-0 serializer entry (IGLP Definition Index-0 Serializer);
    // send_to_user/1 renders the output to the person ([onUserOutput]), and
    // send_to_net/1 sends each message of the network output to the agent it
    // names (IGLP, Remark "Network Output Processing").  The engine's one
    // posting call checks the goal as a body goal against the program's
    // declarations before it runs (TGLP modules.tex, "Type-Compatible
    // Attestation Between Agents") and refuses it, throwing, where the
    // program defines no agent/2 or the goal does not check.  Until
    // 2026-11-03 the host posted an entry goal its caller named, with the
    // agent's id first and constants of the caller's.
    //
    // Where the agent is booted with a person scripted in GLP, [person], the
    // person goal holds the person channel's two ends in the host's place:
    // it reads the agent's output to the person and writes the person's
    // input, and the host holds the network's writer only (IGLP,
    // Implementation Notes, "Boot": "the runtime runs P there in the
    // person's place, its writes being the person's input").  One initial
    // goal either way.
    final personGoal = person;
    final goalText = personGoal == null
        ? 'agent(ch(UserIn?, UserOut), ch(NetIn?, NetOut)), '
            'send_to_user(UserOut?), send_to_net(NetOut?)'
        : 'agent(ch(UserIn?, UserOut), ch(NetIn?, NetOut)), '
            '${personGoal.text('UserOut', 'UserIn')}, send_to_net(NetOut?)';
    final posted = engine.postGoal(goalText,
        inputs: personGoal == null ? const ['UserIn', 'NetIn'] : const ['NetIn']);
    _log('INIT: posted $goalText');

    final netInWriter = posted.inputs['NetIn']!.currentWriterId;
    _ctx!.wp.initializeSerializerEntry(netInWriter);
    _log('INIT: Serializer entry initialized, netIn=$netInWriter');

    // The person's input stream (Dart injects ground terms), and the network
    // input stream (receives from MadContext).
    _userInput = posted.inputs['UserIn'];
    _netInput = posted.inputs['NetIn'];

    // The scheduler over the code image the goal was posted on.
    _scheduler = posted.scheduler..traceSink = (line) => _log('GLP: $line');
    final main = posted.scheduler.runners['main'];
    if (main is! ByteRunner) {
      throw StateError('the entry goal was posted on no code image');
    }
    if (!bytesEqual(codeIdentityOf(main.image), _programIdentity!)) {
      throw StateError('the entry goal was posted on other code than the '
          'program compiles to');
    }

    _output('[GOAL] Started agent(ch(_?,_),ch(_?,_)) at $selfName'
        '${person == null ? '' : ', its person ${person!.functor}'}');
    _log('INIT: GQ length before initial run: ${_runtime!.gq.length}');
  }

  /// Go on from the saved state [bytes], which the store holds, under the
  /// program [initialize] compiled, or the program the store keeps (IGLP
  /// 61f3283, madglp-spec.tex, Termination and Restart; Implementation Notes,
  /// "Persistence").  A state saved under the code [program] compiles to is
  /// restored under it.  A state saved under other code is saved under the
  /// version the store keeps, and the restart loads a new version: "The local
  /// state is then restored under the new version when every procedure called
  /// by a goal of the resolvent is defined in the new version with the same
  /// type identity; otherwise the update is refused, and the agent restarts
  /// under the version it terminated under."  So the state is restored under
  /// the kept version, and its goals' procedures are compared
  /// ([_compareVersions]); where each is defined in the new version with the
  /// same type identity, the state is restored again under the new version,
  /// each goal's program counter the entry of its procedure there, the state
  /// saved, and then the new version kept; otherwise the agent runs the kept
  /// version, and [updateRefusal] says why.  The program is kept after the
  /// state, so that a termination between the two saves leaves a state the
  /// next restart restores under the code [program] compiles to.  Throws
  /// where the state is another agent's, of another key, or saved under code
  /// that is neither [program]'s compilation nor the program the store keeps.
  Future<void> _restoreSaved(AgentStateStore store, Uint8List saved) async {
    final savedUnder = StateReader(saved).programIdentity;
    final keptBytes = await store.loadProgram();
    final kept =
        keptBytes == null ? null : StoredProgram.fromBytes(keptBytes);
    if (bytesEqual(savedUnder, _programIdentity!)) {
      _restore(saved);
      if (kept == null || !bytesEqual(kept.identity, _programIdentity!)) {
        await _keepProgram(store);
      }
      return;
    }
    if (kept == null || !bytesEqual(kept.identity, savedUnder)) {
      throw StateError('the saved state is of another program than $program '
          'compiles to, and the store keeps no compiled program it was saved '
          'under: the program counters of its goals index code the agent '
          'does not have');
    }

    // An update: the new version, compiled by [_setUp], set aside; the state
    // restored under the version it was saved under.
    final newImage = _image!;
    final newModule = _appModule;
    _log('INIT: the saved state is of the version the store keeps, and '
        '$program compiles to a new version');
    _setUp(stored: kept);
    _restore(saved);
    final programs = _runtime!.goalPrograms;
    final (:refused, :entries) = _compareVersions(
        kept, newImage, newModule, _runtime!.resolventGoals, programs);
    if (refused.isNotEmpty) {
      _updateRefusal = UpdateRefusal(refused);
      _log('INIT: the update is refused: $_updateRefusal');
      _output('[INIT] The update is refused, and the agent restarts under the '
          'version it terminated under: $_updateRefusal');
      return;
    }

    // Restored again under the new version, each goal of the program at the
    // entry of its procedure there; a goal of a module launched by run/3 runs
    // that module's code, which the update leaves as it is.
    final savedApp = kept.module;
    _setUp();
    _restore(saved,
        savedUnder: savedUnder,
        savedAppModule: savedApp == null
            ? null
            : Artefact.compiledIdentityOf(
                (savedApp.artefact as Artefact).toBytes()),
        programCounter: (goal, pc) {
          if (goal == null) return pc; // a disarmed record is never woken
          final key = programs[goal];
          if (key is String && key.startsWith('module:')) return pc;
          final entry = entries[pc];
          if (entry == null) {
            throw StateError('goal $goal is at $pc, the entry of no procedure '
                'compared');
          }
          return entry;
        });
    _updated = true;
    _log('INIT: the state is restored under the new version');
    _output('[INIT] The agent\'s program is updated');
    // The new version is kept only once the state restored under it is: a
    // state saved under the version the store keeps is restored under it
    // again at the next restart, where this save failed ([_save]).
    if (await _save(store)) await _keepProgram(store);
  }

  /// The procedures the goals of the resolvent [goals] call in [kept], the
  /// version a state was saved under, compared with the new version, the code
  /// [newImage] and the module [newModule] (IGLP 61f3283, Termination and
  /// Restart: "every procedure called by a goal of the resolvent is defined
  /// in the new version with the same type identity").  A goal's procedure is
  /// the one whose entry its program counter is, named as the code names it
  /// (`name/arity`, a module's procedure under the name the linking gave
  /// it); its type identity is the program's tables' (TGLP, "Type Identity";
  /// [TypeIdentityTables.procedureIdentity]).  A goal of a module launched by
  /// run/3 ([programs]) runs that module's code and is not compared.
  /// [refused] says, for each procedure that fails the condition, why;
  /// [entries] maps the entry of each procedure of [kept] to its entry in
  /// [newImage], where it is defined there.
  static ({List<String> refused, Map<int, int> entries}) _compareVersions(
      StoredProgram kept,
      CodeImage newImage,
      rt.ModuleTerm? newModule,
      Iterable<GoalRef> goals,
      Map<GoalId, Object?> programs) {
    // A signature may name more than one compiled procedure (CodeImage
    // keeps the first for a call by name): the n-th of one version is the
    // n-th of the other.
    Map<(String, int), int> bySignature(CodeImage image) {
      final seen = <String, int>{};
      return {
        for (final sym in image.symbols)
          if (sym.compiled)
            (sym.signature, seen[sym.signature] = (seen[sym.signature] ?? 0) + 1):
                sym.codeOffset,
      };
    }

    final oldEntries = bySignature(kept.image);
    final newEntries = bySignature(newImage);
    final procedureAt = {
      for (final MapEntry(:key, :value) in oldEntries.entries) value: key,
    };
    final entries = <int, int>{
      for (final MapEntry(:key, :value) in oldEntries.entries)
        if (newEntries[key] case final to?) value: to,
    };
    final oldTables = kept.tables;
    final newTables = newModule?.declaredTypes;
    final refused = <String, String>{};
    for (final goal in goals) {
      final key = programs[goal.id];
      if (key is String && key.startsWith('module:')) continue;
      final procedure = procedureAt[goal.pc];
      if (procedure == null) {
        refused['goal ${goal.id}'] = 'goal ${goal.id} is at ${goal.pc}, the '
            'entry of no procedure of the version it terminated under';
        continue;
      }
      final (signature, _) = procedure;
      if (refused.containsKey(signature)) continue;
      if (!entries.containsKey(goal.pc)) {
        refused[signature] = '$signature is not defined in the new version';
        continue;
      }
      final before = oldTables?.procedureIdentity(signature);
      final after = newTables is TypeIdentityTables
          ? newTables.procedureIdentity(signature)
          : null;
      if (before == null) {
        refused[signature] =
            '$signature has no type identity in the version it terminated '
            'under';
      } else if (after == null) {
        refused[signature] =
            '$signature has no type identity in the new version';
      } else if (before != after) {
        refused[signature] = '$signature is defined in the new version with '
            'another type identity';
      }
    }
    return (refused: refused.values.toList(), entries: entries);
  }

  /// Keep in [store] the compiled program the agent runs ([StoredProgram];
  /// IGLP 61f3283, Implementation Notes, "Persistence": "the agent's key
  /// pair, the compiled program it runs").
  Future<void> _keepProgram(AgentStateStore store) async {
    await store.saveProgram(
        StoredProgram.compiled(_code!, _appModule,
                app: installedApp,
                files: StoredProgram.filesOf(_appModule?.directory))
            .toBytes());
    _log('INIT: the store keeps the compiled program the agent runs');
  }

  /// Go on from the saved state [bytes] under the program [_setUp] set up:
  /// the state held to the code it was saved under, [savedUnder], which is
  /// the code the agent runs unless the state is restored under a new
  /// version ([_restoreSaved]), each goal's program counter then mapped by
  /// [programCounter] and the saved module value of the program, of compiled
  /// identity [savedAppModule], standing for the new one; the local state
  /// read back into the engine's runtime and context; the host's ends of the
  /// person's and the network's input streams; and a runner for each module a
  /// goal runs.  Throws where the state is another agent's, of another key,
  /// or of other code.
  void _restore(Uint8List bytes,
      {Uint8List? savedUnder,
      Uint8List? savedAppModule,
      int Function(int? goal, int pc)? programCounter}) {
    final r = StateReader(bytes, programCounter: programCounter);
    if (r.agentName != name) {
      throw StateError('the saved state is agent ${r.agentName}\'s, and this '
          'agent is $name');
    }
    if (!bytesEqual(r.agentKey, _kp!.pub.bytes)) {
      throw StateError('the saved state is of the agent of key '
          '${PubKey(r.agentKey).hex}, and this agent runs under '
          '${_kp!.pub.hex}: an agent has one key, which its store keeps '
          'across the termination');
    }
    if (!bytesEqual(r.programIdentity, savedUnder ?? _programIdentity!)) {
      throw StateError('the saved state is of other code than the agent '
          'runs: the program counters of its goals index other code');
    }
    r.readTables(appModule: _appModule, savedAppModule: savedAppModule);
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
    _scheduler =
        Scheduler(rt: _runtime!, runners: {'main': ByteRunner(_image!)})
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

  // =========================================================================
  // EVENT CYCLES
  // =========================================================================

  /// Submit [event] for its cycle, after the cycles of the events before it;
  /// completes when its cycle is complete.  The first cycle is taken once the
  /// code that submitted it has returned, since a layer may deliver from
  /// inside a call the runtime made of it, and a cycle is never started
  /// inside another.
  Future<void> _submit(_AgentEvent event) {
    if (_disposed) {
      _log('DISPOSED: $event is not taken');
      return Future.value();
    }
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
      if (_disposed) {
        _log('DISPOSED: $event is not taken');
        done.complete();
        continue;
      }
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

  /// One event cycle (IGLP, Implementation Notes, "Event-driven execution": "On each event the agent binds what the event delivers, reduces until quiescent ... and then performs its Sends").  With no [stateStore], the agent applies [event], reduces and performs its Sends ([_apply], [Scheduler.drainAndSend]), and the message it applied is acknowledged.  With one, it saves before it sends ([_cycleSaving]), and a message it applied is acknowledged to the layer once a state that holds it applied is saved: at the end of its cycle, or, where that cycle's save failed, at the end of the first cycle after it whose save is kept ([GlpNetwork.acknowledge]; GNA, Section 2.9: "Confirm that the message delivered under messageId has been applied and saved by the runtime"; IGLP, Implementation Notes, "Persistence": "a delivered message acknowledged to the layer, only from a state the store holds").
  Future<void> _cycle(_AgentEvent event) async {
    final store = stateStore;
    if (store == null) {
      await _applyAndSend(event);
      if (event is _Message && event.messageId.isNotEmpty) {
        _ctx?.network?.acknowledge(event.messageId);
      }
      return;
    }
    await _cycleSaving(event, store);
    if (event is _Message && event.messageId.isNotEmpty) {
      _unacknowledged.add(event.messageId);
    }
    if (_unsaved || _disposed) return;
    for (final id in _unacknowledged) {
      _ctx?.network?.acknowledge(id);
    }
    _unacknowledged.clear();
  }

  /// The cycle of [event] over [store] (IGLP, Implementation Notes, "Persistence": "a message is handed to the layer, and a delivered message acknowledged to the layer, only from a state the store holds, and a restart resumes from the state the store holds").  The agent applies the event and reduces to quiescence, the Sends it makes left undelivered in M_p; saves the state, which holds them undelivered; and then hands them to the layer ([_handOver]).  Where the hand-over leaves the machine idle with a goal waiting on when_idle, the agent reduces again, which re-tries the goal, saves and hands over again, as [Scheduler.drainAndSend] does with no store; [maxQuiescenceCycles] bounds the goals of the whole cycle.  Once the last hand-over has handed messages to the layer, the state is saved again, holding them delivered, so that a restart hands over again only what the store holds undelivered.  The restart's cycle ([_Resume]) applies nothing, and begins with the hand-over of what the restored state holds undelivered.
  ///
  /// Where a save fails ([_save]), the agent runs on its state in memory, and hands nothing to the layer from it: the cycle ends there, its Sends kept undelivered in M_p, and the next cycle that changes the state saves it again, and hands them over once that save is kept.  A cycle that changes nothing, the layer's report of a connection ([_Retry]), hands nothing over while the state in memory is not the one the store holds.  Until 2026-11-09 a cycle whose save failed went back to the state saved before it and was taken again at once, with no bound, so a store that kept failing, as a full disc does, spun the event loop (Cloud #6, Code_inbox 2026-11-08 21:13 UTC).
  Future<void> _cycleSaving(_AgentEvent event, AgentStateStore store) async {
    var goals = 0;
    String? status;
    if (event is! _Resume && event is! _Retry) {
      (status, goals) = _step(() => _apply(event));
      if (!await _save(store)) return;
    } else if (_unsaved) {
      return;
    }
    while (true) {
      final handed = await _handOver();
      // A run that stopped at the net, or on an error of the runtime, is not
      // run again; the cycle ends with its hand-over.
      final stopped =
          status == ExecutionStatus.capped.name || status == 'error';
      final idle = _runtime!.hasIdleWaits && _runtime!.isIdle;
      if (stopped || !idle || goals >= maxQuiescenceCycles || _disposed) {
        if (!stopped && idle && !_disposed) _reportCapped(goals);
        if (handed > 0) await _save(store);
        return;
      }
      final (runStatus, ran) =
          _step(() => _runSync(maxCycles: maxQuiescenceCycles - goals));
      status = runStatus;
      goals += ran;
      if (!await _save(store)) return;
    }
  }

  /// Take [step] --- the event applied, or a run after a hand-over: the status of the step's run, and the number of goals it ran.
  (String?, int) _step(String? Function() step) {
    final before = goalCount;
    final status = step();
    return (status, goalCount - before);
  }

  /// Save in [store] the state the agent holds; true where it is kept.  Where the save fails, the agent runs on its state in memory ([_unsaved]), and the failure is reported once on [onLog], as `[ERROR]`, until a save is kept again; the state is saved again at the next cycle that changes it, not at once.  Nothing is saved once the agent is disposed: its store is released ([dispose]).
  Future<bool> _save(AgentStateStore store) async {
    if (_disposed) return false;
    final bytes = _snapshot();
    try {
      await store.save(bytes);
    } catch (e) {
      _unsaved = true;
      if (!_saveFailureReported) {
        _saveFailureReported = true;
        _log('[ERROR] the agent\'s state was not saved: $e; the agent runs on '
            'its state in memory, hands nothing to the layer from it, and '
            'saves it again at the next cycle that changes it');
      }
      return false;
    }
    if (_disposed) return false;
    if (_unsaved) {
      _log('SAVED: the state is kept again, and what it holds undelivered '
          'is handed to the layer');
    }
    _unsaved = false;
    _saveFailureReported = false;
    return true;
  }

  /// Hand to the layer every message M_p holds undelivered and not held (IGLP, Definition "madGLP Send Transaction": Send "is enabled when (m, q) ∈ M_p is undelivered and not held"), from the state the store holds: the state just saved, or the state a restart resumes from; and wait for the layer's answer to each, a refused message being back in M_p undelivered (IGLP b014a9e, Implementation Notes, "Networking Seam", The contract: "the runtime waits for its answer and, on a refusal, keeps the message").  The number of messages handed over; none once the agent is disposed.
  Future<int> _handOver() async {
    if (_disposed) return 0;
    final handed = _ctx!.flushMessages();
    await _ctx!.answered();
    if (handed > 0) _log('RUN: handed $handed messages to the layer');
    return handed;
  }

  /// Apply [event] and reduce: the event's binding and a run until quiescent, which makes no Sends, its messages staying undelivered in M_p ([_runSync]).  The status of the run, or null where none was made.
  String? _apply(_AgentEvent event) {
    if (!_bind(event)) return null;
    return _reportBoot(event, _runSync());
  }

  /// Apply [event], reduce, and perform the Sends the run makes, waiting for the layer's answer to each ([_runAndSend]).  The status of the run, or null where none was made.
  Future<String?> _applyAndSend(_AgentEvent event) async {
    if (!_bind(event)) return null;
    return _reportBoot(event, await _runAndSend());
  }

  String? _reportBoot(_AgentEvent event, String? status) {
    if (event is _Boot) {
      _log('INIT: Initial run status: $status, GQ after: '
          '${_runtime!.gq.length}');
    }
    return status;
  }

  /// Bind what [event] delivers; false where the person's act could not be injected, and no run is to be made.
  bool _bind(_AgentEvent event) {
    switch (event) {
      case _Boot():
        _boot();
      case _Resume():
        // The restart binds nothing.
        break;
      case _Retry():
        // The layer's report binds nothing.
        break;
      case _Message(:final from, :final payload):
        try {
          _ctx!.handleIncomingPayload(payload: payload, fromAgent: from);
        } catch (e) {
          _log('MAD_ERROR: $e');
        }
        updateStats();
      case _Input(:final act):
        // The act was submitted by [injectUserInput], which drops it where
        // the entry takes no person stream.
        try {
          final activations = _userInput!.inject(act);
          _log('USER_INPUT: ${activations.length} activations');
          for (final goal in activations) {
            _runtime!.enqueueReactivatedGoal(goal);
          }
        } catch (e, st) {
          _log('USER_INPUT ERROR: $e\n$st');
          _output('[ERROR] $e');
          return false;
        }
      case _PlaceEvent(:final place, :final event, :final declaration):
        _ctx!.deliverPlaceEvent(place, event, declaration: declaration);
      case _Presence(:final underlay, :final presence, :final declaration):
        _ctx!.deliverPresence(underlay, presence, declaration: declaration);
      case _Reach(:final reach, :final declaration):
        _ctx!.deliverReach(reach, declaration: declaration);
    }
    return true;
  }

  /// The agent's local state as the bytes saved in a cycle.
  Uint8List _snapshot() {
    final w = StateWriter(
      programIdentity: _programIdentity!,
      agentName: name,
      agentKey: _kp!.pub.bytes,
      appModule: _appModule,
    );
    _runtime!.saveState(w);
    _ctx!.saveState(w);
    w.optCell(_userInput?.currentWriterId);
    w.optCell(_netInput?.currentWriterId);
    return w.finish();
  }

  /// Hand [msg] to [network] for [pk], and return the layer's answer: a refusal is logged, and the message is back in M_p ([MadContext.flushMessages]).
  Future<void> _sendAndNote(
      GlpNetwork network, PubKey pk, String destination, OutboundMessage msg) {
    final payload = Uint8List.fromList(msg.payload);
    return network.send(pk, payload).then((_) {}, onError: (Object e) {
      _log('SEND REFUSED: the layer refused a message to $destination: $e; '
          'it is kept, and sent again at the next hand-over');
      throw e;
    });
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
    _ctx!.network = network; // backs the seam predicates (IGLP Definition Seam Predicates)

    // Outgoing: the destination's name is its key.  A destination that is no
    // key names no agent over the layer, and its message is not sent.
    _ctx!.onMessageReady = (destination, msg) {
      final PubKey pk;
      try {
        pk = PubKey.fromHex(destination);
      } on Object catch (e) {
        _log('MAD_ERROR: a message to $destination, which is no key and so '
            'names no agent over the network, is not sent: $e');
        _output('[ERROR] a message to $destination, which names no agent '
            'over the network, is not sent');
        return null;
      }
      return _sendAndNote(network, pk, destination, msg);
    };

    // Incoming: the Receive transaction, from the sender the layer authenticated, then a run, and the Sends it makes, after the save where there is a store: the message's event cycle ([_cycle]), taken once the code that delivered the message has returned, since a layer may deliver from inside a call the runtime made of it, and a transaction or a run is never started inside another.  A place event the layer reports is a cycle of its own likewise ([MadContext.placeEventSink], set by [_setUp]).
    network.onMessageReceived = (sender, payload, messageId, underlay) {
      _log('MAD_RECV from ${sender.hex} over ${underlay.name} '
          '(${payload.length} bytes)');
      unawaited(_submit(_Message(sender.hex, payload, messageId))
          .catchError((Object _) {}));
    };

    // Connectivity, surfaced as the layer reports it, its attested
    // application with it, and recorded for peer_app/2 ([MadContext.
    // peerConnected]).  A connection the layer reports is also when a message
    // it refused is sent again, where M_p holds one ([_Retry]).
    network.onPeerConnected = (pk, underlay, application) {
      _ctx?.peerConnected(pk, underlay, application);
      onPeerConnected?.call(pk, underlay, application);
      if (_ctx?.mp.hasSendable ?? false) {
        unawaited(_submit(const _Retry()).catchError((Object _) {}));
      }
    };
    network.onPeerDisconnected = (pk, underlay) {
      _ctx?.peerDisconnected(pk, underlay);
      onPeerDisconnected?.call(pk, underlay);
    };
    // A discovery and a loss are recorded for the nearby declarations:
    // seen(K) when the layer discovers on U an agent of key K, and gone(K)
    // when it can no longer be reached over U (GLP-Spec 210cee1, "Networking
    // seam"), the layer's onPeerDiscovered and onPeerLost on U (GNA cfbfeac,
    // Section 3.1: "onPeerDiscovered and onPeerLost behind the stream of
    // declare(nearby(Underlay, Events))").  Until 2026-11-08 a connection
    // and a disconnection fed them too.
    network.onPeerDiscovered = (pk, underlay) {
      _ctx?.peerDiscovered(pk, underlay);
      onPeerDiscovered?.call(pk, underlay);
    };
    network.onPeerLost = (pk, underlay) {
      _ctx?.peerLost(pk, underlay);
      onPeerLost?.call(pk, underlay);
    };
  }

  /// Build and bind the simulation's network, where the host was given none:
  /// a [SimulationNetworkClient] over [directory], whose sends go to the
  /// coordinator by [onSendMadMessage] and whose deliveries the coordinator
  /// makes by [onMadMessageReceived].  A destination is a key or a readable
  /// name standing for one, and the seam resolves both to the one agent (IGLP
  /// 4f84612, Implementation Notes, "Simulation realisation").
  void _bindSimulationNetwork(
      String selfName, ({PubKey pub, Uint8List priv}) kp) {
    directory.register(selfName, kp.pub); // self identity (for sign/verify)
    final network = SimulationNetworkClient(
      selfId: selfName,
      directory: directory,
      keyPair: kp,
      sendToRouter: (toId, payload) =>
          _sendMadPayload(toId, Uint8List.fromList(payload)),
      acknowledgeToRouter: (messageId) => onDelivered?.call(messageId),
    );
    _ctx!.network = network; // backs the seam predicates (IGLP Definition Seam Predicates)

    // A destination, in a global name or in a message, is a key or a
    // readable name standing for one, and the seam resolves both to the one
    // agent (IGLP 4f84612, Implementation Notes, "Simulation realisation"):
    // here by the directory, to the agent's id, before the Send records
    // anything.
    _ctx!.resolveAgent = directory.agentNamed;

    // Outgoing: ctx.onMessageReady(destId, msg) → network.send.
    _ctx!.onMessageReady = (destination, msg) =>
        _sendAndNote(network, _pkFor(destination), destination, msg);

    // Incoming: dispatch on the message kind byte (value/request/
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
      _log('USER_INPUT: the agent holds no person stream; the act is dropped');
      return;
    }

    _output('> $shown');
    await _submit(_Input(act));
  }

  // =========================================================================
  // NETWORK MESSAGES
  // =========================================================================

  /// Handle an incoming madGLP message (IGLP, Implementation Notes,
  /// "Networking Seam"): the opaque payload bytes are surfaced to the
  /// networking layer, which deserializes and dispatches them.
  /// The simulation's delivery, which its coordinator makes; a given network
  /// delivers its own messages, and a delivery here is refused.  Completes
  /// when the message's cycle is complete --- saved, where the agent has a
  /// [stateStore] --- and then the message is acknowledged, [onDelivered] told
  /// [messageId], where the coordinator gives one.
  Future<void> onMadMessageReceived(String from, Uint8List payload,
      {String messageId = ''}) async {
    _log('MAD_RECV from $from (${payload.length} bytes)');

    if (givenNetwork != null) {
      throw StateError('agent $agentId runs over a given network, which '
          'delivers its messages; a coordinator delivers none');
    }

    if (_runtime == null || _ctx == null) {
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
  /// [_runSync] makes no Sends: its messages stay undelivered in M_p, for a cycle over a store to hand to the layer once the state holding them is saved ([_cycleSaving]).  [maxCycles], where given, bounds the goals of this run in place of [maxQuiescenceCycles].
  String? _runSync({int? maxCycles}) {
    if (!_runStarts()) return null;
    try {
      final result = _scheduler!.drainToQuiescence(
          maxCycles: maxCycles ?? maxQuiescenceCycles, debug: _tracing);
      return _runDone(result, 0);
    } catch (e, st) {
      _log('RUN ERROR: $e\n$st');
      return 'error';
    }
  }

  /// [_runAndSend] drains, then performs the Sends and waits for the layer's answer to each, and again while a goal waits on when_idle ([Scheduler.drainAndSend]): "the runtime waits for its answer and, on a refusal, keeps the message and sends it again later" (IGLP b014a9e, Implementation Notes, "Networking Seam", The contract).
  Future<String?> _runAndSend() async {
    if (!_runStarts()) return null;
    try {
      var messagesFlushed = 0;
      final result = await _scheduler!.drainAndSend(() {
        messagesFlushed += _ctx!.flushMessages();
        return _ctx!.answered();
      }, maxCycles: maxQuiescenceCycles, debug: _tracing);
      return _runDone(result, messagesFlushed);
    } catch (e, st) {
      _log('RUN ERROR: $e\n$st');
      return 'error';
    }
  }

  bool _runStarts() {
    _log('RUN: start (GQ=${_runtime?.gq.length ?? 0})');
    if (_scheduler == null || _runtime == null) {
      _log('RUN: early return (not initialized)');
      return false;
    }
    return true;
  }

  String _runDone(DrainResult result, int messagesFlushed) {
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

  /// End this launch of the agent: "shutdown is the embedding application's" (IGLP, Implementation Notes, "Event-driven execution"), and a launch has one agent and one store (Definition "Restart"; "Persistence").  The agent takes no event after it --- a cycle in progress makes no save and no hand-over after it --- and the events waiting are not taken; its channels to the layer and to the person are closed, the callbacks it set on the network unset and [onOutput] with them; and its store is released, no save being made in it, so that the next launch, in this process or another, is the one agent that writes it.  The network itself is the host's, which disposes it.  Until 2026-11-09 it did nothing, and a disposed host's agent went on taking events and saving, two launches in one process writing one state file (Cloud #6, Code_inbox 2026-11-08 21:13 UTC).
  void dispose() {
    if (_disposed) return;
    _disposed = true;
    _log('DISPOSED: the agent takes no event and saves nothing');
    final network = _ctx?.network;
    if (network != null) {
      network.onMessageReceived = null;
      network.onPeerConnected = null;
      network.onPeerDisconnected = null;
      network.onPeerDiscovered = null;
      network.onPeerLost = null;
      network.onPlaceEvent = null;
    }
    final ctx = _ctx;
    if (ctx != null) {
      ctx.placeEventSink = null;
      ctx.presenceSink = null;
      ctx.reachSink = null;
    }
    onOutput = null;
  }

  /// Whether [dispose] has ended this launch of the agent.
  bool get disposed => _disposed;
}

/// An event of the agent: what one cycle applies (IGLP, Implementation Notes,
/// "Event-driven execution": "an event is the initial start, an incoming
/// assignment message, the person's input on the UI channel, or a place event
/// from the networking layer"), a presence on a nearby stream, and the restart.
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

/// The layer reports a peer connected while M_p holds a message the layer refused: the cycle applies nothing, and hands the message to the layer again (IGLP b014a9e, Implementation Notes, "Networking Seam", The contract: "on a refusal, keeps the message and sends it again later").
final class _Retry extends _AgentEvent {
  const _Retry();
  @override
  String toString() => 'a connection the layer reports, after a refusal';
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

/// The grant `refresh`, an Input addressed to no construct, which the runtime
/// grants in the person's place at a restart (IGLP 7a25ff8, after Definition
/// "Restart"; vGLP d433f71, Section 6.3).
final rt.Term refreshGrant = rt.ConstTerm('refresh');

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

/// A presence the layer reports for the nearby declaration on [underlay] that
/// stood when it was reported, [declaration]: `seen(K)` or `gone(K)` (GLP-Spec
/// 210cee1, "Networking seam").
final class _Presence extends _AgentEvent {
  final ProximityUnderlay underlay;
  final rt.Term presence;
  final int declaration;
  const _Presence(this.underlay, this.presence, this.declaration);
  @override
  String toString() => 'the presence $presence over ${underlay.name}';
}

/// A reach the layer reports for the reachable declaration that stood when it
/// was reported, [declaration]: `connected(K)` or `lost(K)` (GLP-Spec
/// cb9df43, "Networking seam").
final class _Reach extends _AgentEvent {
  final rt.Term reach;
  final int declaration;
  const _Reach(this.reach, this.declaration);
  @override
  String toString() => 'the reach $reach';
}

/// A restart's refusal of an update (IGLP 61f3283, madglp-spec.tex,
/// Termination and Restart: "otherwise the update is refused, and the agent
/// restarts under the version it terminated under"): for each procedure a
/// goal of the resolvent calls that the new version does not define with the
/// same type identity, why.
class UpdateRefusal {
  final List<String> reasons;
  const UpdateRefusal(this.reasons);
  @override
  String toString() => reasons.join('; ');
}
