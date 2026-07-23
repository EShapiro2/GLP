/// GrassApp Duo — two phones, each running an interactive agent and an actor.
///
/// Phone 1 runs Bob (live UI) + Eve (scripted actor); phone 2 runs Alice
/// (live UI) + Dana (actor). Each agent is its own isolate booted from
/// `programs/book/grassapp/play_grassapp_duo.glp`; madGLP messages between
/// local agents route through [IsolateRouter], and messages addressed to the
/// other phone's agents travel over the Grassroots networking layer (UDX +
/// Noise, rendezvous pairing) — the networking-seam swap in its intended
/// form: madGLP payloads as transport payloads.
///
/// Roles (--dart-define=DUO_ROLE=...):
///   phone1 — Bob (surface) + Eve;   remote: alice, dana.
///   phone2 — Alice (surface) + Dana; remote: bob, eve.
///   solo   — desktop validation: Bob (surface) + Eve and Dana both courting
///            Bob, all local, no network.
///
/// Network bootstrap: each phone derives BOTH phones' transport identities
/// from fixed demo seeds, so name→pubkey is static; the phones pair through
/// the rendezvous server given by --dart-define=DUO_ANCHOR_ADDR/_PK (the
/// known-peer reconnection machinery does the rest).
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter/material.dart';
import 'package:grassroots_networking/grassroots_networking.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'glp_sources.dart';
import 'isolate_protocol.dart';
import 'mad_router.dart';
import 'manifests/grassroots.dart';
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/runtime.dart';

// =============================================================================
// ROLE / CAST
// =============================================================================

const duoRole = String.fromEnvironment('DUO_ROLE', defaultValue: 'solo');
const anchorAddr = String.fromEnvironment('DUO_ANCHOR_ADDR',
    defaultValue: '127.0.0.1:9516');
const anchorPk = String.fromEnvironment('DUO_ANCHOR_PK');

class DuoCast {
  final String human; // interactive agent (has the surface)
  final String actor; // local scripted actor, courts [human]
  final List<String> remoteAgents; // agents living on the other phone
  const DuoCast(this.human, this.actor, this.remoteAgents);
}

DuoCast castFor(String role) => switch (role) {
      'phone1' => const DuoCast('bob', 'eve', ['alice', 'dana']),
      'phone2' => const DuoCast('alice', 'dana', ['bob', 'eve']),
      _ => const DuoCast('bob', 'eve', []), // solo: dana spawned extra below
    };

/// Fixed demo seed per phone role — both phones derive both identities, so
/// name→pubkey needs no exchange. Demo-only, obviously not a key ceremony.
Uint8List seedFor(String role) {
  final tag = 'grassapp-duo-$role';
  return Uint8List.fromList(
      List.generate(32, (i) => (tag.codeUnitAt(i % tag.length) * 31 + i) & 0xFF));
}

// =============================================================================
// NETWORK ENVELOPE — (from, to, madGLP payload)
// =============================================================================

Uint8List encodeEnvelope(String from, String to, Uint8List payload) {
  final f = utf8.encode(from);
  final t = utf8.encode(to);
  return Uint8List.fromList([f.length, ...f, t.length, ...t, ...payload]);
}

({String from, String to, Uint8List payload}) decodeEnvelope(Uint8List bytes) {
  final fLen = bytes[0];
  final from = utf8.decode(bytes.sublist(1, 1 + fLen));
  final tLen = bytes[1 + fLen];
  final to = utf8.decode(bytes.sublist(2 + fLen, 2 + fLen + tLen));
  final payload = Uint8List.sublistView(bytes, 2 + fLen + tLen);
  return (from: from, to: to, payload: payload);
}

// =============================================================================
// ENTRY
// =============================================================================

void main() {
  runApp(const DuoApp());
}

class DuoApp extends StatelessWidget {
  const DuoApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'GrassApp Duo',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.green,
          brightness: Brightness.light,
        ),
      ),
      home: const DuoScreen(),
    );
  }
}

class AgentState {
  final String agentId;
  SendPort? commandPort;
  String status = 'Spawning...';
  UiRuntime? ui;
  AgentState(this.agentId);
}

class DuoScreen extends StatefulWidget {
  const DuoScreen({super.key});

  @override
  State<DuoScreen> createState() => _DuoScreenState();
}

class _DuoScreenState extends State<DuoScreen> {
  final Map<String, AgentState> _agents = {};
  final List<Isolate> _isolates = [];
  final ReceivePort _replyPort = ReceivePort();
  StreamSubscription? _replySubscription;

  late final DuoCast _cast = castFor(duoRole);

  GrassrootsNetwork? _network;
  Uint8List? _remotePhonePk;
  String _netStatus = duoRole == 'solo' ? 'local only' : 'starting…';
  int _pendingStarts = 0;

  @override
  void initState() {
    super.initState();
    _replySubscription = _replyPort.listen(_handleAgentMessage);
    WidgetsBinding.instance.addPostFrameCallback((_) => _boot());
  }

  // ===========================================================================
  // BOOT
  // ===========================================================================

  Future<void> _boot() async {
    if (duoRole != 'solo') {
      await _startNetwork();
    }
    await _spawnAgents();
  }

  Future<void> _startNetwork() async {
    try {
      final sodium = await SodiumSumoInit.init();
      const myRole = duoRole;
      final otherRole = myRole == 'phone1' ? 'phone2' : 'phone1';
      final ed = Ed25519();
      final myIdentity = await GrassrootsIdentity.create(
        keyPair: await ed.newKeyPairFromSeed(seedFor(myRole)),
        nickname: myRole,
      );
      final otherKeyPair = await ed.newKeyPairFromSeed(seedFor(otherRole));
      final otherPk =
          Uint8List.fromList((await otherKeyPair.extractPublicKey()).bytes);
      _remotePhonePk = otherPk;

      final store = Store<AppState>(
        appReducer,
        initialState: const AppState(
          settings: SettingsState(bluetoothEnabled: false),
        ),
      );
      final network = GrassrootsNetwork(
        identity: myIdentity,
        store: store,
        sodium: sodium,
      );
      network.onMessageReceived = (messageId, sender, payload, transport) {
        final env = decodeEnvelope(payload);
        TraceLog.log('NET', 'recv ${env.from} → ${env.to} '
            '(${env.payload.length}B, $transport)');
        IsolateRouter.instance.route(env.from, env.to, env.payload);
      };
      network.onPeerConnected = (pk, transport) {
        setState(() => _netStatus = 'linked ($transport)');
        TraceLog.log('NET', 'peer phone connected via $transport');
      };
      network.onPeerDisconnected = (pk, transport) {
        setState(() => _netStatus = 'link lost');
      };

      final ok = await network.initialize();
      TraceLog.log('NET', 'network initialize: $ok');
      // Supply the other phone's key immediately: LAN discovery and BLE
      // recognition need it, and neither waits for a public address.
      network.putKnownPeer(otherPk);
      _network = network;
      setState(() => _netStatus = 'pairing…');
      // Dialing the rendezvous server needs our local address candidates —
      // wait for public-address discovery before configuring it.
      final discoveryDeadline =
          DateTime.now().add(const Duration(seconds: 60));
      while (network.getPublicAddress() == null &&
          DateTime.now().isBefore(discoveryDeadline)) {
        await Future.delayed(const Duration(milliseconds: 500));
      }
      TraceLog.log('NET', 'public address: ${network.getPublicAddress()}');
      if (anchorPk.isNotEmpty) {
        var rv = false;
        for (var attempt = 1; attempt <= 3 && !rv; attempt++) {
          rv = await network.addRendezvousServer(
            address: anchorAddr,
            pubkeyHex: anchorPk,
          );
          TraceLog.log('NET', 'rendezvous $anchorAddr attempt $attempt: $rv');
          if (!rv) await Future.delayed(const Duration(seconds: 3));
        }
      }
    } catch (e) {
      setState(() => _netStatus = 'network error: $e');
      TraceLog.log('NET', 'ERROR: $e');
    }
  }

  Future<void> _spawnAgents() async {
    final glp = await resolveGlpPaths();

    const scenarioFiles = [
      'self.glp',
      'currency_txn.glp',
      'grassapp_agent.glp',
      'grassapp_mediator.glp',
      'play_grassapp_duo.glp',
    ];
    final paths = [for (final f in scenarioFiles) '${glp.grassappDir}/$f'];
    final sources = <String>[];
    for (final p in paths) {
      final file = File(p);
      if (!file.existsSync()) {
        setState(() => _netStatus = 'missing GLP source: $p');
        return;
      }
      sources.add(await file.readAsString());
    }

    // Interactive agent (the surface).
    final human = AgentState(_cast.human);
    human.ui = UiRuntime(
      manifest: grassrootsManifest,
      onSend: (text) => human.commandPort?.send(UserInput(text)),
    );
    human.ui!.onChange = () => setState(() {});
    _agents[_cast.human] = human;

    // Local actor(s).
    final actorTargets = <String, String>{_cast.actor: _cast.human};
    if (duoRole == 'solo') actorTargets['dana'] = _cast.human;
    for (final actor in actorTargets.keys) {
      _agents[actor] = AgentState(actor);
    }

    _pendingStarts = 1 + actorTargets.length;

    // Spawn with deferred start so every port is registered (locally and at
    // the router) before any GLP runs.
    _isolates.add(await Isolate.spawn(
        agentIsolateEntry,
        InitAgent(
          agentId: _cast.human,
          glpSources: sources,
          glpSourcePaths: paths,
          rootSelfGlpPath: glp.rootSelfGlp,
          friends: const [],
          replyPort: _replyPort.sendPort,
          goalLabel: 'agent_init/3',
          deferStart: true,
        )));
    for (final entry in actorTargets.entries) {
      _isolates.add(await Isolate.spawn(
          agentIsolateEntry,
          InitAgent(
            agentId: entry.key,
            glpSources: sources,
            glpSourcePaths: paths,
            rootSelfGlpPath: glp.rootSelfGlp,
            friends: const [],
            replyPort: _replyPort.sendPort,
            goalLabel: 'actor_init/3',
            extraArgs: [entry.value],
            deferStart: true,
          )));
    }
    setState(() {});
  }

  // ===========================================================================
  // MESSAGE HANDLING
  // ===========================================================================

  void _handleAgentMessage(dynamic msg) {
    if (msg is AgentReady) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.commandPort = msg.commandPort;
        state.status = 'Ready';
        IsolateRouter.instance.register(msg.agentId, msg.commandPort);
      }
      _pendingStarts--;
      if (_pendingStarts == 0) {
        // All ports registered — release the herd.
        for (final agent in _agents.values) {
          agent.commandPort?.send(StartAgent());
        }
        final human = _agents[_cast.human];
        human?.commandPort?.send(UserInput('balance'));
      }
      setState(() {});
    } else if (msg is AgentOutput) {
      final state = _agents[msg.agentId];
      if (state != null && state.ui != null) {
        final boundaryLine =
            msg.line.startsWith('< ') ? msg.line.substring(2) : msg.line;
        state.ui!.handleLine(boundaryLine);
        setState(() {});
      }
    } else if (msg is AgentLog) {
      TraceLog.log(msg.tag, msg.message);
    } else if (msg is AgentSendMad) {
      _routeMad(msg);
    } else if (msg is AgentError) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.status = 'Error: ${msg.error}';
        setState(() {});
      }
      TraceLog.log('COORD', 'ERROR from ${msg.agentId}: ${msg.error}');
    }
  }

  /// Local names route between isolates; the other phone's names ride the
  /// networking layer.
  void _routeMad(AgentSendMad msg) {
    final to = msg.to.toLowerCase();
    if (_agents.containsKey(to)) {
      IsolateRouter.instance.route(msg.agentId, to, msg.payload);
      return;
    }
    if (_cast.remoteAgents.contains(to)) {
      final network = _network;
      final remotePk = _remotePhonePk;
      if (network == null || remotePk == null) {
        TraceLog.log('NET', 'drop ${msg.agentId} → $to (network not up)');
        return;
      }
      TraceLog.log('NET', 'send ${msg.agentId} → $to (${msg.payload.length}B)');
      unawaited(network.send(
          remotePk, encodeEnvelope(msg.agentId.toLowerCase(), to, msg.payload)));
      return;
    }
    TraceLog.log('COORD', 'MAD to unknown agent $to — dropped');
  }

  @override
  void dispose() {
    _replySubscription?.cancel();
    _replyPort.close();
    for (final isolate in _isolates) {
      isolate.kill(priority: Isolate.immediate);
    }
    super.dispose();
  }

  // ===========================================================================
  // BUILD
  // ===========================================================================

  @override
  Widget build(BuildContext context) {
    final human = _agents[_cast.human];
    final surface = (human != null && human.ui != null)
        ? AgentSurface(agentId: human.agentId, runtime: human.ui!)
        : const Center(child: CircularProgressIndicator(color: Colors.green));
    final banner = Container(
      color: const Color(0xFF1B3D2F),
      padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 4),
      child: Row(
        children: [
          Text(
            '${_cast.human.toUpperCase()}  ·  actor: ${_cast.actor}',
            style: const TextStyle(color: Colors.white, fontSize: 12),
          ),
          const Spacer(),
          Text(
            _netStatus,
            style: const TextStyle(color: Color(0xFF7CFC8A), fontSize: 12),
          ),
        ],
      ),
    );
    final body = Column(
      children: [banner, Expanded(child: surface)],
    );
    if (Platform.isIOS) {
      return Material(
          color: Colors.white, child: SafeArea(child: body));
    }
    return Scaffold(
      backgroundColor: const Color(0xFF2B2B33),
      body: Center(
        child: Container(
          width: 400,
          height: 860,
          padding: const EdgeInsets.all(12),
          decoration: BoxDecoration(
            color: Colors.black,
            borderRadius: BorderRadius.circular(44),
          ),
          child: ClipRRect(
            borderRadius: BorderRadius.circular(32),
            child: Container(color: Colors.white, child: body),
          ),
        ),
      ),
    );
  }
}

// =============================================================================
// Minimal file/console logger (mirrors main.dart's TraceLogger)
// =============================================================================

class TraceLog {
  static const _logPath = '/tmp/glp_duo_trace.log';
  static void log(String tag, String message) {
    final ts = DateTime.now().toIso8601String().substring(11, 23);
    final line = '[$ts] [$tag] $message\n';
    try {
      File(_logPath).writeAsStringSync(line, mode: FileMode.append, flush: true);
    } catch (_) {}
    debugPrint(line.trim());
  }
}
