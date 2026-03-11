/// GLP Child-Safe Social Graph — Modules + Multi-Isolate
///
/// Same as main_cssg_mad.dart but uses project-compiled modules from
/// cssg_modules/ instead of loading monolithic source files.
///
/// Each agent isolate:
///   1. Loads the linked project via engine.loadProject(cssg_modules/)
///   2. Loads mad_boot.glp on top (parent_init, child_init, tee, ui_actor, ...)
///   3. Runs parent_init/4 or child_init/3 — which call agent(), ui_mediator(),
///      merge(), etc. via entry-point aliases from the linked project.
library;

import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter/material.dart';

import 'isolate_protocol.dart';
import 'mad_router.dart';

// =============================================================================
// CONSTANTS
// =============================================================================

/// Project directory for static linking (repo-relative from glp_multiagent/).
const _projectDir = '../programs/cssg_modules';

/// madGLP boot source — loaded on top of the linked project.
const _bootFileName = 'mad_boot.glp';

/// Resolve absolute path to programs/self.glp.
String _resolveRootSelfGlpPath() {
  final candidate = File('../programs/self.glp').absolute.path;
  if (File(candidate).existsSync()) return candidate;
  const fallback = '/Users/udi/Grassroots/GLP/programs/self.glp';
  if (File(fallback).existsSync()) return fallback;
  return candidate;
}

final _rootSelfGlpPath = _resolveRootSelfGlpPath();

/// Tagged output regex: tagged(alice, cmd(connect(bob)))
final _taggedRegex = RegExp(r'^tagged\((\w+), (cmd|notify)\((.+)\)\)$');

/// Agent display info.
class _AgentInfo {
  final String id;
  final String role;    // "Parent" or "Child"
  final Color headerColor;
  final Color bgColor;

  const _AgentInfo(this.id, this.role, this.headerColor, this.bgColor);
}

/// Panel order: Parent, Child, Parent, Child — grouped by family.
const _agentInfos = [
  _AgentInfo('Alice', 'Parent', Color(0xFF3949AB), Color(0xFFE8EAF6)),
  _AgentInfo('Carol', 'Child',  Color(0xFF7986CB), Color(0xFFF5F5FF)),
  _AgentInfo('Bob',   'Parent', Color(0xFF00897B), Color(0xFFE0F2F1)),
  _AgentInfo('Dave',  'Child',  Color(0xFF4DB6AC), Color(0xFFF5FFFE)),
];

/// CSSG isolate spawn config: agentId, goalLabel, extraArgs.
class _SpawnConfig {
  final String agentId;
  final String goalLabel;
  final List<String> extraArgs;
  const _SpawnConfig(this.agentId, this.goalLabel, this.extraArgs);
}

/// Build spawn configs for a given play number.
List<_SpawnConfig> _cssgSpawnConfigs(int playNum) => [
  _SpawnConfig('alice', 'parent_init/4', ['carol', '$playNum']),
  _SpawnConfig('carol', 'child_init/3', ['$playNum']),
  _SpawnConfig('bob', 'parent_init/4', ['dave', '$playNum']),
  _SpawnConfig('dave', 'child_init/3', ['$playNum']),
];

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const CssgMadModulesApp());
}

// =============================================================================
// APP
// =============================================================================

class CssgMadModulesApp extends StatelessWidget {
  const CssgMadModulesApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'CSSG (Modules + Multi-Isolate)',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.indigo,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.indigo,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Colors.indigo,
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: Colors.indigo,
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const CssgMadModulesScreen(),
    );
  }
}

// =============================================================================
// PER-AGENT UI STATE
// =============================================================================

class _AgentState {
  final _AgentInfo info;
  final List<String> outputLog = [];
  final ScrollController scrollController = ScrollController();
  SendPort? commandPort;

  _AgentState(this.info);

  String get agentId => info.id;

  void dispose() {
    scrollController.dispose();
  }
}

// =============================================================================
// SCREEN
// =============================================================================

class CssgMadModulesScreen extends StatefulWidget {
  const CssgMadModulesScreen({super.key});

  @override
  State<CssgMadModulesScreen> createState() => _CssgMadModulesScreenState();
}

class _CssgMadModulesScreenState extends State<CssgMadModulesScreen> {
  final Map<String, _AgentState> _agents = {};
  final List<String> _log = [];
  String? _cachedBootSource;

  final ReceivePort _replyPort = ReceivePort();
  StreamSubscription? _replySubscription;

  /// Number of agents expected for the current play.
  int _expectedAgentCount = 0;

  /// Completed when all agents have sent [AgentReady] and are registered.
  Completer<void>? _allReadyCompleter;

  @override
  void initState() {
    super.initState();
    _replySubscription = _replyPort.listen(_handleAgentMessage);
    _log.add('Ready. Click a Play button to run a scenario.');
    _log.add('Using project-compiled modules from cssg_modules/.');
  }

  @override
  void dispose() {
    _closeAll();
    _replySubscription?.cancel();
    _replyPort.close();
    super.dispose();
  }

  // ===========================================================================
  // GLP SOURCE LOADING
  // ===========================================================================

  Future<String?> _loadBootSource() async {
    if (_cachedBootSource != null) return _cachedBootSource;

    try {
      final file = File('$_projectDir/$_bootFileName');
      if (!file.existsSync()) {
        _log.add('ERROR: Boot file not found: $_projectDir/$_bootFileName');
        setState(() {});
        return null;
      }
      _cachedBootSource = await file.readAsString();
      return _cachedBootSource;
    } catch (e) {
      _log.add('ERROR reading boot file: $e');
      setState(() {});
      return null;
    }
  }

  // ===========================================================================
  // AGENT MESSAGE HANDLING
  // ===========================================================================

  void _handleAgentMessage(dynamic msg) {
    if (msg is AgentReady) {
      final key = msg.agentId[0].toUpperCase() + msg.agentId.substring(1);
      final state = _agents[key];
      if (state != null) {
        state.commandPort = msg.commandPort;
        IsolateRouter.instance.register(msg.agentId, msg.commandPort);
      }
      setState(() {});

      // Check if all agents are now registered.
      final readyCount =
          _agents.values.where((a) => a.commandPort != null).length;
      if (readyCount >= _expectedAgentCount &&
          _allReadyCompleter != null &&
          !_allReadyCompleter!.isCompleted) {
        _allReadyCompleter!.complete();
      }
    } else if (msg is AgentOutput) {
      _routeOutput(msg.agentId, msg.line);
    } else if (msg is AgentLog) {
      if (msg.message.contains('INIT:') || msg.message.contains('ERROR') || msg.message.contains('RUN:')) {
        debugPrint('[LOG ${msg.agentId}] ${msg.message}');
      }
    } else if (msg is AgentSendMad) {
      debugPrint('[MAD] ${msg.agentId} -> ${msg.to} (${msg.payload.length} bytes)');
      IsolateRouter.instance.route(msg.agentId, msg.to, msg.payload);
    } else if (msg is AgentError) {
      setState(() {
        _log.add('ERROR from ${msg.agentId}: ${msg.error}');
      });
    }
  }

  /// Parse tagged output and route to per-agent panel.
  void _routeOutput(String sourceAgent, String line) {
    final stripped = line.startsWith('< ') ? line.substring(2) : line;
    final match = _taggedRegex.firstMatch(stripped);
    if (match == null) return;

    final agentId = match.group(1)!;
    final kind = match.group(2)!;
    final content = match.group(3)!;

    final key = agentId[0].toUpperCase() + agentId.substring(1);
    final state = _agents[key];
    if (state == null) return;

    final displayLine = kind == 'cmd' ? '> $content' : '< $content';
    state.outputLog.add(displayLine);
    setState(() {});
    _scrollToBottom(state);
  }

  void _scrollToBottom(_AgentState agent) {
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (agent.scrollController.hasClients) {
        agent.scrollController.animateTo(
          agent.scrollController.position.maxScrollExtent,
          duration: const Duration(milliseconds: 200),
          curve: Curves.easeOut,
        );
      }
    });
  }

  // ===========================================================================
  // PLAY EXECUTION
  // ===========================================================================

  Future<void> _runPlay(int playNumber) async {
    _closeAll();

    final bootSource = await _loadBootSource();
    if (bootSource == null) return;

    // Create read-only agent panels
    for (final info in _agentInfos) {
      _agents[info.id] = _AgentState(info);
    }

    final configs = _cssgSpawnConfigs(playNumber);
    _expectedAgentCount = configs.length;
    _allReadyCompleter = Completer<void>();

    setState(() {
      _log.add('Starting Play $playNumber (modules + multi-isolate, 4 agents)...');
    });

    // Phase 1: Spawn all isolates with deferStart — no GLP runs yet.
    for (final config in configs) {
      final initMsg = InitAgent(
        agentId: config.agentId,
        glpSources: [bootSource],  // Only the madGLP boot source
        rootSelfGlpPath: _rootSelfGlpPath,
        friends: [],
        replyPort: _replyPort.sendPort,
        goalLabel: config.goalLabel,
        extraArgs: config.extraArgs,
        projectDir: _projectDir,   // Linked project provides agent, mediator, actors
        deferStart: true,
      );

      try {
        await Isolate.spawn(agentIsolateEntry, initMsg);
        debugPrint('Spawned isolate for ${config.agentId} (${config.goalLabel}, modules)');
      } catch (e) {
        setState(() {
          _log.add('ERROR spawning ${config.agentId}: $e');
        });
      }
    }

    // Phase 2: Wait for all agents to register their ports.
    await _allReadyCompleter!.future;

    // Phase 3: All ports registered — send StartAgent to begin GLP execution.
    for (final agent in _agents.values) {
      agent.commandPort?.send(StartAgent());
    }

    debugPrint('All ${configs.length} agents started (modules)');
  }

  void _closeAll() {
    for (final agent in _agents.values) {
      if (agent.commandPort != null) {
        agent.commandPort!.send(DisposeAgent());
        IsolateRouter.instance.unregister(agent.agentId.toLowerCase());
      }
      agent.dispose();
    }
    _agents.clear();
    IsolateRouter.instance.clearLog();
  }

  // ===========================================================================
  // BUILD
  // ===========================================================================

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('CSSG (Modules + Multi-Isolate)'),
      ),
      body: Column(
        children: [
          _buildControlBar(),
          Expanded(
            child: _agents.isEmpty
                ? const Center(
                    child: Text('Click a Play button above to run a scenario.'))
                : Row(
                    crossAxisAlignment: CrossAxisAlignment.stretch,
                    children: _agents.values
                        .map((agent) => Expanded(child: _buildAgentPanel(agent)))
                        .toList(),
                  ),
          ),
          _buildLog(),
        ],
      ),
    );
  }

  Widget _buildControlBar() {
    return Container(
      padding: const EdgeInsets.all(16.0),
      color: Colors.indigo.shade50,
      child: Row(
        children: [
          ElevatedButton.icon(
            onPressed: () => _runPlay(4),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 4'),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: () => _runPlay(5),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 5'),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: () => _runPlay(6),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 6'),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: () => _runPlay(7),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 7'),
          ),
        ],
      ),
    );
  }

  Widget _buildAgentPanel(_AgentState agent) {
    final info = agent.info;
    return Container(
      decoration: BoxDecoration(
        border: Border(
          right: BorderSide(color: Colors.grey.shade300),
        ),
      ),
      child: Column(
        children: [
          Container(
            padding:
                const EdgeInsets.symmetric(horizontal: 8.0, vertical: 6.0),
            color: info.headerColor,
            child: Row(
              children: [
                Text(
                  '${info.role}: ${info.id}',
                  style: const TextStyle(
                    color: Colors.white,
                    fontWeight: FontWeight.bold,
                    fontSize: 13,
                  ),
                ),
              ],
            ),
          ),
          Expanded(
            child: Container(
              color: info.bgColor,
              child: ListView.builder(
                controller: agent.scrollController,
                padding: const EdgeInsets.all(8.0),
                itemCount: agent.outputLog.length,
                itemBuilder: (context, index) {
                  final line = agent.outputLog[index];
                  return Padding(
                    padding: const EdgeInsets.symmetric(vertical: 2.0),
                    child: Text(
                      line,
                      style: TextStyle(
                        fontFamily: 'monospace',
                        fontSize: 13,
                        color: line.startsWith('>')
                            ? Colors.indigo.shade800
                            : Colors.green.shade800,
                        fontWeight: line.startsWith('<')
                            ? FontWeight.bold
                            : FontWeight.normal,
                      ),
                    ),
                  );
                },
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildLog() {
    return Container(
      height: 60,
      color: Colors.indigo.shade50,
      child: ListView.builder(
        padding: const EdgeInsets.all(8.0),
        itemCount: _log.length,
        itemBuilder: (context, index) {
          return Text(
            _log[index],
            style: const TextStyle(fontSize: 11),
          );
        },
      ),
    );
  }
}
