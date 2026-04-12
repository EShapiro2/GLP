/// GLP Social Graph — Multi-Isolate SG Plays
///
/// Runs SG plays (1-3) via AgentRuntime + IsolateRouter (multi-isolate madGLP),
/// with tagged output parsed and routed to per-agent read-only panels
/// (Alice, Bob, Charlie).
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

/// GLP source files for SG madGLP plays (loaded from cssg/ directory).
const _glpFileNames = [
  'typed_social_agent.glp',
  'typed_ui_mediator.glp',
  'typed_ui_actors.glp',
  'play_ui_madglp_boot.glp',
];

/// Resolve absolute path to programs/self.glp.
String _resolveRootSelfGlpPath() {
  final candidate = File('../programs/self.glp').absolute.path;
  if (File(candidate).existsSync()) return candidate;
  const fallback = '/Users/udi/Grassroots/GLP/programs/self.glp';
  if (File(fallback).existsSync()) return fallback;
  return candidate;
}

final _rootSelfGlpPath = _resolveRootSelfGlpPath();

/// GLP source directory (repo-relative from glp_multiagent/).
const _glpDir = '../programs/typed_book/cssg';

/// Agents in each SG play.
const _sgAgents = ['alice', 'bob', 'charlie'];

/// Tagged output regex: tagged(alice, cmd(connect(bob)))
final _taggedRegex = RegExp(r'^tagged\((\w+), (cmd|notify)\((.+)\)\)$');

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const SgMadApp());
}

// =============================================================================
// APP
// =============================================================================

class SgMadApp extends StatelessWidget {
  const SgMadApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Social Graph (Multi-Isolate)',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.green,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.green,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Colors.green,
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: Colors.green,
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const SgMadScreen(),
    );
  }
}

// =============================================================================
// PER-AGENT UI STATE
// =============================================================================

class _AgentState {
  final String agentId;
  final List<String> outputLog = [];
  final ScrollController scrollController = ScrollController();
  SendPort? commandPort;

  _AgentState(this.agentId);

  void dispose() {
    scrollController.dispose();
  }
}

// =============================================================================
// SCREEN
// =============================================================================

class SgMadScreen extends StatefulWidget {
  const SgMadScreen({super.key});

  @override
  State<SgMadScreen> createState() => _SgMadScreenState();
}

class _SgMadScreenState extends State<SgMadScreen> {
  final Map<String, _AgentState> _agents = {};
  final List<String> _log = [];
  List<String>? _cachedGlpSources;

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

  Future<List<String>?> _loadGlpSources() async {
    if (_cachedGlpSources != null) return _cachedGlpSources;

    try {
      final sources = <String>[];
      for (final filename in _glpFileNames) {
        final file = File('$_glpDir/$filename');
        if (!file.existsSync()) {
          _log.add('ERROR: File not found: $_glpDir/$filename');
          setState(() {});
          return null;
        }
        sources.add(await file.readAsString());
      }
      _cachedGlpSources = sources;
      return sources;
    } catch (e) {
      _log.add('ERROR reading GLP files: $e');
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
      debugPrint('[${msg.tag}] ${msg.message}');
    } else if (msg is AgentSendMad) {
      IsolateRouter.instance.route(msg.agentId, msg.to, msg.payload);
    } else if (msg is AgentError) {
      setState(() {
        _log.add('ERROR from ${msg.agentId}: ${msg.error}');
      });
    }
  }

  /// Parse tagged output from _output/1 and route to per-agent panel.
  /// Output format from AgentRuntime: "< tagged(alice, cmd(connect(bob)))"
  void _routeOutput(String sourceAgent, String line) {
    // Strip "< " prefix added by AgentRuntime
    final stripped = line.startsWith('< ') ? line.substring(2) : line;
    final match = _taggedRegex.firstMatch(stripped);
    if (match == null) return; // Skip non-tagged output (init messages etc.)

    final agentId = match.group(1)!;
    final kind = match.group(2)!;
    final content = match.group(3)!;

    // Capitalize to match _AgentState keys
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

    final sources = await _loadGlpSources();
    if (sources == null) return;

    // Create read-only agent panels
    for (final id in ['Alice', 'Bob', 'Charlie']) {
      _agents[id] = _AgentState(id);
    }

    _expectedAgentCount = _sgAgents.length;
    _allReadyCompleter = Completer<void>();

    setState(() {
      _log.add('Starting Play $playNumber (multi-isolate)...');
    });

    // Phase 1: Spawn all isolates with deferStart — no GLP runs yet.
    for (final agentId in _sgAgents) {
      final initMsg = InitAgent(
        agentId: agentId,
        glpSources: sources,
        rootSelfGlpPath: _rootSelfGlpPath,
        friends: [],
        replyPort: _replyPort.sendPort,
        goalLabel: 'agent_init_play/3',
        extraArgs: ['$playNumber'],
        deferStart: true,
      );

      try {
        await Isolate.spawn(agentIsolateEntry, initMsg);
        debugPrint('Spawned isolate for $agentId');
      } catch (e) {
        setState(() {
          _log.add('ERROR spawning $agentId: $e');
        });
      }
    }

    // Phase 2: Wait for all agents to register their ports.
    await _allReadyCompleter!.future;

    // Phase 3: All ports registered — send StartAgent to begin GLP execution.
    for (final agent in _agents.values) {
      agent.commandPort?.send(StartAgent());
    }

    debugPrint('All ${_sgAgents.length} agents started');
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
        title: const Text('Social Graph (Multi-Isolate)'),
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
      color: Colors.green.shade50,
      child: Row(
        children: [
          ElevatedButton.icon(
            onPressed: () => _runPlay(1),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 1'),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: () => _runPlay(2),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 2'),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: () => _runPlay(3),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 3'),
          ),
        ],
      ),
    );
  }

  Widget _buildAgentPanel(_AgentState agent) {
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
            color: Colors.green,
            child: Row(
              children: [
                Text(
                  agent.agentId,
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
              color: Colors.grey.shade100,
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
                            ? Colors.blue.shade800
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
      color: Colors.green.shade50,
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
