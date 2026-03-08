/// GLP Social Graph — Simulated SG Plays
///
/// Runs SG plays (fplay1-3) via REPL subprocess, with tagged output
/// parsed and routed to per-agent read-only panels (Alice, Bob, Charlie).
library;

import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter/material.dart';
import 'package:glp_runtime/multiagent/repl_play_runner.dart';

import 'isolate_protocol.dart';
import 'mad_router.dart';

// =============================================================================
// SHARED FILE LOGGER
// =============================================================================

/// Shared file logger for tracing execution across coordinator and agents.
/// Uses simple file append to avoid stream conflicts across isolates.
class TraceLogger {
  static final TraceLogger _instance = TraceLogger._();
  static TraceLogger get instance => _instance;

  TraceLogger._();

  static const String _logPath = '/tmp/glp_multiagent_trace.log';
  bool _initialized = false;

  void init({bool clear = true}) {
    if (_initialized) return;
    final file = File(_logPath);
    if (clear && file.existsSync()) {
      file.deleteSync();
    }
    _initialized = true;
    log('TRACE', '=== Logger initialized ===');
  }

  void log(String source, String message) {
    final timestamp = DateTime.now().toIso8601String().substring(11, 23);
    final line = '[$timestamp] [$source] $message\n';
    try {
      File(_logPath).writeAsStringSync(line, mode: FileMode.append, flush: true);
    } catch (_) {}
    debugPrint(line.trim());
  }

  void close() {
    _initialized = false;
  }
}

/// Default GLP program directory.
/// Override locally by creating glp_multiagent/glp_config.json:
///   { "glp_dir": "/your/path/to/GLP/programs/typed_book/social_graph" }
/// That file is .gitignore'd so it won't be pushed.
final _defaultGlpDir = () {
  try {
    // Look for glp_config.json next to the repo's glp_multiagent/ directory.
    // Walk up from executable to find it, or check current working directory.
    for (final base in [
      File(Platform.resolvedExecutable).parent,
      Directory.current,
    ]) {
      var dir = base is File ? (base as File).parent : base as Directory;
      for (var i = 0; i < 10; i++) {
        final configFile = File('${dir.path}/glp_multiagent/glp_config.json');
        if (configFile.existsSync()) {
          final content = configFile.readAsStringSync();
          // Simple JSON parse — extract glp_dir value
          final match = RegExp(r'"glp_dir"\s*:\s*"([^"]+)"').firstMatch(content);
          if (match != null) return match.group(1)!;
        }
        dir = dir.parent;
      }
    }
  } catch (_) {}
  // Fallback to original default
  return '/Users/ohadey/Desktop/Grassroots/GLP2/GLP/programs/typed_book/social_graph';
}();

/// Stdlib directory (repo-relative from glp_multiagent/)
const _stdlibDir = '../programs/stdlib';

/// GLP files loaded for UI agents (order matters: shared first, then boot)
const _glpFiles = [
  'social_graph_agent.glp',
  'social_graph_ui_mediator.glp',
  'social_graph_ui_boot.glp',
];

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const CoordinatorApp());
}

// =============================================================================
// COORDINATOR APP
// =============================================================================

class CoordinatorApp extends StatelessWidget {
  const CoordinatorApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Social Graph',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.orange,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.orange,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Colors.orange,
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: Colors.orange,
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const CoordinatorScreen(),
    );
  }
}

// =============================================================================
// PER-AGENT UI STATE
// =============================================================================

class AgentState {
  final String agentId;
  final List<String> friends;
  final bool readOnly;
  SendPort? commandPort;
  bool initialized = false;
  String status = 'Spawning...';
  final List<String> outputLog = [];
  int goalCount = 0;
  int heapVars = 0;
  int wpSize = 0;
  int mpSize = 0;
  final TextEditingController inputController = TextEditingController();
  final ScrollController scrollController = ScrollController();
  final FocusNode inputFocusNode = FocusNode();

  AgentState(this.agentId, this.friends, {this.readOnly = false});

  void dispose() {
    inputController.dispose();
    scrollController.dispose();
    inputFocusNode.dispose();
  }
}

// =============================================================================
// COORDINATOR SCREEN
// =============================================================================

class CoordinatorScreen extends StatefulWidget {
  const CoordinatorScreen({super.key});

  @override
  State<CoordinatorScreen> createState() => _CoordinatorScreenState();
}

class _CoordinatorScreenState extends State<CoordinatorScreen> {
  final Map<String, AgentState> _agents = {};
  final List<String> _log = [];
  final TextEditingController _glpPathController =
      TextEditingController(text: _defaultGlpDir);
  String _currentGlpDir = _defaultGlpDir;
  List<String>? _cachedGlpSources;

  final ReceivePort _replyPort = ReceivePort();
  StreamSubscription? _replySubscription;

  @override
  void initState() {
    super.initState();

    TraceLogger.instance.init(clear: true);
    TraceLogger.instance.log('COORD', 'Coordinator started (isolate mode)');

    IsolateRouter.instance.onLogUpdate = () {
      setState(() {});
    };

    // Listen for messages from all agent isolates.
    _replySubscription = _replyPort.listen(_handleAgentMessage);

    _log.add('Coordinator started (isolate mode)');
    _log.add('GLP dir: $_currentGlpDir');
  }

  void _handleAgentMessage(dynamic msg) {
    if (msg is AgentReady) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.commandPort = msg.commandPort;
        state.initialized = true;
        state.status = 'Ready';
        IsolateRouter.instance.register(msg.agentId, msg.commandPort);
        TraceLogger.instance.log('COORD', '${msg.agentId} ready');
      }
      setState(() {});
    } else if (msg is AgentOutput) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.outputLog.add(msg.line);
        setState(() {});
        _scrollAgentToBottom(state);
      }
    } else if (msg is AgentLog) {
      TraceLogger.instance.log(msg.tag, msg.message);
    } else if (msg is AgentSendMad) {
      TraceLogger.instance.log(
          'COORD', 'ROUTING: ${msg.agentId} -> ${msg.to} (${msg.payload.length} bytes)');
      IsolateRouter.instance.route(msg.agentId, msg.to, msg.payload);
    } else if (msg is AgentStats) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.goalCount = msg.goals;
        state.heapVars = msg.heap;
        state.wpSize = msg.wp;
        state.mpSize = msg.mp;
        setState(() {});
      }
    } else if (msg is AgentError) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.status = 'Error: ${msg.error}';
        setState(() {});
      }
      TraceLogger.instance.log('COORD', 'ERROR from ${msg.agentId}: ${msg.error}');
    }
  }

  void _scrollAgentToBottom(AgentState agent) {
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

  @override
  void dispose() {
    _replySubscription?.cancel();
    _replyPort.close();
    _glpPathController.dispose();
    for (final agent in _agents.values) {
      agent.dispose();
    }
    super.dispose();
  }

  // ===========================================================================
  // GLP SOURCE LOADING
  // ===========================================================================

  Future<void> _updateGlpPath() async {
    final newDir = _glpPathController.text.trim();
    if (newDir.isEmpty || !Directory(newDir).existsSync()) {
      setState(() {
        _log.add('ERROR: Directory not found: $newDir');
      });
      return;
    }

    try {
      final sources = <String>[];
      for (final filename in _glpFiles) {
        final file = File('$newDir/$filename');
        if (!file.existsSync()) {
          setState(() {
            _log.add('ERROR: File not found: $newDir/$filename');
          });
          return;
        }
        sources.add(await file.readAsString());
      }
      _cachedGlpSources = sources;

      setState(() {
        _currentGlpDir = newDir;
        final totalChars = sources.fold<int>(0, (sum, s) => sum + s.length);
        _log.add(
            'GLP loaded: ${_glpFiles.join(", ")} ($totalChars chars in ${sources.length} files)');
      });
    } catch (e) {
      setState(() {
        _log.add('ERROR reading files: $e');
      });
    }
  }

  // ===========================================================================
  // AGENT SPAWNING
  // ===========================================================================

  Future<void> _spawnAgent(String agentId, List<String> friends) async {
    if (_agents.containsKey(agentId)) {
      _log.add('$agentId already spawned');
      setState(() {});
      return;
    }

    // Load GLP source if not cached
    if (_cachedGlpSources == null) {
      await _updateGlpPath();
      if (_cachedGlpSources == null) {
        _log.add('ERROR: Could not load GLP source');
        setState(() {});
        return;
      }
    }

    final agentState = AgentState(agentId, friends);
    _agents[agentId] = agentState;
    setState(() {});

    final initMsg = InitAgent(
      agentId: agentId,
      glpSources: _cachedGlpSources!,
      stdlibDir: _stdlibDir,
      friends: friends,
      replyPort: _replyPort.sendPort,
    );

    try {
      await Isolate.spawn(agentIsolateEntry, initMsg);
      TraceLogger.instance.log('COORD', 'Spawned isolate for $agentId');
      _log.add('Spawned $agentId (friends=${friends.join(", ")})');
      setState(() {});
    } catch (e) {
      _log.add('ERROR spawning $agentId: $e');
      _agents.remove(agentId);
      agentState.dispose();
      setState(() {});
    }
  }

  Future<void> _spawnLinearTopology() async {
    // Linear: Alice↔Bob↔Charlie
    await _spawnAgent('Alice', ['Bob']);
    await _spawnAgent('Bob', ['Alice', 'Charlie']);
    await _spawnAgent('Charlie', ['Bob']);
  }

  Future<void> _closeAll() async {
    // Kill REPL subprocess if running
    _playRunner?.kill();
    _playRunner = null;

    for (final agent in _agents.values) {
      if (agent.commandPort != null) {
        agent.commandPort!.send(DisposeAgent());
        IsolateRouter.instance.unregister(agent.agentId);
      }
      agent.dispose();
    }
    _agents.clear();
    IsolateRouter.instance.clearLog();
    _log.add('Closed all agents');
    setState(() {});
  }

  // ===========================================================================
  // SIMULATED PLAYS (via ReplPlayRunner)
  // ===========================================================================

  ReplPlayRunner? _playRunner;

  /// Resolve the GLP repo root from the glp_multiagent working directory.
  /// The app may run from the repo (development) or from a bundle (release).
  static String _resolveRepoRoot() {
    // In development, cwd is glp_multiagent/ and repo root is ..
    // Check for the glp_runtime sibling directory as a landmark.
    final devRoot = Directory.current.parent.path;
    if (Directory('$devRoot/glp_runtime').existsSync()) {
      return devRoot;
    }
    // Fallback: try absolute paths
    const fallbacks = [
      '/Users/ohadey/Desktop/Grassroots/GLP2/GLP',
      '/Users/udi/Grassroots/GLP',
    ];
    for (final fallback in fallbacks) {
      if (Directory('$fallback/glp_runtime').existsSync()) {
        return fallback;
      }
    }
    return devRoot; // best guess
  }

  Future<void> _runPlay(int playNumber) async {
    await _closeAll();

    // Create read-only agent panels based on play type
    final List<String> agentIds;
    if (playNumber == 15) {
      agentIds = ['Alice', 'Bob', 'Charlie', 'Dave'];
    } else if (playNumber <= 3 || (playNumber >= 12 && playNumber <= 14)) {
      agentIds = ['Alice', 'Bob', 'Charlie'];
    } else if (playNumber >= 8) {
      agentIds = ['Alice', 'Bob'];
    } else {
      agentIds = ['Alice', 'Carol', 'Bob', 'Dave'];
    }
    for (final id in agentIds) {
      final agent = AgentState(id, [], readOnly: true);
      agent.initialized = true;
      agent.status = 'Play $playNumber';
      _agents[id] = agent;
    }

    final repoRoot = _resolveRepoRoot();
    setState(() {
      _log.add('Starting fplay$playNumber (repo: $repoRoot)...');
    });

    final runner = ReplPlayRunner(repoRoot: repoRoot, sourceFiles: const [
      '../programs/typed_book/gsn/typed_social_agent.glp',
      '../programs/typed_book/gsn/typed_ui_mediator.glp',
      '../programs/typed_book/gsn/typed_ui_actors.glp',
      '../programs/typed_book/gsn/play_ui_sim_boot.glp',
    ]);
    _playRunner = runner;

    runner.onOutput = (output) {
      // Capitalize to match AgentState keys (Alice, Bob, Charlie)
      final key = output.agentId[0].toUpperCase() + output.agentId.substring(1);
      final state = _agents[key];
      if (state == null) return;

      final displayLine = output.kind == 'cmd' ? '> ${output.content}' : '< ${output.content}';
      state.outputLog.add(displayLine);
      setState(() {});
      _scrollAgentToBottom(state);
    };

    runner.onLog = (line) {
      TraceLogger.instance.log('REPL', line);
    };

    runner.onError = (error) {
      TraceLogger.instance.log('REPL-ERR', error);
      setState(() {
        _log.add('REPL ERROR: $error');
      });
    };

    runner.onDone = (exitCode) {
      _playRunner = null;
      setState(() {
        _log.add('fplay$playNumber finished (exit $exitCode)');
      });
    };

    await runner.run(playNumber);
  }

  // ===========================================================================
  // INPUT HANDLING
  // ===========================================================================

  void _sendInputToAgent(AgentState agent) {
    final text = agent.inputController.text.trim();
    if (text.isEmpty || agent.commandPort == null) return;
    agent.inputController.clear();
    agent.commandPort!.send(UserInput(text));
    agent.inputFocusNode.requestFocus();
  }

  // ===========================================================================
  // BUILD
  // ===========================================================================

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Social Graph'),
      ),
      body: Column(
        children: [
          _buildGlpPathBar(),
          _buildControlBar(),
          // Agent panels
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
          // Routing log
          _buildRoutingLog(),
          // Coordinator log
          _buildCoordinatorLog(),
        ],
      ),
    );
  }

  Widget _buildGlpPathBar() {
    return Container(
      padding: const EdgeInsets.all(8.0),
      color: Colors.orange.shade100,
      child: Row(
        children: [
          const Text('GLP dir: ', style: TextStyle(fontWeight: FontWeight.bold)),
          Expanded(
            child: TextField(
              controller: _glpPathController,
              style: const TextStyle(fontSize: 12, fontFamily: 'monospace'),
              decoration: const InputDecoration(
                isDense: true,
                contentPadding:
                    EdgeInsets.symmetric(horizontal: 8, vertical: 8),
                border: OutlineInputBorder(),
              ),
              onSubmitted: (_) => _updateGlpPath(),
            ),
          ),
          const SizedBox(width: 8),
          ElevatedButton(
            onPressed: _updateGlpPath,
            child: const Text('Set'),
          ),
        ],
      ),
    );
  }

  Widget _buildControlBar() {
    Widget playButton(int n) => ElevatedButton.icon(
          onPressed: () => _runPlay(n),
          icon: const Icon(Icons.play_arrow),
          label: Text('Play $n'),
          style: ElevatedButton.styleFrom(backgroundColor: Colors.green),
        );
    Widget sep() => Padding(
          padding: const EdgeInsets.symmetric(horizontal: 8),
          child: Container(width: 1, height: 30, color: Colors.grey),
        );

    return Container(
      padding: const EdgeInsets.all(16.0),
      color: Colors.orange.shade50,
      child: SingleChildScrollView(
        scrollDirection: Axis.horizontal,
        child: Row(
          children: [
            for (final n in [1, 2, 3]) ...[playButton(n), const SizedBox(width: 8)],
            sep(),
            for (final n in [4, 5, 6, 7]) ...[playButton(n), const SizedBox(width: 8)],
            sep(),
            for (final n in [8, 9, 10, 11]) ...[playButton(n), const SizedBox(width: 8)],
            sep(),
            for (final n in [12, 13, 14]) ...[playButton(n), const SizedBox(width: 8)],
            sep(),
            playButton(15), const SizedBox(width: 8),
          ],
        ),
      ),
    );
  }

  Widget _buildAgentPanel(AgentState agent) {
    return Container(
      decoration: BoxDecoration(
        border: Border(
          right: BorderSide(color: Colors.grey.shade300),
        ),
      ),
      child: Column(
        children: [
          // Agent header
          Container(
            padding:
                const EdgeInsets.symmetric(horizontal: 8.0, vertical: 6.0),
            color: Colors.orange,
            child: Row(
              children: [
                Text(
                  '${agent.agentId} (${agent.friends.join(", ")})',
                  style: const TextStyle(
                    color: Colors.white,
                    fontWeight: FontWeight.bold,
                    fontSize: 13,
                  ),
                ),
              ],
            ),
          ),
          // Output log
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
                        color: _outputLineColor(line),
                        fontWeight: _outputLineBold(line)
                            ? FontWeight.bold
                            : FontWeight.normal,
                      ),
                    ),
                  );
                },
              ),
            ),
          ),
          // Input area (hidden for read-only play panels)
          if (!agent.readOnly)
            Container(
              padding: const EdgeInsets.all(8.0),
              color: Colors.orange.shade50,
              child: Row(
                children: [
                  Expanded(
                    child: TextField(
                      controller: agent.inputController,
                      focusNode: agent.inputFocusNode,
                      enabled: agent.initialized,
                      decoration: InputDecoration(
                        hintText: agent.initialized
                            ? 'connect ${agent.friends.isNotEmpty ? agent.friends.first.toLowerCase() : "friend"}'
                            : 'Initializing...',
                        border: OutlineInputBorder(
                          borderRadius: BorderRadius.circular(8),
                        ),
                        contentPadding: const EdgeInsets.symmetric(
                          horizontal: 12,
                          vertical: 8,
                        ),
                      ),
                      onSubmitted: (_) => _sendInputToAgent(agent),
                    ),
                  ),
                  const SizedBox(width: 8),
                  ElevatedButton(
                    onPressed:
                        agent.initialized ? () => _sendInputToAgent(agent) : null,
                    child: const Text('Send'),
                  ),
                ],
              ),
            ),
          // Status bar
          Container(
            padding:
                const EdgeInsets.symmetric(horizontal: 8.0, vertical: 8.0),
            color: Colors.orange.shade100,
            child: Row(
              children: [
                Text(
                  'G:${agent.goalCount} H:${agent.heapVars} W:${agent.wpSize} M:${agent.mpSize}',
                  style: const TextStyle(
                      fontWeight: FontWeight.w500, fontSize: 10),
                ),
                const SizedBox(width: 8),
                Container(
                  width: 8,
                  height: 8,
                  decoration: BoxDecoration(
                    shape: BoxShape.circle,
                    color: agent.initialized ? Colors.orange : Colors.grey,
                  ),
                ),
                const SizedBox(width: 4),
                Expanded(
                  child: Text(
                    agent.status,
                    style: const TextStyle(fontSize: 10),
                    overflow: TextOverflow.ellipsis,
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildRoutingLog() {
    final log = IsolateRouter.instance.routingLog;
    return Container(
      height: 100,
      color: Colors.grey.shade100,
      child: ListView.builder(
        padding: const EdgeInsets.all(8.0),
        itemCount: log.length,
        itemBuilder: (context, index) {
          return Text(
            log[index],
            style: const TextStyle(fontFamily: 'monospace', fontSize: 12),
          );
        },
      ),
    );
  }

  Widget _buildCoordinatorLog() {
    return Container(
      height: 80,
      color: Colors.orange.shade50,
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

  // ===========================================================================
  // OUTPUT LINE STYLING (same as previous AgentScreen)
  // ===========================================================================

  Color _outputLineColor(String line) {
    if (line.startsWith('>')) return Colors.blue.shade800;
    if (line.startsWith('<')) return Colors.green.shade800;
    if (line.startsWith('[SEND') || line.startsWith('[IRMA SEND')) {
      return Colors.purple.shade800;
    }
    if (line.startsWith('[RECV') || line.startsWith('[IRMA RECV')) {
      return Colors.teal.shade800;
    }
    if (line.startsWith('[IRMA')) return Colors.deepPurple.shade800;
    if (line.startsWith('[')) return Colors.orange.shade800;
    return Colors.black87;
  }

  bool _outputLineBold(String line) {
    return line.startsWith('[') || line.startsWith('<');
  }
}
