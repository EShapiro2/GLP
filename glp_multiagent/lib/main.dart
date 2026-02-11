/// GLP Multiagent - madGLP Integration
///
/// Coordinator window spawns agent windows, routes messages between them.
/// Uses AgentRuntime (from glp_runtime) for GLP execution and madGLP context.
/// Supports opaque byte payloads for inter-agent communication.
library;

import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter/material.dart';
import 'package:desktop_multi_window/desktop_multi_window.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

import 'mad_router.dart';

// =============================================================================
// SHARED FILE LOGGER
// =============================================================================

/// Shared file logger for tracing execution across coordinator and agents
/// Uses simple file append to avoid stream conflicts across isolates
class TraceLogger {
  static final TraceLogger _instance = TraceLogger._();
  static TraceLogger get instance => _instance;

  TraceLogger._();

  static const String _logPath = '/tmp/glp_multiagent_trace.log';
  bool _initialized = false;

  /// Initialize the logger (call once from coordinator)
  void init({bool clear = true}) {
    if (_initialized) return;
    final file = File(_logPath);
    if (clear && file.existsSync()) {
      file.deleteSync();
    }
    _initialized = true;
    log('TRACE', '=== Logger initialized ===');
  }

  /// Log a message with source tag
  void log(String source, String message) {
    final timestamp = DateTime.now().toIso8601String().substring(11, 23);
    final line = '[$timestamp] [$source] $message\n';
    // Use sync write to avoid stream issues across processes
    try {
      File(_logPath).writeAsStringSync(line, mode: FileMode.append, flush: true);
    } catch (_) {
      // Ignore file write errors
    }
    // Also print to console
    debugPrint(line.trim());
  }

  /// Close the logger (no-op for sync writes)
  void close() {
    _initialized = false;
  }
}

/// Default GLP program directory
const _defaultGlpDir = '/Users/udi/Grassroots/GLP/programs/typed_book/social_graph';

/// GLP files loaded for UI agents (order matters: shared first, then boot)
const _glpFiles = [
  'social_agent.glp',
  'ui_mediator.glp',
  'play_ui_boot.glp',
];

/// Entry point - checks if spawned window or main coordinator
void main(List<String> args) {
  debugPrint('=== MAIN ARGS: $args ===');

  if (args.firstOrNull == 'multi_window') {
    // Spawned agent window
    final windowId = int.parse(args[1]);
    final params = jsonDecode(args[2]) as Map<String, dynamic>;
    final agentId = params['agentId'] as String;
    final friends = (params['friends'] as List).cast<String>();
    final glpSource = params['glpSource'] as String;
    debugPrint('=== SPAWNED AGENT WINDOW: $agentId (friends=$friends, windowId=$windowId) ===');
    runAgentWindow(windowId, agentId, friends, glpSource);
  } else {
    // Main/coordinator window
    debugPrint('=== COORDINATOR WINDOW ===');
    runCoordinatorWindow();
  }
}

/// Run the coordinator window (spawns agent windows)
void runCoordinatorWindow() {
  runApp(const CoordinatorApp());
}

/// Run an agent window (spawned by coordinator)
void runAgentWindow(int windowId, String agentId, List<String> friends, String glpSource) {
  runApp(AgentApp(windowId: windowId, agentId: agentId, friends: friends, glpSource: glpSource));
}

// ============================================================================
// COORDINATOR APP
// ============================================================================

class CoordinatorApp extends StatelessWidget {
  const CoordinatorApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'GLP Coordinator',
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

class CoordinatorScreen extends StatefulWidget {
  const CoordinatorScreen({super.key});

  @override
  State<CoordinatorScreen> createState() => _CoordinatorScreenState();
}

class _CoordinatorScreenState extends State<CoordinatorScreen> {
  final Map<String, WindowController> _windows = {};
  final List<String> _log = [];
  final TextEditingController _glpPathController = TextEditingController(text: _defaultGlpDir);
  String _currentGlpDir = _defaultGlpDir;
  String? _cachedGlpSource;

  @override
  void initState() {
    super.initState();

    // Initialize shared file logger
    TraceLogger.instance.init(clear: true);
    TraceLogger.instance.log('COORD', 'Coordinator started');

    MadRouter.instance.onLogUpdate = () {
      setState(() {});
    };

    // Set up handler for messages from agent windows
    DesktopMultiWindow.setMethodHandler((call, fromWindowId) async {
      TraceLogger.instance.log('COORD', 'RECEIVED: ${call.method} from window $fromWindowId');

      if (call.method == 'send_mad') {
        // Handle madGLP binary message routing
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final to = args['to'] as String;
        final encodedPayload = args['payload'] as String;
        final payload = base64Decode(encodedPayload);
        TraceLogger.instance.log('COORD', 'ROUTING: $from -> $to (${payload.length} bytes)');
        await MadRouter.instance.route(from, to, Uint8List.fromList(payload));
      } else if (call.method == 'send') {
        // Legacy JSON message routing (for backwards compatibility)
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final to = args['to'] as String;
        final payload = args['payload'];
        TraceLogger.instance.log('COORD', 'ROUTING (legacy): $from -> $to');
        final payloadBytes = utf8.encode(jsonEncode(payload));
        await MadRouter.instance.route(from, to, Uint8List.fromList(payloadBytes));
      }
      return null;
    });

    _log.add('Coordinator started (madGLP mode)');
    _log.add('GLP dir: $_currentGlpDir');
  }

  @override
  void dispose() {
    _glpPathController.dispose();
    super.dispose();
  }

  Future<void> _updateGlpPath() async {
    final newDir = _glpPathController.text.trim();
    if (newDir.isEmpty || !Directory(newDir).existsSync()) {
      setState(() { _log.add('ERROR: Directory not found: $newDir'); });
      return;
    }

    try {
      // Load all GLP files and concatenate
      final sources = <String>[];
      for (final filename in _glpFiles) {
        final file = File('$newDir/$filename');
        if (!file.existsSync()) {
          setState(() { _log.add('ERROR: File not found: $newDir/$filename'); });
          return;
        }
        sources.add(await file.readAsString());
      }
      _cachedGlpSource = sources.join('\n');

      setState(() {
        _currentGlpDir = newDir;
        _log.add('GLP loaded: ${_glpFiles.join(", ")} (${_cachedGlpSource!.length} chars)');
      });
    } catch (e) {
      setState(() { _log.add('ERROR reading files: $e'); });
    }
  }

  Future<void> _spawnAgent(String agentId, List<String> friends, double x, double y) async {
    if (_windows.containsKey(agentId)) {
      _log.add('$agentId already spawned');
      setState(() {});
      return;
    }

    // Load GLP source if not cached
    if (_cachedGlpSource == null) {
      await _updateGlpPath();
      if (_cachedGlpSource == null) {
        _log.add('ERROR: Could not load GLP source');
        setState(() {});
        return;
      }
    }

    try {
      final window = await DesktopMultiWindow.createWindow(
        jsonEncode({
          'agentId': agentId,
          'friends': friends,
          'glpSource': _cachedGlpSource,
        }),
      );
      await window.setTitle('$agentId - madGLP Agent');
      await window.setFrame(Rect.fromLTWH(x, y, 400, 600));
      await window.show();

      _windows[agentId] = window;
      MadRouter.instance.register(agentId, window.windowId);

      TraceLogger.instance.log('COORD', 'Spawned $agentId (window ${window.windowId})');
      _log.add('Spawned $agentId (friends=${friends.join(", ")}, window ${window.windowId})');
      setState(() {});
    } catch (e) {
      _log.add('ERROR spawning $agentId: $e');
      setState(() {});
    }
  }

  Future<void> _spawnLinearTopology() async {
    // Linear: Alice↔Bob↔Charlie
    // Alice knows Bob only
    // Bob knows Alice and Charlie
    // Charlie knows Bob only
    await _spawnAgent('Alice', ['Bob'], 100, 100);
    await _spawnAgent('Bob', ['Alice', 'Charlie'], 520, 100);
    await _spawnAgent('Charlie', ['Bob'], 940, 100);
  }

  Future<void> _closeAll() async {
    for (final entry in _windows.entries) {
      try {
        await entry.value.close();
        MadRouter.instance.unregister(entry.key);
      } catch (e) {
        _log.add('ERROR closing ${entry.key}: $e');
      }
    }
    _windows.clear();
    MadRouter.instance.clearLog();
    _log.add('Closed all windows');
    setState(() {});
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('GLP Coordinator (madGLP)'),
      ),
      body: Column(
        children: [
          // GLP file path input
          Container(
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
                      contentPadding: EdgeInsets.symmetric(horizontal: 8, vertical: 8),
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
          ),

          // Control buttons
          Container(
            padding: const EdgeInsets.all(16.0),
            color: Colors.orange.shade50,
            child: Row(
              children: [
                ElevatedButton.icon(
                  onPressed: _spawnLinearTopology,
                  icon: const Icon(Icons.people),
                  label: const Text('Alice↔Bob↔Charlie'),
                ),
                const SizedBox(width: 16),
                ElevatedButton.icon(
                  onPressed: _closeAll,
                  icon: const Icon(Icons.close),
                  label: const Text('Close All'),
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.red,
                  ),
                ),
              ],
            ),
          ),

          // Active agents
          Container(
            padding: const EdgeInsets.all(8.0),
            child: Wrap(
              spacing: 8,
              children: _windows.keys.map((id) => Chip(
                label: Text(id),
                avatar: const Icon(Icons.person, size: 18),
                backgroundColor: Colors.orange.shade100,
              )).toList(),
            ),
          ),

          const Divider(),

          // Routing log
          Expanded(
            child: Container(
              color: Colors.grey.shade100,
              child: ListView.builder(
                padding: const EdgeInsets.all(8.0),
                itemCount: MadRouter.instance.routingLog.length,
                itemBuilder: (context, index) {
                  return Text(
                    MadRouter.instance.routingLog[index],
                    style: const TextStyle(
                      fontFamily: 'monospace',
                      fontSize: 12,
                    ),
                  );
                },
              ),
            ),
          ),

          // Coordinator log
          Container(
            height: 100,
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
          ),
        ],
      ),
    );
  }
}

// ============================================================================
// AGENT APP (spawned windows)
// ============================================================================

class AgentApp extends StatelessWidget {
  final int windowId;
  final String agentId;
  final List<String> friends;
  final String glpSource;

  const AgentApp({
    super.key,
    required this.windowId,
    required this.agentId,
    required this.friends,
    required this.glpSource,
  });

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: '$agentId - madGLP Agent',
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
      home: AgentScreen(
        windowId: windowId,
        agentId: agentId,
        friends: friends,
        glpSource: glpSource,
      ),
    );
  }
}

class AgentScreen extends StatefulWidget {
  final int windowId;
  final String agentId;
  final List<String> friends;
  final String glpSource;

  const AgentScreen({
    super.key,
    required this.windowId,
    required this.agentId,
    required this.friends,
    required this.glpSource,
  });

  @override
  State<AgentScreen> createState() => _AgentScreenState();
}

class _AgentScreenState extends State<AgentScreen> {
  final TextEditingController _inputController = TextEditingController();
  final ScrollController _scrollController = ScrollController();
  final FocusNode _inputFocusNode = FocusNode();
  final List<String> _outputLog = [];

  late final AgentRuntime _agent;
  String _status = 'Not initialized';

  @override
  void initState() {
    super.initState();

    _agent = AgentRuntime(
      agentId: widget.agentId,
      glpSource: widget.glpSource,
      friends: widget.friends,
    );

    // Wire callbacks
    _agent.onOutput = (line) {
      setState(() {
        _outputLog.add(line);
      });
      _scrollToBottom();
    };

    _agent.onLog = (tag, message) {
      TraceLogger.instance.log(tag, message);
    };

    _agent.onSendMadMessage = (to, payload) async {
      TraceLogger.instance.log(widget.agentId.toUpperCase(), 'SEND_MAD to $to (${payload.length} bytes)');
      try {
        final encodedPayload = base64Encode(payload);
        await DesktopMultiWindow.invokeMethod(
          0, // Main window ID is always 0
          'send_mad',
          jsonEncode({
            'from': widget.agentId,
            'to': to,
            'payload': encodedPayload,
          }),
        );
      } catch (e) {
        TraceLogger.instance.log(widget.agentId.toUpperCase(), 'SEND_MAD ERROR: $e');
      }
    };

    _setupMethodChannel();
    _initializeRuntime();
  }

  String get _tag => widget.agentId.toUpperCase();

  void _setupMethodChannel() {
    // Handle messages from coordinator
    DesktopMultiWindow.setMethodHandler((call, fromWindowId) async {
      TraceLogger.instance.log(_tag, 'METHOD_CHANNEL: ${call.method}');

      if (call.method == 'deliver_mad') {
        // Handle madGLP binary message
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final encodedPayload = args['payload'] as String;
        final payload = Uint8List.fromList(base64Decode(encodedPayload));
        TraceLogger.instance.log(_tag, 'DELIVER_MAD from $from (${payload.length} bytes)');
        _agent.onMadMessageReceived(from, payload);
        setState(() {
          _agent.updateStats();
          _status = 'Ready';
        });
      } else if (call.method == 'deliver') {
        // Legacy JSON message (backwards compatibility)
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final payload = args['payload'];
        TraceLogger.instance.log(_tag, 'DELIVER (legacy) from $from');
        _agent.onLegacyMessageReceived(from, payload);
        setState(() {
          _agent.updateStats();
          _status = 'Ready';
        });
      }
      return null;
    });
  }

  Future<void> _initializeRuntime() async {
    try {
      await _agent.initialize();

      setState(() {
        _status = 'Ready';
      });

      // Request focus on input field after initialization
      WidgetsBinding.instance.addPostFrameCallback((_) {
        _inputFocusNode.requestFocus();
      });
    } catch (e, st) {
      debugPrint('$st');
      setState(() {
        _status = 'Error: $e';
      });
    }
  }

  void _sendInput() {
    final text = _inputController.text.trim();
    if (text.isEmpty || !_agent.initialized) return;

    _inputController.clear();
    _agent.injectUserInput(text);
    _scrollToBottom();
  }

  void _scrollToBottom() {
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (_scrollController.hasClients) {
        _scrollController.animateTo(
          _scrollController.position.maxScrollExtent,
          duration: const Duration(milliseconds: 200),
          curve: Curves.easeOut,
        );
      }
    });
  }

  @override
  void dispose() {
    _inputController.dispose();
    _scrollController.dispose();
    _inputFocusNode.dispose();
    _agent.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('${widget.agentId} (${widget.friends.join(", ")})'),
        actions: [
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16.0),
            child: Center(
              child: Text(
                'Win ${widget.windowId}',
                style: const TextStyle(fontSize: 12),
              ),
            ),
          ),
        ],
      ),
      body: Column(
        children: [
          // Output log
          Expanded(
            child: Container(
              color: Colors.grey.shade100,
              child: ListView.builder(
                controller: _scrollController,
                padding: const EdgeInsets.all(8.0),
                itemCount: _outputLog.length,
                itemBuilder: (context, index) {
                  final line = _outputLog[index];
                  final isInput = line.startsWith('>');
                  final isOutput = line.startsWith('<');
                  final isSend = line.startsWith('[SEND') || line.startsWith('[IRMA SEND');
                  final isRecv = line.startsWith('[RECV') || line.startsWith('[IRMA RECV');
                  final isIrma = line.startsWith('[IRMA');
                  final isControl = line.startsWith('[');
                  return Padding(
                    padding: const EdgeInsets.symmetric(vertical: 2.0),
                    child: SelectableText(
                      line,
                      style: TextStyle(
                        fontFamily: 'monospace',
                        fontSize: 13,
                        color: isInput
                            ? Colors.blue.shade800
                            : isOutput
                                ? Colors.green.shade800
                                : isSend
                                    ? Colors.purple.shade800
                                    : isRecv
                                        ? Colors.teal.shade800
                                        : isIrma
                                            ? Colors.deepPurple.shade800
                                            : isControl
                                                ? Colors.orange.shade800
                                                : Colors.black87,
                        fontWeight: (isControl || isOutput || isSend || isRecv || isIrma)
                            ? FontWeight.bold
                            : FontWeight.normal,
                      ),
                    ),
                  );
                },
              ),
            ),
          ),

          // GLP input area
          Container(
            padding: const EdgeInsets.all(8.0),
            color: Colors.orange.shade50,
            child: Row(
              children: [
                Expanded(
                  child: TextField(
                    controller: _inputController,
                    focusNode: _inputFocusNode,
                    enabled: _agent.initialized,
                    autofocus: true,
                    decoration: InputDecoration(
                      hintText: _agent.initialized
                          ? 'connect ${widget.friends.isNotEmpty ? widget.friends.first.toLowerCase() : "friend"}'
                          : 'Initializing...',
                      border: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(8),
                      ),
                      contentPadding: const EdgeInsets.symmetric(
                        horizontal: 12,
                        vertical: 8,
                      ),
                    ),
                    onSubmitted: (_) => _sendInput(),
                  ),
                ),
                const SizedBox(width: 8),
                ElevatedButton(
                  onPressed: _agent.initialized ? _sendInput : null,
                  child: const Text('Send'),
                ),
              ],
            ),
          ),

          // Status bar with V_p and M_p stats
          Container(
            padding: const EdgeInsets.symmetric(horizontal: 8.0, vertical: 8.0),
            color: Colors.orange.shade100,
            child: Row(
              children: [
                Text(
                  'G:${_agent.goalCount} H:${_agent.heapVars} W:${_agent.wpSize} M:${_agent.mpSize}',
                  style: const TextStyle(fontWeight: FontWeight.w500, fontSize: 10),
                ),
                const SizedBox(width: 8),
                Container(
                  width: 8,
                  height: 8,
                  decoration: BoxDecoration(
                    shape: BoxShape.circle,
                    color: _agent.initialized ? Colors.orange : Colors.grey,
                  ),
                ),
                const SizedBox(width: 4),
                Expanded(
                  child: Text(
                    _status,
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
}
