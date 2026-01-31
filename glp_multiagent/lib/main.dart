/// GLP Multiagent - madGLP Integration
///
/// Coordinator window spawns agent windows, routes messages between them.
/// Uses MadContext for proper multiagent W_p/M_p semantics.
/// Supports opaque byte payloads for inter-agent communication.
library;

import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';
import 'dart:ui';

import 'package:flutter/material.dart';
import 'package:desktop_multi_window/desktop_multi_window.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';
import 'package:glp_runtime/multiagent/global_send.dart';

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

/// Default GLP program path - using the v2 agent based on revised play protocols
const _defaultGlpPath = '/Users/udi/Grassroots/GLP/programs/multiagent/social_agent_v2.glp';

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
  final TextEditingController _glpPathController = TextEditingController(text: _defaultGlpPath);
  String _currentGlpPath = _defaultGlpPath;
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
    _log.add('GLP: $_currentGlpPath');
  }

  @override
  void dispose() {
    _glpPathController.dispose();
    super.dispose();
  }

  Future<void> _updateGlpPath() async {
    final newPath = _glpPathController.text.trim();
    if (newPath.isNotEmpty && File(newPath).existsSync()) {
      try {
        // Load user GLP program only (stdlib compiled separately in agent)
        final userSource = await File(newPath).readAsString();
        _cachedGlpSource = userSource;
        
        setState(() {
          _currentGlpPath = newPath;
          _log.add('GLP loaded: $newPath (${_cachedGlpSource!.length} chars)');
        });
      } catch (e) {
        setState(() {
          _log.add('ERROR reading file: $e');
        });
      }
    } else {
      setState(() {
        _log.add('ERROR: File not found: $newPath');
      });
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
                const Text('GLP: ', style: TextStyle(fontWeight: FontWeight.bold)),
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

  // madGLP: GlpRuntime + MadContext for multiagent support
  GlpRuntime? _runtime;
  MadContext? _ctx;
  Scheduler? _scheduler;
  _MultiAgentIOContext? _ioContext;
  Map<String, BytecodeProgram> _programs = {};
  int _goalId = 1;

  // Pending output terms (to be dereferenced after execution)
  final List<rt.Term> _pendingUserOutputTerms = [];

  int _goalCount = 0;
  int _heapVars = 0;
  int _wpSize = 0;  // W_p size (global writers table)
  int _mpSize = 0;
  bool _isRunning = false;
  bool _initialized = false;
  String _status = 'Not initialized';

  @override
  void initState() {
    super.initState();
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
        _onMadMessageReceived(from, payload);
      } else if (call.method == 'deliver') {
        // Legacy JSON message (backwards compatibility)
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final payload = args['payload'];
        TraceLogger.instance.log(_tag, 'DELIVER (legacy) from $from');
        _onLegacyMessageReceived(from, payload);
      }
      return null;
    });
  }

  /// Handle outgoing network messages - route via madGLP
  ///
  /// Message formats from social_agent_v2.glp:
  /// - 2-arg msg: msg(Target, Content) - cold-call to target
  /// - 3-arg msg: msg(From, To, Content) - friend-to-friend message
  void _handleNetOutput(rt.Term term) {
    if (_runtime == null) return;

    final derefTerm = _derefTerm(term);
    final formatted = _formatTerm(derefTerm);
    TraceLogger.instance.log(_tag, 'NET_OUT: $formatted');
    _addOutput('[NET OUT] $formatted');

    // Parse message to get destination
    if (derefTerm is rt.StructTerm && derefTerm.functor == 'msg') {
      String? destination;

      if (derefTerm.args.length == 2) {
        // 2-arg msg: msg(Target, Content)
        final target = _derefTerm(derefTerm.args[0]);
        if (target is rt.ConstTerm) {
          destination = target.value?.toString();
        }
        TraceLogger.instance.log(_tag, 'NET_OUT: 2-arg msg, target=$destination');
      } else if (derefTerm.args.length == 3) {
        // 3-arg msg: msg(From, To, Content)
        final to = _derefTerm(derefTerm.args[1]);
        if (to is rt.ConstTerm) {
          destination = to.value?.toString();
        }
        TraceLogger.instance.log(_tag, 'NET_OUT: 3-arg msg, to=$destination');
      }

      if (destination != null && destination != 'user' && destination != 'net') {
        TraceLogger.instance.log(_tag, 'NET_OUT: Sending to $destination');
        _sendAgentMessage(destination, derefTerm);
      } else {
        TraceLogger.instance.log(_tag, 'NET_OUT: Not routing (dest=$destination)');
      }
    } else {
      TraceLogger.instance.log(_tag, 'NET_OUT: Not a msg struct');
    }
  }

  /// Handle incoming madGLP binary message
  void _onMadMessageReceived(String from, Uint8List payload) {
    TraceLogger.instance.log(_tag, 'MAD_RECV from $from (${payload.length} bytes)');
    _addOutput('[MAD RECV from $from] ${payload.length} bytes');

    if (_runtime == null || _ctx == null || _ioContext == null) {
      TraceLogger.instance.log(_tag, 'MAD_RECV: ERROR - runtime/ctx/ioContext is null');
      return;
    }

    // Deserialize the outer message to check type
    final serializer = PayloadSerializer(widget.agentId.toLowerCase());
    final msg = serializer.deserializeMessage(payload);
    TraceLogger.instance.log(_tag, 'MAD_RECV: type=${msg.type}, dest=${msg.destination}');

    if (msg.type == MessageType.assignment) {
      // madGLP assignment message
      try {
        final (globalName, value) = serializer.deserializeGlobalSendPayload(
          msg.payload,
          (isReader) {
            final (w, r) = _runtime!.heap.allocateVariable();
            return isReader ? r : w;
          },
        );

        TraceLogger.instance.log(_tag, 'MAD_ASSIGN: $globalName := ${_formatTerm(value)}');
        _addOutput('[MAD ASSIGN] $globalName := ${_formatTerm(value)}');

        _ctx!.handleMadAssignment(
          globalName: globalName,
          value: value,
          fromAgent: from.toLowerCase(),
        );
      } catch (e) {
        TraceLogger.instance.log(_tag, 'MAD_ERROR: $e');
        _addOutput('[MAD ERROR] $e');
      }
    } else if (msg.type == MessageType.agentMessage) {
      // Cold-call agent message - deserialize and inject into NET input
      final term = serializer.deserializeAgentMessagePayload(
        msg.payload,
        (isReader) {
          final (w, r) = _runtime!.heap.allocateVariable();
          return isReader ? r : w;
        },
      );

      final formatted = _formatTerm(term);
      TraceLogger.instance.log(_tag, 'AGENT_MSG: $formatted');
      _addOutput('[AGENT MSG] $formatted');

      // Inject into the NET input stream
      TraceLogger.instance.log(_tag, 'INJECT into netInput');
      final activations = _ioContext!.netInput.inject(term);
      TraceLogger.instance.log(_tag, 'INJECT: ${activations.length} activations');
      for (final goal in activations) {
        _runtime!.gq.enqueue(goal);
      }
    } else {
      TraceLogger.instance.log(_tag, 'MAD_RECV: Unknown message type ${msg.type}');
    }

    // Update stats
    _updateStats();

    // Run to process any reactivated goals
    _runUntilQuiescent();
  }

  /// Handle legacy JSON message (backwards compatibility)
  void _onLegacyMessageReceived(String from, dynamic payload) {
    _addOutput('[RECV from $from] $payload');

    if (_ioContext == null || _runtime == null) return;

    final fromLower = from.toLowerCase();

    // Inject msg(From, Id, Payload) into NET input stream
    final msgTerm = rt.StructTerm('msg', [
      rt.ConstTerm(fromLower),
      rt.ConstTerm(widget.agentId.toLowerCase()),
      rt.ConstTerm(payload),
    ]);

    final activations = _ioContext!.netInput.inject(msgTerm);
    for (final goal in activations) {
      _runtime!.gq.enqueue(goal);
    }

    // Run to process the message
    _runUntilQuiescent();
  }

  /// Send madGLP binary message to coordinator for routing
  Future<void> _sendMadMessage(String to, Uint8List payload) async {
    TraceLogger.instance.log(_tag, 'SEND_MAD to $to (${payload.length} bytes)');
    _addOutput('[MAD SEND to $to] ${payload.length} bytes');

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
      TraceLogger.instance.log(_tag, 'SEND_MAD: sent to coordinator');
    } catch (e) {
      TraceLogger.instance.log(_tag, 'SEND_MAD ERROR: $e');
      _addOutput('[ERROR] Failed to send mad message: $e');
    }
  }

  /// Send legacy message (for backwards compatibility)
  Future<void> _sendLegacyMessage(String to, dynamic payload) async {
    _addOutput('[SEND to $to] $payload');

    try {
      await DesktopMultiWindow.invokeMethod(
        0, // Main window ID is always 0
        'send',
        jsonEncode({
          'from': widget.agentId,
          'to': to,
          'payload': payload,
        }),
      );
    } catch (e) {
      _addOutput('[ERROR] Failed to send: $e');
    }
  }

  /// Send agent message with proper term serialization
  /// This preserves structure and variables for cross-agent communication
  Future<void> _sendAgentMessage(String to, rt.Term msgTerm) async {
    TraceLogger.instance.log(_tag, 'SEND_AGENT_MSG to $to: ${_formatTerm(msgTerm)}');
    if (_runtime == null || _ctx == null) {
      TraceLogger.instance.log(_tag, 'SEND_AGENT_MSG: ERROR - runtime/ctx is null');
      return;
    }

    try {
      // Per spec section 4.3: register exported variables in W_p before sending
      // This enables Bob to route assignments for variables he creates and sends
      _ctx!.exportTerm(msgTerm);

      // Serialize the full term (preserving structure and variables)
      final serializer = PayloadSerializer(widget.agentId.toLowerCase());
      final termPayload = serializer.createAgentMessagePayload(
        msgTerm,
        (addr) => _runtime!.heap.isReader(addr),
      );
      TraceLogger.instance.log(_tag, 'SEND_AGENT_MSG: serialized ${termPayload.length} bytes');

      // Wrap in OutboundMessage with agentMessage type
      final msg = OutboundMessage(
        destination: to,
        type: MessageType.agentMessage,
        payload: termPayload,
      );
      final payload = serializer.serializeMessage(msg);
      TraceLogger.instance.log(_tag, 'SEND_AGENT_MSG: wrapped ${payload.length} bytes');

      await _sendMadMessage(to, payload);
    } catch (e, st) {
      TraceLogger.instance.log(_tag, 'SEND_AGENT_MSG ERROR: $e\n$st');
      _addOutput('[ERROR] Failed to send agent message: $e');
    }
  }

  Future<void> _initializeRuntime() async {
    try {
      TraceLogger.instance.log(_tag, 'INIT: Creating MadContext');
      _addOutput('[INIT] Creating MadContext...');
      _addOutput('[INIT] Friends: ${widget.friends.join(", ")}');

      // Create GlpRuntime and MadContext directly
      _runtime = GlpRuntime();
      _ctx = MadContext(agentId: widget.agentId.toLowerCase(), runtime: _runtime!);
      TraceLogger.instance.log(_tag, 'INIT: MadContext created');

      // Set up callback for outbound madGLP messages
      _ctx!.onMessageReady = (destination, msg) async {
        final serializer = PayloadSerializer(widget.agentId.toLowerCase());
        final payload = serializer.serializeMessage(msg);
        await _sendMadMessage(destination, payload);
      };

      // Register standard predicates
      registerStandardPredicates(_runtime!.systemPredicates);

      // Create user and net channels
      final userChannel = createExternalChannel(_runtime!.heap, 'user');
      final netChannel = createExternalChannel(_runtime!.heap, 'net');

      // Create input injectors for user and network (Dart holds writer)
      final userInput = InputInjector(_runtime!.heap, 'user', userChannel.inputWriterAddr);
      final netInput = InputInjector(_runtime!.heap, 'net', netChannel.inputWriterAddr);

      // Create user output observer - displays to UI (Dart observes via reader)
      final userOutput = OutputObserver(
        _runtime!.heap,
        'user',
        userChannel.outputReaderAddr,
        (term) {
          _pendingUserOutputTerms.add(term);
        },
        () {
          setState(() {
            _outputLog.add('[USER OUTPUT CLOSED]');
          });
        },
      );

      // Create net output observer - watches for outgoing network messages
      // and routes them via madGLP to the destination agent (Dart observes via reader)
      final netOutput = OutputObserver(
        _runtime!.heap,
        'net',
        netChannel.outputReaderAddr,
        (term) {
          _handleNetOutput(term);
        },
        () {
          setState(() {
            _outputLog.add('[NET OUTPUT CLOSED]');
          });
        },
      );

      _ioContext = _MultiAgentIOContext(
        userChannel: userChannel,
        netChannel: netChannel,
        userInput: userInput,
        netInput: netInput,
        userOutput: userOutput,
        netOutput: netOutput,
      );

      // Compile stdlib separately (provides =/2)
      const stdlibSource = 'X? = X.\n';
      final stdlibCompiler = GlpCompiler();
      final stdlibProgram = stdlibCompiler.compile(stdlibSource);
      _programs['stdlib'] = stdlibProgram;

      // Compile user program separately (preserves type definition ordering)
      final userCompiler = GlpCompiler();
      final userProgram = userCompiler.compile(widget.glpSource);
      _programs['user'] = userProgram;

      _addOutput('[INIT] Loaded GLP program');

      // Start goal: agent_init(Id, UserCh, NetCh) - v2 protocol
      final agentIdLower = widget.agentId.toLowerCase();
      _addOutput('[INIT] Starting: agent_init($agentIdLower, UserCh, NetCh)');
      _startAgentGoal(agentIdLower);

      setState(() {
        _initialized = true;
        _status = 'Ready';
        _updateStats();
      });

      final myId = widget.agentId.toLowerCase();
      final firstFriend = widget.friends.isNotEmpty ? widget.friends.first.toLowerCase() : 'friend';
      _addOutput('[INIT] Ready! Commands:');
      _addOutput('  Cold-call: msg(user, $myId, connect($firstFriend))');
      _addOutput('  Send text: msg(user, $myId, send($firstFriend, hello))');
      _addOutput('  Accept:    msg(user, $myId, decision(yes, <from>, <Resp>))');

      // Request focus on input field after initialization
      WidgetsBinding.instance.addPostFrameCallback((_) {
        _inputFocusNode.requestFocus();
      });
    } catch (e, st) {
      _addOutput('[ERROR] $e');
      debugPrint('$st');
      setState(() {
        _status = 'Error: $e';
      });
    }
  }

  void _startAgentGoal(String agentId) {
    if (_runtime == null || _ioContext == null) return;

    try {
      // Combine loaded programs
      final allOps = <dynamic>[];
      for (final loaded in _programs.values) {
        allOps.addAll(loaded.ops);
      }
      final combinedProgram = BytecodeProgram(allOps);

      // Find entry point for agent_init/3 (v2 protocol)
      final entryPC = combinedProgram.labels['agent_init/3'];
      TraceLogger.instance.log(_tag, 'GOAL: entryPC=$entryPC, labels=${combinedProgram.labels.keys.toList()}');
      if (entryPC == null) {
        _addOutput('[ERROR] Predicate agent_init/3 not found');
        return;
      }

      // Allocate heap cells for arguments and bind values
      // CallEnv requires VarRefs to READER addresses since procedure
      // declaration is agent_init(_?, Channel?, Channel?) - all reader modes
      final heap = _runtime!.heap;

      // Arg 0: agentId - _? mode (reader)
      final (arg0Writer, arg0Reader) = heap.allocateVariable();
      heap.bindVariable(arg0Writer, rt.ConstTerm(agentId));
      TraceLogger.instance.log(_tag, 'GOAL: arg0 writer=$arg0Writer reader=$arg0Reader value=$agentId');

      // Arg 1: userChannelTerm - Channel? mode (reader)
      final (arg1Writer, arg1Reader) = heap.allocateVariable();
      final userChTerm = _ioContext!.userChannelTerm;
      heap.bindVariable(arg1Writer, userChTerm);
      TraceLogger.instance.log(_tag, 'GOAL: arg1 writer=$arg1Writer reader=$arg1Reader value=${_formatTerm(userChTerm)}');

      // Arg 2: netChannelTerm - Channel? mode (reader)
      final (arg2Writer, arg2Reader) = heap.allocateVariable();
      final netChTerm = _ioContext!.netChannelTerm;
      heap.bindVariable(arg2Writer, netChTerm);
      TraceLogger.instance.log(_tag, 'GOAL: arg2 writer=$arg2Writer reader=$arg2Reader value=${_formatTerm(netChTerm)}');

      // Set up arguments: agent_init(Id, UserCh, NetCh)
      // Pass READER addresses since procedure expects reader mode arguments
      final argSlots = <int, rt.Term>{
        0: rt.VarRef(arg0Reader),
        1: rt.VarRef(arg1Reader),
        2: rt.VarRef(arg2Reader),
      };

      // Set up goal environment
      final env = CallEnv(args: argSlots);
      _runtime!.setGoalEnv(_goalId, env);
      _runtime!.setGoalProgram(_goalId, 'main');

      // Create scheduler
      final runner = BytecodeRunner(combinedProgram);
      _scheduler = Scheduler(rt: _runtime!, runners: {'main': runner});
      _scheduler!.resetDisplayNumbering();

      // Enqueue goal
      _runtime!.gq.enqueue(GoalRef(_goalId, entryPC));
      _goalId++;

      _addOutput('[GOAL] Started agent_init($agentId, UserCh, NetCh)');

      // Initial run to set up merge
      _runUntilQuiescent();
    } catch (e, st) {
      _addOutput('[ERROR] Starting goal: $e');
      debugPrint('$st');
    }
  }

  void _sendInput() {
    final text = _inputController.text.trim();
    TraceLogger.instance.log(_tag, 'USER_INPUT: $text');
    if (text.isEmpty || _ioContext == null || _runtime == null) {
      TraceLogger.instance.log(_tag, 'USER_INPUT: early return (empty or not initialized)');
      return;
    }

    setState(() {
      _outputLog.add('> $text');
    });

    try {
      // Parse and inject term into UserIn
      final term = _parseTerm(text);
      TraceLogger.instance.log(_tag, 'USER_INPUT: parsed -> ${_formatTerm(term)}');

      final activations = _ioContext!.userInput.inject(term);
      TraceLogger.instance.log(_tag, 'USER_INPUT: ${activations.length} activations');

      // Enqueue activated goals
      for (final goal in activations) {
        _runtime!.gq.enqueue(goal);
      }

      _inputController.clear();
      _scrollToBottom();

      // Auto-run after injection
      _runUntilQuiescent();
    } catch (e, st) {
      TraceLogger.instance.log(_tag, 'USER_INPUT ERROR: $e\n$st');
      _addOutput('[ERROR] $e');
    }
  }

  rt.Term _parseTerm(String termStr) {
    final parseInput = '_temp_($termStr).';
    final lexer = Lexer(parseInput);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final parsedAst = parser.parse();

    if (parsedAst.procedures.isEmpty || parsedAst.procedures[0].clauses.isEmpty) {
      throw Exception('Could not parse term');
    }

    final clause = parsedAst.procedures[0].clauses[0];
    if (clause.head.args.isEmpty) {
      throw Exception('No term to inject');
    }

    return _astToRuntimeTerm(clause.head.args[0]);
  }

  rt.Term _astToRuntimeTerm(ast.Term astTerm) {
    if (astTerm is ast.ConstTerm) {
      return rt.ConstTerm(astTerm.value);
    } else if (astTerm is ast.VarTerm) {
      final (writerAddr, readerAddr) = _runtime!.heap.allocateVariable();
      return rt.VarRef(astTerm.isReader ? readerAddr : writerAddr);
    } else if (astTerm is ast.StructTerm) {
      final args = astTerm.args.map(_astToRuntimeTerm).toList();
      return rt.StructTerm(astTerm.functor, args);
    } else if (astTerm is ast.ListTerm) {
      return _astListToRuntimeTerm(astTerm);
    }
    throw Exception('Unknown AST term type: ${astTerm.runtimeType}');
  }

  rt.Term _astListToRuntimeTerm(ast.ListTerm list) {
    if (list.isNil) {
      return rt.ConstTerm('nil');
    }

    final head = _astToRuntimeTerm(list.head!);
    final tail = list.tail is ast.ListTerm
        ? _astListToRuntimeTerm(list.tail as ast.ListTerm)
        : list.tail != null
            ? _astToRuntimeTerm(list.tail!)
            : rt.ConstTerm('nil');

    return rt.StructTerm('.', [head, tail]);
  }

  // Enable GLP trace output
  bool _glpTraceEnabled = false;

  Future<void> _runUntilQuiescent() async {
    TraceLogger.instance.log(_tag, 'RUN: start (GQ=${_runtime?.gq.length ?? 0})');
    if (_scheduler == null || _runtime == null) {
      TraceLogger.instance.log(_tag, 'RUN: early return (not initialized)');
      return;
    }

    setState(() {
      _isRunning = true;
      _status = 'Running...';
    });

    try {
      final result = await _scheduler!.drainAsyncWithStatus(
        maxCycles: 1000,
        debug: _glpTraceEnabled,
      );
      TraceLogger.instance.log(_tag, 'RUN: status=${result.status}, goals=${result.goalsRan.length}');
      _goalCount += result.goalsRan.length;

      // Per spec section 5.2 Case 2: On suspension, call request(X?) for blocking readers
      if (result.status == ExecutionStatus.suspended && result.blockingReaders.isNotEmpty) {
        TraceLogger.instance.log(_tag, 'RUN: suspended, ${result.blockingReaders.length} blocking readers');
        _ctx!.processSuspension(result.blockingReaders);
        _addOutput('[MAD] Waiting for ${result.blockingReaders.length} blocking readers');
      }

      // Flush any pending madGLP messages
      final messagesFlushed = _ctx!.flushMessages();
      if (messagesFlushed > 0) {
        TraceLogger.instance.log(_tag, 'RUN: flushed $messagesFlushed messages');
        _addOutput('[MAD] Flushed $messagesFlushed messages');
      }

      // Display pending output
      _displayPendingOutput();

      setState(() {
        _isRunning = false;
        _status = result.status.name;
        _updateStats();
      });
      TraceLogger.instance.log(_tag, 'RUN: done (status=${result.status.name})');
    } catch (e, st) {
      TraceLogger.instance.log(_tag, 'RUN ERROR: $e\n$st');
      setState(() {
        _isRunning = false;
        _status = 'Error: $e';
      });
    }
  }

  void _updateStats() {
    if (_runtime != null && _ctx != null) {
      _heapVars = _runtime!.heap.HP;  // Heap pointer = total cells allocated
      _wpSize = _ctx!.wp.globalizeEntryCount + _ctx!.wp.localizeEntryCount;  // W_p size
      _mpSize = _ctx!.mp.totalLength;
    }
  }

  void _displayPendingOutput() {
    // Display user output terms
    for (final term in _pendingUserOutputTerms) {
      final derefTerm = _derefTerm(term);
      setState(() {
        _outputLog.add('< ${_formatTerm(derefTerm)}');
      });
    }
    _pendingUserOutputTerms.clear();

    // Friend output is now handled via V_p onBind callbacks
    // which automatically route assignments through IRMA

    _scrollToBottom();
  }

  String _termToString(rt.Term term) {
    if (term is rt.ConstTerm) {
      return term.value?.toString() ?? '';
    }
    return _formatTerm(term);
  }

  void _addOutput(String text) {
    setState(() {
      _outputLog.add(text);
    });
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

  rt.Term _derefTerm(rt.Term term) {
    if (_runtime == null) return term;

    if (term is rt.VarRef) {
      final value = _runtime!.heap.getValue(term.addr);
      if (value != null && value is! rt.VarRef) {
        return _derefTerm(value);
      }
      return term;
    }
    if (term is rt.StructTerm) {
      final derefArgs = term.args.map(_derefTerm).toList();
      return rt.StructTerm(term.functor, derefArgs);
    }
    return term;
  }

  String _formatTerm(rt.Term term) {
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
          elements.add(_formatTerm(current.args[0]));
          current = current.args[1];
        }
        if (current is rt.ConstTerm && (current.value == 'nil' || current.value == null)) {
          return '[${elements.join(', ')}]';
        }
        return '[${elements.join(', ')} | ${_formatTerm(current)}]';
      }
      final args = term.args.map(_formatTerm).join(', ');
      return '${term.functor}($args)';
    }
    return term.toString();
  }

  @override
  void dispose() {
    _inputController.dispose();
    _scrollController.dispose();
    _inputFocusNode.dispose();
    _ioContext?.dispose();
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
                    enabled: _initialized,
                    autofocus: true,
                    decoration: InputDecoration(
                      hintText: _initialized
                          ? 'msg(user, ${widget.agentId.toLowerCase()}, connect(${widget.friends.isNotEmpty ? widget.friends.first.toLowerCase() : "friend"}))'
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
                  onPressed: _initialized ? _sendInput : null,
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
                  'G:$_goalCount H:$_heapVars W:$_wpSize M:$_mpSize',
                  style: const TextStyle(fontWeight: FontWeight.w500, fontSize: 10),
                ),
                const SizedBox(width: 8),
                Container(
                  width: 8,
                  height: 8,
                  decoration: BoxDecoration(
                    shape: BoxShape.circle,
                    color: _isRunning ? Colors.green : (_initialized ? Colors.orange : Colors.grey),
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

// ============================================================================
// HELPER CLASSES
// ============================================================================

/// Multi-agent I/O context with user and network channels
///
/// v2: Network output is observed and routed via IRMA.
/// Friends are added dynamically through cold-call protocol.
class _MultiAgentIOContext {
  final ExternalChannel userChannel;
  final ExternalChannel netChannel;
  final InputInjector userInput;
  final InputInjector netInput;  // For incoming network messages
  final OutputObserver userOutput;
  final OutputObserver netOutput;  // For outgoing network messages

  _MultiAgentIOContext({
    required this.userChannel,
    required this.netChannel,
    required this.userInput,
    required this.netInput,
    required this.userOutput,
    required this.netOutput,
  });

  rt.Term get userChannelTerm => buildChannelTerm(userChannel);
  rt.Term get netChannelTerm => buildChannelTerm(netChannel);

  void dispose() {
    userOutput.dispose();
    netOutput.dispose();
  }
}
