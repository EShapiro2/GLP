/// GLP Multiagent - irmaGLP Integration
///
/// Coordinator window spawns agent windows, routes messages between them.
/// Uses IrmaAgent for proper multiagent V_p/M_p semantics.
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
import 'package:glp_runtime/multiagent/irma_agent.dart';
import 'package:glp_runtime/multiagent/irma_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';

import 'irma_router.dart';

/// Default GLP program path
const _defaultGlpPath = '/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp';

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
    IrmaRouter.instance.onLogUpdate = () {
      setState(() {});
    };

    // Set up handler for messages from agent windows
    DesktopMultiWindow.setMethodHandler((call, fromWindowId) async {
      debugPrint('=== COORDINATOR RECEIVED: ${call.method} from $fromWindowId ===');
      
      if (call.method == 'send_irma') {
        // Handle irmaGLP binary message routing
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final to = args['to'] as String;
        final encodedPayload = args['payload'] as String;
        final payload = base64Decode(encodedPayload);
        await IrmaRouter.instance.route(from, to, Uint8List.fromList(payload));
      } else if (call.method == 'send') {
        // Legacy JSON message routing (for backwards compatibility)
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final to = args['to'] as String;
        final payload = args['payload'];
        // Convert to simple string payload for legacy support
        final payloadBytes = utf8.encode(jsonEncode(payload));
        await IrmaRouter.instance.route(from, to, Uint8List.fromList(payloadBytes));
      }
      return null;
    });

    _log.add('Coordinator started (irmaGLP mode)');
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
      await window.setTitle('$agentId - irmaGLP Agent');
      await window.setFrame(Rect.fromLTWH(x, y, 400, 600));
      await window.show();

      _windows[agentId] = window;
      IrmaRouter.instance.register(agentId, window.windowId);

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
        IrmaRouter.instance.unregister(entry.key);
      } catch (e) {
        _log.add('ERROR closing ${entry.key}: $e');
      }
    }
    _windows.clear();
    IrmaRouter.instance.clearLog();
    _log.add('Closed all windows');
    setState(() {});
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('GLP Coordinator (irmaGLP)'),
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
                itemCount: IrmaRouter.instance.routingLog.length,
                itemBuilder: (context, index) {
                  return Text(
                    IrmaRouter.instance.routingLog[index],
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
      title: '$agentId - irmaGLP Agent',
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
  final List<String> _outputLog = [];

  // IrmaAgent wraps GlpRuntime with multiagent support
  IrmaAgent? _agent;
  Scheduler? _scheduler;
  _MultiAgentIOContext? _ioContext;
  Map<String, BytecodeProgram> _programs = {};
  int _goalId = 1;

  // Pending output terms (to be dereferenced after execution)
  // Note: Friend output is now handled via V_p onBind callbacks
  final List<rt.Term> _pendingUserOutputTerms = [];

  int _goalCount = 0;
  int _heapVars = 0;
  int _vpSize = 0;
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

  void _setupMethodChannel() {
    // Handle messages from coordinator
    DesktopMultiWindow.setMethodHandler((call, fromWindowId) async {
      debugPrint('=== AGENT ${widget.agentId} RECEIVED: ${call.method} ===');
      
      if (call.method == 'deliver_irma') {
        // Handle irmaGLP binary message
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final encodedPayload = args['payload'] as String;
        final payload = Uint8List.fromList(base64Decode(encodedPayload));
        _onIrmaMessageReceived(from, payload);
      } else if (call.method == 'deliver') {
        // Legacy JSON message (backwards compatibility)
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final payload = args['payload'];
        _onLegacyMessageReceived(from, payload);
      }
      return null;
    });
  }

  /// Handle incoming irmaGLP binary message
  void _onIrmaMessageReceived(String from, Uint8List payload) {
    _addOutput('[IRMA RECV from $from] ${payload.length} bytes');

    if (_agent == null || _ioContext == null) return;

    // Deserialize the outer message to check type
    final serializer = PayloadSerializer(widget.agentId.toLowerCase());
    final msg = serializer.deserializeMessage(payload);
    
    if (msg.type == MessageType.agentMessage) {
      // Agent message: use the new deserializeAndImportTerm which handles
      // single-cell allocation and VariableEntry attachment automatically
      final term = _agent!.context.deserializeAndImportTerm(
        msg.payload,
        from.toLowerCase(),
      );
      
      _addOutput('[AGENT MSG] ${_formatTerm(term)}');
      
      // Inject into the NET input stream (goes through merge with UserIn)
      final activations = _ioContext!.netInput.inject(term);
      for (final goal in activations) {
        _agent!.runtime.gq.enqueue(goal);
      }
    } else {
      // V_p operation (assignment, readRequest, abandon) - route to IrmaAgent
      _agent!.handleIncomingMessage(from, payload);
    }
    
    // Update stats
    _updateStats();
    
    // Run to process any reactivated goals
    _runUntilQuiescent();
  }

  /// Handle legacy JSON message (backwards compatibility)
  void _onLegacyMessageReceived(String from, dynamic payload) {
    _addOutput('[RECV from $from] $payload');

    if (_ioContext == null || _agent == null) return;

    final fromLower = from.toLowerCase();

    // Inject msg(From, Id, Payload) into NET input stream
    final msgTerm = rt.StructTerm('msg', [
      rt.ConstTerm(fromLower),
      rt.ConstTerm(widget.agentId.toLowerCase()),
      rt.ConstTerm(payload),
    ]);

    final activations = _ioContext!.netInput.inject(msgTerm);
    for (final goal in activations) {
      _agent!.runtime.gq.enqueue(goal);
    }

    // Run to process the message
    _runUntilQuiescent();
  }

  /// Send irmaGLP binary message to coordinator for routing
  Future<void> _sendIrmaMessage(String to, Uint8List payload) async {
    _addOutput('[IRMA SEND to $to] ${payload.length} bytes');

    try {
      final encodedPayload = base64Encode(payload);
      await DesktopMultiWindow.invokeMethod(
        0, // Main window ID is always 0
        'send_irma',
        jsonEncode({
          'from': widget.agentId,
          'to': to,
          'payload': encodedPayload,
        }),
      );
    } catch (e) {
      _addOutput('[ERROR] Failed to send irma message: $e');
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
    if (_agent == null) return;

    try {
      // Per spec section 4.3: register exported variables in V_p before sending
      // This enables Bob to route assignments for variables he creates and sends
      _agent!.context.exportTerm(msgTerm);

      // Serialize the full term (preserving structure and variables)
      final serializer = PayloadSerializer(widget.agentId.toLowerCase());
      final termPayload = serializer.createAgentMessagePayload(msgTerm);
      
      // Wrap in OutboundMessage with agentMessage type
      final msg = OutboundMessage(
        destination: to,
        type: MessageType.agentMessage,
        payload: termPayload,
      );
      final payload = serializer.serializeMessage(msg);
      
      await _sendIrmaMessage(to, payload);
    } catch (e) {
      _addOutput('[ERROR] Failed to send agent message: $e');
    }
  }

  Future<void> _initializeRuntime() async {
    try {
      _addOutput('[INIT] Creating IrmaAgent...');
      _addOutput('[INIT] Friends: ${widget.friends.join(", ")}');

      // Create IrmaAgent (wraps GlpRuntime with multiagent support)
      _agent = IrmaAgent(agentId: widget.agentId.toLowerCase());
      
      // Set up coordinator callback for outbound messages
      _agent!.onSendToCoordinator = (destination, payload) async {
        await _sendIrmaMessage(destination, payload);
      };
      
      // Set up logging callback
      _agent!.onLog = (message) {
        debugPrint(message);
      };

      // Register standard predicates
      registerStandardPredicates(_agent!.runtime.systemPredicates);

      // Create user and net channels
      final userChannel = createExternalChannel(_agent!.runtime.heap, 'user');
      final netChannel = createExternalChannel(_agent!.runtime.heap, 'net');

      // Create input injectors for user and network
      final userInput = InputInjector(_agent!.runtime.heap, 'user', userChannel.inputVarId);
      final netInput = InputInjector(_agent!.runtime.heap, 'net', netChannel.inputVarId);

      // Create user output observer - displays to UI
      final userOutput = OutputObserver(
        _agent!.runtime.heap,
        'user',
        userChannel.outputVarId,
        (term) {
          _pendingUserOutputTerms.add(term);
        },
        () {
          setState(() {
            _outputLog.add('[USER OUTPUT CLOSED]');
          });
        },
      );

      // Create V_p-based channels for each friend
      // Each friend gets:
      // - An output writer (we write to it, friend receives via IRMA)
      // - An imported reader (friend writes to it, we receive via IRMA)
      final friendChannels = <String, _VpFriendChannel>{};

      for (final friend in widget.friends) {
        final friendLower = friend.toLowerCase();
        final channel = _createVpFriendChannel(friendLower);
        friendChannels[friendLower] = channel;
        _addOutput('[INIT] Created V_p channel for $friendLower (out=${channel.outputWriterAddr}, in=${channel.inputReaderAddr})');
      }

      _ioContext = _MultiAgentIOContext(
        userChannel: userChannel,
        netChannel: netChannel,
        friendChannels: friendChannels,
        userInput: userInput,
        netInput: netInput,
        userOutput: userOutput,
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

      // Start goal: agent(Id, FriendPairs, UserCh, NetCh)
      final agentIdLower = widget.agentId.toLowerCase();
      _addOutput('[INIT] Starting: agent($agentIdLower, [...], ...)');
      _startAgentGoal(agentIdLower);

      setState(() {
        _initialized = true;
        _status = 'Ready';
        _updateStats();
      });

      final firstFriend = widget.friends.isNotEmpty ? widget.friends.first.toLowerCase() : 'friend';
      _addOutput('[INIT] Ready! Type: send($firstFriend, ping)');
    } catch (e, st) {
      _addOutput('[ERROR] $e');
      debugPrint('$st');
      setState(() {
        _status = 'Error: $e';
      });
    }
  }

  void _startAgentGoal(String agentId) {
    if (_agent == null || _ioContext == null) return;

    try {
      // Combine loaded programs
      final allOps = <dynamic>[];
      for (final loaded in _programs.values) {
        allOps.addAll(loaded.ops);
      }
      final combinedProgram = BytecodeProgram(allOps);

      // Find entry point for agent/4
      final entryPC = combinedProgram.labels['agent/4'];
      if (entryPC == null) {
        _addOutput('[ERROR] Predicate agent/4 not found');
        return;
      }

      // Build FriendPairs list: [(name1, out1), (name2, out2), ...]
      final friendPairsList = _buildFriendPairsList();

      // Set up arguments: agent(Id, FriendPairs, UserCh, NetCh)
      final argSlots = <int, rt.Term>{
        0: rt.ConstTerm(agentId),
        1: friendPairsList,
        2: _ioContext!.userChannelTerm,
        3: _ioContext!.netChannelTerm,
      };

      // Set up goal environment
      final env = CallEnv(args: argSlots);
      _agent!.runtime.setGoalEnv(_goalId, env);
      _agent!.runtime.setGoalProgram(_goalId, 'main');

      // Create scheduler
      final runner = BytecodeRunner(combinedProgram);
      _scheduler = Scheduler(rt: _agent!.runtime, runners: {'main': runner});
      _scheduler!.resetDisplayNumbering();

      // Enqueue goal
      _agent!.runtime.gq.enqueue(GoalRef(_goalId, entryPC));
      _goalId++;

      _addOutput('[GOAL] Started agent($agentId, [${widget.friends.join(", ")}], ...)');

      // Initial run to set up merge
      _runUntilQuiescent();
    } catch (e, st) {
      _addOutput('[ERROR] Starting goal: $e');
      debugPrint('$st');
    }
  }

  /// Build GLP list: [(name1, Out1), (name2, Out2), ...]
  /// Uses V_p-based channels where output writers are registered in V_p
  rt.Term _buildFriendPairsList() {
    rt.Term list = rt.ConstTerm('nil');

    // Build in reverse order so first friend is at head
    for (int i = widget.friends.length - 1; i >= 0; i--) {
      final friendLower = widget.friends[i].toLowerCase();
      final channel = _ioContext!.friendChannels[friendLower]!;

      // Create pair: (name, OutStream)
      // The output writer is registered in V_p - when GLP binds it,
      // the assignment goes to the friend via IRMA
      final pair = rt.StructTerm(',', [
        rt.ConstTerm(friendLower),
        rt.VarRef(channel.outputWriterAddr),  // V_p-registered writer
      ]);

      // Cons onto list
      list = rt.StructTerm('.', [pair, list]);
    }

    return list;
  }

  /// Create a V_p-based friend channel
  ///
  /// Sets up:
  /// - Local writer for outgoing (registered in V_p with requester=friend)
  /// - Imported reader for incoming (registered in V_p with creator=friend)
  _VpFriendChannel _createVpFriendChannel(String friendName) {
    final heap = _agent!.runtime.heap;
    final context = _agent!.context;
    final myId = widget.agentId.toLowerCase();

    // Create local writer for outgoing messages to friend
    // Per spec: when we bind this, assignment goes to friend
    final (outWriterAddr, _) = heap.allocateVariable();

    // Register in V_p as createdWriter with requester=friend
    // This sets up the onBind callback to route assignments
    final outKey = VarKey(outWriterAddr, false);
    context.vp.add(outKey, VariableEntry(
      varId: outWriterAddr,
      isReader: false,
      creator: myId,
      role: VariableRole.createdWriter,
      requester: friendName,  // Assignments go here
    ));

    // Set up onBind callback for outgoing messages
    heap.onBind(outWriterAddr, (rt.Term value) {
      debugPrint('=== V_p onBind: $myId -> $friendName, value=$value ===');
      context._onWriterBound(outWriterAddr, value);
    });

    // Create imported reader for incoming messages from friend
    // Per spec: when friend binds their writer, we receive via handleAssignment
    final inReaderAddr = heap.allocateImportedReader();

    // Create and attach VariableEntry to heap cell
    final inEntry = VariableEntry(
      varId: inReaderAddr,
      isReader: true,
      creator: friendName,  // Friend is the creator
      role: VariableRole.importedReader,
      creatorLocalId: inReaderAddr,  // Will be updated when we know friend's ID
    );
    heap.cells[inReaderAddr].content = inEntry;

    // Register in V_p
    final inKey = VarKey(inReaderAddr, true);
    context.vp.add(inKey, inEntry);

    return _VpFriendChannel(
      friendName: friendName,
      outputWriterAddr: outWriterAddr,
      inputReaderAddr: inReaderAddr,
    );
  }

  void _sendInput() {
    final text = _inputController.text.trim();
    if (text.isEmpty || _ioContext == null || _agent == null) return;

    setState(() {
      _outputLog.add('> $text');
    });

    try {
      // Parse and inject term into UserIn
      final term = _parseTerm(text);
      final activations = _ioContext!.userInput.inject(term);

      // Enqueue activated goals
      for (final goal in activations) {
        _agent!.runtime.gq.enqueue(goal);
      }

      _inputController.clear();
      _scrollToBottom();

      // Auto-run after injection
      _runUntilQuiescent();
    } catch (e) {
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
      final (writerAddr, readerAddr) = _agent!.runtime.heap.allocateVariable();
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
    if (_scheduler == null || _agent == null) return;

    setState(() {
      _isRunning = true;
      _status = 'Running...';
    });

    try {
      final result = await _scheduler!.drainAsyncWithStatus(
        maxCycles: 1000,
        debug: _glpTraceEnabled,
      );
      _goalCount += result.goalsRan.length;

      // Per spec section 5.2 Case 2: On suspension, call request(X?) for blocking readers
      if (result.status == ExecutionStatus.suspended && result.blockingReaders.isNotEmpty) {
        _agent!.context.processSuspension(result.blockingReaders);
        _addOutput('[IRMA] Sent read requests for ${result.blockingReaders.length} blocking readers');
      }

      // Flush any pending irmaGLP messages
      final messagesFlushed = _agent!.flushMessages();
      if (messagesFlushed > 0) {
        _addOutput('[IRMA] Flushed $messagesFlushed messages');
      }

      // Display pending output
      _displayPendingOutput();

      setState(() {
        _isRunning = false;
        _status = result.status.name;
        _updateStats();
      });
    } catch (e) {
      setState(() {
        _isRunning = false;
        _status = 'Error: $e';
      });
    }
  }

  void _updateStats() {
    if (_agent != null) {
      _heapVars = _agent!.runtime.heap.HP;  // Heap pointer = total cells allocated
      _vpSize = _agent!.vp.entries.length;
      _mpSize = _agent!.mp.totalLength;
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
    if (_agent == null) return term;

    if (term is rt.VarRef) {
      final value = _agent!.runtime.heap.getValue(term.addr);
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
      final isReader = _agent?.runtime.heap.isReader(term.addr) ?? false;
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
                    child: Text(
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
                    enabled: _initialized,
                    decoration: InputDecoration(
                      hintText: _initialized 
                          ? 'send(${widget.friends.isNotEmpty ? widget.friends.first.toLowerCase() : "friend"}, ping)' 
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
                  'G:$_goalCount H:$_heapVars V:$_vpSize M:$_mpSize',
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

/// V_p-based friend channel
///
/// Uses proper IRMA V_p semantics:
/// - outputWriterAddr: Local writer we bind to send to friend
/// - inputReaderAddr: Imported reader that receives from friend
class _VpFriendChannel {
  final String friendName;
  final int outputWriterAddr;  // We write here, friend receives
  final int inputReaderAddr;   // Friend writes here, we receive

  _VpFriendChannel({
    required this.friendName,
    required this.outputWriterAddr,
    required this.inputReaderAddr,
  });
}

/// Multi-agent I/O context with user, network, and V_p-based friend channels
class _MultiAgentIOContext {
  final ExternalChannel userChannel;
  final ExternalChannel netChannel;
  final Map<String, _VpFriendChannel> friendChannels;
  final InputInjector userInput;
  final InputInjector netInput;  // For incoming network messages
  final OutputObserver userOutput;

  _MultiAgentIOContext({
    required this.userChannel,
    required this.netChannel,
    required this.friendChannels,
    required this.userInput,
    required this.netInput,
    required this.userOutput,
  });

  rt.Term get userChannelTerm => buildChannelTerm(userChannel);
  rt.Term get netChannelTerm => buildChannelTerm(netChannel);

  void dispose() {
    userOutput.dispose();
  }
}
