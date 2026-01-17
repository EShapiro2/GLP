/// GLP Multiagent - Social Graph Agent with Friends List
///
/// Coordinator window spawns agent windows, routes messages between them.
/// GLP program loaded from external file.
library;

import 'dart:convert';
import 'dart:io';
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

/// Default GLP program path
const _defaultGlpPath = '/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp';

/// Stdlib directory
const _stdlibPath = '/Users/udi/Grassroots/GLP/programs/stdlib';

/// Entry point - checks if spawned window or main coordinator
void main(List<String> args) {
  debugPrint('=== MAIN ARGS: $args ===');

  if (args.firstOrNull == 'multi_window') {
    // Spawned agent window
    final windowId = int.parse(args[1]);
    final params = jsonDecode(args[2]) as Map<String, dynamic>;
    final agentId = params['agentId'] as String;
    final friendId = params['friendId'] as String;
    final glpSource = params['glpSource'] as String;
    debugPrint('=== SPAWNED AGENT WINDOW: $agentId (friend=$friendId, windowId=$windowId) ===');
    runAgentWindow(windowId, agentId, friendId, glpSource);
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
void runAgentWindow(int windowId, String agentId, String friendId, String glpSource) {
  runApp(AgentApp(windowId: windowId, agentId: agentId, friendId: friendId, glpSource: glpSource));
}

// ============================================================================
// SIMPLE ROUTER - Routes messages between agent windows
// ============================================================================

/// Simple message router using MethodChannel
class SimpleRouter {
  static final SimpleRouter _instance = SimpleRouter._();
  static SimpleRouter get instance => _instance;

  SimpleRouter._();

  final Map<String, int> _agentWindows = {}; // agentId -> windowId
  final List<String> _routingLog = [];
  VoidCallback? onLogUpdate;

  List<String> get routingLog => List.unmodifiable(_routingLog);

  void _log(String message) {
    final timestamp = DateTime.now().toIso8601String().substring(11, 19);
    _routingLog.add('[$timestamp] $message');
    onLogUpdate?.call();
  }

  void clearLog() {
    _routingLog.clear();
    onLogUpdate?.call();
  }

  void register(String agentId, int windowId) {
    _agentWindows[agentId.toLowerCase()] = windowId;
    _log('Registered $agentId (window $windowId)');
  }

  void unregister(String agentId) {
    _agentWindows.remove(agentId.toLowerCase());
    _log('Unregistered $agentId');
  }

  Future<void> route(String from, String to, dynamic payload) async {
    _log('Route: $from -> $to: $payload');

    final targetWindowId = _agentWindows[to.toLowerCase()];
    if (targetWindowId == null) {
      _log('ERROR: Unknown recipient $to');
      return;
    }

    try {
      await DesktopMultiWindow.invokeMethod(
        targetWindowId,
        'deliver',
        jsonEncode({
          'from': from,
          'payload': payload,
        }),
      );
      _log('Delivered to $to');
    } catch (e) {
      _log('ERROR delivering to $to: $e');
    }
  }
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
    SimpleRouter.instance.onLogUpdate = () {
      setState(() {});
    };

    // Set up handler for messages from agent windows
    DesktopMultiWindow.setMethodHandler((call, fromWindowId) async {
      debugPrint('=== COORDINATOR RECEIVED: ${call.method} from $fromWindowId ===');
      if (call.method == 'send') {
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final to = args['to'] as String;
        final payload = args['payload'];
        await SimpleRouter.instance.route(from, to, payload);
      }
      return null;
    });

    _log.add('Coordinator started');
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
        // Load stdlib unify.glp first (provides =/2)
        String stdlibSource = '';
        final unifyFile = File('$_stdlibPath/unify.glp');
        if (unifyFile.existsSync()) {
          stdlibSource = await unifyFile.readAsString();
        } else {
          // Inline the minimal unify.glp if file not found
          stdlibSource = 'X? = X.\n';
        }
        
        // Load user GLP program
        final userSource = await File(newPath).readAsString();
        
        // Combine: stdlib first, then user program
        _cachedGlpSource = '$stdlibSource\n$userSource';
        
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

  Future<void> _spawnAgent(String agentId, String friendId, double x, double y) async {
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
          'friendId': friendId,
          'glpSource': _cachedGlpSource,
        }),
      );
      await window.setTitle('$agentId - GLP Agent');
      await window.setFrame(Rect.fromLTWH(x, y, 400, 600));
      await window.show();

      _windows[agentId] = window;
      SimpleRouter.instance.register(agentId, window.windowId);

      _log.add('Spawned $agentId (friend=$friendId, window ${window.windowId})');
      setState(() {});
    } catch (e) {
      _log.add('ERROR spawning $agentId: $e');
      setState(() {});
    }
  }

  Future<void> _spawnAliceAndBob() async {
    // Alice's friend is Bob, Bob's friend is Alice
    await _spawnAgent('Alice', 'Bob', 100, 100);
    await _spawnAgent('Bob', 'Alice', 520, 100);
  }

  Future<void> _closeAll() async {
    for (final entry in _windows.entries) {
      try {
        await entry.value.close();
        SimpleRouter.instance.unregister(entry.key);
      } catch (e) {
        _log.add('ERROR closing ${entry.key}: $e');
      }
    }
    _windows.clear();
    SimpleRouter.instance.clearLog();
    _log.add('Closed all windows');
    setState(() {});
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('GLP Coordinator'),
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
                  onPressed: _spawnAliceAndBob,
                  icon: const Icon(Icons.people),
                  label: const Text('Spawn Alice & Bob'),
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
                itemCount: SimpleRouter.instance.routingLog.length,
                itemBuilder: (context, index) {
                  return Text(
                    SimpleRouter.instance.routingLog[index],
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
  final String friendId;
  final String glpSource;

  const AgentApp({
    super.key,
    required this.windowId,
    required this.agentId,
    required this.friendId,
    required this.glpSource,
  });

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: '$agentId - GLP Agent',
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
        friendId: friendId,
        glpSource: glpSource,
      ),
    );
  }
}

class AgentScreen extends StatefulWidget {
  final int windowId;
  final String agentId;
  final String friendId;
  final String glpSource;

  const AgentScreen({
    super.key,
    required this.windowId,
    required this.agentId,
    required this.friendId,
    required this.glpSource,
  });

  @override
  State<AgentScreen> createState() => _AgentScreenState();
}

class _AgentScreenState extends State<AgentScreen> {
  final TextEditingController _inputController = TextEditingController();
  final ScrollController _scrollController = ScrollController();
  final List<String> _outputLog = [];

  // GLP Runtime components
  GlpRuntime? _runtime;
  Scheduler? _scheduler;
  _FullIOContext? _ioContext;
  Map<String, BytecodeProgram> _programs = {};
  int _goalId = 1;

  // Pending output terms (to be dereferenced after execution)
  final List<rt.Term> _pendingUserOutputTerms = [];
  final List<rt.Term> _pendingFriendOutputTerms = [];

  int _goalCount = 0;
  int _heapVars = 0;
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
    // Handle messages from coordinator - inject into GLP friend input
    DesktopMultiWindow.setMethodHandler((call, fromWindowId) async {
      debugPrint('=== AGENT ${widget.agentId} RECEIVED: ${call.method} ===');
      if (call.method == 'deliver') {
        final args = jsonDecode(call.arguments as String) as Map<String, dynamic>;
        final from = args['from'] as String;
        final payload = args['payload'];
        _onFriendMessageReceived(from, payload);
      }
      return null;
    });
  }

  void _onFriendMessageReceived(String from, dynamic payload) {
    _addOutput('[RECV from $from] $payload');

    if (_ioContext == null || _runtime == null) return;

    // Inject msg(From, Id, Payload) into friend input stream
    final msgTerm = rt.StructTerm('msg', [
      rt.ConstTerm(from.toLowerCase()),
      rt.ConstTerm(widget.agentId.toLowerCase()),
      rt.ConstTerm(payload),
    ]);

    final activations = _ioContext!.friendInput.inject(msgTerm);
    for (final goal in activations) {
      _runtime!.gq.enqueue(goal);
    }

    // Run to process the message
    _runUntilQuiescent();
  }

  Future<void> _sendFriendMessage(String to, dynamic payload) async {
    _addOutput('[SEND to $to] $payload');

    // Send to coordinator for routing
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

  Future<void> _initializeRuntime() async {
    try {
      _addOutput('[INIT] Creating GLP runtime...');
      _addOutput('[INIT] GLP source: ${widget.glpSource.length} chars');

      // Use GLP source passed from coordinator
      final glpSource = widget.glpSource;

      // Create runtime
      _runtime = GlpRuntime();
      registerStandardPredicates(_runtime!.systemPredicates);

      // Create channels
      final userChannel = createExternalChannel(_runtime!.heap, 'user');
      final netChannel = createExternalChannel(_runtime!.heap, 'net');
      final friendChannel = createExternalChannel(_runtime!.heap, 'friend');

      // Create input injectors
      final userInput = InputInjector(_runtime!.heap, 'user', userChannel.inputVarId);
      final friendInput = InputInjector(_runtime!.heap, 'friend', friendChannel.inputVarId);

      // Create user output observer - displays to UI
      final userOutput = OutputObserver(
        _runtime!.heap,
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

      // Create friend output observer - sends to friend via coordinator
      final friendOutput = OutputObserver(
        _runtime!.heap,
        'friend',
        friendChannel.outputVarId,
        (term) {
          _pendingFriendOutputTerms.add(term);
        },
        () {
          debugPrint('=== FRIEND OUTPUT CLOSED ===');
        },
      );

      _ioContext = _FullIOContext(
        userChannel: userChannel,
        netChannel: netChannel,
        friendChannel: friendChannel,
        userInput: userInput,
        friendInput: friendInput,
        userOutput: userOutput,
        friendOutput: friendOutput,
      );

      // Compile GLP program
      final compiler = GlpCompiler();
      final program = compiler.compile(glpSource);
      _programs['agent.glp'] = program;

      _addOutput('[INIT] Loaded GLP program');

      // Start goal: agent(Id, FriendName, UserCh, NetCh, FriendOut)
      final agentIdLower = widget.agentId.toLowerCase();
      final friendIdLower = widget.friendId.toLowerCase();
      _addOutput('[INIT] Starting: agent($agentIdLower, $friendIdLower, ...)');
      _startAgentGoal(agentIdLower, friendIdLower);

      setState(() {
        _initialized = true;
        _status = 'Ready';
        _updateStats();
      });

      _addOutput('[INIT] Ready! Type: send($friendIdLower, ping)');
    } catch (e, st) {
      _addOutput('[ERROR] $e');
      debugPrint('$st');
      setState(() {
        _status = 'Error: $e';
      });
    }
  }

  void _startAgentGoal(String agentId, String friendId) {
    if (_runtime == null || _ioContext == null) return;

    try {
      // Combine loaded programs
      final allOps = <dynamic>[];
      for (final loaded in _programs.values) {
        allOps.addAll(loaded.ops);
      }
      final combinedProgram = BytecodeProgram(allOps);

      // Find entry point for agent/5
      final entryPC = combinedProgram.labels['agent/5'];
      if (entryPC == null) {
        _addOutput('[ERROR] Predicate agent/5 not found');
        return;
      }

      // Set up arguments: agent(Id, FriendName, UserCh, NetCh, FriendOut)
      final argSlots = <int, rt.Term>{
        0: rt.ConstTerm(agentId),
        1: rt.ConstTerm(friendId),
        2: _ioContext!.userChannelTerm,
        3: _ioContext!.netChannelTerm,
        4: rt.VarRef(_ioContext!.friendChannel.outputVarId, isReader: false),
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

      _addOutput('[GOAL] Started agent($agentId, $friendId, ...)');

      // Initial run to set up merge
      _runUntilQuiescent();
    } catch (e, st) {
      _addOutput('[ERROR] Starting goal: $e');
      debugPrint('$st');
    }
  }

  void _sendInput() {
    final text = _inputController.text.trim();
    if (text.isEmpty || _ioContext == null || _runtime == null) return;

    setState(() {
      _outputLog.add('> $text');
    });

    try {
      // Parse and inject term into UserIn
      final term = _parseTerm(text);
      final activations = _ioContext!.userInput.inject(term);

      // Enqueue activated goals
      for (final goal in activations) {
        _runtime!.gq.enqueue(goal);
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
      final (writerId, readerId) = _runtime!.heap.allocateFreshPair();
      return rt.VarRef(astTerm.isReader ? readerId : writerId, isReader: astTerm.isReader);
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

  Future<void> _runUntilQuiescent() async {
    if (_scheduler == null) return;

    setState(() {
      _isRunning = true;
      _status = 'Running...';
    });

    try {
      final result = await _scheduler!.drainAsyncWithStatus(
        maxCycles: 1000,
        debug: false,
      );
      _goalCount += result.goalsRan.length;

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
    if (_runtime != null) {
      _heapVars = _runtime!.heap.allVarIds.length;
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

    // Process friend output terms - send via coordinator
    for (final term in _pendingFriendOutputTerms) {
      final derefTerm = _derefTerm(term);
      // Expect msg(From, To, Content) from GLP
      if (derefTerm is rt.StructTerm && derefTerm.functor == 'msg' && derefTerm.args.length == 3) {
        final to = _termToString(derefTerm.args[1]);
        final content = _termToString(derefTerm.args[2]);
        _sendFriendMessage(to, content);
      } else {
        _addOutput('[FRIEND OUT] ${_formatTerm(derefTerm)}');
      }
    }
    _pendingFriendOutputTerms.clear();
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
      final value = _runtime!.heap.getValue(term.varId);
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
      return term.isReader ? 'X${term.varId}?' : 'X${term.varId}';
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
        title: Text('${widget.agentId} (friend: ${widget.friendId})'),
        actions: [
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16.0),
            child: Center(
              child: Text(
                'Window ${widget.windowId}',
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
                  final isSend = line.startsWith('[SEND');
                  final isRecv = line.startsWith('[RECV');
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
                                        : isControl
                                            ? Colors.orange.shade800
                                            : Colors.black87,
                        fontWeight: (isControl || isOutput || isSend || isRecv)
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
                          ? 'send(${widget.friendId.toLowerCase()}, ping)' 
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

          // Status bar
          Container(
            padding: const EdgeInsets.symmetric(horizontal: 8.0, vertical: 8.0),
            color: Colors.orange.shade100,
            child: Row(
              children: [
                Text(
                  'G:$_goalCount H:$_heapVars',
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
// HELPER CLASS
// ============================================================================

/// Full I/O context with user, network, and friend channels
class _FullIOContext {
  final ExternalChannel userChannel;
  final ExternalChannel netChannel;
  final ExternalChannel friendChannel;
  final InputInjector userInput;
  final InputInjector friendInput;
  final OutputObserver userOutput;
  final OutputObserver friendOutput;

  _FullIOContext({
    required this.userChannel,
    required this.netChannel,
    required this.friendChannel,
    required this.userInput,
    required this.friendInput,
    required this.userOutput,
    required this.friendOutput,
  });

  rt.Term get userChannelTerm => buildChannelTerm(userChannel);
  rt.Term get netChannelTerm => buildChannelTerm(netChannel);

  void dispose() {
    userOutput.dispose();
    friendOutput.dispose();
  }
}
