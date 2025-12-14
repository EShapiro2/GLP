/// GLP Multiagent - Phase 2 UI with GLPSAM Integration
///
/// Orange theme, wired to echo_io.glp via Phase 0 I/O classes.
library;

import 'package:flutter/material.dart';
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

/// Embedded echo_io.glp program source
const _echoIoSource = '''
%% echo_io.glp - Simple echo for I/O testing (Phase 0)
%% Out? in head reads the output position, Out in body writes the tail
echo_test(ch([H|In], [echo(H?)|Out?])) :-
    ground(H?) |
    echo_test(ch(In?, Out)).
echo_test(ch([], [])).
''';

/// Simple holder for I/O context (avoids AgentIOContext late field issues)
class _SimpleIOContext {
  final ExternalChannel userChannel;
  final ExternalChannel netChannel;
  final InputInjector userInput;
  final OutputObserver userOutput;

  _SimpleIOContext({
    required this.userChannel,
    required this.netChannel,
    required this.userInput,
    required this.userOutput,
  });

  rt.Term get userChannelTerm => buildChannelTerm(userChannel);
  rt.Term get netChannelTerm => buildChannelTerm(netChannel);

  void dispose() {
    userOutput.dispose();
  }
}

void main() {
  runApp(const GlpApp());
}

class GlpApp extends StatelessWidget {
  const GlpApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'GLP Multiagent',
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
        floatingActionButtonTheme: const FloatingActionButtonThemeData(
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
      home: const AgentScreen(),
    );
  }
}

class AgentScreen extends StatefulWidget {
  const AgentScreen({super.key});

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
  _SimpleIOContext? _ioContext;
  Map<String, BytecodeProgram> _programs = {};
  int _goalId = 1;

  // Pending output terms (to be dereferenced after execution)
  final List<rt.Term> _pendingOutputTerms = [];

  String _agentId = 'agent_001';
  int _goalCount = 0;
  int _heapVars = 0;
  bool _isRunning = false;
  bool _initialized = false;
  String _status = 'Not initialized';

  @override
  void initState() {
    super.initState();
    _initializeRuntime();
  }

  Future<void> _initializeRuntime() async {
    try {
      debugPrint('=== INIT START ===');
      _addOutput('[INIT] Creating GLP runtime...');

      // Create runtime
      _runtime = GlpRuntime();
      registerStandardPredicates(_runtime!.systemPredicates);

      // Create channels manually (don't use AgentIOContext.create which sets up default observers)
      final userChannel = createExternalChannel(_runtime!.heap, 'user');
      final netChannel = createExternalChannel(_runtime!.heap, 'net');

      // Create input injector
      final userInput = InputInjector(_runtime!.heap, 'user', userChannel.inputVarId);

      // Create output observer - collects terms to be displayed after execution
      final userOutput = OutputObserver(
        _runtime!.heap,
        'user',
        userChannel.outputVarId,
        (term) {
          debugPrint('=== OUTPUT RECEIVED: $term ===');
          // Store term for later display (after execution completes)
          _pendingOutputTerms.add(term);
        },
        () {
          setState(() {
            _outputLog.add('[OUTPUT CLOSED]');
          });
        },
      );

      // Store in a simple holder
      _ioContext = _SimpleIOContext(
        userChannel: userChannel,
        netChannel: netChannel,
        userInput: userInput,
        userOutput: userOutput,
      );

      debugPrint('=== CHANNELS CREATED ===');
      _addOutput('[INIT] Loading echo_io.glp...');

      // Compile embedded echo_io.glp program
      final compiler = GlpCompiler();
      final program = compiler.compile(_echoIoSource);
      _programs['echo_io.glp'] = program;
      debugPrint('=== PROGRAM COMPILED ===');
      _addOutput('[INIT] Loaded echo_io.glp');

      // Start goal: echo_test(Ch)
      _addOutput('[INIT] Starting goal: echo_test(Ch)');
      _startGoal('echo_test(Ch)');
      debugPrint('=== GOAL STARTED ===');

      setState(() {
        _initialized = true;
        _status = 'Ready';
        _updateStats();
      });

      debugPrint('=== INIT COMPLETE ===');
      _addOutput('[INIT] Ready! Type text and click Send.');
    } catch (e, st) {
      debugPrint('=== INIT ERROR: $e ===');
      debugPrint('$st');
      _addOutput('[ERROR] $e');
      _addOutput('[TRACE] $st');
    }
  }

  void _startGoal(String goalStr) {
    if (_runtime == null || _ioContext == null) return;

    try {
      // Parse goal
      final parseInput = goalStr.endsWith('.') ? goalStr : '$goalStr.';
      final lexer = Lexer(parseInput);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final parsedAst = parser.parse();

      if (parsedAst.procedures.isEmpty || parsedAst.procedures[0].clauses.isEmpty) {
        _addOutput('[ERROR] Could not parse goal');
        return;
      }

      final goalClause = parsedAst.procedures[0].clauses[0];
      final goalAtom = goalClause.head;
      final functor = goalAtom.functor;
      final arity = goalAtom.arity;
      final args = goalAtom.args;

      // Combine loaded programs
      final allOps = <dynamic>[];
      for (final loaded in _programs.values) {
        allOps.addAll(loaded.ops);
      }
      final combinedProgram = BytecodeProgram(allOps);

      // Find entry point
      final procedureLabel = '$functor/$arity';
      final entryPC = combinedProgram.labels[procedureLabel];
      if (entryPC == null) {
        _addOutput('[ERROR] Predicate $procedureLabel not found');
        return;
      }

      // Set up arguments
      final argSlots = <int, rt.Term>{};
      for (int i = 0; i < args.length; i++) {
        final arg = args[i];
        if (arg is ast.VarTerm) {
          final name = arg.name;
          if (name == 'Ch' || name == 'UserCh') {
            argSlots[i] = _ioContext!.userChannelTerm;
          } else if (name == 'Net' || name == 'NetCh') {
            argSlots[i] = _ioContext!.netChannelTerm;
          } else {
            final (writerId, readerId) = _runtime!.heap.allocateFreshPair();
            argSlots[i] = rt.VarRef(arg.isReader ? readerId : writerId, isReader: arg.isReader);
          }
        }
      }

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

      _addOutput('[GOAL] Started $goalStr');
    } catch (e) {
      _addOutput('[ERROR] Starting goal: $e');
    }
  }

  void _sendInput() {
    final text = _inputController.text.trim();
    if (text.isEmpty || _ioContext == null || _runtime == null) return;

    debugPrint('=== SEND INPUT: $text ===');
    setState(() {
      _outputLog.add('> $text');
    });

    try {
      // Parse and inject term
      final term = _parseTerm(text);
      debugPrint('=== PARSED TERM: $term ===');
      final activations = _ioContext!.userInput.inject(term);
      debugPrint('=== INJECTED, activations=${activations.length} ===');

      // Enqueue activated goals
      for (final goal in activations) {
        debugPrint('=== ENQUEUE reactivated goal ${goal.id} at PC ${goal.pc} ===');
        _runtime!.gq.enqueue(goal);
      }

      _inputController.clear();
      _scrollToBottom();

      // Auto-run after injection
      debugPrint('=== RUNNING... ===');
      _runUntilQuiescent();
    } catch (e, st) {
      debugPrint('=== SEND ERROR: $e ===');
      debugPrint('$st');
      _addOutput('[ERROR] $e');
    }
  }

  rt.Term _parseTerm(String termStr) {
    // Parse term by wrapping as fact
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

  void _step() {
    if (_scheduler == null) {
      _addOutput('[ERROR] No scheduler');
      return;
    }

    setState(() {
      _outputLog.add('[STEP]');
    });

    final result = _scheduler!.drainWithStatus(maxCycles: 1, debug: false);
    _goalCount += result.goalsRan.length;
    _updateStats();
    _scrollToBottom();
  }

  Future<void> _runUntilQuiescent() async {
    if (_scheduler == null) {
      debugPrint('=== NO SCHEDULER ===');
      return;
    }

    setState(() {
      _isRunning = true;
      _status = 'Running...';
    });

    try {
      debugPrint('=== DRAIN START, queue=${_runtime!.gq.length} ===');
      final result = await _scheduler!.drainAsyncWithStatus(
        maxCycles: 1000,
        debug: true,
      );
      debugPrint('=== DRAIN DONE: ${result.status}, ran=${result.goalsRan.length} ===');
      _goalCount += result.goalsRan.length;

      // Display pending output (now that variables are bound)
      _displayPendingOutput();

      setState(() {
        _isRunning = false;
        _status = result.status.name;
        _updateStats();
      });
    } catch (e, st) {
      debugPrint('=== RUN ERROR: $e ===');
      debugPrint('$st');
      setState(() {
        _isRunning = false;
        _status = 'Error: $e';
      });
    }
  }

  void _run() {
    _addOutput('[RUN]');
    _runUntilQuiescent();
  }

  void _stop() {
    setState(() {
      _isRunning = false;
      _status = 'Stopped';
      _outputLog.add('[STOP]');
    });
    _scrollToBottom();
  }

  void _updateStats() {
    if (_runtime != null) {
      _heapVars = _runtime!.heap.allVarIds.length;
    }
  }

  /// Display pending output terms (after execution, when variables are bound)
  void _displayPendingOutput() {
    for (final term in _pendingOutputTerms) {
      final derefTerm = _derefTerm(term);
      debugPrint('=== DISPLAY OUTPUT: $derefTerm ===');
      setState(() {
        _outputLog.add('< ${_formatTerm(derefTerm)}');
      });
    }
    _pendingOutputTerms.clear();
    _scrollToBottom();
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

  /// Dereference a term - replace VarRefs with their actual values
  rt.Term _derefTerm(rt.Term term) {
    if (_runtime == null) return term;

    if (term is rt.VarRef) {
      // Dereference the variable
      final value = _runtime!.heap.getValue(term.varId);
      if (value != null && value is! rt.VarRef) {
        // Got a concrete value - dereference it recursively
        return _derefTerm(value);
      }
      // Still unbound or is another VarRef - return as-is
      return term;
    }
    if (term is rt.StructTerm) {
      // Recursively dereference args
      final derefArgs = term.args.map(_derefTerm).toList();
      return rt.StructTerm(term.functor, derefArgs);
    }
    // ConstTerm or other - return as-is
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
        title: const Text('GLP Multiagent'),
        actions: [
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16.0),
            child: Center(
              child: Text(
                _agentId,
                style: const TextStyle(fontWeight: FontWeight.bold),
              ),
            ),
          ),
        ],
      ),
      body: Column(
        children: [
          // Control buttons
          Container(
            padding: const EdgeInsets.all(8.0),
            color: Colors.orange.shade50,
            child: Row(
              children: [
                ElevatedButton.icon(
                  onPressed: _initialized ? _step : null,
                  icon: const Icon(Icons.skip_next),
                  label: const Text('Step'),
                ),
                const SizedBox(width: 8),
                ElevatedButton.icon(
                  onPressed: (_initialized && !_isRunning) ? _run : null,
                  icon: const Icon(Icons.play_arrow),
                  label: const Text('Run'),
                ),
                const SizedBox(width: 8),
                ElevatedButton.icon(
                  onPressed: _isRunning ? _stop : null,
                  icon: const Icon(Icons.stop),
                  label: const Text('Stop'),
                  style: ElevatedButton.styleFrom(
                    backgroundColor: _isRunning ? Colors.red : Colors.grey,
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
                controller: _scrollController,
                padding: const EdgeInsets.all(8.0),
                itemCount: _outputLog.length,
                itemBuilder: (context, index) {
                  final line = _outputLog[index];
                  final isInput = line.startsWith('>');
                  final isOutput = line.startsWith('<');
                  final isControl = line.startsWith('[');
                  return Padding(
                    padding: const EdgeInsets.symmetric(vertical: 2.0),
                    child: Text(
                      line,
                      style: TextStyle(
                        fontFamily: 'monospace',
                        fontSize: 14,
                        color: isInput
                            ? Colors.blue.shade800
                            : isOutput
                                ? Colors.green.shade800
                                : isControl
                                    ? Colors.orange.shade800
                                    : Colors.black87,
                        fontWeight: (isControl || isOutput) ? FontWeight.bold : FontWeight.normal,
                      ),
                    ),
                  );
                },
              ),
            ),
          ),

          // Input area
          Container(
            padding: const EdgeInsets.all(8.0),
            color: Colors.white,
            child: Row(
              children: [
                Expanded(
                  child: TextField(
                    controller: _inputController,
                    enabled: _initialized,
                    decoration: InputDecoration(
                      hintText: _initialized ? 'Enter term to send...' : 'Initializing...',
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
            padding: const EdgeInsets.symmetric(horizontal: 16.0, vertical: 8.0),
            color: Colors.orange.shade100,
            child: Row(
              children: [
                Text(
                  'Goals: $_goalCount',
                  style: const TextStyle(fontWeight: FontWeight.w500),
                ),
                const SizedBox(width: 24),
                Text(
                  'Heap vars: $_heapVars',
                  style: const TextStyle(fontWeight: FontWeight.w500),
                ),
                const Spacer(),
                Container(
                  width: 12,
                  height: 12,
                  decoration: BoxDecoration(
                    shape: BoxShape.circle,
                    color: _isRunning ? Colors.green : (_initialized ? Colors.orange : Colors.grey),
                  ),
                ),
                const SizedBox(width: 8),
                Text(_status),
              ],
            ),
          ),
        ],
      ),
    );
  }
}
