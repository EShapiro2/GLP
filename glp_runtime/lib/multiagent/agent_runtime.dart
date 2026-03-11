/// AgentRuntime — encapsulates GLP agent runtime for UI integration.
///
/// Extracted from glp_multiagent/lib/main.dart.
/// Uses GlpEngine (the ONE way to run GLP programs) for compilation,
/// MadContext for madGLP messaging, and Scheduler for execution.
///
/// Boot approach: GlpEngine loads stdlib, enableMadGLP loads madPredicates,
/// starts agent_init(Id, UserIn, NetIn). Network output goes through
/// send_to_net → global_send → MadContext. User output goes through
/// send_to_user → _output/1 kernel → outputCallback.
library;

import 'dart:typed_data';

import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

/// Agent runtime encapsulating GLP execution, madGLP context, and I/O.
///
/// Usage:
/// 1. Create with agent ID and GLP source
/// 2. Set callbacks: onOutput, onLog, onSendMadMessage
/// 3. Call initialize() to compile and start
/// 4. Call injectUserInput(text) for user commands
/// 5. Call onMadMessageReceived(from, payload) for network messages
class AgentRuntime {
  final String agentId;
  final List<String> glpSources;
  final String rootSelfGlpPath;
  final List<String> friends;

  /// Entry-point goal label, e.g. 'agent_init/3', 'agent_init_play/3',
  /// 'parent_init/4', 'child_init/3'.
  final String goalLabel;

  /// Extra arguments inserted between Id (arg 0) and NetIn (last arg).
  /// For example, ['carol', '4'] for parent_init(alice, carol, 4, NetIn).
  final List<String> extraArgs;

  /// Optional project directory for static linking.
  /// When set, the engine calls loadProject(projectDir) first, then loads
  /// glpSources on top (typically just the madGLP boot source).
  final String? projectDir;

  // Callbacks set by UI layer
  void Function(String line)? onOutput;
  void Function(String tag, String message)? onLog;
  Future<void> Function(String destination, Uint8List payload)? onSendMadMessage;

  // Runtime state
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

  // Enable GLP trace output
  bool glpTraceEnabled = true;

  AgentRuntime({
    required this.agentId,
    required this.glpSources,
    required this.rootSelfGlpPath,
    this.friends = const [],
    this.goalLabel = 'agent_init/3',
    this.extraArgs = const [],
    this.projectDir,
  });

  bool get initialized => _initialized;
  GlpRuntime? get runtime => _runtime;
  MadContext? get ctx => _ctx;

  String get _tag => agentId.toUpperCase();

  void _log(String message) {
    onLog?.call(_tag, message);
  }

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

  Future<void> initialize() async {
    final agentIdLower = agentId.toLowerCase();
    _log('INIT: Starting');
    _output('[INIT] Creating MadContext...');

    // Use GlpEngine — the ONE way to run GLP programs.
    final engine = GlpEngine(rootSelfGlpPath: rootSelfGlpPath)..strictTypes = false;

    // Enable madGLP mode (loads madPredicates + creates MadContext)
    engine.enableMadGLP(agentId: agentIdLower);

    // Load program: either project-linked or individual source files.
    if (projectDir != null) {
      // Project mode: load linked project, then boot source(s) on top.
      _log('INIT: Loading project from $projectDir');
      engine.loadProject(projectDir!);
      _log('INIT: Project loaded, loading ${glpSources.length} boot source(s)');
      for (var i = 0; i < glpSources.length; i++) {
        engine.loadSource(glpSources[i], filename: 'source_$i');
      }
      // Diagnostic: check key labels
      final program = engine.combinedProgram;
      final keyLabels = ['parent_init/4', 'child_init/3', 'agent/4', 'ui_mediator/5', 'merge/3', 'tee/3'];
      for (final key in keyLabels) {
        final pc = program.labels[key];
        _log('INIT: Label $key -> ${pc != null ? "PC=$pc" : "NOT FOUND"}');
      }
      _log('INIT: Program loaded via project linking ($projectDir) + ${glpSources.length} boot source(s), ${program.labels.length} labels');
    } else {
      // Legacy mode: load each source file separately.
      for (var i = 0; i < glpSources.length; i++) {
        engine.loadSource(glpSources[i], filename: 'source_$i');
      }
      _log('INIT: Program loaded via GlpEngine (stdlib + madPredicates + ${glpSources.length} source files)');
    }

    _runtime = engine.runtime;
    _ctx = engine.madContext;
    _log('INIT: MadContext created');

    // Wire _output/1 kernel to our output callback
    _runtime!.outputCallback = (text) {
      _output('< $text');
    };

    // Wire outbound madGLP messages
    _ctx!.onMessageReady = (destination, msg) async {
      final serializer = PayloadSerializer(agentIdLower);
      final payload = serializer.serializeMessage(msg);
      await _sendMadPayload(destination, payload);
    };

    // Initialize serializer entry for network input (index 0)
    // Spec Section 4.1: permanent entry mapping _r(p, 0) to local writer
    final (netInWriter, netInReader) = _runtime!.heap.allocateVariable();
    _ctx!.wp.initializeSerializerEntry(netInWriter);
    _log('INIT: Serializer entry initialized, netIn=($netInWriter,$netInReader)');

    // Create user input stream (Dart injects ground terms)
    final (userInWriter, userInReader) = _runtime!.heap.allocateVariable();
    _userInput = InputInjector(_runtime!.heap, 'user', userInWriter);

    // Create net input stream (receives from MadContext)
    _netInput = InputInjector(_runtime!.heap, 'net', netInWriter);

    _output('[INIT] Loaded GLP program');

    // Get combined program and create scheduler
    final program = engine.combinedProgram;
    final runner = BytecodeRunner(program);
    _scheduler = Scheduler(rt: _runtime!, runners: {'main': runner},
      traceSink: (line) => _log('GLP: $line'));
    _scheduler!.resetDisplayNumbering();

    // Start goal using configurable goalLabel and extraArgs.
    // Args are always: [Id, ...extraArgs, NetIn].
    // For backward compatibility, agent_init/3 also gets UserIn before NetIn.
    final entryPC = program.labels[goalLabel];
    _log('INIT: $goalLabel entryPC=$entryPC');
    if (entryPC == null) {
      _output('[ERROR] Predicate $goalLabel not found');
      return;
    }

    final heap = _runtime!.heap;
    final args = <int, rt.Term>{};
    var argIdx = 0;

    // Arg 0: agent ID (always first)
    final (arg0Writer, arg0Reader) = heap.allocateVariable();
    heap.bindVariable(arg0Writer, rt.ConstTerm(agentIdLower));
    args[argIdx++] = rt.VarRef(arg0Reader);

    // For agent_init/3 (legacy): insert UserIn before NetIn
    if (goalLabel == 'agent_init/3') {
      final (userWriter, userReader) = heap.allocateVariable();
      heap.bindVariable(userWriter, rt.VarRef(userInReader));
      args[argIdx++] = rt.VarRef(userReader);
    }

    // Extra args (e.g. child name, play number) — inserted as constants
    for (final extra in extraArgs) {
      final (eWriter, eReader) = heap.allocateVariable();
      // Try to parse as int, otherwise use as atom
      final intVal = int.tryParse(extra);
      heap.bindVariable(eWriter, rt.ConstTerm(intVal ?? extra));
      args[argIdx++] = rt.VarRef(eReader);
    }

    // Last arg: NetIn (always last)
    final (netWriter, netReader) = heap.allocateVariable();
    heap.bindVariable(netWriter, rt.VarRef(netInReader));
    args[argIdx++] = rt.VarRef(netReader);

    final env = CallEnv(args: args);
    _runtime!.setGoalEnv(1, env);
    _runtime!.setGoalProgram(1, 'main');
    _runtime!.gq.enqueue(GoalRef(1, entryPC));

    final argsDesc = [agentIdLower, ...extraArgs, 'NetIn'].join(', ');
    final goalName = goalLabel.split('/').first;
    _output('[GOAL] Started $goalName($argsDesc)');
    _log('INIT: GQ length before initial run: ${_runtime!.gq.length}');

    // Initial run
    final initStatus = await _runUntilQuiescent();
    _log('INIT: Initial run status: $initStatus, GQ after: ${_runtime!.gq.length}');

    _initialized = true;
    updateStats();

    final firstFriend = friends.isNotEmpty ? friends.first.toLowerCase() : 'friend';
    _output('[INIT] Ready! Commands:');
    _output('  connect($firstFriend)         - cold-call $firstFriend');
    _output('  send($firstFriend, hello)     - send text message');
    _output('  decision(yes, $firstFriend, 1) - accept befriend (req ID from output)');
    _output('  introduce(alice, charlie)     - introduce two friends');
  }

  // =========================================================================
  // USER INPUT
  // =========================================================================

  /// Inject user input text.
  Future<void> injectUserInput(String text) async {
    _log('USER_INPUT: $text');
    if (text.isEmpty || _userInput == null || _runtime == null) {
      _log('USER_INPUT: early return (empty or not initialized)');
      return;
    }

    _output('> $text');

    try {
      // Parse as GLP term and inject into user input stream
      final term = parseTerm(text);
      _log('USER_INPUT: parsed -> ${formatTerm(term)}');

      final activations = _userInput!.inject(term);
      _log('USER_INPUT: ${activations.length} activations');
      for (final goal in activations) {
        _runtime!.gq.enqueue(goal);
      }

      await _runUntilQuiescent();
    } catch (e, st) {
      _log('USER_INPUT ERROR: $e\n$st');
      _output('[ERROR] $e');
    }
  }

  // =========================================================================
  // NETWORK MESSAGES
  // =========================================================================

  /// Handle incoming madGLP binary message.
  Future<void> onMadMessageReceived(String from, Uint8List payload) async {
    _log('MAD_RECV from $from (${payload.length} bytes)');

    if (_runtime == null || _ctx == null || _netInput == null) {
      _log('MAD_RECV: ERROR - runtime/ctx/netInput is null');
      return;
    }

    final serializer = PayloadSerializer(agentId.toLowerCase());
    final msg = serializer.deserializeMessage(payload);
    _log('MAD_RECV: type=${msg.type}, dest=${msg.destination}');

    if (msg.type == MessageType.assignment) {
      try {
        final (globalName, value) = serializer.deserializeGlobalSendPayload(
          msg.payload,
          (isReader) {
            final (w, r) = _runtime!.heap.allocateVariable();
            return isReader ? r : w;
          },
        );
        _log('MAD_ASSIGN: $globalName := ${formatTerm(value)}');
        _ctx!.handleMadAssignment(
          globalName: globalName,
          value: value,
          fromAgent: from.toLowerCase(),
        );
        // Reactivation handled by bindVariable() inside MadContext.handleMadAssignment
      } catch (e) {
        _log('MAD_ERROR: $e');
      }
    } else if (msg.type == MessageType.agentMessage) {
      final term = serializer.deserializeAgentMessagePayload(
        msg.payload,
        (isReader) {
          final (w, r) = _runtime!.heap.allocateVariable();
          return isReader ? r : w;
        },
      );
      final formatted = formatTerm(term);
      _log('AGENT_MSG: $formatted');

      _log('INJECT into netInput');
      final activations = _netInput!.inject(term);
      _log('INJECT: ${activations.length} activations');
      for (final goal in activations) {
        _runtime!.gq.enqueue(goal);
      }
    } else {
      _log('MAD_RECV: Unknown message type ${msg.type}');
    }

    updateStats();
    await _runUntilQuiescent();
  }

  /// Handle legacy JSON message (backwards compatibility).
  Future<void> onLegacyMessageReceived(String from, dynamic payload) async {
    _output('[RECV from $from] $payload');
    if (_netInput == null || _runtime == null) return;

    final msgTerm = rt.StructTerm('msg', [
      rt.ConstTerm(from.toLowerCase()),
      rt.ConstTerm(agentId.toLowerCase()),
      rt.ConstTerm(payload),
    ]);

    final activations = _netInput!.inject(msgTerm);
    for (final goal in activations) {
      _runtime!.gq.enqueue(goal);
    }
    await _runUntilQuiescent();
  }

  Future<void> _sendMadPayload(String to, Uint8List payload) async {
    _log('SEND_MAD to $to (${payload.length} bytes)');
    await onSendMadMessage?.call(to, payload);
  }

  // =========================================================================
  // EXECUTION
  // =========================================================================

  /// Run the scheduler until quiescent.
  /// Returns the execution status name, or null if not initialized.
  ///
  /// Per agent-runtime-spec.md Section 3: one drain (run all runnable goals
  /// until quiescent), one flush (send all queued outbound messages).
  Future<String?> runUntilQuiescent() async {
    return _runUntilQuiescent();
  }

  Future<String?> _runUntilQuiescent() async {
    _log('RUN: start (GQ=${_runtime?.gq.length ?? 0})');
    if (_scheduler == null || _runtime == null) {
      _log('RUN: early return (not initialized)');
      return null;
    }

    try {
      // Per spec: drain all runnable goals, then flush outbound messages.
      final result = _scheduler!.drainWithStatus(debug: glpTraceEnabled);
      _log('RUN: status=${result.status}, goals=${result.goalsRan.length}');
      goalCount += result.goalsRan.length;

      final messagesFlushed = _ctx!.flushMessages();
      if (messagesFlushed > 0) {
        _log('RUN: flushed $messagesFlushed messages');
      }

      updateStats();
      _log('RUN: done (status=${result.status.name})');
      return result.status.name;
    } catch (e, st) {
      _log('RUN ERROR: $e\n$st');
      return 'error';
    }
  }

  // =========================================================================
  // TERM UTILITIES
  // =========================================================================

  rt.Term parseTerm(String termStr) {
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

  rt.Term derefTerm(rt.Term term) {
    if (_runtime == null) return term;

    if (term is rt.VarRef) {
      final value = _runtime!.heap.getValue(term.addr);
      if (value != null && value is! rt.VarRef) {
        return derefTerm(value);
      }
      return term;
    }
    if (term is rt.StructTerm) {
      final derefArgs = term.args.map(derefTerm).toList();
      return rt.StructTerm(term.functor, derefArgs);
    }
    return term;
  }

  String formatTerm(rt.Term term) {
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
          elements.add(formatTerm(current.args[0]));
          current = current.args[1];
        }
        if (current is rt.ConstTerm && (current.value == 'nil' || current.value == null)) {
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

  void dispose() {
    // No OutputObservers to dispose — output goes through _output/1 kernel
  }
}
