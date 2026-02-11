/// AgentRuntime — encapsulates GLP agent runtime for UI integration.
///
/// Extracted from glp_multiagent/lib/main.dart.
/// Manages GlpRuntime, MadContext, Scheduler, I/O channels, and GLP program
/// compilation. The UI layer (Flutter, CLI, test harness) instantiates
/// AgentRuntime, wires callbacks, and calls methods to inject input and
/// process messages.
library;

import 'dart:typed_data';

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
  final String glpSource;
  final List<String> friends;

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
  OutputObserver? _userOutput;
  OutputObserver? _netOutput;
  ExternalChannel? _userChannel;
  ExternalChannel? _netChannel;
  final Map<String, BytecodeProgram> _programs = {};
  int _goalId = 1;
  bool _initialized = false;

  // Pending output terms (to be dereferenced after execution)
  final List<rt.Term> _pendingUserOutputTerms = [];

  // Track writers shown to user: address -> name (e.g., 35 -> "X35")
  final Map<int, String> _knownWriters = {};

  // Stats
  int goalCount = 0;
  int heapVars = 0;
  int wpSize = 0;
  int mpSize = 0;

  // Enable GLP trace output
  bool glpTraceEnabled = true;

  AgentRuntime({
    required this.agentId,
    required this.glpSource,
    this.friends = const [],
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
    _log('INIT: Creating MadContext');
    _output('[INIT] Creating MadContext...');
    _output('[INIT] Friends: ${friends.join(", ")}');

    _runtime = GlpRuntime();
    _ctx = MadContext(agentId: agentId.toLowerCase(), runtime: _runtime!);
    _log('INIT: MadContext created');

    // Wire outbound madGLP messages
    _ctx!.onMessageReady = (destination, msg) async {
      final serializer = PayloadSerializer(agentId.toLowerCase());
      final payload = serializer.serializeMessage(msg);
      await _sendMadPayload(destination, payload);
    };

    // Register standard predicates
    registerStandardPredicates(_runtime!.systemPredicates);

    // Create external channels
    _userChannel = createExternalChannel(_runtime!.heap, 'user');
    _netChannel = createExternalChannel(_runtime!.heap, 'net');

    _userInput = InputInjector(_runtime!.heap, 'user', _userChannel!.inputWriterAddr);
    _netInput = InputInjector(_runtime!.heap, 'net', _netChannel!.inputWriterAddr);

    _userOutput = OutputObserver(
      _runtime!.heap,
      'user',
      _userChannel!.outputReaderAddr,
      (term) { _pendingUserOutputTerms.add(term); },
      () { _output('[USER OUTPUT CLOSED]'); },
    );

    _netOutput = OutputObserver(
      _runtime!.heap,
      'net',
      _netChannel!.outputReaderAddr,
      (term) { _handleNetOutput(term); },
      () { _output('[NET OUTPUT CLOSED]'); },
    );

    // Compile programs
    const stdlibSource = 'X? = X.\n';
    final stdlibCompiler = GlpCompiler();
    _programs['stdlib'] = stdlibCompiler.compile(stdlibSource);

    final userCompiler = GlpCompiler();
    _programs['user'] = userCompiler.compile(glpSource);

    _output('[INIT] Loaded GLP program');

    // Start agent goal
    final agentIdLower = agentId.toLowerCase();
    _output('[INIT] Starting: agent_init($agentIdLower, UserCh, NetCh)');
    _startAgentGoal(agentIdLower);

    _initialized = true;
    updateStats();

    final firstFriend = friends.isNotEmpty ? friends.first.toLowerCase() : 'friend';
    _output('[INIT] Ready! GLP term interface:');
    _output('  connect($firstFriend)         - cold-call $firstFriend');
    _output('  send($firstFriend, hello)     - send text message');
    _output('  X35 = accept(Ch)              - bind writer X35 to accept');
    _output('  X35 = no                      - bind writer X35 to reject');
    _output('  introduce(alice, charlie)     - introduce two friends');
  }

  void _startAgentGoal(String agentId) {
    if (_runtime == null) return;

    // Combine loaded programs
    final allOps = <dynamic>[];
    for (final loaded in _programs.values) {
      allOps.addAll(loaded.ops);
    }
    final combinedProgram = BytecodeProgram(allOps);

    final runner = BytecodeRunner(combinedProgram);
    _scheduler = Scheduler(rt: _runtime!, runners: {'main': runner});
    _scheduler!.resetDisplayNumbering();

    final heap = _runtime!.heap;

    // Create internal channel between social agent and UI agent
    final (agentInWriter, agentInReader) = heap.allocateVariable();
    final (agentOutWriter, agentOutReader) = heap.allocateVariable();
    final agentChTerm = rt.StructTerm('ch', [
      rt.VarRef(agentInReader),
      rt.VarRef(agentOutWriter),
    ]);
    final uiAgentChTerm = rt.StructTerm('ch', [
      rt.VarRef(agentOutReader),
      rt.VarRef(agentInWriter),
    ]);
    _log('GOAL: Created internal agent<->ui channel');

    // --- Goal 1: agent_init(Id, AgentCh, NetCh) ---
    final agentEntryPC = combinedProgram.labels['agent_init/3'];
    _log('GOAL: agent_init/3 entryPC=$agentEntryPC');
    if (agentEntryPC == null) {
      _output('[ERROR] Predicate agent_init/3 not found');
      return;
    }

    final (arg0Writer, arg0Reader) = heap.allocateVariable();
    heap.bindVariable(arg0Writer, rt.ConstTerm(agentId));
    final (arg1Writer, arg1Reader) = heap.allocateVariable();
    heap.bindVariable(arg1Writer, agentChTerm);
    final (arg2Writer, arg2Reader) = heap.allocateVariable();
    heap.bindVariable(arg2Writer, buildChannelTerm(_netChannel!));

    final agentEnv = CallEnv(args: {
      0: rt.VarRef(arg0Reader),
      1: rt.VarRef(arg1Reader),
      2: rt.VarRef(arg2Reader),
    });
    _runtime!.setGoalEnv(_goalId, agentEnv);
    _runtime!.setGoalProgram(_goalId, 'main');
    _runtime!.gq.enqueue(GoalRef(_goalId, agentEntryPC));
    _goalId++;
    _output('[GOAL] Started agent_init($agentId, AgentCh, NetCh)');

    // --- Goal 2: ui_agent(UICh, DartCh, [], 1) ---
    final uiEntryPC = combinedProgram.labels['ui_agent/4'];
    _log('GOAL: ui_agent/4 entryPC=$uiEntryPC');
    if (uiEntryPC == null) {
      _output('[WARN] Predicate ui_agent/4 not found - running without UI mediation');
    } else {
      final (uiArg0Writer, uiArg0Reader) = heap.allocateVariable();
      heap.bindVariable(uiArg0Writer, uiAgentChTerm);
      final (uiArg1Writer, uiArg1Reader) = heap.allocateVariable();
      heap.bindVariable(uiArg1Writer, buildChannelTerm(_userChannel!));
      final (uiArg2Writer, uiArg2Reader) = heap.allocateVariable();
      heap.bindVariable(uiArg2Writer, rt.ConstTerm('nil'));
      final (uiArg3Writer, uiArg3Reader) = heap.allocateVariable();
      heap.bindVariable(uiArg3Writer, rt.ConstTerm(1));

      final uiEnv = CallEnv(args: {
        0: rt.VarRef(uiArg0Reader),
        1: rt.VarRef(uiArg1Reader),
        2: rt.VarRef(uiArg2Reader),
        3: rt.VarRef(uiArg3Reader),
      });
      _runtime!.setGoalEnv(_goalId, uiEnv);
      _runtime!.setGoalProgram(_goalId, 'main');
      _runtime!.gq.enqueue(GoalRef(_goalId, uiEntryPC));
      _goalId++;
      _output('[GOAL] Started ui_agent(UICh, DartCh, [], 1)');
    }

    // Initial run
    _runUntilQuiescent();
  }

  // =========================================================================
  // USER INPUT
  // =========================================================================

  /// Inject user input text.
  void injectUserInput(String text) {
    _log('USER_INPUT: $text');
    if (text.isEmpty || _userInput == null || _runtime == null) {
      _log('USER_INPUT: early return (empty or not initialized)');
      return;
    }

    _output('> $text');

    try {
      // Try writer binding first: "X35 = term"
      if (_tryHandleWriterBinding(text)) return;

      // Parse as GLP term and inject
      final term = parseTerm(text);
      _log('USER_INPUT: parsed -> ${formatTerm(term)}');

      final activations = _userInput!.inject(term);
      _log('USER_INPUT: ${activations.length} activations');
      for (final goal in activations) {
        _runtime!.gq.enqueue(goal);
      }

      _runUntilQuiescent();
    } catch (e, st) {
      _log('USER_INPUT ERROR: $e\n$st');
      _output('[ERROR] $e');
    }
  }

  bool _tryHandleWriterBinding(String text) {
    final bindingMatch = RegExp(r'^X(\d+)\s*=\s*(.+)$', caseSensitive: false).firstMatch(text);
    if (bindingMatch == null) return false;

    final addrStr = bindingMatch.group(1)!;
    final termStr = bindingMatch.group(2)!;
    final addr = int.tryParse(addrStr);
    if (addr == null) return false;

    if (!_knownWriters.containsKey(addr)) {
      _output('[ERROR] X$addr is not a known writer');
      return true;
    }

    if (_runtime!.heap.isReader(addr)) {
      _output('[ERROR] X$addr is a reader, not a writer');
      return true;
    }

    try {
      final valueTerm = parseTerm(termStr);
      _log('BIND: X$addr = ${formatTerm(valueTerm)}');
      _runtime!.heap.bindVariable(addr, valueTerm);
      _output('[BOUND] X$addr = ${formatTerm(valueTerm)}');
      _runUntilQuiescent();
      return true;
    } catch (e) {
      _output('[ERROR] Failed to parse term: $e');
      return true;
    }
  }

  // =========================================================================
  // NETWORK MESSAGES
  // =========================================================================

  void _handleNetOutput(rt.Term term) {
    if (_runtime == null) return;

    final derefed = derefTerm(term);
    final formatted = formatTerm(derefed);
    _log('NET_OUT: $formatted');
    _output('[NET OUT] $formatted');

    if (derefed is rt.StructTerm && derefed.functor == 'msg') {
      String? destination;

      if (derefed.args.length == 2) {
        final target = derefTerm(derefed.args[0]);
        if (target is rt.ConstTerm) destination = target.value?.toString();
        _log('NET_OUT: 2-arg msg, target=$destination');
      } else if (derefed.args.length == 3) {
        final to = derefTerm(derefed.args[1]);
        if (to is rt.ConstTerm) destination = to.value?.toString();
        _log('NET_OUT: 3-arg msg, to=$destination');
      }

      if (destination != null && destination != 'user' && destination != 'net') {
        _log('NET_OUT: Sending to $destination');
        _sendAgentMessage(destination, derefed);
      } else {
        _log('NET_OUT: Not routing (dest=$destination)');
      }
    } else {
      _log('NET_OUT: Not a msg struct');
    }
  }

  /// Handle incoming madGLP binary message.
  void onMadMessageReceived(String from, Uint8List payload) {
    _log('MAD_RECV from $from (${payload.length} bytes)');
    _output('[MAD RECV from $from] ${payload.length} bytes');

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
        _output('[MAD ASSIGN] $globalName := ${formatTerm(value)}');
        _ctx!.handleMadAssignment(
          globalName: globalName,
          value: value,
          fromAgent: from.toLowerCase(),
        );
      } catch (e) {
        _log('MAD_ERROR: $e');
        _output('[MAD ERROR] $e');
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
      _output('[AGENT MSG] $formatted');

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
    _runUntilQuiescent();
  }

  /// Handle legacy JSON message (backwards compatibility).
  void onLegacyMessageReceived(String from, dynamic payload) {
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
    _runUntilQuiescent();
  }

  Future<void> _sendMadPayload(String to, Uint8List payload) async {
    _log('SEND_MAD to $to (${payload.length} bytes)');
    _output('[MAD SEND to $to] ${payload.length} bytes');
    await onSendMadMessage?.call(to, payload);
  }

  Future<void> _sendAgentMessage(String to, rt.Term msgTerm) async {
    _log('SEND_AGENT_MSG to $to: ${formatTerm(msgTerm)}');
    if (_runtime == null || _ctx == null) {
      _log('SEND_AGENT_MSG: ERROR - runtime/ctx is null');
      return;
    }

    try {
      _ctx!.exportTerm(msgTerm);

      final serializer = PayloadSerializer(agentId.toLowerCase());
      final termPayload = serializer.createAgentMessagePayload(
        msgTerm,
        (addr) => _runtime!.heap.isReader(addr),
      );
      _log('SEND_AGENT_MSG: serialized ${termPayload.length} bytes');

      final msg = OutboundMessage(
        destination: to,
        type: MessageType.agentMessage,
        payload: termPayload,
      );
      final payload = serializer.serializeMessage(msg);
      _log('SEND_AGENT_MSG: wrapped ${payload.length} bytes');

      await _sendMadPayload(to, payload);
    } catch (e, st) {
      _log('SEND_AGENT_MSG ERROR: $e\n$st');
      _output('[ERROR] Failed to send agent message: $e');
    }
  }

  // =========================================================================
  // EXECUTION
  // =========================================================================

  /// Run the scheduler until quiescent.
  /// Returns the execution status name, or null if not initialized.
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
      final result = await _scheduler!.drainAsyncWithStatus(
        maxCycles: 1000,
        debug: glpTraceEnabled,
      );
      _log('RUN: status=${result.status}, goals=${result.goalsRan.length}');
      goalCount += result.goalsRan.length;

      if (result.status == ExecutionStatus.suspended && result.blockingReaders.isNotEmpty) {
        _log('RUN: suspended, ${result.blockingReaders.length} blocking readers');
        _ctx!.processSuspension(result.blockingReaders);
        _output('[MAD] Waiting for ${result.blockingReaders.length} blocking readers');
      }

      final messagesFlushed = _ctx!.flushMessages();
      if (messagesFlushed > 0) {
        _log('RUN: flushed $messagesFlushed messages');
        _output('[MAD] Flushed $messagesFlushed messages');
      }

      _displayPendingOutput();
      updateStats();
      _log('RUN: done (status=${result.status.name})');
      return result.status.name;
    } catch (e, st) {
      _log('RUN ERROR: $e\n$st');
      return 'error';
    }
  }

  // =========================================================================
  // OUTPUT DISPLAY
  // =========================================================================

  /// Returns pending output lines and clears the buffer.
  List<String> flushPendingOutputLines() {
    final lines = <String>[];
    for (final term in _pendingUserOutputTerms) {
      final derefed = derefTerm(term);
      _collectWriters(derefed);
      lines.add('< ${formatTerm(derefed)}');
    }
    _pendingUserOutputTerms.clear();
    return lines;
  }

  void _displayPendingOutput() {
    for (final line in flushPendingOutputLines()) {
      _output(line);
    }
  }

  void _collectWriters(rt.Term term) {
    if (term is rt.VarRef) {
      final value = _runtime?.heap.getValue(term.addr);
      if (value == null || value is rt.VarRef) {
        final isReader = _runtime?.heap.isReader(term.addr) ?? false;
        if (!isReader) {
          _knownWriters[term.addr] = 'X${term.addr}';
          _log('WRITER: registered X${term.addr}');
        }
      }
    } else if (term is rt.StructTerm) {
      for (final arg in term.args) {
        _collectWriters(arg);
      }
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
    _userOutput?.dispose();
    _netOutput?.dispose();
  }
}
