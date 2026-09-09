/// A compiled vGLP program on a live person's screen.
///
/// One app shell for any of them: it runs the program's live-person harness in
/// an agent isolate, feeds what the person channel carries — the mediator's
/// cards and their closing, and the agent's screen messages — into a
/// [UiRuntime], and renders it through [AgentSurface] from the program's
/// manifest. Nothing here is particular to a program: the manifest is the
/// image of its display declarations, and the vocabulary between them is the
/// canonical compilation's, the same for every compiled vGLP program.
///
/// The two entries beside this file, `main_coins.dart` and
/// `main_superapp.dart`, are the currency mini-app and the Grassroots
/// Super-App; a third program is a third manifest and a third entry, and no
/// change here.
library;

import 'dart:async';
import 'dart:isolate';

import 'package:flutter/material.dart';

import 'glp_sources.dart';
import 'isolate_protocol.dart';
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/manifest.dart';
import 'ui_runtime/runtime.dart';

/// What it takes to run one compiled vGLP program for a live person.
class VglpProgram {
  /// The window title.
  final String title;

  /// The person operating this phone. The harness gives her the live channel
  /// and scripts her counterparties.
  final String person;

  /// The program directory, resolved against the tree the engine reads.
  final String Function(GlpPaths) directory;

  /// The harness entry, whose three arguments are (Id, UserIn, NetIn).
  final String goalLabel;

  /// The image of the program's display declarations.
  final Manifest manifest;

  /// The counterparties the harness scripts, for the runtime's own logging.
  final List<String> friends;

  const VglpProgram({
    required this.title,
    required this.person,
    required this.directory,
    required this.goalLabel,
    required this.manifest,
    this.friends = const [],
  });
}

/// Run [program] as an app.
void runVglpApp(VglpProgram program) => runApp(VglpApp(program));

class VglpApp extends StatelessWidget {
  final VglpProgram program;
  const VglpApp(this.program, {super.key});

  @override
  Widget build(BuildContext context) => MaterialApp(
        title: program.title,
        debugShowCheckedModeBanner: false,
        theme: ThemeData(
          colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
          useMaterial3: true,
        ),
        home: VglpScreen(program),
      );
}

class VglpScreen extends StatefulWidget {
  final VglpProgram program;
  const VglpScreen(this.program, {super.key});
  @override
  State<VglpScreen> createState() => _VglpScreenState();
}

class _VglpScreenState extends State<VglpScreen> {
  final ReceivePort _replyPort = ReceivePort();
  StreamSubscription<dynamic>? _replies;
  Isolate? _isolate;
  SendPort? _commands;
  UiRuntime? _ui;
  String _status = 'Starting…';

  VglpProgram get _p => widget.program;

  @override
  void initState() {
    super.initState();
    _replies = _replyPort.listen(_onAgentMessage);
    WidgetsBinding.instance.addPostFrameCallback((_) => _spawn());
  }

  @override
  void dispose() {
    _replies?.cancel();
    _replyPort.close();
    _isolate?.kill(priority: Isolate.immediate);
    super.dispose();
  }

  Future<void> _spawn() async {
    final glp = await resolveGlpPaths();
    _ui = UiRuntime(
      manifest: _p.manifest,
      onSend: (text) => _commands?.send(UserInput(text)),
    )..onChange = () {
        if (mounted) setState(() {});
      };
    setState(() {});
    try {
      _isolate = await Isolate.spawn(
        agentIsolateEntry,
        InitAgent(
          agentId: _p.person,
          glpSources: const [],
          programDir: _p.directory(glp),
          goalLabel: _p.goalLabel,
          rootSelfGlpPath: glp.rootSelfGlp,
          friends: _p.friends,
          replyPort: _replyPort.sendPort,
          deferStart: false,
        ),
      );
    } catch (e) {
      setState(() => _status = 'Error: $e');
    }
  }

  void _onAgentMessage(dynamic msg) {
    if (msg is AgentReady) {
      _commands = msg.commandPort;
      setState(() => _status = 'Ready');
    } else if (msg is AgentOutput) {
      // The isolate marks a genuine `_output/1` term with a leading '< ';
      // its own chatter ([INIT]…, > …) carries no such marker.
      final line =
          msg.line.startsWith('< ') ? msg.line.substring(2) : msg.line;
      _ui?.handleLine(line);
      setState(() {});
    } else if (msg is AgentError) {
      setState(() => _status = 'Error: ${msg.error}');
    }
  }

  @override
  Widget build(BuildContext context) {
    final ui = _ui;
    final body = ui == null
        ? Center(
            child: Text(_status, style: const TextStyle(color: Colors.grey)))
        : AgentSurface(agentId: _p.person, runtime: ui);
    return Scaffold(
      backgroundColor: const Color(0xFF2B2B33),
      body: Center(child: PhoneFrame(child: body)),
    );
  }
}

/// A phone-shaped bezel around the surface, as the single-phone app draws it.
class PhoneFrame extends StatelessWidget {
  final Widget child;
  const PhoneFrame({super.key, required this.child});

  @override
  Widget build(BuildContext context) => Container(
        width: 400,
        height: 860,
        padding: const EdgeInsets.all(12),
        decoration: BoxDecoration(
          color: Colors.black,
          borderRadius: BorderRadius.circular(44),
          boxShadow: const [
            BoxShadow(color: Colors.black54, blurRadius: 40, spreadRadius: 4),
          ],
        ),
        child: ClipRRect(
          borderRadius: BorderRadius.circular(32),
          child: Container(color: Colors.white, child: child),
        ),
      );
}
