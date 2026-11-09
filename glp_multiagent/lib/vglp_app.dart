/// A compiled vGLP program on a live person's screen.
///
/// One app shell for any of them: it runs the program's live-person harness in
/// an agent isolate, feeds what the person channel carries — a compiled
/// module's draws, which the runtime's bridge draws as constructs; the
/// deployed mediator's cards and their closing; and the agent's screen
/// messages — into a [UiRuntime], and renders it through [AgentSurface] from
/// the program's manifest.  The person's grants on the constructs go back as
/// every act of the person does. Nothing here is particular to a program: the manifest is the
/// image of its display declarations, and the vocabulary between them is the
/// canonical compilation's, the same for every compiled vGLP program.
///
/// The entry beside this file, `main_superapp_ui.dart`, is the Grassroots
/// App's demonstration, its peers scripted; another program is another manifest and another entry, and no
/// change here.  The coins and sovereign mini-apps have no entry of their
/// own: each runs as an agent, started by its agent/2 (IGLP, Implementation
/// Notes, "Boot"), and under the super-app.
library;

import 'dart:async';
import 'dart:isolate';

import 'package:flutter/material.dart';

import 'glp_sources.dart';
import 'isolate_protocol.dart';
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/look.dart';
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

  /// The image of the program's display declarations.
  final Manifest manifest;

  /// The entry the application starts: at the agent's first start the
  /// isolate sends on the person channel `<entry>(Me, [])`, Me the agent's
  /// key ([InitAgent.entry]; IGLP, Implementation Notes, "Boot").  Null where
  /// the program takes no first act.
  final String? entry;

  /// The contract the program is compiled from, whose look it takes: its
  /// icon and its family's colours (`programs/apps/look.json`, look.dart).
  final String contract;

  const VglpProgram({
    required this.title,
    required this.contract,
    required this.person,
    required this.directory,
    required this.manifest,
    this.entry,
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
        theme: ThemeData(useMaterial3: true),
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

  /// The look of the program's contract, once read; null until then.
  ContractLook? _look;

  Future<void> _spawn() async {
    final glp = await resolveGlpPaths();
    try {
      _look = (await Looks.read(glp.rootSelfGlp)).of(_p.contract);
    } catch (e) {
      _status = 'Error: no look: $e';
    }
    _ui = UiRuntime(
      manifest: _p.manifest,
      onSend: (cmd) => _commands?.send(UserInput(cmd)),
    )..onChange = () {
        if (mounted) setState(() {});
      };
    setState(() {});
    try {
      _isolate = await Isolate.spawn(
        agentIsolateEntry,
        InitAgent(
          agentId: _p.person,
          program: _p.directory(glp),
          rootSelfGlpPath: glp.rootSelfGlp,
          replyPort: _replyPort.sendPort,
          deferStart: false,
          entry: _p.entry,
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
    } else if (msg is AgentNotify) {
      // What the agent's program emits to the person, the term whole; the
      // agent's transcript (AgentOutput) is not the screen's.
      _ui?.handle(msg.term);
      setState(() {});
    } else if (msg is AgentError) {
      setState(() => _status = 'Error: ${msg.error}');
    }
  }

  /// [w] in the theme of the program's contract, once its look is read.
  Widget _looked(Widget w) {
    final look = _look;
    return look == null ? w : Theme(data: look.theme(), child: w);
  }

  @override
  Widget build(BuildContext context) {
    final ui = _ui;
    final body = ui == null
        ? Center(
            child: Text(_status, style: const TextStyle(color: Colors.grey)))
        : _looked(AgentSurface(
            agentId: _p.person, runtime: ui, icon: _look?.icon));
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
