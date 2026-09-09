/// The currency mini-app on a live person's screen — one phone, one person,
/// the interface derived from the program.
///
/// The app runs `programs/coins` in an agent isolate at `coins_ui/3`: alice's
/// execution of Currencies' mini-app with its mediator, and bob's, scripted,
/// over one conversation. What alice's mediator shows her — the cards of her
/// four request clauses and of an arriving swap, their closing, and her
/// agent's screen — arrives here as ground terms, and her taps go back as
/// `answer(req(N), xs_C(...))`.
///
/// Nothing on this screen is written here. The surface renders from
/// [coinsManifest], which is the image of the display declarations of
/// `programs/coins/currency/coins_agent.vglp` (vGLP, Definition "Display
/// Declaration"), and the runtime speaks the person-channel vocabulary of the
/// canonical compilation, which is the same for every compiled vGLP program.
library;

import 'dart:async';
import 'dart:isolate';

import 'package:flutter/material.dart';

import 'glp_sources.dart';
import 'isolate_protocol.dart';
import 'manifests/coins_ui.dart';
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/runtime.dart';

/// The person operating this phone. `coins_ui/3` gives her the live channel
/// and scripts her counterparty.
const String _person = 'alice';

void main() => runApp(const CoinsApp());

class CoinsApp extends StatelessWidget {
  const CoinsApp({super.key});
  @override
  Widget build(BuildContext context) => MaterialApp(
        title: 'Coins',
        debugShowCheckedModeBanner: false,
        theme: ThemeData(
          colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
          useMaterial3: true,
        ),
        home: const CoinsScreen(),
      );
}

class CoinsScreen extends StatefulWidget {
  const CoinsScreen({super.key});
  @override
  State<CoinsScreen> createState() => _CoinsScreenState();
}

class _CoinsScreenState extends State<CoinsScreen> {
  final ReceivePort _replyPort = ReceivePort();
  StreamSubscription<dynamic>? _replies;
  Isolate? _isolate;
  SendPort? _commands;
  UiRuntime? _ui;
  String _status = 'Starting…';

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
      manifest: coinsManifest,
      onSend: (text) => _commands?.send(UserInput(text)),
    )..onChange = () {
        if (mounted) setState(() {});
      };
    setState(() {});
    try {
      _isolate = await Isolate.spawn(
        agentIsolateEntry,
        InitAgent(
          agentId: _person,
          glpSources: const [],
          programDir: glp.coinsDir,
          goalLabel: 'coins_ui/3',
          rootSelfGlpPath: glp.rootSelfGlp,
          friends: const ['bob'],
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
        : AgentSurface(agentId: _person, runtime: ui);
    return Scaffold(
      backgroundColor: const Color(0xFF2B2B33),
      body: Center(child: _PhoneFrame(child: body)),
    );
  }
}

/// A phone-shaped bezel around the surface, as the single-phone app draws it.
class _PhoneFrame extends StatelessWidget {
  final Widget child;
  const _PhoneFrame({required this.child});

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
