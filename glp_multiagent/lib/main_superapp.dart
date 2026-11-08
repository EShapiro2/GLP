/// The Grassroots App on a phone: the person's own agent over the real
/// network, Bluetooth and the local network (GSA paper, Section 2 and
/// Appendix "The Operating System").
///
/// The app makes or reads the agent's key pair and instantiates the
/// networking layer with it, PAN and LAN (phone_network.dart); the agent runs
/// `programs/social/graph/core` from every agent's initial goal, and at its
/// first start the app sends on the person channel the act that starts the
/// Grassroots App, `superapp(Me, [])` (superapp_host.dart; GSA paper,
/// Appendix, "Boot"; IGLP, Implementation Notes, "Boot").  The person sees
/// home, the Grassroots App's own person interface, as the bridge draws the
/// constructs of its questions, and the thread of what the agent tells them:
/// opening the place (GPE:1), the persons nearby they come to know by an
/// encounter (GPE:2), and befriending (GSG:1).
///
/// The demonstration with scripted peers in one heap, `superapp_ui/4`, is
/// `main_superapp_ui.dart`.
library;

import 'package:flutter/material.dart';

import 'glp_sources.dart';
import 'manifests/superapp_ui.dart';
import 'phone_network.dart';
import 'superapp_host.dart';
import 'ui_runtime/agent_surface.dart';

/// The persona's name in the seed of the agent's key pair, made once at the
/// first start (IGLP, Implementation Notes, "Boot").
const String _persona = 'person';

void main() {
  WidgetsFlutterBinding.ensureInitialized();
  runApp(const GrassrootsApp());
}

class GrassrootsApp extends StatelessWidget {
  const GrassrootsApp({super.key});

  @override
  Widget build(BuildContext context) => MaterialApp(
        title: 'Grassroots',
        debugShowCheckedModeBanner: false,
        theme: ThemeData(
          colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
          useMaterial3: true,
        ),
        home: const GrassrootsScreen(),
      );
}

class GrassrootsScreen extends StatefulWidget {
  const GrassrootsScreen({super.key});
  @override
  State<GrassrootsScreen> createState() => _GrassrootsScreenState();
}

class _GrassrootsScreenState extends State<GrassrootsScreen> {
  PhoneNetwork? _net;
  SuperAppHost? _host;
  String _status = 'Starting…';

  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addPostFrameCallback((_) => _start());
  }

  @override
  void dispose() {
    _host?.dispose();
    _net?.dispose();
    super.dispose();
  }

  Future<void> _start() async {
    try {
      final glp = await resolveGlpPaths();
      final net = await PhoneNetwork.open(persona: _persona);
      _net = net;
      final host = SuperAppHost(
        agentId: _persona,
        program: glp.coreDir,
        rootSelfGlpPath: glp.rootSelfGlp,
        network: net.network,
        keyPair: net.keyPair,
        manifest: superappManifest,
      );
      host.ui.onChange = () {
        if (mounted) setState(() {});
      };
      host.onOutput = (line) {
        if (line.startsWith('[ERROR]') && mounted) {
          setState(() => _status = line);
        }
      };
      setState(() {
        _host = host;
        _status = net.transports
            ? 'Ready'
            : 'Ready, with neither Bluetooth nor the local network';
      });
      await host.start();
      if (mounted) setState(() {});
    } catch (e) {
      if (mounted) setState(() => _status = 'Error: $e');
    }
  }

  @override
  Widget build(BuildContext context) {
    final host = _host;
    // The person sees what the program draws (GSA paper, Appendix "The
    // Operating System", "The person"); the app shows its own line only
    // while the agent is not running, or where the layer or the agent fails.
    return Scaffold(
      body: SafeArea(
        child: host == null
            ? Center(
                child: Text(_status,
                    style: const TextStyle(color: Colors.grey)))
            : Column(
                children: [
                  if (_status != 'Ready')
                    Padding(
                      padding: const EdgeInsets.symmetric(
                          horizontal: 12, vertical: 4),
                      child: Text(_status,
                          style: const TextStyle(
                              fontSize: 11, color: Colors.grey)),
                    ),
                  Expanded(
                    child: AgentSurface(agentId: _persona, runtime: host.ui),
                  ),
                ],
              ),
      ),
    );
  }
}
