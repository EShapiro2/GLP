import 'dart:io';
import 'package:flutter/material.dart';
import 'package:desktop_multi_window/desktop_multi_window.dart';
import 'package:window_manager/window_manager.dart';

/// GLP Multi-Window Multiagent Simulation
///
/// Phase 1: Basic Flutter setup with multi-window support
void main(List<String> args) async {
  WidgetsFlutterBinding.ensureInitialized();

  // Check if this is a sub-window
  if (args.firstOrNull == 'multi_window') {
    // This is a sub-window spawned by desktop_multi_window
    final windowId = int.parse(args[1]);
    final argument = args.length > 2 ? args[2] : '';

    runApp(AgentWindowApp(
      windowId: windowId,
      argument: argument,
    ));
  } else {
    // This is the main coordinator window
    await windowManager.ensureInitialized();

    WindowOptions windowOptions = const WindowOptions(
      size: Size(800, 600),
      center: true,
      title: 'GLP Multiagent Coordinator',
    );

    windowManager.waitUntilReadyToShow(windowOptions, () async {
      await windowManager.show();
      await windowManager.focus();
    });

    runApp(const CoordinatorApp());
  }
}

/// Main coordinator window application
class CoordinatorApp extends StatelessWidget {
  const CoordinatorApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'GLP Multiagent Coordinator',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.blue),
        useMaterial3: true,
      ),
      home: const CoordinatorPage(),
    );
  }
}

/// Coordinator page - will manage agent windows
class CoordinatorPage extends StatefulWidget {
  const CoordinatorPage({super.key});

  @override
  State<CoordinatorPage> createState() => _CoordinatorPageState();
}

class _CoordinatorPageState extends State<CoordinatorPage> {
  final List<int> _agentWindows = [];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('GLP Multiagent Coordinator'),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            const Text(
              'Phase 1: Flutter Project Setup Complete',
              style: TextStyle(fontSize: 24),
            ),
            const SizedBox(height: 20),
            Text(
              'Active agent windows: ${_agentWindows.length}',
              style: const TextStyle(fontSize: 18),
            ),
            const SizedBox(height: 40),
            ElevatedButton.icon(
              onPressed: _spawnAgentWindow,
              icon: const Icon(Icons.add),
              label: const Text('Spawn Agent Window'),
            ),
            const SizedBox(height: 20),
            if (_agentWindows.isNotEmpty)
              ElevatedButton.icon(
                onPressed: _closeAllWindows,
                icon: const Icon(Icons.close),
                label: const Text('Close All Agent Windows'),
                style: ElevatedButton.styleFrom(
                  backgroundColor: Colors.red[100],
                ),
              ),
          ],
        ),
      ),
    );
  }

  Future<void> _spawnAgentWindow() async {
    final agentId = 'agent_${_agentWindows.length + 1}';

    final window = await DesktopMultiWindow.createWindow(agentId);
    window
      ..setFrame(const Offset(100, 100) & const Size(400, 300))
      ..setTitle('Agent: $agentId')
      ..show();

    setState(() {
      _agentWindows.add(window.windowId);
    });
  }

  Future<void> _closeAllWindows() async {
    for (final windowId in _agentWindows) {
      await WindowController.fromWindowId(windowId).close();
    }
    setState(() {
      _agentWindows.clear();
    });
  }
}

/// Agent window application (spawned as sub-window)
class AgentWindowApp extends StatelessWidget {
  final int windowId;
  final String argument;

  const AgentWindowApp({
    super.key,
    required this.windowId,
    required this.argument,
  });

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Agent Window',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
        useMaterial3: true,
      ),
      home: AgentPage(
        windowId: windowId,
        agentId: argument,
      ),
    );
  }
}

/// Agent page - will host agent I/O in future phases
class AgentPage extends StatelessWidget {
  final int windowId;
  final String agentId;

  const AgentPage({
    super.key,
    required this.windowId,
    required this.agentId,
  });

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Agent: $agentId'),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Text(
              'Agent ID: $agentId',
              style: const TextStyle(fontSize: 20),
            ),
            const SizedBox(height: 10),
            Text(
              'Window ID: $windowId',
              style: const TextStyle(fontSize: 16, color: Colors.grey),
            ),
            const SizedBox(height: 40),
            const Text(
              'Phase 2+ will add:\n'
              '• GLP Runtime\n'
              '• External I/O channels\n'
              '• Message display\n'
              '• Input controls',
              textAlign: TextAlign.center,
              style: TextStyle(color: Colors.grey),
            ),
          ],
        ),
      ),
    );
  }
}
