/// GLP Grassroots Social Network — Simulated GSN Plays
///
/// Runs GSN plays (fplay4-14) via REPL subprocess, with tagged output
/// parsed and routed to per-agent read-only panels.
/// Plays 4-7: 4 agents (Alice, Carol, Bob, Dave) — parent-child protocol
/// Plays 8-11: 2 agents (Alice, Bob) — unfriend protocol
/// Plays 12-14: 3 agents (Alice, Bob, Charlie) — friendship update protocol
library;

import 'dart:io';

import 'package:flutter/material.dart';
import 'package:glp_runtime/multiagent/repl_play_runner.dart';

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const GsnApp());
}

// =============================================================================
// GSN APP
// =============================================================================

class GsnApp extends StatelessWidget {
  const GsnApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Grassroots Social Network',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.blue,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.blue,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Colors.blue,
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: Colors.blue,
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const GsnScreen(),
    );
  }
}

// =============================================================================
// PER-AGENT UI STATE
// =============================================================================

class _AgentInfo {
  final String id;
  final String role;    // "Parent", "Child", or "Agent"
  final Color headerColor;
  final Color bgColor;

  const _AgentInfo(this.id, this.role, this.headerColor, this.bgColor);
}

/// Panel order for 4-agent plays (4-7): Parent, Child, Parent, Child.
/// Alice/Carol = indigo family, Bob/Dave = teal family.
const _agentInfos4 = [
  _AgentInfo('Alice', 'Parent', Color(0xFF3949AB), Color(0xFFE8EAF6)),  // indigo 600, indigo 50
  _AgentInfo('Carol', 'Child',  Color(0xFF7986CB), Color(0xFFF5F5FF)),  // indigo 300, very light
  _AgentInfo('Bob',   'Parent', Color(0xFF00897B), Color(0xFFE0F2F1)),  // teal 600, teal 50
  _AgentInfo('Dave',  'Child',  Color(0xFF4DB6AC), Color(0xFFF5FFFE)),  // teal 300, very light
];

/// Panel order for 2-agent plays (8-11): Alice, Bob.
const _agentInfos2 = [
  _AgentInfo('Alice', 'Agent', Color(0xFF3949AB), Color(0xFFE8EAF6)),  // indigo 600, indigo 50
  _AgentInfo('Bob',   'Agent', Color(0xFF00897B), Color(0xFFE0F2F1)),  // teal 600, teal 50
];

/// Panel order for 3-agent plays (12-14): Alice, Bob, Charlie.
const _agentInfos3 = [
  _AgentInfo('Alice',   'Agent', Color(0xFF3949AB), Color(0xFFE8EAF6)),  // indigo 600, indigo 50
  _AgentInfo('Bob',     'Agent', Color(0xFF00897B), Color(0xFFE0F2F1)),  // teal 600, teal 50
  _AgentInfo('Charlie', 'Agent', Color(0xFFEF6C00), Color(0xFFFFF3E0)),  // orange 800, orange 50
];

/// Panel order for 4-agent peer plays (15+): Alice, Bob, Charlie, Dave.
const _agentInfos4peer = [
  _AgentInfo('Alice',   'Agent', Color(0xFF3949AB), Color(0xFFE8EAF6)),  // indigo 600, indigo 50
  _AgentInfo('Bob',     'Agent', Color(0xFF00897B), Color(0xFFE0F2F1)),  // teal 600, teal 50
  _AgentInfo('Charlie', 'Agent', Color(0xFFEF6C00), Color(0xFFFFF3E0)),  // orange 800, orange 50
  _AgentInfo('Dave',    'Agent', Color(0xFF7B1FA2), Color(0xFFF3E5F5)),  // purple 800, purple 50
];

class _AgentState {
  final _AgentInfo info;
  final List<String> outputLog = [];
  final ScrollController scrollController = ScrollController();

  _AgentState(this.info);

  String get agentId => info.id;

  void dispose() {
    scrollController.dispose();
  }
}

// =============================================================================
// GSN SOURCE FILES (relative to glp_runtime/)
// =============================================================================

const _gsnFiles = [
  '../programs/typed_book/gsn/typed_social_agent.glp',
  '../programs/typed_book/gsn/typed_ui_mediator.glp',
  '../programs/typed_book/gsn/typed_ui_actors.glp',
  '../programs/typed_book/gsn/play_ui_sim_boot.glp',
];

// =============================================================================
// GSN SCREEN
// =============================================================================

class GsnScreen extends StatefulWidget {
  const GsnScreen({super.key});

  @override
  State<GsnScreen> createState() => _GsnScreenState();
}

class _GsnScreenState extends State<GsnScreen> {
  final Map<String, _AgentState> _agents = {};
  final List<String> _log = [];
  ReplPlayRunner? _playRunner;

  @override
  void dispose() {
    _playRunner?.kill();
    for (final agent in _agents.values) {
      agent.dispose();
    }
    super.dispose();
  }

  /// Resolve the GLP repo root.
  static String _resolveRepoRoot() {
    final devRoot = Directory.current.parent.path;
    if (Directory('$devRoot/glp_runtime').existsSync()) {
      return devRoot;
    }
    const fallback1 = '/Users/ohadey/Desktop/Grassroots/GLP2/GLP';
    if (Directory('$fallback1/glp_runtime').existsSync()) {
      return fallback1;
    }
    const fallback2 = '/Users/udi/Grassroots/GLP';
    if (Directory('$fallback2/glp_runtime').existsSync()) {
      return fallback2;
    }
    return devRoot;
  }

  Future<void> _runPlay(int playNumber) async {
    // Kill previous run
    _playRunner?.kill();
    _playRunner = null;

    // Clear previous panels
    for (final agent in _agents.values) {
      agent.dispose();
    }
    _agents.clear();

    // Create agent panels based on play type
    final List<_AgentInfo> agentInfos;
    if (playNumber == 15) {
      agentInfos = _agentInfos4peer;
    } else if (playNumber <= 3 || (playNumber >= 12 && playNumber <= 14)) {
      agentInfos = _agentInfos3;
    } else if (playNumber >= 8) {
      agentInfos = _agentInfos2;
    } else {
      agentInfos = _agentInfos4;
    }
    for (final info in agentInfos) {
      _agents[info.id] = _AgentState(info);
    }

    final repoRoot = _resolveRepoRoot();
    setState(() {
      _log.add('Starting fplay$playNumber (repo: $repoRoot)...');
    });

    final runner = ReplPlayRunner(repoRoot: repoRoot, sourceFiles: _gsnFiles);
    _playRunner = runner;

    runner.onOutput = (output) {
      // Capitalize to match _AgentState keys (Alice, Bob, Carol, Dave, Charlie)
      final key = output.agentId[0].toUpperCase() + output.agentId.substring(1);
      final state = _agents[key];
      if (state == null) return;

      final displayLine = output.kind == 'cmd'
          ? '> ${output.content}'
          : '< ${output.content}';
      state.outputLog.add(displayLine);
      setState(() {});
      _scrollToBottom(state);
    };

    runner.onLog = (line) {
      // Silently log REPL output
      debugPrint('REPL: $line');
    };

    runner.onError = (error) {
      setState(() {
        _log.add('REPL ERROR: $error');
      });
    };

    runner.onDone = (exitCode) {
      _playRunner = null;
      setState(() {
        _log.add('fplay$playNumber finished (exit $exitCode)');
      });
    };

    await runner.run(playNumber);
  }

  void _scrollToBottom(_AgentState agent) {
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (agent.scrollController.hasClients) {
        agent.scrollController.animateTo(
          agent.scrollController.position.maxScrollExtent,
          duration: const Duration(milliseconds: 200),
          curve: Curves.easeOut,
        );
      }
    });
  }

  // ===========================================================================
  // BUILD
  // ===========================================================================

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Grassroots Social Network'),
      ),
      body: Column(
        children: [
          _buildControlBar(),
          // Agent panels
          Expanded(
            child: _agents.isEmpty
                ? const Center(
                    child: Text('Click a Play button above to run a scenario.'))
                : Row(
                    crossAxisAlignment: CrossAxisAlignment.stretch,
                    children: _agents.values
                        .map((agent) => Expanded(child: _buildAgentPanel(agent)))
                        .toList(),
                  ),
          ),
          // Log
          _buildLog(),
        ],
      ),
    );
  }

  Widget _buildControlBar() {
    return Container(
      padding: const EdgeInsets.all(16.0),
      color: Colors.blue.shade50,
      child: SingleChildScrollView(
        scrollDirection: Axis.horizontal,
        child: Row(
          children: [
            ElevatedButton.icon(
              onPressed: () => _runPlay(1),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 1'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(2),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 2'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(3),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 3'),
            ),
            const SizedBox(width: 16),
            Container(width: 1, height: 30, color: Colors.grey),
            const SizedBox(width: 16),
            ElevatedButton.icon(
              onPressed: () => _runPlay(4),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 4'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(5),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 5'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(6),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 6'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(7),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 7'),
            ),
            const SizedBox(width: 16),
            Container(width: 1, height: 30, color: Colors.grey),
            const SizedBox(width: 16),
            ElevatedButton.icon(
              onPressed: () => _runPlay(8),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 8'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(9),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 9'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(10),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 10'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(11),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 11'),
            ),
            const SizedBox(width: 16),
            Container(width: 1, height: 30, color: Colors.grey),
            const SizedBox(width: 16),
            ElevatedButton.icon(
              onPressed: () => _runPlay(12),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 12'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(13),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 13'),
            ),
            const SizedBox(width: 8),
            ElevatedButton.icon(
              onPressed: () => _runPlay(14),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 14'),
            ),
            const SizedBox(width: 16),
            Container(width: 1, height: 30, color: Colors.grey),
            const SizedBox(width: 16),
            ElevatedButton.icon(
              onPressed: () => _runPlay(15),
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play 15'),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildAgentPanel(_AgentState agent) {
    final info = agent.info;
    return Container(
      decoration: BoxDecoration(
        border: Border(
          right: BorderSide(color: Colors.grey.shade300),
        ),
      ),
      child: Column(
        children: [
          // Agent header: "Agent: Alice"
          Container(
            padding:
                const EdgeInsets.symmetric(horizontal: 8.0, vertical: 6.0),
            color: info.headerColor,
            child: Row(
              children: [
                Text(
                  '${info.role}: ${info.id}',
                  style: const TextStyle(
                    color: Colors.white,
                    fontWeight: FontWeight.bold,
                    fontSize: 13,
                  ),
                ),
              ],
            ),
          ),
          // Output log
          Expanded(
            child: Container(
              color: info.bgColor,
              child: ListView.builder(
                controller: agent.scrollController,
                padding: const EdgeInsets.all(8.0),
                itemCount: agent.outputLog.length,
                itemBuilder: (context, index) {
                  final line = agent.outputLog[index];
                  return Padding(
                    padding: const EdgeInsets.symmetric(vertical: 2.0),
                    child: Text(
                      line,
                      style: TextStyle(
                        fontFamily: 'monospace',
                        fontSize: 13,
                        color: line.startsWith('>')
                            ? Colors.blue.shade800
                            : Colors.green.shade800,
                        fontWeight: line.startsWith('<')
                            ? FontWeight.bold
                            : FontWeight.normal,
                      ),
                    ),
                  );
                },
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildLog() {
    return Container(
      height: 60,
      color: Colors.blue.shade50,
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
    );
  }
}
