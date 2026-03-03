/// GLP Child-Safe Social Groups — 6-Agent Demo
///
/// Runs CSSN play 11 (fplay11) via REPL subprocess: 3 parents (Alice, Bob,
/// Charlie) introduce their children (Carol, Dave, Eve) to each other.
/// Carol creates a group chat and invites Dave and Eve directly.
/// Tagged output is parsed and routed to per-agent read-only panels
/// arranged in a 2×3 grid (parents on top, children below).
library;

import 'dart:io';

import 'package:flutter/material.dart';
import 'package:glp_runtime/multiagent/repl_play_runner.dart';

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const CssgGroupsApp());
}

// =============================================================================
// APP
// =============================================================================

class CssgGroupsApp extends StatelessWidget {
  const CssgGroupsApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Child-Safe Social Groups',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.deepPurple,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.deepPurple,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Color(0xFF5E35B1), // deep purple 600
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: const Color(0xFF5E35B1),
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const CssgGroupsScreen(),
    );
  }
}

// =============================================================================
// PER-AGENT UI STATE
// =============================================================================

class _AgentInfo {
  final String id;
  final String role; // "Parent" or "Child"
  final Color headerColor;
  final Color bgColor;
  final int row; // 0 = top (parents), 1 = bottom (children)
  final int col; // 0, 1, 2 — family column

  const _AgentInfo(
      this.id, this.role, this.headerColor, this.bgColor, this.row, this.col);
}

/// 6 agents arranged in 2×3 grid:
///   Row 0 (parents): Alice, Bob, Charlie
///   Row 1 (children): Carol, Dave, Eve
/// Each column is a family pair.
const _agentInfos = [
  // Parents (row 0)
  _AgentInfo('Alice', 'Parent', Color(0xFF3949AB), Color(0xFFE8EAF6), 0, 0), // indigo
  _AgentInfo('Bob', 'Parent', Color(0xFF00897B), Color(0xFFE0F2F1), 0, 1), // teal
  _AgentInfo('Charlie', 'Parent', Color(0xFF6D4C41), Color(0xFFEFEBE9), 0, 2), // brown
  // Children (row 1)
  _AgentInfo('Carol', 'Child', Color(0xFF7986CB), Color(0xFFF5F5FF), 1, 0), // light indigo
  _AgentInfo('Dave', 'Child', Color(0xFF4DB6AC), Color(0xFFF5FFFE), 1, 1), // light teal
  _AgentInfo('Eve', 'Child', Color(0xFFA1887F), Color(0xFFFBF8F6), 1, 2), // light brown
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
// MAIN SCREEN
// =============================================================================

class CssgGroupsScreen extends StatefulWidget {
  const CssgGroupsScreen({super.key});

  @override
  State<CssgGroupsScreen> createState() => _CssgGroupsScreenState();
}

class _CssgGroupsScreenState extends State<CssgGroupsScreen> {
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
    const fallback = '/Users/udi/Grassroots/GLP';
    if (Directory('$fallback/glp_runtime').existsSync()) {
      return fallback;
    }
    return devRoot;
  }

  Future<void> _runPlay() async {
    // Kill previous run
    _playRunner?.kill();
    _playRunner = null;

    // Clear previous panels
    for (final agent in _agents.values) {
      agent.dispose();
    }
    _agents.clear();

    // Create all 6 agent panels
    for (final info in _agentInfos) {
      _agents[info.id] = _AgentState(info);
    }

    final repoRoot = _resolveRepoRoot();
    setState(() {
      _log.add('Starting fplay11 (repo: $repoRoot)...');
    });

    final runner = ReplPlayRunner(
      repoRoot: repoRoot,
      glpFiles: ReplPlayRunner.cssnFiles,
    );
    _playRunner = runner;

    runner.onOutput = (output) {
      // Capitalize to match _AgentState keys
      final key =
          output.agentId[0].toUpperCase() + output.agentId.substring(1);
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
        _log.add('fplay11 finished (exit $exitCode)');
      });
    };

    await runner.run(11);
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
        title: const Text('Child-Safe Social Groups'),
      ),
      body: Column(
        children: [
          _buildControlBar(),
          // Agent panels: 2×3 grid
          Expanded(
            child: _agents.isEmpty
                ? const Center(
                    child: Text(
                        'Click "Run Demo" to start the 6-agent scenario.'))
                : _buildAgentGrid(),
          ),
          // Log
          _buildLog(),
        ],
      ),
    );
  }

  Widget _buildControlBar() {
    return Container(
      padding: const EdgeInsets.symmetric(horizontal: 16.0, vertical: 12.0),
      color: const Color(0xFFEDE7F6), // deep purple 50
      child: Row(
        children: [
          ElevatedButton.icon(
            onPressed: _runPlay,
            icon: const Icon(Icons.play_arrow),
            label: const Text('Run Demo'),
          ),
          const SizedBox(width: 16),
          const Expanded(
            child: Text(
              '3 parents introduce their children ▸ '
              'children become friends ▸ '
              'Carol creates group ▸ all chat',
              style: TextStyle(fontSize: 12, color: Color(0xFF4A148C)),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildAgentGrid() {
    // Row 0: parents (Alice, Bob, Charlie)
    // Row 1: children (Carol, Dave, Eve)
    final parents =
        _agentInfos.where((info) => info.row == 0).map((info) => _agents[info.id]!);
    final children =
        _agentInfos.where((info) => info.row == 1).map((info) => _agents[info.id]!);

    return Column(
      children: [
        // Parents row (1/3 of space)
        Expanded(
          flex: 1,
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: parents
                .map((agent) => Expanded(child: _buildAgentPanel(agent)))
                .toList(),
          ),
        ),
        // Thin separator
        Container(height: 2, color: Colors.grey.shade400),
        // Children row (2/3 of space — more activity here)
        Expanded(
          flex: 2,
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: children
                .map((agent) => Expanded(child: _buildAgentPanel(agent)))
                .toList(),
          ),
        ),
      ],
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
          // Agent header: "Parent: Alice" or "Child: Carol"
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
                        fontSize: 12,
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
      height: 50,
      color: const Color(0xFFEDE7F6), // deep purple 50
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
