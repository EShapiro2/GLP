/// GLP CSSN Village Scenario — Six-Agent Narrative Panels
///
/// Runs CSSN village play (fplay13) via REPL subprocess, with narrative
/// output parsed and routed to 6 per-agent panels arranged in a 2×3 grid:
///   Row 0 (adults):   Alice (parent), Bob (parent), Frank (teacher)
///   Row 1 (children): Carol (Alice's), Dave (Bob's), Eve (Bob's)
///
/// Story mode only — displays friend, say, act, event narrative items.
library;

import 'dart:io';

import 'package:flutter/material.dart';
import 'package:glp_runtime/multiagent/repl_play_runner.dart';

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const CssnVillageApp());
}

// =============================================================================
// APP
// =============================================================================

class CssnVillageApp extends StatelessWidget {
  const CssnVillageApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'CSSN Village Scenario',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.teal,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.teal,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Color(0xFF00695C), // teal 800
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: const Color(0xFF00695C),
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const CssnVillageScreen(),
    );
  }
}

// =============================================================================
// PER-AGENT UI STATE
// =============================================================================

class _AgentInfo {
  final String id;
  final String role;
  final Color headerColor;
  final Color bgColor;
  final int row; // 0 = top (adults), 1 = bottom (children)

  const _AgentInfo(
      this.id, this.role, this.headerColor, this.bgColor, this.row);
}

/// 6 agents arranged in 2 rows:
///   Row 0 (adults):   Alice (parent), Bob (parent), Frank (teacher)
///   Row 1 (children): Carol (Alice's child), Dave (Bob's child), Eve (Bob's child)
const _agentInfos = [
  // Adults (row 0) — dark headers
  _AgentInfo('Alice', 'Parent', Color(0xFF3949AB), Color(0xFFE8EAF6), 0),
  _AgentInfo('Bob', 'Parent', Color(0xFF00897B), Color(0xFFE0F2F1), 0),
  _AgentInfo('Frank', 'Teacher', Color(0xFF546E7A), Color(0xFFECEFF1), 0),
  // Children (row 1) — lighter headers
  _AgentInfo(
      'Carol', "Alice's child", Color(0xFF7986CB), Color(0xFFF0F1FA), 1),
  _AgentInfo('Dave', "Bob's child", Color(0xFF4DB6AC), Color(0xFFF5FFFE), 1),
  _AgentInfo('Eve', "Bob's child", Color(0xFF80CBC4), Color(0xFFF5FFFE), 1),
];

/// A single log entry with its kind for filtering/styling.
class _AgentLogEntry {
  final String kind; // friend, say, act, event
  final String displayLine;
  const _AgentLogEntry(this.kind, this.displayLine);
}

class _AgentState {
  final _AgentInfo info;
  final List<_AgentLogEntry> outputLog = [];
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

class CssnVillageScreen extends StatefulWidget {
  const CssnVillageScreen({super.key});

  @override
  State<CssnVillageScreen> createState() => _CssnVillageScreenState();
}

class _CssnVillageScreenState extends State<CssnVillageScreen> {
  final Map<String, _AgentState> _agents = {};
  final List<String> _log = [];
  ReplPlayRunner? _playRunner;

  @override
  void initState() {
    super.initState();
    // Auto-run on startup.
    WidgetsBinding.instance.addPostFrameCallback((_) => _runPlay());
  }

  @override
  void dispose() {
    _playRunner?.kill();
    for (final agent in _agents.values) {
      agent.dispose();
    }
    super.dispose();
  }

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
    _playRunner?.kill();
    _playRunner = null;

    for (final agent in _agents.values) {
      agent.dispose();
    }
    _agents.clear();

    for (final info in _agentInfos) {
      _agents[info.id] = _AgentState(info);
    }

    final repoRoot = _resolveRepoRoot();
    setState(() {
      _log.add('Starting fplay13 (repo: $repoRoot)...');
    });

    final runner = ReplPlayRunner(
      repoRoot: repoRoot,
      glpFiles: ReplPlayRunner.cssnVillageFiles,
    );
    _playRunner = runner;

    runner.onOutput = (output) {
      // Only show narrative kinds (Story mode).
      if (output.kind != 'friend' &&
          output.kind != 'say' &&
          output.kind != 'act' &&
          output.kind != 'event') {
        return;
      }

      final key =
          output.agentId[0].toUpperCase() + output.agentId.substring(1);
      final state = _agents[key];
      if (state == null) return;

      final displayLine = _formatDisplayLine(output.kind, output.content);
      state.outputLog.add(_AgentLogEntry(output.kind, displayLine));
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
        _log.add('fplay13 finished (exit $exitCode)');
      });
    };

    await runner.run(13);
  }

  /// Format display text based on narrative kind.
  String _formatDisplayLine(String kind, String content) {
    switch (kind) {
      case 'friend':
        final name = content.isEmpty
            ? content
            : content[0].toUpperCase() + content.substring(1);
        return 'Now friends with $name';
      case 'say':
        return '"$content"';
      case 'act':
        return '  $content';
      case 'event':
        return content;
      default:
        return content;
    }
  }

  /// Style for each narrative kind.
  TextStyle _styleForKind(String kind) {
    switch (kind) {
      case 'friend':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 12,
          color: Colors.grey.shade600,
        );
      case 'say':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 12,
          color: Colors.indigo.shade700,
          fontStyle: FontStyle.italic,
        );
      case 'act':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 12,
          color: Colors.grey.shade800,
        );
      case 'event':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 12,
          color: Colors.teal.shade700,
        );
      default:
        return const TextStyle(fontFamily: 'monospace', fontSize: 12);
    }
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
        title: const Text('CSSN Village Scenario'),
      ),
      body: Column(
        children: [
          _buildControlBar(),
          Expanded(
            child: _agents.isEmpty
                ? const Center(
                    child: Text(
                        'Click "Run" to start the village scenario.'))
                : _buildAgentGrid(),
          ),
          _buildLog(),
        ],
      ),
    );
  }

  Widget _buildControlBar() {
    return Container(
      padding: const EdgeInsets.symmetric(horizontal: 16.0, vertical: 10.0),
      color: const Color(0xFFE0F2F1), // teal 50
      child: Row(
        children: [
          ElevatedButton.icon(
            onPressed: _runPlay,
            icon: const Icon(Icons.play_arrow),
            label: const Text('Run'),
          ),
          const SizedBox(width: 16),
          const Expanded(
            child: Text(
              'Befriend \u25b8 '
              'Introduce children \u25b8 '
              'Kids chat (consent) \u25b8 '
              'Study group \u25b8 '
              'Remove \u25b8 '
              'Unfriend',
              style: TextStyle(fontSize: 12, color: Color(0xFF004D40)),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildAgentGrid() {
    // 3-column layout:
    //   Col 0: Alice / Carol (Alice's child)
    //   Col 1: Bob / Dave (Bob's child)
    //   Col 2: Frank / Eve (Bob's child)
    return Column(
      children: [
        // Adults row
        Expanded(
          flex: 1,
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Expanded(child: _buildAgentPanel(_agents['Alice']!)),
              Expanded(child: _buildAgentPanel(_agents['Bob']!)),
              Expanded(child: _buildAgentPanel(_agents['Frank']!)),
            ],
          ),
        ),
        Container(height: 2, color: Colors.grey.shade400),
        // Children row
        Expanded(
          flex: 1,
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Expanded(child: _buildAgentPanel(_agents['Carol']!)),
              Expanded(child: _buildAgentPanel(_agents['Dave']!)),
              Expanded(child: _buildAgentPanel(_agents['Eve']!)),
            ],
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
          Container(
            padding:
                const EdgeInsets.symmetric(horizontal: 8.0, vertical: 6.0),
            color: info.headerColor,
            child: Row(
              children: [
                Text(
                  '${info.id} (${info.role})',
                  style: const TextStyle(
                    color: Colors.white,
                    fontWeight: FontWeight.bold,
                    fontSize: 13,
                  ),
                ),
              ],
            ),
          ),
          Expanded(
            child: Container(
              color: info.bgColor,
              child: ListView.builder(
                controller: agent.scrollController,
                padding: const EdgeInsets.all(8.0),
                itemCount: agent.outputLog.length,
                itemBuilder: (context, index) {
                  final entry = agent.outputLog[index];
                  return Padding(
                    padding: const EdgeInsets.symmetric(vertical: 2.0),
                    child: Text(
                      entry.displayLine,
                      style: _styleForKind(entry.kind),
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
      color: const Color(0xFFE0F2F1), // teal 50
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
