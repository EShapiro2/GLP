/// GLP Child-Safe Social Groups — Parental Consent Demo
///
/// Runs CSSN play 12 (fplay12) via REPL subprocess: Alice (adult) creates a
/// "study" star group, invites Bob and Charlie (adults) who accept, then
/// invites Dave (Bob's child) via Bob and Eve (Charlie's child) via Charlie.
/// Both parents approve.  All 5 members chat.
/// Tagged output is parsed and routed to per-agent read-only panels
/// arranged in a 2-row layout (3 adults on top, 2 children below).
library;

import 'dart:io';

import 'package:flutter/material.dart';
import 'package:glp_runtime/multiagent/repl_play_runner.dart';

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const CssgConsentApp());
}

// =============================================================================
// APP
// =============================================================================

class CssgConsentApp extends StatelessWidget {
  const CssgConsentApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Parental Consent for Groups',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.teal,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.teal,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Color(0xFF00796B), // teal 700
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: const Color(0xFF00796B),
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const CssgConsentScreen(),
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

  const _AgentInfo(this.id, this.role, this.headerColor, this.bgColor, this.row);
}

/// 5 agents arranged in 2 rows:
///   Row 0 (adults):   Alice (creator), Bob (parent), Charlie (parent)
///   Row 1 (children): Dave (Bob's child), Eve (Charlie's child)
const _agentInfos = [
  // Adults (row 0)
  _AgentInfo('Alice', 'Teacher', Color(0xFF3949AB), Color(0xFFE8EAF6), 0), // indigo
  _AgentInfo('Bob', 'Parent', Color(0xFF00897B), Color(0xFFE0F2F1), 0), // teal
  _AgentInfo('Charlie', 'Parent', Color(0xFF6D4C41), Color(0xFFEFEBE9), 0), // brown
  // Children (row 1)
  _AgentInfo('Dave', 'Child (Bob\'s)', Color(0xFF4DB6AC), Color(0xFFF5FFFE), 1), // light teal
  _AgentInfo('Eve', 'Child (Charlie\'s)', Color(0xFFA1887F), Color(0xFFFBF8F6), 1), // light brown
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

class CssgConsentScreen extends StatefulWidget {
  const CssgConsentScreen({super.key});

  @override
  State<CssgConsentScreen> createState() => _CssgConsentScreenState();
}

class _CssgConsentScreenState extends State<CssgConsentScreen> {
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
      _log.add('Starting fplay12 (repo: $repoRoot)...');
    });

    final runner = ReplPlayRunner(
      repoRoot: repoRoot,
      glpFiles: ReplPlayRunner.cssnFiles,
    );
    _playRunner = runner;

    runner.onOutput = (output) {
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
        _log.add('fplay12 finished (exit $exitCode)');
      });
    };

    await runner.run(12);
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
        title: const Text('Parental Consent for Groups'),
      ),
      body: Column(
        children: [
          _buildControlBar(),
          Expanded(
            child: _agents.isEmpty
                ? const Center(
                    child: Text(
                        'Click "Run Demo" to start the parental consent scenario.'))
                : _buildAgentGrid(),
          ),
          _buildLog(),
        ],
      ),
    );
  }

  Widget _buildControlBar() {
    return Container(
      padding: const EdgeInsets.symmetric(horizontal: 16.0, vertical: 12.0),
      color: const Color(0xFFE0F2F1), // teal 50
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
              'Alice creates group ▸ '
              'invites Bob & Charlie ▸ '
              'invites Dave via Bob ▸ '
              'invites Eve via Charlie ▸ '
              'parents approve ▸ '
              'all chat',
              style: TextStyle(fontSize: 12, color: Color(0xFF004D40)),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildAgentGrid() {
    // 3-column layout: each column is a family.
    //   Col 0: Alice (no child)
    //   Col 1: Bob / Dave
    //   Col 2: Charlie / Eve
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
              Expanded(child: _buildAgentPanel(_agents['Charlie']!)),
            ],
          ),
        ),
        Container(height: 2, color: Colors.grey.shade400),
        // Children row: blank under Alice, Dave under Bob, Eve under Charlie
        Expanded(
          flex: 1,
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Expanded(child: _buildBlankPanel()),
              Expanded(child: _buildAgentPanel(_agents['Dave']!)),
              Expanded(child: _buildAgentPanel(_agents['Eve']!)),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildBlankPanel() {
    return Container(
      decoration: BoxDecoration(
        color: Colors.grey.shade100,
        border: Border(
          right: BorderSide(color: Colors.grey.shade300),
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
