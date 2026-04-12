/// GLP Grassroots Bonds — Simulated Plays
///
/// Runs bond plays (fplay1-12) via REPL subprocess, with tagged output
/// parsed and routed to per-agent read-only panels.
///
/// Play 12 adds narrative kinds (friend, say, act, event) with a
/// Code/Story toggle to switch between protocol trace and narrative view.
library;

import 'dart:io';

import 'package:flutter/material.dart';
import 'package:glp_runtime/multiagent/repl_play_runner.dart';

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const BondsApp());
}

// =============================================================================
// BONDS APP
// =============================================================================

class BondsApp extends StatelessWidget {
  const BondsApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Grassroots Bonds',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.indigo,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.indigo,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Colors.indigo,
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: Colors.indigo,
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const BondsScreen(),
    );
  }
}

// =============================================================================
// PER-AGENT UI STATE
// =============================================================================

class _AgentInfo {
  final String id;
  final Color headerColor;
  final Color bgColor;

  const _AgentInfo(this.id, this.headerColor, this.bgColor);
}

const _twoAgentInfos = [
  _AgentInfo('Alice', Color(0xFF3949AB), Color(0xFFE8EAF6)), // indigo
  _AgentInfo('Bob', Color(0xFF00897B), Color(0xFFE0F2F1)), // teal
];

const _sixAgentInfos = [
  _AgentInfo('Alice', Color(0xFF3949AB), Color(0xFFE8EAF6)), // indigo
  _AgentInfo('Bob', Color(0xFF00897B), Color(0xFFE0F2F1)), // teal
  _AgentInfo('Charlie', Color(0xFFEF6C00), Color(0xFFFFF3E0)), // orange
  _AgentInfo('Diana', Color(0xFF7B1FA2), Color(0xFFF3E5F5)), // purple
  _AgentInfo('Eve', Color(0xFFC2185B), Color(0xFFFCE4EC)), // pink
  _AgentInfo('Frank', Color(0xFF546E7A), Color(0xFFECEFF1)), // blue-grey
];

/// A single log entry with its kind for filtering.
class _AgentLogEntry {
  final String kind; // cmd, notify, friend, say, act, event
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
// BONDS SCREEN
// =============================================================================

class BondsScreen extends StatefulWidget {
  const BondsScreen({super.key});

  @override
  State<BondsScreen> createState() => _BondsScreenState();
}

class _BondsScreenState extends State<BondsScreen> {
  final Map<String, _AgentState> _agents = {};
  final List<String> _log = [];
  ReplPlayRunner? _playRunner;
  int _currentPlay = 0;
  bool _storyMode = false;

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

  Future<void> _runPlay(int playNumber) async {
    _playRunner?.kill();
    _playRunner = null;

    for (final agent in _agents.values) {
      agent.dispose();
    }
    _agents.clear();

    // Play 12 uses 6 agents; plays 1-11 use 2.
    final infos = playNumber == 12 ? _sixAgentInfos : _twoAgentInfos;
    for (final info in infos) {
      _agents[info.id] = _AgentState(info);
    }

    _currentPlay = playNumber;
    // Default view: Story for play 12, Code for plays 1-11.
    _storyMode = playNumber == 12;

    final repoRoot = _resolveRepoRoot();
    setState(() {
      _log.add('Starting fplay$playNumber (repo: $repoRoot)...');
    });

    final runner = ReplPlayRunner(
      repoRoot: repoRoot,
      glpFiles: playNumber == 12
          ? ReplPlayRunner.bondsPlay12Files
          : ReplPlayRunner.bondsFiles,
    );
    _playRunner = runner;

    runner.onOutput = (output) {
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
        _log.add('fplay$playNumber finished (exit $exitCode)');
      });
    };

    await runner.run(playNumber);
  }

  /// Format display text based on narrative kind.
  /// Formatting rules are based on the tag type (part of the GLP→Dart protocol).
  String _formatDisplayLine(String kind, String content) {
    switch (kind) {
      case 'cmd':
        return '> $content';
      case 'notify':
        return '< $content';
      case 'friend':
        // Capitalize first letter of content
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

  /// Style for each output kind.
  TextStyle _styleForKind(String kind) {
    switch (kind) {
      case 'cmd':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 13,
          color: Colors.blue.shade800,
        );
      case 'notify':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 13,
          color: Colors.green.shade800,
          fontWeight: FontWeight.bold,
        );
      case 'friend':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 13,
          color: Colors.grey.shade600,
        );
      case 'say':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 13,
          color: Colors.indigo.shade700,
          fontStyle: FontStyle.italic,
        );
      case 'act':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 13,
          color: Colors.grey.shade800,
        );
      case 'event':
        return TextStyle(
          fontFamily: 'monospace',
          fontSize: 13,
          color: Colors.teal.shade700,
        );
      default:
        return const TextStyle(fontFamily: 'monospace', fontSize: 13);
    }
  }

  /// Whether a log entry is visible in the current view mode.
  bool _isVisible(_AgentLogEntry entry) {
    if (_storyMode) {
      return entry.kind == 'friend' ||
          entry.kind == 'say' ||
          entry.kind == 'act' ||
          entry.kind == 'event';
    } else {
      return entry.kind == 'cmd' || entry.kind == 'notify';
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
        title: const Text('Grassroots Bonds'),
      ),
      body: Column(
        children: [
          _buildControlBar(),
          Expanded(
            child: _agents.isEmpty
                ? const Center(
                    child:
                        Text('Click a Play button above to run a scenario.'))
                : Row(
                    crossAxisAlignment: CrossAxisAlignment.stretch,
                    children: _agents.values
                        .map((agent) =>
                            Expanded(child: _buildAgentPanel(agent)))
                        .toList(),
                  ),
          ),
          _buildLog(),
        ],
      ),
    );
  }

  Widget _buildControlBar() {
    return Container(
      padding: const EdgeInsets.all(16.0),
      color: Colors.indigo.shade50,
      child: Row(
        children: [
          // Play 12 (primary)
          ElevatedButton.icon(
            onPressed: () => _runPlay(12),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 12 (Market)'),
          ),
          const SizedBox(width: 8),
          // Escrow plays
          ElevatedButton.icon(
            onPressed: () => _runPlay(11),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 11 (Cancel)'),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: () => _runPlay(10),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 10 (Time)'),
          ),
          const SizedBox(width: 16),
          Container(width: 1, height: 24, color: Colors.indigo.shade300),
          const SizedBox(width: 16),
          // Earlier plays
          for (int i = 1; i <= 9; i++) ...[
            ElevatedButton(
              onPressed: () => _runPlay(i),
              child: Text('$i'),
            ),
            if (i < 9) const SizedBox(width: 4),
          ],
          const Spacer(),
          // Code/Story toggle (visible when a play is running)
          if (_currentPlay > 0)
            ToggleButtons(
              borderRadius: BorderRadius.circular(8),
              isSelected: [!_storyMode, _storyMode],
              onPressed: (index) {
                setState(() {
                  _storyMode = index == 1;
                });
              },
              children: const [
                Padding(
                  padding: EdgeInsets.symmetric(horizontal: 12),
                  child: Text('Code'),
                ),
                Padding(
                  padding: EdgeInsets.symmetric(horizontal: 12),
                  child: Text('Story'),
                ),
              ],
            ),
        ],
      ),
    );
  }

  Widget _buildAgentPanel(_AgentState agent) {
    final info = agent.info;
    final filteredLog =
        agent.outputLog.where(_isVisible).toList(growable: false);

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
                  info.id,
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
                itemCount: filteredLog.length,
                itemBuilder: (context, index) {
                  final entry = filteredLog[index];
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
      height: 60,
      color: Colors.indigo.shade50,
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
