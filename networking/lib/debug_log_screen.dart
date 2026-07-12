import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:logger/logger.dart';
import 'src/debug/log_buffer.dart';

/// Debug log viewer screen.
///
/// Shows all captured log entries with level filtering and auto-scroll.
class DebugLogScreen extends StatefulWidget {
  const DebugLogScreen({super.key});

  @override
  State<DebugLogScreen> createState() => _DebugLogScreenState();
}

class _DebugLogScreenState extends State<DebugLogScreen> {
  Level _minLevel = Level.info;
  bool _autoScroll = true;
  final ScrollController _scrollController = ScrollController();

  @override
  void initState() {
    super.initState();
    LogBuffer.instance.addListener(_onNewLog);
  }

  @override
  void dispose() {
    LogBuffer.instance.removeListener(_onNewLog);
    _scrollController.dispose();
    super.dispose();
  }

  void _onNewLog() {
    if (!mounted) return;
    setState(() {});
    if (_autoScroll) {
      WidgetsBinding.instance.addPostFrameCallback((_) {
        if (_scrollController.hasClients) {
          _scrollController.jumpTo(_scrollController.position.maxScrollExtent);
        }
      });
    }
  }

  List<LogEntry> get _filteredEntries =>
      LogBuffer.instance.entries.where((e) => e.level.index >= _minLevel.index).toList();

  Color _colorForLevel(Level level) {
    switch (level) {
      case Level.trace:
        return Colors.grey;
      case Level.debug:
        return Colors.grey[400]!;
      case Level.info:
        return Colors.blue[300]!;
      case Level.warning:
        return Colors.orange[300]!;
      case Level.error:
        return Colors.red[300]!;
      case Level.fatal:
        return Colors.red;
      default:
        return Colors.white;
    }
  }

  @override
  Widget build(BuildContext context) {
    final entries = _filteredEntries;

    return Scaffold(
      appBar: AppBar(
        title: Text('Debug Logs (${entries.length})'),
        backgroundColor: const Color(0xFF1B3D2F),
        actions: [
          // Auto-scroll toggle
          IconButton(
            icon: Icon(
              _autoScroll ? Icons.vertical_align_bottom : Icons.vertical_align_center,
              color: _autoScroll ? Colors.green : Colors.grey,
            ),
            tooltip: _autoScroll ? 'Auto-scroll on' : 'Auto-scroll off',
            onPressed: () => setState(() => _autoScroll = !_autoScroll),
          ),
          // Copy all logs
          IconButton(
            icon: const Icon(Icons.copy),
            tooltip: 'Copy logs',
            onPressed: () {
              final text = entries
                  .map((e) => '[${e.levelLabel}] ${e.message}')
                  .join('\n');
              Clipboard.setData(ClipboardData(text: text));
              ScaffoldMessenger.of(context).showSnackBar(
                SnackBar(content: Text('Copied ${entries.length} log entries')),
              );
            },
          ),
          // Clear logs
          IconButton(
            icon: const Icon(Icons.delete_outline),
            tooltip: 'Clear logs',
            onPressed: () {
              LogBuffer.instance.clear();
              setState(() {});
            },
          ),
        ],
      ),
      body: Column(
        children: [
          // Level filter chips
          Container(
            padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
            color: Colors.black26,
            child: SingleChildScrollView(
              scrollDirection: Axis.horizontal,
              child: Row(
                children: [
                  _buildFilterChip('All', Level.trace),
                  const SizedBox(width: 6),
                  _buildFilterChip('Debug', Level.debug),
                  const SizedBox(width: 6),
                  _buildFilterChip('Info', Level.info),
                  const SizedBox(width: 6),
                  _buildFilterChip('Warn', Level.warning),
                  const SizedBox(width: 6),
                  _buildFilterChip('Error', Level.error),
                ],
              ),
            ),
          ),
          // Log list
          Expanded(
            child: entries.isEmpty
                ? const Center(
                    child: Text(
                      'No log entries',
                      style: TextStyle(color: Colors.grey),
                    ),
                  )
                : ListView.builder(
                    controller: _scrollController,
                    itemCount: entries.length,
                    itemBuilder: (context, index) {
                      final entry = entries[index];
                      final time =
                          '${entry.timestamp.hour.toString().padLeft(2, '0')}:'
                          '${entry.timestamp.minute.toString().padLeft(2, '0')}:'
                          '${entry.timestamp.second.toString().padLeft(2, '0')}';

                      return Padding(
                        padding: const EdgeInsets.symmetric(
                            horizontal: 8, vertical: 1),
                        child: Row(
                          crossAxisAlignment: CrossAxisAlignment.start,
                          children: [
                            SizedBox(
                              width: 55,
                              child: Text(
                                time,
                                style: TextStyle(
                                  fontFamily: 'monospace',
                                  fontSize: 10,
                                  color: Colors.grey[600],
                                ),
                              ),
                            ),
                            SizedBox(
                              width: 40,
                              child: Text(
                                entry.levelLabel,
                                style: TextStyle(
                                  fontFamily: 'monospace',
                                  fontSize: 10,
                                  fontWeight: FontWeight.bold,
                                  color: _colorForLevel(entry.level),
                                ),
                              ),
                            ),
                            Expanded(
                              child: Text(
                                entry.message,
                                style: const TextStyle(
                                  fontFamily: 'monospace',
                                  fontSize: 10,
                                  color: Colors.white70,
                                ),
                              ),
                            ),
                          ],
                        ),
                      );
                    },
                  ),
          ),
        ],
      ),
    );
  }

  Widget _buildFilterChip(String label, Level level) {
    final isSelected = _minLevel == level;
    return GestureDetector(
      onTap: () => setState(() => _minLevel = level),
      child: Container(
        padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
        decoration: BoxDecoration(
          color: isSelected
              ? const Color(0xFFE8A33C).withOpacity(0.3)
              : Colors.white.withOpacity(0.08),
          borderRadius: BorderRadius.circular(16),
          border: Border.all(
            color: isSelected
                ? const Color(0xFFE8A33C)
                : Colors.white.withOpacity(0.15),
          ),
        ),
        child: Text(
          label,
          style: TextStyle(
            fontSize: 12,
            color: isSelected ? const Color(0xFFE8A33C) : Colors.grey[400],
            fontWeight: isSelected ? FontWeight.bold : FontWeight.normal,
          ),
        ),
      ),
    );
  }
}
