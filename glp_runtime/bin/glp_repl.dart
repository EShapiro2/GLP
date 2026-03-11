/// GLP REPL - Command Line Interface
///
/// Thin CLI wrapper around GlpEngine.
/// This is the user-facing REPL; all execution logic is in GlpEngine.
library;

import 'dart:io';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

void main() async {
  final gitCommit = await _getGitCommit();
  final buildTime = '2026-02-01 (GlpEngine refactor)';

  print('╔════════════════════════════════════════╗');
  print('║  GLP REPL - With Type Checking         ║');
  print('╚════════════════════════════════════════╝');
  print('');
  if (gitCommit != null) {
    print('Build: $gitCommit');
  }
  print('Compiled: $buildTime');
  print('Working directory: ${Directory.current.path}');
  print('');
  print('Input: filename.glp to load, or goal to execute');
  print('Commands: :quit, :help, :trace, :debug, :limit');
  print('');

  final stdlibDir = File('../programs/stdlib').absolute.path;
  final engine = GlpEngine(stdlibDir: stdlibDir);
  print('Loaded stdlib');
  print('');

  while (true) {
    stdout.write('GLP> ');
    final input = stdin.readLineSync();

    if (input == null) {
      break;
    }

    if (input.trim().isEmpty) {
      continue;
    }

    var trimmed = input.trim();
    if (trimmed.endsWith('.') && !trimmed.endsWith('.glp')) {
      trimmed = trimmed.substring(0, trimmed.length - 1).trim();
    }

    // Handle commands
    if (trimmed == ':quit' || trimmed == ':q') {
      print('Goodbye!');
      break;
    }

    if (trimmed == ':help' || trimmed == ':h') {
      _printHelp();
      continue;
    }

    if (trimmed == ':trace' || trimmed == ':t') {
      engine.debugTrace = !engine.debugTrace;
      print('Trace ${engine.debugTrace ? "enabled" : "disabled"}');
      continue;
    }

    if (trimmed == ':debug' || trimmed == ':d') {
      engine.debugOutput = !engine.debugOutput;
      print('Debug output ${engine.debugOutput ? "enabled" : "disabled"}');
      continue;
    }

    if (trimmed == ':strict' || trimmed == ':s') {
      engine.strictTypes = !engine.strictTypes;
      print('Strict type checking ${engine.strictTypes ? "enabled" : "disabled"}');
      continue;
    }

    if (trimmed == ':clear' || trimmed == ':c') {
      engine.clear();
      print('Cleared loaded programs (stdlib retained)');
      continue;
    }

    if (trimmed.startsWith(':limit')) {
      final parts = trimmed.split(RegExp(r'\s+'));
      if (parts.length != 2) {
        print('Usage: :limit <number>');
        continue;
      }
      final limit = int.tryParse(parts[1]);
      if (limit == null || limit <= 0) {
        print('Error: limit must be a positive integer');
        continue;
      }
      engine.maxCycles = limit;
      print('Goal reduction limit set to ${engine.maxCycles}');
      continue;
    }

    if (trimmed.startsWith(':bytecode') || trimmed.startsWith(':bc')) {
      if (engine.loadedPrograms.isEmpty) {
        print('No programs loaded');
        continue;
      }
      for (final entry in engine.loadedPrograms.entries) {
        print('\nBytecode for ${entry.key}:');
        print('=' * 60);
        final prog = entry.value;
        for (int i = 0; i < prog.ops.length; i++) {
          print('  ${i.toString().padLeft(4)}: ${prog.ops[i]}');
        }
      }
      continue;
    }

    // Check if input is a project directory to load.
    // Supports: <dir> or <dir> <top_module>
    {
      final parts = trimmed.split(' ');
      final dirCandidate = parts[0];
      if (!dirCandidate.endsWith('.glp') &&
          Directory(dirCandidate).existsSync()) {
        final topModule = parts.length > 1 ? parts[1] : null;
        try {
          engine.loadProject(dirCandidate, topModuleName: topModule);
          print('✓ Loaded project: $dirCandidate');
        } catch (e) {
          print('Error loading project $dirCandidate: $e');
        }
        continue;
      }
    }

    // Check if input is a .glp file to load
    if (trimmed.endsWith('.glp')) {
      String filename;
      if (trimmed.startsWith('load ')) {
        filename = trimmed.substring(5).trim();
      } else if (!trimmed.contains(' ')) {
        filename = trimmed;
      } else {
        filename = '';
      }
      if (filename.isNotEmpty) {
        try {
          // Resolve path
          final File sourceFile;
          if (filename.startsWith('/') ||
              filename.startsWith('../') ||
              filename.startsWith('./')) {
            sourceFile = File(filename);
          } else {
            sourceFile = File('glp/$filename');
          }

          if (!sourceFile.existsSync()) {
            print('Error: File not found: ${sourceFile.path}');
            continue;
          }

          engine.loadFile(sourceFile.path);
          print('✓ Loaded: $filename');
        } catch (e) {
          print('Error loading $filename: $e');
        }
        continue;
      }
    }

    // Run goal
    try {
      final result = await engine.runGoal(trimmed);

      // Print bindings
      if (result.bindings.isNotEmpty) {
        for (final entry in result.bindings.entries) {
          final varName = entry.key;
          final value = entry.value;
          if (value != null) {
            print('$varName = ${_formatTerm(value, engine)}');
          } else {
            print('$varName = <unbound>');
          }
        }
      }

      // Print status
      _printStatus(result.status);

      if (result.error != null) {
        print('Error: ${result.error}');
      }
    } catch (e) {
      print('Error: $e');
    }

    print('');
  }
}

void _printStatus(ExecutionStatus status) {
  switch (status) {
    case ExecutionStatus.succeeded:
      print('→ succeeds');
    case ExecutionStatus.failed:
      print('→ failed');
    case ExecutionStatus.suspended:
      print('→ suspended');
  }
}

void _printHelp() {
  print('');
  print('GLP REPL Usage:');
  print('  filename.glp           Load and compile glp/<filename>');
  print('  goal.                  Execute a goal (must end with .)');
  print('');
  print('Commands:');
  print('  :help, :h              Show this help');
  print('  :quit, :q              Exit REPL');
  print('  :clear, :c             Clear loaded programs (keep stdlib)');
  print('  :trace, :t             Toggle trace output (reductions)');
  print('  :debug, :d             Toggle DEBUG output');
  print('  :strict, :s            Toggle strict type checking (default: on)');
  print('  :limit <n>             Set goal reduction limit to <n>');
  print('  :bytecode, :bc         Show loaded bytecode');
  print('');
  print('Type Checking:');
  print('  Programs with procedure declarations are type-checked');
  print('  Type errors abort loading by default (use :strict to toggle)');
  print('');
  print('Examples:');
  print('  GLP> merge.glp                        # Load typed program');
  print('  GLP> merge([1,2],[a,b],X).            # Execute goal');
  print('');
}

String _formatTerm(rt.Term? term, [GlpEngine? engine, Set<int>? path]) {
  if (term == null) return '[]';

  path ??= <int>{};

  if (term is rt.ConstTerm) {
    if (term.value == null || term.value == 'nil') return '[]';
    return term.value.toString();
  }

  if (term is rt.StructTerm && term.functor == '.' && term.args.length == 2) {
    final elements = <String>[];
    rt.Term? current = term;

    while (true) {
      if (current is! rt.StructTerm || current.functor != '.') break;

      final head = current.args[0];
      final tail = current.args[1];

      String headStr;
      if (head is rt.VarRef && engine != null) {
        final addr = head.addr;
        if (path.contains(addr)) {
          headStr = '<circular>';
        } else {
          path.add(addr);
          final derefHead = engine.runtime.heap.dereference(head);
          if (derefHead is rt.VarRef) {
            final displayId = derefHead.addr;
            headStr = engine.runtime.heap.isReader(derefHead.addr)
                ? 'X$displayId?'
                : 'X$displayId';
          } else {
            headStr = _formatTerm(derefHead, engine, path);
          }
          path.remove(addr);
        }
      } else {
        headStr = _formatTerm(head, engine, path);
      }

      elements.add(headStr);

      if (tail is rt.VarRef && engine != null) {
        final addr = tail.addr;
        if (path.contains(addr)) {
          final displayId = addr;
          final label = engine.runtime.heap.isReader(tail.addr)
              ? 'X$displayId?'
              : 'X$displayId';
          return '[${elements.join(', ')} | <circular $label>]';
        }
        path.add(addr);
        final derefTail = engine.runtime.heap.dereference(tail);
        if (derefTail is rt.VarRef) {
          path.remove(addr);
          final displayId = derefTail.addr;
          final label = engine.runtime.heap.isReader(derefTail.addr)
              ? 'X$displayId?'
              : 'X$displayId';
          return '[${elements.join(', ')} | $label]';
        }
        current = derefTail;
        path.remove(addr);
        if (current is! rt.StructTerm) break;
      } else if (tail is rt.ConstTerm &&
          (tail.value == 'nil' || tail.value == null)) {
        break;
      } else if (tail is rt.StructTerm && tail.functor == '.') {
        current = tail;
      } else {
        break;
      }
    }

    return '[${elements.join(', ')}]';
  }

  if (term is rt.StructTerm) {
    final currentPath = path;
    final formattedArgs = term.args.map((arg) {
      if (arg is rt.VarRef && engine != null) {
        final addr = arg.addr;
        if (currentPath.contains(addr)) {
          return '<circular>';
        }
        currentPath.add(addr);
        final deref = engine.runtime.heap.dereference(arg);
        String result;
        if (deref is rt.VarRef) {
          final displayId = deref.addr;
          result = engine.runtime.heap.isReader(deref.addr)
              ? 'X$displayId?'
              : 'X$displayId';
        } else {
          result = _formatTerm(deref, engine, currentPath);
        }
        currentPath.remove(addr);
        return result;
      }
      return _formatTerm(arg, engine, currentPath);
    }).join(', ');
    return '${term.functor}($formattedArgs)';
  }

  return term.toString();
}

Future<String?> _getGitCommit() async {
  try {
    final result = await Process.run('git', ['log', '-1', '--format=%h %s']);
    if (result.exitCode == 0) {
      return result.stdout.toString().trim();
    }
  } catch (e) {
    // Git not available or not a git repo
  }
  return null;
}
