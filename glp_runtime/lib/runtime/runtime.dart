import 'dart:io';
import 'dart:ffi' as ffi;

import 'machine_state.dart';
import 'heap.dart';
import 'roq.dart';
import 'suspend_ops.dart';
import 'commit.dart';
import 'abandon.dart';
import 'fairness.dart';
import 'hanger.dart';
import 'system_predicates.dart';
import '../bytecode/runner.dart' show CallEnv;

class GlpRuntime {
  final Heap heap;
  final ROQueues roq;
  final GoalQueue gq;
  final SystemPredicateRegistry systemPredicates;

  final Map<GoalId, int> _budgets = <GoalId, int>{};
  final Map<GoalId, CallEnv> _goalEnvs = <GoalId, CallEnv>{};
  final Map<GoalId, Object?> _goalPrograms = <GoalId, Object?>{};

  // File handle management
  final Map<int, RandomAccessFile> _fileHandles = <int, RandomAccessFile>{};
  int _nextFileHandle = 1;

  // FFI/Dynamic library management
  final Map<int, ffi.DynamicLibrary> _libraries = <int, ffi.DynamicLibrary>{};
  int _nextLibraryHandle = 1;

  // Goal ID counter for spawn
  int nextGoalId = 10000;  // Start at 10000 to avoid collisions with test goal IDs

  GlpRuntime({Heap? heap, ROQueues? roq, GoalQueue? gq, SystemPredicateRegistry? systemPredicates})
      : heap = heap ?? Heap(),  // Using single-ID Heap
        roq = roq ?? ROQueues(),
        gq = gq ?? GoalQueue(),
        systemPredicates = systemPredicates ?? SystemPredicateRegistry();

  List<GoalRef> commitWriters(Iterable<int> writerIds) {
    final acts = CommitOps.applySigmaHat(
      heap: heap,
      roq: roq,
      writerIds: writerIds,
    );
    _enqueueAll(acts);
    return acts;
  }

  List<GoalRef> abandonWriter(int writerId) {
    final acts = AbandonOps.abandonWriter(
      heap: heap,
      roq: roq,
      writerId: writerId,
    );
    _enqueueAll(acts);
    return acts;
  }

  Hanger suspendGoal({
    required GoalId goalId,
    required Pc kappa,
    required Iterable<ReaderId> readers,
  }) {
    return SuspendOps.suspendGoal(
      goalId: goalId,
      kappa: kappa,
      roq: roq,
      readers: readers,
    );
  }

  bool tailReduce(GoalId g) {
    final current = _budgets[g] ?? tailRecursionBudgetInit;
    final next = nextTailBudget(current);
    if (next == 0) {
      _budgets[g] = resetTailBudget();
      return true;
    } else {
      _budgets[g] = next;
      return false;
    }
  }

  int budgetOf(GoalId g) => _budgets[g] ?? tailRecursionBudgetInit;

  void setGoalEnv(GoalId g, CallEnv env) {
    _goalEnvs[g] = env;
  }

  CallEnv? getGoalEnv(GoalId g) => _goalEnvs[g];

  void setGoalProgram(GoalId g, Object? program) {
    _goalPrograms[g] = program;
  }

  Object? getGoalProgram(GoalId g) => _goalPrograms[g];

  void _enqueueAll(List<GoalRef> acts) {
    for (final a in acts) {
      gq.enqueue(a);
    }
  }

  // File handle management methods

  /// Allocate a new file handle and register the file
  int allocateFileHandle(RandomAccessFile file) {
    final handle = _nextFileHandle++;
    _fileHandles[handle] = file;
    return handle;
  }

  /// Get file by handle
  RandomAccessFile? getFile(int handle) => _fileHandles[handle];

  /// Check if handle is valid
  bool isValidHandle(int handle) => _fileHandles.containsKey(handle);

  /// Close and remove file handle
  void closeFileHandle(int handle) {
    final file = _fileHandles.remove(handle);
    if (file != null) {
      try {
        file.closeSync();
      } catch (e) {
        // Ignore close errors
      }
    }
  }

  /// Close all open file handles (cleanup)
  void closeAllFiles() {
    for (final file in _fileHandles.values) {
      try {
        file.closeSync();
      } catch (e) {
        // Ignore close errors
      }
    }
    _fileHandles.clear();
  }

  // FFI/Dynamic library management methods

  /// Load a dynamic library and allocate handle
  int loadLibrary(String path) {
    try {
      final lib = ffi.DynamicLibrary.open(path);
      final handle = _nextLibraryHandle++;
      _libraries[handle] = lib;
      return handle;
    } catch (e) {
      throw Exception('Failed to load library $path: $e');
    }
  }

  /// Get library by handle
  ffi.DynamicLibrary? getLibrary(int handle) => _libraries[handle];

  /// Check if library handle is valid
  bool isValidLibrary(int handle) => _libraries.containsKey(handle);

  /// Close library handle (note: DynamicLibrary doesn't have close method)
  void closeLibrary(int handle) {
    _libraries.remove(handle);
  }

  /// Close all libraries
  void closeAllLibraries() {
    _libraries.clear();
  }
}
