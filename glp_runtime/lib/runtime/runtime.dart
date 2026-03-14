import 'dart:io';
import 'dart:ffi' as ffi;

import 'machine_state.dart';
import 'heap_fcp.dart';
import 'suspend_ops.dart';
import 'commit.dart';
import 'abandon.dart';
import 'fairness.dart';
import 'system_predicates.dart';
import 'body_kernels.dart';
import 'package:glp_runtime/bytecode/runner.dart' show CallEnv, BytecodeRunner;
import 'package:glp_runtime/runtime/glp_activation.dart' show GlpChannelHandle;

class GlpRuntime {
  final HeapFCP heap;
  final GoalQueue gq;
  final SystemPredicateRegistry systemPredicates;
  final BodyKernelRegistry bodyKernels;

  /// Shared runners map: program key → BytecodeRunner.
  /// Used by the Scheduler to find the runner for a goal's program.
  /// Body kernels (e.g., _activate) can register new runners here.
  final Map<Object?, BytecodeRunner> runners = {};

  /// GLP channel handles: module name → GlpChannelHandle.
  /// Registered by activateModule() (Phase 4). Used by the runner's
  /// Distribute/Transmit opcodes to route RPCs via GLP channels
  /// instead of Dart-level dispatch.
  final Map<String, GlpChannelHandle> glpChannels = {};

  final Map<GoalId, int> _budgets = <GoalId, int>{};
  final Map<GoalId, CallEnv> _goalEnvs = <GoalId, CallEnv>{};
  final Map<GoalId, Object?> _goalPrograms = <GoalId, Object?>{};
  final Map<GoalId, Object?> _goalModuleContexts = <GoalId, Object?>{};  // Module context for RPC;

  // File handle management
  final Map<int, RandomAccessFile> _fileHandles = <int, RandomAccessFile>{};
  int _nextFileHandle = 1;

  // FFI/Dynamic library management
  final Map<int, ffi.DynamicLibrary> _libraries = <int, ffi.DynamicLibrary>{};
  int _nextLibraryHandle = 1;

  // Goal ID counter for spawn
  int nextGoalId = 10000;  // Start at 10000 to avoid collisions with test goal IDs

  // Timer tracking for wait() guards
  int _pendingTimers = 0;
  int get pendingTimers => _pendingTimers;
  void incrementPendingTimers() => _pendingTimers++;
  void decrementPendingTimers() => _pendingTimers--;

  // Wait state tracking for wait() guards
  // Maps goalId to the reader ID that the timer will signal
  // When goal resumes, we check if this reader is bound (timer fired)
  final Map<int, int> _waitReaders = <int, int>{};

  // Suspension tracking for scheduler-IRMA integration (spec section 8.4)
  // Maps reader varId -> Set<GoalRef> of goals blocked on that reader
  // Updated by suspendGoalFCP, cleared when goals reactivate
  final Map<int, Set<GoalRef>> suspended = <int, Set<GoalRef>>{};

  // Infrastructure goal IDs (spec §3.4): serve goals spawned by auto-activation.
  // Their suspension does not affect user goal status determination.
  final Set<int> infrastructureGoalIds = {};

  // madGLP context (set when running in multiagent mode)
  // Used by '_cold_send' kernel to access globalization infrastructure
  Object? madContext;

  // Output callback for '_output'/1 kernel.
  // If set, called instead of print(). Used by tests and Flutter UI.
  void Function(String)? outputCallback;

  /// Check if a goal has a pending wait and if the timer has fired
  /// Returns null if no wait state, true if timer fired, false if still waiting
  bool? checkWaitState(int goalId) {
    final readerId = _waitReaders[goalId];
    if (readerId == null) return null;
    // Check if the writer has been bound (timer fired)
    return heap.isFullyBound(readerId);
  }

  /// Clear wait state for a goal (after timer completes)
  void clearWaitState(int goalId) {
    _waitReaders.remove(goalId);
  }

  /// Set wait state for a goal
  void setWaitReader(int goalId, int readerId) {
    _waitReaders[goalId] = readerId;
  }

  /// Get the wait reader for a goal (if any)
  int? getWaitReader(int goalId) => _waitReaders[goalId];

  GlpRuntime({HeapFCP? heap, GoalQueue? gq, SystemPredicateRegistry? systemPredicates, BodyKernelRegistry? bodyKernels})
      : heap = heap ?? HeapFCP(),
        gq = gq ?? GoalQueue(),
        systemPredicates = systemPredicates ?? SystemPredicateRegistry(),
        bodyKernels = bodyKernels ?? _createDefaultBodyKernels();

  /// Create body kernel registry with standard kernels registered
  static BodyKernelRegistry _createDefaultBodyKernels() {
    final registry = BodyKernelRegistry();
    registerStandardBodyKernels(registry);
    return registry;
  }

  /// Commit writer bindings using FCP-exact semantics
  /// sigmaHat: Map from varId to tentative value
  List<GoalRef> commitSigmaHat(Map<int, Object?> sigmaHat) {
    final acts = CommitOps.applySigmaHatFCP(
      heap: heap,
      sigmaHat: sigmaHat,
    );
    _enqueueAll(acts);
    return acts;
  }

  /// Legacy commit method (deprecated - for backward compatibility)
  /// TODO: Remove after runner.dart updated to use commitSigmaHat
  List<GoalRef> commitWriters(Iterable<int> writerIds) {
    throw UnimplementedError('Legacy commitWriters deprecated - use commitSigmaHat');
  }

  /// Legacy abandon method (deprecated)
  /// TODO: Remove after runner.dart updated
  List<GoalRef> abandonWriter(int writerId) {
    throw UnimplementedError('Legacy abandonWriter deprecated - FCP has no abandon');
  }

  /// Suspend goal using FCP-exact shared suspension records
  void suspendGoalFCP({
    required int goalId,
    required int kappa,
    required Set<int> readerVarIds,
  }) {
    // Track which readers have this goal suspended (spec section 8.4)
    final goalRef = GoalRef(goalId, kappa);
    for (final readerId in readerVarIds) {
      suspended.putIfAbsent(readerId, () => <GoalRef>{}).add(goalRef);
    }

    SuspendOps.suspendGoalFCP(
      heap: heap,
      goalId: goalId,
      kappa: kappa,
      readerVarIds: readerVarIds,
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

  /// Set module context for a goal (for distribute/transmit handlers)
  void setGoalModuleContext(GoalId g, Object? ctx) {
    _goalModuleContexts[g] = ctx;
  }

  /// Get module context for a goal
  Object? getGoalModuleContext(GoalId g) => _goalModuleContexts[g];

  void _enqueueAll(List<GoalRef> acts) {
    for (final a in acts) {
      enqueueReactivatedGoal(a);
    }
  }

  /// Enqueue a reactivated goal and clean up from suspended map
  /// Use this instead of gq.enqueue() when reactivating suspended goals
  void enqueueReactivatedGoal(GoalRef goal) {
    gq.enqueue(goal);
    // Clean up from suspended map - goal is now reactivated
    _removeFromSuspended(goal);
  }

  /// Remove a goal from all entries in the suspended map
  void _removeFromSuspended(GoalRef goal) {
    final toRemove = <int>[];
    for (final entry in suspended.entries) {
      entry.value.remove(goal);
      if (entry.value.isEmpty) {
        toRemove.add(entry.key);
      }
    }
    for (final key in toRemove) {
      suspended.remove(key);
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
