/// Dispatcher: Module RPC dispatcher process
/// Following FCP domain_server.cp dispatcher pattern
///
/// Each module has a dispatcher that reads its input channel and
/// dispatches goals to local procedures.

import 'dart:async';
import 'module_messages.dart';
import 'loaded_module.dart';

/// Error types for dispatcher (FCP domain_server.cp:574-601)
enum DispatchError {
  /// Procedure not exported
  unknown,
  /// Invalid goal format
  unrecognized,
  /// Execution failed
  failed,
}

/// Dispatcher error info
class DispatchErrorInfo {
  final DispatchError type;
  final String message;
  final ExportMessage? source;

  DispatchErrorInfo(this.type, this.message, this.source);

  @override
  String toString() => 'DispatchError.$type: $message';
}

/// Callback for executing a goal in module context
typedef GoalExecutor = Future<void> Function(ExportMessage message, LoadedModule module);

/// Module dispatcher: reads input channel, dispatches to local procedures
///
/// Trust mode behavior (v1):
/// - Unknown procedures fail (don't suspend)
/// - No CCC threading
/// - Errors logged, computation continues
class Dispatcher {
  final LoadedModule module;
  final GoalExecutor executor;

  /// Error callback
  final void Function(DispatchErrorInfo error)? onError;

  /// Subscription to input channel
  StreamSubscription<ExportMessage>? _subscription;

  Dispatcher({
    required this.module,
    required this.executor,
    this.onError,
  });

  /// Start the dispatcher process
  void start() {
    _subscription = module.inputStream.listen(
      _handleMessage,
      onError: _handleStreamError,
      onDone: _handleDone,
    );
  }

  /// Stop the dispatcher process
  void stop() {
    _subscription?.cancel();
    _subscription = null;
  }

  /// Handle incoming RPC message
  Future<void> _handleMessage(ExportMessage message) async {
    final signature = message.signature;

    // Check if procedure is exported
    if (!module.isExported(signature)) {
      onError?.call(DispatchErrorInfo(
        DispatchError.unknown,
        'Procedure $signature not exported from module ${module.name}',
        message,
      ));
      return;
    }

    // Execute goal
    try {
      await executor(message, module);
    } catch (e) {
      onError?.call(DispatchErrorInfo(
        DispatchError.failed,
        'Execution of $signature failed: $e',
        message,
      ));
    }
  }

  /// Handle stream errors
  void _handleStreamError(Object error) {
    onError?.call(DispatchErrorInfo(
      DispatchError.unrecognized,
      'Input stream error: $error',
      null,
    ));
  }

  /// Handle stream completion
  void _handleDone() {
    _subscription = null;
  }
}

/// Create and start a dispatcher for a module
Dispatcher startDispatcher({
  required LoadedModule module,
  required GoalExecutor executor,
  void Function(DispatchErrorInfo error)? onError,
}) {
  final dispatcher = Dispatcher(
    module: module,
    executor: executor,
    onError: onError,
  );
  dispatcher.start();
  return dispatcher;
}
