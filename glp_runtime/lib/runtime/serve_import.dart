/// serve_import: Bridge process between import vector and target module
/// Following FCP domain_server.cp:964-1013
///
/// Each import in a module's -import([...]) declaration gets a serve_import
/// process that reads from the vector stream and forwards to the target module.

import 'dart:async';
import 'module_messages.dart';
import 'module_registry.dart';
import 'loaded_module.dart';

/// Bridge process: reads from vector stream, forwards to target module
///
/// Key FCP behaviors:
/// - Lazy connection: Target module looked up on FIRST message, not at creation
/// - Lazy loading: Registry triggers module loading if not yet loaded
/// - Buffering: Messages queue on stream until target ready
class ServeImport {
  final Stream<ExportMessage> reader;
  final String targetName;
  final FcpModuleRegistry registry;

  /// Target module channel (lazy initialized on first message)
  LoadedModule? _targetModule;

  /// Subscription to reader stream
  StreamSubscription<ExportMessage>? _subscription;

  /// Error callback for reporting issues
  final void Function(String error, ExportMessage? message)? onError;

  ServeImport({
    required this.reader,
    required this.targetName,
    required this.registry,
    this.onError,
  });

  /// Start the serve_import process
  void start() {
    _subscription = reader.listen(
      _handleMessage,
      onError: _handleStreamError,
      onDone: _handleDone,
    );
  }

  /// Stop the serve_import process
  void stop() {
    _subscription?.cancel();
    _subscription = null;
  }

  /// Handle incoming message
  Future<void> _handleMessage(ExportMessage message) async {
    try {
      // Lazy connection on first message (FCP behavior)
      if (_targetModule == null) {
        _targetModule = await registry.lookupOrLoad(targetName);
      }

      // Forward message to target module's input channel
      _targetModule!.inputSink.add(message);
    } catch (e) {
      // Handle module not found or load errors
      onError?.call('Failed to route to $targetName: $e', message);
    }
  }

  /// Handle stream errors
  void _handleStreamError(Object error) {
    onError?.call('Stream error: $error', null);
  }

  /// Handle stream completion
  void _handleDone() {
    // Stream closed, cleanup
    _subscription = null;
  }
}

/// Start serve_import processes for all imports in a module
List<ServeImport> startServeImports({
  required List<Stream<ExportMessage>> readers,
  required List<String> importNames,
  required FcpModuleRegistry registry,
  void Function(String error, ExportMessage? message)? onError,
}) {
  if (readers.length != importNames.length) {
    throw ArgumentError(
      'Reader count (${readers.length}) must match import count (${importNames.length})'
    );
  }

  final processes = <ServeImport>[];
  for (int i = 0; i < readers.length; i++) {
    final process = ServeImport(
      reader: readers[i],
      targetName: importNames[i],
      registry: registry,
      onError: onError,
    );
    process.start();
    processes.add(process);
  }
  return processes;
}
