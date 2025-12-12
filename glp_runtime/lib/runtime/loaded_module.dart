/// LoadedModule: Runtime representation of a loaded GLP module
/// Following FCP module structure (domain_server.cp)

import 'dart:async';
import 'package:glp_runtime/bytecode/runner.dart';
import 'import_vector.dart';
import 'module_messages.dart';

/// A loaded and activated GLP module
class LoadedModule {
  /// Module name (from -module(name) declaration)
  final String name;

  /// Compiled bytecode for this module
  final BytecodeProgram bytecode;

  /// Set of exported procedure signatures (e.g., {"factorial/2", "gcd/3"})
  final Set<String> exports;

  /// List of imported module names (from -import([...]))
  final List<String> imports;

  /// Import vector for sending RPCs to other modules
  ImportVector? importVector;

  /// Input channel for receiving RPCs from other modules
  late final StreamController<ExportMessage> _inputController;

  /// Stream of incoming messages (read by dispatcher)
  Stream<ExportMessage> get inputStream => _inputController.stream;

  /// Sink for adding messages to input channel
  StreamSink<ExportMessage> get inputSink => _inputController.sink;

  LoadedModule({
    required this.name,
    required this.bytecode,
    required this.exports,
    required this.imports,
  }) {
    _inputController = StreamController<ExportMessage>.broadcast();
  }

  /// Check if a procedure signature is exported
  bool isExported(String signature) => exports.contains(signature);

  /// Get the entry point PC for a procedure signature
  /// Returns null if not found
  int? getEntryPoint(String signature) {
    return bytecode.labels[signature];
  }

  /// Close the module's input channel (for cleanup)
  void close() {
    _inputController.close();
    importVector?.close();
  }

  @override
  String toString() => 'LoadedModule($name, exports: $exports, imports: $imports)';
}
