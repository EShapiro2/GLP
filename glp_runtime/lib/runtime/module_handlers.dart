/// Module system opcode handlers for Distribute and Transmit
/// Following FCP design for cross-module RPC

import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/runtime/import_vector.dart';
import 'package:glp_runtime/runtime/module_messages.dart';
import 'package:glp_runtime/runtime/loaded_module.dart';
import 'package:glp_runtime/runtime/module_registry.dart';
import 'package:glp_runtime/runtime/terms.dart';

/// Result of handling a distribute/transmit opcode
enum ModuleOpResult {
  /// Message sent successfully
  success,
  /// Target module not found
  moduleNotFound,
  /// Import index out of range
  indexOutOfRange,
  /// Variable not bound (for transmit)
  variableUnbound,
  /// No module context available
  noModuleContext,
}

/// Module execution context - provides module-level state for RPC handlers
class ModuleContext {
  /// Current module (where the goal is executing)
  final LoadedModule currentModule;

  /// Module registry for dynamic lookup
  final FcpModuleRegistry registry;

  /// Source module name for call info
  final String sourceModule;

  /// Current source location (for call info)
  final int line;
  final int column;

  ModuleContext({
    required this.currentModule,
    required this.registry,
    required this.sourceModule,
    this.line = 0,
    this.column = 0,
  });
}

/// Handle Distribute opcode: send RPC to module at known import index
///
/// Following FCP: distribute # {Index, Goal}
/// Writes message to import vector at Index, which routes to target module.
ModuleOpResult handleDistribute(
  Distribute op,
  List<Term> args,
  ModuleContext? ctx,
) {
  if (ctx == null) {
    return ModuleOpResult.noModuleContext;
  }

  final vector = ctx.currentModule.importVector;
  if (vector == null) {
    return ModuleOpResult.noModuleContext;
  }

  if (op.importIndex < 1 || op.importIndex > vector.size) {
    return ModuleOpResult.indexOutOfRange;
  }

  // Build export message
  final message = ExportMessage(
    callInfo: CallInfo(ctx.sourceModule, ctx.line, ctx.column),
    scope: [],
    functor: op.functor,
    arity: op.arity,
    args: args,
    ccc: const {},  // Trust mode - no CCC
  );

  // Write to import vector (routes via serve_import)
  vector.write(op.importIndex, message);

  return ModuleOpResult.success;
}

/// Handle Transmit opcode: send RPC to module resolved at runtime
///
/// Following FCP: transmit # {ModuleVar, Goal}
/// Resolves module name from variable, looks up in registry, sends message.
Future<ModuleOpResult> handleTransmit(
  Transmit op,
  Term moduleVar,
  List<Term> args,
  ModuleContext? ctx,
) async {
  if (ctx == null) {
    return ModuleOpResult.noModuleContext;
  }

  // Resolve module name from variable
  final moduleName = _resolveModuleName(moduleVar);
  if (moduleName == null) {
    return ModuleOpResult.variableUnbound;
  }

  // Look up target module (may trigger lazy loading)
  LoadedModule targetModule;
  try {
    targetModule = await ctx.registry.lookupOrLoad(moduleName);
  } catch (e) {
    return ModuleOpResult.moduleNotFound;
  }

  // Build export message
  final message = ExportMessage(
    callInfo: CallInfo(ctx.sourceModule, ctx.line, ctx.column),
    scope: [],
    functor: op.functor,
    arity: op.arity,
    args: args,
    ccc: const {},  // Trust mode - no CCC
  );

  // Send directly to target module's input channel
  targetModule.inputSink.add(message);

  return ModuleOpResult.success;
}

/// Resolve module name from a term
String? _resolveModuleName(Term term) {
  if (term is ConstTerm) {
    final value = term.value;
    if (value is String) {
      return value;
    }
  }
  // VarRef would need dereferencing through the heap
  // For now, only support ground terms
  return null;
}
