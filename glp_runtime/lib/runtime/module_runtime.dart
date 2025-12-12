/// ModuleRuntime: Orchestrates the GLP module system
/// Following FCP boot sequence (domain_server.cp)
///
/// Entry point: `root_module # boot(Args)`
/// - Loads root module
/// - Creates infrastructure (vector, serve_imports, dispatcher)
/// - Executes boot/1

import 'dart:async';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart' show GoalRef;
import 'module_registry.dart';
import 'module_loader.dart';
import 'loaded_module.dart';
import 'import_vector.dart';
import 'serve_import.dart';
import 'dispatcher.dart';
import 'module_messages.dart';

/// Result of module execution
enum ModuleBootResult {
  success,
  moduleNotFound,
  procedureNotExported,
  executionError,
}

/// Boot result with details
class BootResult {
  final ModuleBootResult status;
  final String? error;
  final Term? result;

  BootResult.success(this.result) : status = ModuleBootResult.success, error = null;
  BootResult.error(ModuleBootResult this.status, this.error) : result = null;

  bool get isSuccess => status == ModuleBootResult.success;
}

/// Module system runtime orchestrator
class ModuleRuntime {
  /// Module registry
  final FcpModuleRegistry registry = FcpModuleRegistry();

  /// Module loader
  late final ModuleLoader loader;

  /// GLP runtime for goal execution
  final GlpRuntime rt;

  /// Active serve_import processes
  final List<ServeImport> _serveImports = [];

  /// Active dispatchers
  final Map<String, Dispatcher> _dispatchers = {};

  /// Debug output flag
  bool debugOutput = false;

  /// Error log
  final List<String> errors = [];

  ModuleRuntime({
    required String basePath,
    required this.rt,
  }) {
    loader = ModuleLoader(
      basePath: basePath,
      onModuleLoaded: _onModuleLoaded,
    );
    registry.setLoader(loader);
  }

  /// Callback when a module is loaded
  void _onModuleLoaded(LoadedModule module) {
    if (debugOutput) {
      print('[MODULE] Loaded: ${module.name}');
      print('  Exports: ${module.exports}');
      print('  Imports: ${module.imports}');
    }
  }

  /// Boot the module system with a root module
  ///
  /// Entry point: `rootModule # boot(args)`
  Future<BootResult> boot(String rootModule, {List<Term> args = const []}) async {
    try {
      // Load root module
      final module = await registry.lookupOrLoad(rootModule);

      // Set up infrastructure for this module
      await _setupModuleInfrastructure(module);

      // Check boot/1 is exported
      final bootSig = 'boot/${args.isEmpty ? 1 : args.length}';
      if (!module.isExported(bootSig)) {
        return BootResult.error(
          ModuleBootResult.procedureNotExported,
          'Module $rootModule does not export $bootSig',
        );
      }

      // Create boot message
      final bootMessage = ExportMessage.trust(
        sourceModule: '_system',
        functor: 'boot',
        arity: args.isEmpty ? 1 : args.length,
        args: args.isEmpty ? [VarRef(rt.heap.nextVarId++, isReader: false)] : args,
      );

      // Send to module's input channel
      module.inputSink.add(bootMessage);

      // Run until completion
      await _drainGoals();

      return BootResult.success(null);
    } on ModuleNotFoundError catch (e) {
      return BootResult.error(ModuleBootResult.moduleNotFound, e.toString());
    } catch (e) {
      return BootResult.error(ModuleBootResult.executionError, e.toString());
    }
  }

  /// Set up infrastructure for a module (vector, serve_imports, dispatcher)
  Future<void> _setupModuleInfrastructure(LoadedModule module) async {
    // Skip if already set up
    if (_dispatchers.containsKey(module.name)) return;

    // Create import vector and serve_import processes
    if (module.imports.isNotEmpty) {
      final result = ImportVector.make(module.imports.length);
      module.importVector = result.vector;

      // Start serve_import processes
      final processes = startServeImports(
        readers: result.readers,
        importNames: module.imports,
        registry: registry,
        onError: (error, msg) {
          errors.add('[${module.name}] $error');
          if (debugOutput) print('[ERROR] ${module.name}: $error');
        },
      );
      _serveImports.addAll(processes);
    }

    // Create dispatcher
    final dispatcher = startDispatcher(
      module: module,
      executor: _executeGoal,
      onError: (error) {
        errors.add('[${module.name}] $error');
        if (debugOutput) print('[ERROR] ${module.name}: $error');
      },
    );
    _dispatchers[module.name] = dispatcher;
  }

  /// Execute a goal in module context
  Future<void> _executeGoal(ExportMessage message, LoadedModule module) async {
    // Get entry point for procedure
    final signature = message.signature;
    final entryPc = module.getEntryPoint(signature);
    if (entryPc == null) {
      errors.add('No entry point for $signature in ${module.name}');
      return;
    }

    // Create environment from message args
    final env = CallEnv(
      args: {for (int i = 0; i < message.args.length; i++) i: message.args[i]},
    );

    // Create goal
    final goalId = rt.nextGoalId++;
    final goalRef = GoalRef(goalId, entryPc);

    // Register with runtime
    rt.setGoalEnv(goalId, env);
    rt.setGoalProgram(goalId, module.bytecode);

    // Set module context for this goal (for distribute/transmit handlers)
    rt.setGoalModuleContext(goalId, ModuleGoalContext(
      module: module,
      registry: registry,
    ));

    // Enqueue goal
    rt.gq.enqueue(goalRef);

    if (debugOutput) {
      print('[EXECUTE] ${module.name}::$signature goal=$goalId');
    }
  }

  /// Drain goal queue until empty
  Future<void> _drainGoals({int maxCycles = 10000}) async {
    var cycles = 0;
    while (rt.gq.length > 0 && cycles < maxCycles) {
      final goalRef = rt.gq.dequeue();
      if (goalRef == null) break;

      final env = rt.getGoalEnv(goalRef.id);
      final prog = rt.getGoalProgram(goalRef.id);
      if (env == null || prog == null) {
        errors.add('Missing env/prog for goal ${goalRef.id}');
        continue;
      }

      // Get module context if available
      final moduleCtx = rt.getGoalModuleContext(goalRef.id);

      final runner = BytecodeRunner(prog as BytecodeProgram);
      final ctx = RunnerContext(
        rt: rt,
        goalId: goalRef.id,
        kappa: goalRef.pc,
        env: env,
        debugOutput: debugOutput,
        moduleContext: moduleCtx,
      );

      runner.runWithStatus(ctx);
      cycles++;

      // Allow async processing (serve_import, dispatcher)
      await Future.delayed(Duration.zero);
    }
  }

  /// Shutdown the module runtime
  void shutdown() {
    // Stop serve_import processes
    for (final process in _serveImports) {
      process.stop();
    }
    _serveImports.clear();

    // Stop dispatchers
    for (final dispatcher in _dispatchers.values) {
      dispatcher.stop();
    }
    _dispatchers.clear();

    // Close registry
    registry.close();
  }
}

/// Module context for goal execution (used by distribute/transmit handlers)
class ModuleGoalContext {
  final LoadedModule module;
  final FcpModuleRegistry registry;

  ModuleGoalContext({required this.module, required this.registry});
}
