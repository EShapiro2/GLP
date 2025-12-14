/// ModuleRegistry: Global registry of loaded modules
/// Following FCP domain_server.cp module management
///
/// Supports:
/// - Registration of loaded modules
/// - Lookup by name
/// - Lazy loading (lookupOrLoad triggers load if not present)

import 'loaded_module.dart';
import 'module_loader.dart';

/// Global registry of loaded modules
class FcpModuleRegistry {
  /// Map of module name to loaded module
  final Map<String, LoadedModule> _modules = {};

  /// Module loader for lazy loading
  ModuleLoader? _loader;

  /// Set the module loader (must be set before lookupOrLoad is called)
  void setLoader(ModuleLoader loader) {
    _loader = loader;
  }

  /// Register a loaded module
  void register(LoadedModule module) {
    _modules[module.name] = module;
  }

  /// Lookup module by name
  /// Returns null if not loaded
  LoadedModule? lookup(String name) => _modules[name];

  /// Check if a module is loaded
  bool isLoaded(String name) => _modules.containsKey(name);

  /// Lookup or load module (lazy loading)
  /// This is the primary interface for resolving modules at runtime
  Future<LoadedModule> lookupOrLoad(String name) async {
    // Check if already loaded
    var module = _modules[name];
    if (module != null) {
      return module;
    }

    // Load the module
    if (_loader == null) {
      throw StateError('ModuleRegistry has no loader set - cannot load module "$name"');
    }

    module = await _loader!.load(name);
    _modules[name] = module;
    return module;
  }

  /// Get all loaded module names
  Iterable<String> get loadedModules => _modules.keys;

  /// Get count of loaded modules
  int get moduleCount => _modules.length;

  /// Close all modules and cleanup
  void close() {
    for (final module in _modules.values) {
      module.close();
    }
    _modules.clear();
  }

  @override
  String toString() => 'FcpModuleRegistry(${_modules.keys.join(", ")})';
}
