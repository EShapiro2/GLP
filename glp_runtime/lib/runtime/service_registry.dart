import 'loaded_module.dart';

/// Runtime registry of loaded modules.
///
/// Provides lookup, registration, and hot reload of modules.
/// This is the central registry for the module system.
class ServiceRegistry {
  final Map<String, LoadedModule> _modules = {};

  /// Lookup module by name.
  LoadedModule? lookup(String name) => _modules[name];

  /// Register a module.
  /// Throws if module with same name already exists.
  void register(LoadedModule module) {
    if (_modules.containsKey(module.name)) {
      throw StateError('Module already registered: ${module.name}');
    }
    _modules[module.name] = module;
  }

  /// Register or replace a module.
  /// Use this for initial loading when you don't care if it exists.
  void registerOrReplace(LoadedModule module) {
    _modules[module.name] = module;
  }

  /// Check if module is loaded.
  bool isLoaded(String name) => _modules.containsKey(name);

  /// Unload a module.
  /// Returns true if module was loaded, false if not found.
  bool unload(String name) => _modules.remove(name) != null;

  /// Reload a module (hot reload).
  /// Replaces existing module with same name.
  void reload(LoadedModule module) {
    _modules[module.name] = module;
  }

  /// Get all loaded module names.
  Iterable<String> get moduleNames => _modules.keys;

  /// Get number of loaded modules.
  int get moduleCount => _modules.length;

  /// Clear all loaded modules.
  void clear() => _modules.clear();

  @override
  String toString() => 'ServiceRegistry(${_modules.length} modules: ${_modules.keys.join(', ')})';
}
