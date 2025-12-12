/// ModuleLoader: Loads GLP modules from files
/// Following FCP get_module.cp search order and loading logic

import 'dart:io';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/analyzer.dart';
import 'package:glp_runtime/compiler/codegen.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'loaded_module.dart';
import 'import_vector.dart';

/// Error thrown when a module cannot be found
class ModuleNotFoundError implements Exception {
  final String moduleName;
  final List<String> searchedPaths;

  ModuleNotFoundError(this.moduleName, this.searchedPaths);

  @override
  String toString() =>
      'ModuleNotFoundError: Module "$moduleName" not found. Searched: $searchedPaths';
}

/// Error thrown when module compilation fails
class ModuleCompileError implements Exception {
  final String moduleName;
  final String message;

  ModuleCompileError(this.moduleName, this.message);

  @override
  String toString() => 'ModuleCompileError: Failed to compile "$moduleName": $message';
}

/// Loads modules from files following FCP search order
class ModuleLoader {
  /// Base path for module search
  final String basePath;

  /// Callback to register loaded modules
  final void Function(LoadedModule) onModuleLoaded;

  ModuleLoader({
    required this.basePath,
    required this.onModuleLoaded,
  });

  /// Load module by name (FCP get_module.cp search order)
  ///
  /// Search order:
  /// 1. Bin/<name>.glpc  (compiled in Bin subdirectory)
  /// 2. <name>.glpc      (compiled)
  /// 3. <name>.glp       (source - compile on load)
  Future<LoadedModule> load(String name) async {
    // Build search paths (FCP convention)
    final paths = [
      '$basePath/Bin/$name.glpc',  // Compiled in Bin/
      '$basePath/$name.glpc',       // Compiled
      '$basePath/$name.glp',        // Source
    ];

    for (final path in paths) {
      final file = File(path);
      if (await file.exists()) {
        if (path.endsWith('.glpc')) {
          return _loadCompiled(path, name);
        } else {
          return _compileAndLoad(path, name);
        }
      }
    }

    throw ModuleNotFoundError(name, paths);
  }

  /// Load pre-compiled module from .glpc file
  Future<LoadedModule> _loadCompiled(String path, String name) async {
    // TODO: Implement bytecode deserialization when binary format is defined
    // For now, throw unsupported
    throw UnimplementedError('Binary module loading not yet implemented');
  }

  /// Compile source and load module
  Future<LoadedModule> _compileAndLoad(String path, String name) async {
    try {
      final source = await File(path).readAsString();
      return _compileSource(source, name);
    } catch (e) {
      throw ModuleCompileError(name, e.toString());
    }
  }

  /// Compile GLP source code to a LoadedModule
  LoadedModule _compileSource(String source, String name) {
    // Tokenize
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();

    // Parse (using module-aware parser)
    final parser = Parser(tokens);
    final module = parser.parseModule();

    // Analyze
    final analyzer = Analyzer();
    final annotatedProgram = analyzer.analyze(module.toProgram());

    // Generate bytecode
    final generator = CodeGenerator();
    final bytecode = generator.generate(annotatedProgram);

    // Extract exports from module declarations
    // Default: if no -export declarations, export ALL procedures (backwards compatibility)
    Set<String> exports;
    if (module.exports.isEmpty) {
      // No explicit exports - export all procedures
      exports = <String>{};
      for (final proc in module.procedures) {
        exports.add('${proc.name}/${proc.arity}');
      }
    } else {
      // Explicit exports - only export listed procedures
      exports = <String>{};
      for (final exportDecl in module.exports) {
        for (final procRef in exportDecl.exports) {
          exports.add(procRef.signature);
        }
      }
    }

    // Extract imports (default: empty list if no -import declarations)
    final imports = module.importedModules;

    // Create LoadedModule
    final loadedModule = LoadedModule(
      name: name,
      bytecode: bytecode,
      exports: exports,
      imports: imports,
    );

    // Create import vector if there are imports
    if (imports.isNotEmpty) {
      final result = ImportVector.make(imports.length);
      loadedModule.importVector = result.vector;
      // Note: result.readers will be consumed by serve_import processes (Phase 4)
    }

    // Notify registry
    onModuleLoaded(loadedModule);

    return loadedModule;
  }

  /// Compile from in-memory source (for testing and REPL)
  LoadedModule compileFromSource(String source, String name) {
    return _compileSource(source, name);
  }
}

// Extension to convert Module AST to Program for analyzer
extension ModuleToProgram on Module {
  Program toProgram() {
    return Program(procedures, line, column);
  }
}
