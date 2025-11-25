# GLP Module System Implementation Plan

**Version 2.0**

## Overview

This document provides a detailed implementation plan for adding modules to GLP. The implementation follows the FCP/Logix runtime service model with dynamic module loading.

## Architecture Decision: Runtime Service Model

GLP uses **runtime service resolution** (following FCP):
- Modules loaded dynamically on first use
- `Module # Goal` resolved at runtime via ServiceRegistry
- Supports hot reloading and multi-agent distribution
- ServiceRegistry manages loaded modules

**NOT using static linking** - compile-time resolution was rejected because:
1. GLP will be multi-agent with modules sent across agents
2. Dynamic loading required for hot reloading
3. FCP uses runtime resolution

---

## Phase 1: Core Runtime Module System

### 1.1 Lexer Changes

**File**: `lib/compiler/lexer.dart`

**New Tokens**:
```dart
enum TokenType {
  // ... existing tokens ...

  // Module tokens
  HASH,           // #
  MODULE,         // module (after -)
  EXPORT,         // export (after -)
  IMPORT,         // import (after -)
  LANGUAGE,       // language (after -)
  MODE,           // mode (after -)
  SERVICE_TYPE,   // service_type (after -)
}
```

**Tasks**:
- [ ] Add `HASH` token recognition (`#`)
- [ ] Add keyword recognition for `module`, `export`, `import` after `-`
- [ ] Add keyword recognition for `language`, `mode`, `service_type` after `-`
- [ ] Handle hierarchical module names (`utils.list`)

---

### 1.2 AST Changes

**File**: `lib/compiler/ast.dart`

**New AST Nodes**:

```dart
/// Module declaration: -module(name).
class ModuleDeclaration extends ASTNode {
  final String name;
  ModuleDeclaration(this.name, int line, int col) : super(line, col);
}

/// Export declaration: -export([proc/arity, ...]).
class ExportDeclaration extends ASTNode {
  final List<ProcedureRef> exports;
  ExportDeclaration(this.exports, int line, int col) : super(line, col);
}

/// Import declaration: -import([module, ...]).
class ImportDeclaration extends ASTNode {
  final List<String> imports;
  ImportDeclaration(this.imports, int line, int col) : super(line, col);
}

/// Language declaration: -language(mode).
class LanguageDeclaration extends ASTNode {
  final String language;  // 'glp' or 'compound'
  LanguageDeclaration(this.language, int line, int col) : super(line, col);
}

/// Mode declaration: -mode(security).
class ModeDeclaration extends ASTNode {
  final String mode;  // 'trust' or 'user'
  ModeDeclaration(this.mode, int line, int col) : super(line, col);
}

/// Service type declaration: -service_type(type).
class ServiceTypeDeclaration extends ASTNode {
  final String serviceType;  // 'procedures', 'monitor', 'director'
  ServiceTypeDeclaration(this.serviceType, int line, int col) : super(line, col);
}

/// Reference to a procedure: name/arity
class ProcedureRef {
  final String name;
  final int arity;
  ProcedureRef(this.name, this.arity);
  String get signature => '$name/$arity';
}

/// Remote goal: Module # Goal
class RemoteGoal extends Goal {
  final String moduleName;
  final Goal goal;

  RemoteGoal(this.moduleName, this.goal, int line, int col)
      : super('${moduleName}#${goal.functor}', goal.args, line, col);
}

/// A complete module (compilation unit)
class ModuleAST extends ASTNode {
  final String name;
  final Set<String> exports;      // "name/arity" strings
  final List<String> imports;
  final String language;          // 'glp' default
  final String mode;              // 'user' default
  final String serviceType;       // 'procedures' default
  final List<Procedure> procedures;

  ModuleAST({
    required this.name,
    required this.exports,
    required this.imports,
    required this.language,
    required this.mode,
    required this.serviceType,
    required this.procedures,
    required int line,
    required int col,
  }) : super(line, col);

  bool isExported(String procName, int arity) {
    return exports.isEmpty || exports.contains('$procName/$arity');
  }
}
```

**Tasks**:
- [ ] Add all declaration AST classes
- [ ] Add `RemoteGoal` class
- [ ] Add `ModuleAST` class
- [ ] Modify `Program` to reference modules

---

### 1.3 Parser Changes

**File**: `lib/compiler/parser.dart`

**New Parsing Methods**:

```dart
/// Parse a module file
ModuleAST parseModule() {
  String? moduleName;
  final exports = <ProcedureRef>[];
  final imports = <String>[];
  String language = 'glp';
  String mode = 'user';
  String serviceType = 'procedures';

  // Parse declarations at start of file
  while (_checkDeclaration()) {
    if (_match(TokenType.MINUS)) {
      if (_match(TokenType.MODULE)) {
        moduleName = _parseModuleDecl();
      } else if (_match(TokenType.EXPORT)) {
        exports.addAll(_parseExportDecl());
      } else if (_match(TokenType.IMPORT)) {
        imports.addAll(_parseImportDecl());
      } else if (_match(TokenType.LANGUAGE)) {
        language = _parseLanguageDecl();
      } else if (_match(TokenType.MODE)) {
        mode = _parseModeDecl();
      } else if (_match(TokenType.SERVICE_TYPE)) {
        serviceType = _parseServiceTypeDecl();
      }
    }
  }

  // Parse procedures
  final procedures = <Procedure>[];
  while (!_isAtEnd()) {
    procedures.add(_parseProcedure());
  }

  final name = moduleName ?? '_anonymous_${_fileId}';
  final exportSet = exports.isEmpty
      ? <String>{}  // Empty = all exported
      : exports.map((e) => e.signature).toSet();

  return ModuleAST(
    name: name,
    exports: exportSet,
    imports: imports,
    language: language,
    mode: mode,
    serviceType: serviceType,
    procedures: procedures,
    line: 1,
    col: 1,
  );
}

/// Parse goal, handling Module # Goal syntax
Goal _parseGoal() {
  final first = _parseSimpleGoal();

  if (_match(TokenType.HASH)) {
    // This is a remote call: Module # Goal
    if (first is! Atom) {
      throw CompileError('Module name must be an atom', first.line, first.column);
    }
    final goal = _parseSimpleGoal();
    return RemoteGoal(first.functor, goal, first.line, first.column);
  }

  return first;
}
```

**Tasks**:
- [ ] Add `parseModule()` method
- [ ] Add all declaration parsing methods
- [ ] Modify `_parseGoal()` to handle `#` operator

---

### 1.4 Service Registry

**File**: `lib/runtime/service_registry.dart` (new file)

```dart
/// Runtime registry of loaded modules
class ServiceRegistry {
  final Map<String, LoadedModule> _modules = {};

  /// Lookup a module by name
  LoadedModule? lookup(String name) => _modules[name];

  /// Register a loaded module
  void register(LoadedModule module) {
    _modules[module.name] = module;
  }

  /// Check if module is loaded
  bool isLoaded(String name) => _modules.containsKey(name);

  /// Unload a module
  void unload(String name) {
    _modules.remove(name);
  }

  /// Reload a module (for hot reloading)
  void reload(String name, LoadedModule module) {
    _modules[name] = module;
  }

  /// Get all loaded module names
  Iterable<String> get loadedModules => _modules.keys;
}

/// A module loaded into the runtime
class LoadedModule {
  final String name;
  final Set<String> exports;      // "name/arity" signatures
  final String language;
  final String mode;
  final String serviceType;
  final List<int> bytecode;
  final Map<String, int> procedureOffsets;  // "name/arity" -> bytecode offset

  LoadedModule({
    required this.name,
    required this.exports,
    required this.language,
    required this.mode,
    required this.serviceType,
    required this.bytecode,
    required this.procedureOffsets,
  });

  /// Check if procedure is exported
  bool isExported(String name, int arity) {
    return exports.isEmpty || exports.contains('$name/$arity');
  }

  /// Get procedure offset, or null if not found
  int? getProcedureOffset(String name, int arity) {
    return procedureOffsets['$name/$arity'];
  }
}
```

**Tasks**:
- [ ] Create `service_registry.dart`
- [ ] Implement `ServiceRegistry` class
- [ ] Implement `LoadedModule` class

---

### 1.5 Module Loader

**File**: `lib/runtime/module_loader.dart` (new file)

```dart
/// Loads and compiles modules
class ModuleLoader {
  final ServiceRegistry registry;
  final ModuleCompiler compiler;

  ModuleLoader(this.registry, this.compiler);

  /// Load module from source
  LoadedModule load(String name, String source) {
    // Parse
    final lexer = Lexer(source);
    final tokens = lexer.scanTokens();
    final parser = Parser(tokens);
    final ast = parser.parseModule();

    // Validate module name matches
    if (ast.name != name && ast.name != '_anonymous_$name') {
      throw ModuleError(
        'Module declares name "${ast.name}" but loaded as "$name"'
      );
    }

    // Compile
    final compiled = compiler.compileModule(ast);

    // Register
    registry.register(compiled);

    return compiled;
  }

  /// Load module from file path
  Future<LoadedModule> loadFile(String path) async {
    final source = await File(path).readAsString();
    final name = _pathToModuleName(path);
    return load(name, source);
  }

  /// Reload a module
  LoadedModule reload(String name, String source) {
    final module = load(name, source);
    registry.reload(name, module);
    return module;
  }

  String _pathToModuleName(String path) {
    final filename = path.split('/').last;
    return filename.endsWith('.glp')
        ? filename.substring(0, filename.length - 4)
        : filename;
  }
}
```

**Tasks**:
- [ ] Create `module_loader.dart`
- [ ] Implement `ModuleLoader` class
- [ ] Implement file loading
- [ ] Implement hot reload

---

### 1.6 Compiler Changes

**File**: `lib/compiler/codegen.dart`

**Module Compilation**:

```dart
class ModuleCompiler {
  /// Compile a module AST to loaded module
  LoadedModule compileModule(ModuleAST ast) {
    // Validate exports
    for (final export in ast.exports) {
      final parts = export.split('/');
      final name = parts[0];
      final arity = int.parse(parts[1]);

      final found = ast.procedures.any(
        (p) => p.name == name && p.arity == arity
      );
      if (!found) {
        throw CompileError('Export "$export" not defined in module ${ast.name}');
      }
    }

    // Compile procedures
    final bytecode = <int>[];
    final procedureOffsets = <String, int>{};

    for (final proc in ast.procedures) {
      procedureOffsets['${proc.name}/${proc.arity}'] = bytecode.length;
      bytecode.addAll(_compileProc(proc, ast));
    }

    return LoadedModule(
      name: ast.name,
      exports: ast.exports,
      language: ast.language,
      mode: ast.mode,
      serviceType: ast.serviceType,
      bytecode: bytecode,
      procedureOffsets: procedureOffsets,
    );
  }
}
```

**Tasks**:
- [ ] Add `compileModule()` method
- [ ] Compile `RemoteGoal` to runtime lookup

---

### 1.7 Runtime Changes

**File**: `lib/bytecode/runner.dart`

**Remote Goal Execution**:

```dart
class BytecodeRunner {
  final ServiceRegistry registry;
  final ModuleLoader loader;

  /// Execute a remote goal: Module # Goal
  void executeRemoteGoal(RemoteGoalInfo info) {
    final moduleName = info.moduleName;
    final goalName = info.goalName;
    final goalArity = info.arity;

    // Lookup module
    var module = registry.lookup(moduleName);

    // Load if not present
    if (module == null) {
      module = loader.loadByName(moduleName);
      if (module == null) {
        throw RuntimeError('Module not found: $moduleName');
      }
    }

    // Check export
    if (!module.isExported(goalName, goalArity)) {
      throw RuntimeError(
        '$goalName/$goalArity not exported by $moduleName'
      );
    }

    // Get procedure offset
    final offset = module.getProcedureOffset(goalName, goalArity);
    if (offset == null) {
      throw RuntimeError(
        '$goalName/$goalArity not found in $moduleName'
      );
    }

    // Execute in module context
    _executeInModule(module, offset, info.args);
  }
}
```

**Tasks**:
- [ ] Add `executeRemoteGoal()` method
- [ ] Add module context switching
- [ ] Integrate with existing goal execution

---

### 1.8 Bytecode Extensions

**New Opcode**:

```dart
enum Opcode {
  // ... existing opcodes ...

  /// Call procedure in another module
  /// CallRemote <moduleNameIndex> <procNameIndex> <arity>
  CallRemote,
}
```

**CallRemote Execution**:
1. Read module name from constant pool
2. Read procedure name from constant pool
3. Resolve module via ServiceRegistry
4. Verify export
5. Push call frame with module context
6. Jump to procedure in target module

**Tasks**:
- [ ] Add `CallRemote` opcode
- [ ] Implement handler in runner
- [ ] Add module context to call frame

---

## Phase 2: Service Hierarchy

### 2.1 Service Hierarchy

```dart
/// Hierarchical service tree (FCP-style)
class ServiceHierarchy {
  final ServiceNode root = ServiceNode('root');

  /// Register module at path
  void register(List<String> path, LoadedModule module) {
    var node = root;
    for (final segment in path.take(path.length - 1)) {
      node = node.getOrCreateChild(segment);
    }
    node.setModule(path.last, module);
  }

  /// Lookup module by path
  LoadedModule? lookup(List<String> path) {
    var node = root;
    for (final segment in path.take(path.length - 1)) {
      node = node.getChild(segment);
      if (node == null) return null;
    }
    return node.getModule(path.last);
  }
}

class ServiceNode {
  final String name;
  final Map<String, ServiceNode> children = {};
  final Map<String, LoadedModule> modules = {};

  ServiceNode(this.name);

  ServiceNode getOrCreateChild(String name) {
    return children.putIfAbsent(name, () => ServiceNode(name));
  }

  ServiceNode? getChild(String name) => children[name];

  void setModule(String name, LoadedModule module) {
    modules[name] = module;
  }

  LoadedModule? getModule(String name) => modules[name];
}
```

### 2.2 Hierarchical Resolution

```dart
/// Convert module name to path
List<String> moduleNameToPath(String name) {
  return name.split('.');  // "utils.list" -> ["utils", "list"]
}
```

**Tasks**:
- [ ] Implement `ServiceHierarchy`
- [ ] Implement path-based resolution
- [ ] Update loader to use hierarchy

---

## Phase 3: Monitor Services (Stateful)

### 3.1 Monitor Module Support

```dart
/// Monitor module with state
class MonitorModule extends LoadedModule {
  dynamic state;
  final List<dynamic> requestQueue = [];

  MonitorModule({...}) : super(...);

  /// Handle request to monitor
  void handleRequest(dynamic request) {
    requestQueue.add(request);
    _processQueue();
  }
}
```

**Tasks**:
- [ ] Implement `MonitorModule` class
- [ ] Implement state management
- [ ] Implement request queue processing

---

## Phase 4: Multi-Agent Support

### 4.1 Module Serialization

```dart
/// Serialize module for transfer
class ModuleSerializer {
  /// Serialize module to bytes
  List<int> serialize(LoadedModule module) {
    // Serialize: name, exports, bytecode, metadata
  }

  /// Deserialize module from bytes
  LoadedModule deserialize(List<int> bytes) {
    // Reconstruct LoadedModule
  }
}
```

### 4.2 Remote Module Loading

```dart
/// Load module from remote agent
Future<LoadedModule> loadFromAgent(String agentId, String moduleName) async {
  // Request module from agent
  // Receive serialized module
  // Deserialize and register
}
```

**Tasks**:
- [ ] Implement serialization
- [ ] Implement remote loading protocol
- [ ] Implement agent communication

---

## Testing Plan

### Unit Tests

**File**: `test/compiler/module_test.dart`

```dart
void main() {
  group('Module Lexer', () {
    test('recognizes # token', () { ... });
    test('recognizes all declaration tokens', () { ... });
  });

  group('Module Parser', () {
    test('parses module declaration', () { ... });
    test('parses export list', () { ... });
    test('parses import list', () { ... });
    test('parses language declaration', () { ... });
    test('parses mode declaration', () { ... });
    test('parses service_type declaration', () { ... });
    test('parses remote goal', () { ... });
  });

  group('Service Registry', () {
    test('registers and lookups module', () { ... });
    test('reloads module', () { ... });
    test('unloads module', () { ... });
  });

  group('Module Loader', () {
    test('loads module from source', () { ... });
    test('validates exports', () { ... });
    test('hot reloads module', () { ... });
  });
}
```

### Integration Tests

**File**: `test/integration/module_integration_test.dart`

```dart
test('remote goal execution', () {
  final mathSource = '''
    -module(math).
    -export([double/2]).
    double(X?, Y) :- Y := X? * 2.
  ''';

  final mainSource = '''
    -module(main).
    -import([math]).
    test(R) :- math # double(5, R).
  ''';

  final registry = ServiceRegistry();
  final loader = ModuleLoader(registry, ModuleCompiler());

  loader.load('math', mathSource);
  loader.load('main', mainSource);

  final result = execute(registry, 'main', 'test', [_]);
  expect(result, equals(10));
});
```

---

## Task Summary

### Phase 1: Core Runtime (Must Have)

| # | Task | File | Status |
|---|------|------|--------|
| 1 | Add HASH and declaration tokens | lexer.dart | [ ] |
| 2 | Add all declaration AST nodes | ast.dart | [ ] |
| 3 | Add RemoteGoal AST | ast.dart | [ ] |
| 4 | Add ModuleAST | ast.dart | [ ] |
| 5 | Parse all declarations | parser.dart | [ ] |
| 6 | Parse remote goal (M # G) | parser.dart | [ ] |
| 7 | Create ServiceRegistry | service_registry.dart | [ ] |
| 8 | Create ModuleLoader | module_loader.dart | [ ] |
| 9 | Add compileModule() | codegen.dart | [ ] |
| 10 | Add CallRemote opcode | opcodes.dart | [ ] |
| 11 | Implement remote goal execution | runner.dart | [ ] |
| 12 | Unit tests | module_test.dart | [ ] |
| 13 | Integration tests | module_integration_test.dart | [ ] |

### Phase 2: Service Hierarchy

| # | Task | Priority |
|---|------|----------|
| 14 | ServiceHierarchy class | Medium |
| 15 | Path-based resolution | Medium |
| 16 | Director modules | Medium |

### Phase 3: Monitor Services

| # | Task | Priority |
|---|------|----------|
| 17 | MonitorModule class | Medium |
| 18 | State management | Medium |
| 19 | Request queue | Medium |

### Phase 4: Multi-Agent

| # | Task | Priority |
|---|------|----------|
| 20 | Module serialization | Low |
| 21 | Remote loading | Low |
| 22 | Agent communication | Low |

---

## Implementation Order

```
Phase 1 (Core):
1. Lexer tokens
   ↓
2. AST nodes
   ↓
3. Parser
   ↓
4. ServiceRegistry
   ↓
5. ModuleLoader
   ↓
6. Compiler (module support)
   ↓
7. Runtime (CallRemote)
   ↓
8. Testing

Phase 2 (Hierarchy):
9. ServiceHierarchy
   ↓
10. Path resolution
   ↓
11. Director support

Phase 3 (Monitors):
12. MonitorModule
   ↓
13. State management

Phase 4 (Multi-Agent):
14. Serialization
   ↓
15. Remote loading
```

---

*Document Version: 2.0*
*Updated for runtime service model (not static linking)*
*Following FCP/Logix architecture*
