# GLP Module System Implementation Plan

**Version 3.0**

## Overview

This document provides a detailed implementation plan for adding modules to GLP. The implementation follows the FCP/Logix runtime service model with dynamic module loading.

## Current State

GLP currently has:
- Lexer → Parser → AST → Analyzer → CodeGenerator pipeline
- **Instruction objects** (`List<Op>`), not raw bytecode (`List<int>`)
- Single `BytecodeProgram` with all procedures merged
- Global label table

## Architecture Decisions

1. **Runtime service model** (following FCP) - not static linking
2. **Per-module compilation** - each module gets its own instruction list
3. **Instruction objects for now** - serialization added later for multi-agent
4. **Namespace separation** - each module has isolated label table

---

## Phase 1: Per-Module Compilation (Foundation)

**Goal**: Change from single merged program to per-module compilation.

### 1.1 LoadedModule Structure

**File**: `lib/runtime/loaded_module.dart` (new)

```dart
/// A compiled module ready for execution
class LoadedModule {
  final String name;
  final Set<String> exports;           // "proc/arity" signatures
  final List<String> imports;
  final List<Op> instructions;         // This module's code only
  final Map<String, int> labels;       // Local: "factorial/2" → PC
  final Map<String, int> procOffsets;  // "proc/arity" → PC

  LoadedModule({
    required this.name,
    required this.exports,
    required this.imports,
    required this.instructions,
    required this.labels,
    required this.procOffsets,
  });

  /// Check if procedure is exported (empty exports = all exported)
  bool isExported(String proc, int arity) {
    return exports.isEmpty || exports.contains('$proc/$arity');
  }

  /// Get procedure entry point
  int? getProcOffset(String proc, int arity) {
    return procOffsets['$proc/$arity'];
  }
}
```

### 1.2 ServiceRegistry

**File**: `lib/runtime/service_registry.dart` (new)

```dart
/// Runtime registry of loaded modules
class ServiceRegistry {
  final Map<String, LoadedModule> _modules = {};

  /// Lookup module by name
  LoadedModule? lookup(String name) => _modules[name];

  /// Register a module
  void register(LoadedModule module) {
    _modules[module.name] = module;
  }

  /// Check if loaded
  bool isLoaded(String name) => _modules.containsKey(name);

  /// Unload module
  void unload(String name) => _modules.remove(name);

  /// Reload module (hot reload)
  void reload(LoadedModule module) {
    _modules[module.name] = module;
  }

  /// All loaded module names
  Iterable<String> get moduleNames => _modules.keys;
}
```

### 1.3 Compiler Changes

**File**: `lib/compiler/codegen.dart`

Add method to compile single module:

```dart
class CodeGenerator {
  /// Compile to LoadedModule (new - per-module)
  LoadedModule generateModule(AnnotatedProgram program, {
    String? moduleName,
    Set<String>? exports,
    List<String>? imports,
  }) {
    final ctx = CodeGenContext();
    final procOffsets = <String, int>{};

    for (final proc in program.procedures) {
      procOffsets[proc.signature] = ctx.currentPC;
      _generateProcedure(proc, ctx);
    }

    return LoadedModule(
      name: moduleName ?? '_anonymous',
      exports: exports ?? {},        // Empty = all exported
      imports: imports ?? [],
      instructions: List<Op>.from(ctx.instructions),
      labels: Map<String, int>.from(ctx.labels),
      procOffsets: procOffsets,
    );
  }

  /// Existing method - compile to BytecodeProgram (keep for compatibility)
  BytecodeProgram generate(AnnotatedProgram program) {
    // ... existing code ...
  }
}
```

### 1.4 GlpCompiler Updates

**File**: `lib/compiler/compiler.dart`

```dart
class GlpCompiler {
  /// Existing: compile to BytecodeProgram
  BytecodeProgram compile(String source) { ... }

  /// New: compile to LoadedModule
  LoadedModule compileModule(String source, {String? moduleName}) {
    final lexer = _createLexer(source);
    final tokens = lexer.tokenize();
    final parser = _createParser(tokens);
    final ast = parser.parse();
    final analyzer = _createAnalyzer();
    final annotatedAst = analyzer.analyze(ast);
    final codegen = _createCodegen();

    return codegen.generateModule(
      annotatedAst,
      moduleName: moduleName,
    );
  }
}
```

### 1.5 Tasks

| # | Task | File | Status |
|---|------|------|--------|
| 1 | Create LoadedModule class | loaded_module.dart | [ ] |
| 2 | Create ServiceRegistry class | service_registry.dart | [ ] |
| 3 | Add generateModule() to CodeGenerator | codegen.dart | [ ] |
| 4 | Add compileModule() to GlpCompiler | compiler.dart | [ ] |
| 5 | Unit tests for per-module compilation | module_compile_test.dart | [ ] |

---

## Phase 2: Module Syntax

**Goal**: Parse module declarations and `#` operator.

### 2.1 Lexer Changes

**File**: `lib/compiler/lexer.dart`

**New Tokens**:
```dart
enum TokenType {
  // ... existing ...
  HASH,           // #
  // Declaration keywords recognized after '-'
}
```

Recognize `#` token and declaration keywords (`module`, `export`, `import`, `language`, `mode`, `service_type`) after `-`.

### 2.2 AST Changes

**File**: `lib/compiler/ast.dart`

```dart
/// Remote goal: Module # Goal
class RemoteGoal extends Goal {
  final String moduleName;
  final Goal goal;

  RemoteGoal(this.moduleName, this.goal, int line, int col)
      : super(goal.functor, goal.args, line, col);
}

/// Module metadata (parsed from declarations)
class ModuleInfo {
  final String? name;
  final Set<String> exports;
  final List<String> imports;
  final String language;      // 'glp' default
  final String mode;          // 'user' default
  final String serviceType;   // 'procedures' default

  ModuleInfo({
    this.name,
    this.exports = const {},
    this.imports = const [],
    this.language = 'glp',
    this.mode = 'user',
    this.serviceType = 'procedures',
  });
}
```

### 2.3 Parser Changes

**File**: `lib/compiler/parser.dart`

```dart
class Parser {
  /// Parse module declarations at start of file
  ModuleInfo parseModuleInfo() {
    String? name;
    final exports = <String>{};
    final imports = <String>[];
    // ... parse -module, -export, -import, etc.
    return ModuleInfo(name: name, exports: exports, imports: imports);
  }

  /// Parse goal, handling Module # Goal
  Goal _parseGoal() {
    final first = _parseSimpleGoal();
    if (_match(TokenType.HASH)) {
      if (first is! Atom || first.args.isNotEmpty) {
        throw CompileError('Module name must be atom');
      }
      final goal = _parseSimpleGoal();
      return RemoteGoal(first.functor, goal, first.line, first.column);
    }
    return first;
  }
}
```

### 2.4 Tasks

| # | Task | File | Status |
|---|------|------|--------|
| 6 | Add HASH token | lexer.dart | [ ] |
| 7 | Add declaration keyword recognition | lexer.dart | [ ] |
| 8 | Add RemoteGoal AST node | ast.dart | [ ] |
| 9 | Add ModuleInfo class | ast.dart | [ ] |
| 10 | Parse module declarations | parser.dart | [ ] |
| 11 | Parse `#` operator | parser.dart | [ ] |
| 12 | Unit tests for module syntax | module_parser_test.dart | [ ] |

---

## Phase 3: Runtime Module Execution

**Goal**: Execute remote goals via ServiceRegistry.

**Status**: ✅ COMPLETE

### 3.1 CodeGen for RemoteGoal

**File**: `lib/compiler/codegen.dart`

```dart
void _generateBodyGoal(Goal goal, VariableTable varTable, CodeGenContext ctx) {
  if (goal is RemoteGoal) {
    // Setup arguments
    for (int j = 0; j < goal.goal.args.length; j++) {
      _generatePutArgument(goal.goal.args[j], j, varTable, ctx);
    }
    // Emit CallRemote instead of Spawn
    ctx.emit(CallRemote(
      goal.moduleName,
      goal.goal.functor,
      goal.goal.arity,
    ));
  } else {
    // Existing local goal handling
    // ...
  }
}
```

### 3.2 CallRemote Opcode

**File**: `lib/bytecode/opcodes.dart`

```dart
/// Call procedure in another module
class CallRemote implements Op {
  final String moduleName;
  final String procedure;
  final int arity;
  CallRemote(this.moduleName, this.procedure, this.arity);
}
```

### 3.3 Runner Changes

**File**: `lib/bytecode/runner.dart`

```dart
class BytecodeRunner {
  final ServiceRegistry registry;
  LoadedModule? currentModule;

  /// Handle CallRemote
  void _executeCallRemote(CallRemote op) {
    // 1. Lookup module
    final module = registry.lookup(op.moduleName);
    if (module == null) {
      throw RuntimeError('Module not found: ${op.moduleName}');
    }

    // 2. Check export
    if (!module.isExported(op.procedure, op.arity)) {
      throw RuntimeError(
        '${op.procedure}/${op.arity} not exported by ${op.moduleName}'
      );
    }

    // 3. Get procedure offset
    final pc = module.getProcOffset(op.procedure, op.arity);
    if (pc == null) {
      throw RuntimeError(
        '${op.procedure}/${op.arity} not found in ${op.moduleName}'
      );
    }

    // 4. Spawn goal in target module context
    _spawnInModule(module, pc, op.arity);
  }

  void _spawnInModule(LoadedModule module, int pc, int arity) {
    // Create new goal with module context
    // Push to goal queue with reference to module's instructions
  }
}
```

### 3.4 Tasks

| # | Task | File | Status |
|---|------|------|--------|
| 13 | Generate CallRemote for RemoteGoal | codegen.dart | ✅ |
| 14 | Add CallRemote opcode | opcodes.dart | ✅ |
| 15 | Add ServiceRegistry to runner | runner.dart | ✅ |
| 16 | Implement CallRemote handler | runner.dart | ✅ |
| 17 | Module context in goal execution | runner.dart | ✅ |
| 18 | Integration tests | module_execution_test.dart | ✅ |

---

## Phase 3.5: Transition to Module System

**Goal**: Make module system the standard path (following FCP/Logix where everything is a module).

### 3.5.1 REPL Transition

**File**: `udi/glp_repl.dart`

Update REPL to use module system:
- Use `compileModule()` instead of `compile()`
- Register loaded module in ServiceRegistry
- Execute via LoadedModule context

```dart
// Before (old path):
final program = compiler.compile(source);
final runner = BytecodeRunner(program);

// After (module path):
final module = compiler.compileModule(source, moduleName: filename);
rt.serviceRegistry.register(module);
// Execute via module's instructions
```

### 3.5.2 Implicit Anonymous Module

Files without `-module()` declaration become anonymous modules:
- Module name derived from filename (e.g., `merge.glp` → module `merge`)
- All procedures exported by default
- Works transparently with existing .glp files

### 3.5.3 Migration Order

**Important**: All tests must be migrated and passing BEFORE deprecating old code.

1. **First**: Migrate REPL to module system
2. **Second**: Verify all 60 REPL tests pass with module path
3. **Third**: Migrate dart unit tests to module path
4. **Fourth**: Verify all dart tests pass with module path
5. **Only then**: Mark old `compile()` path as deprecated
6. **Finally**: Remove old code when confident

### 3.5.4 Tasks

| # | Task | File | Status |
|---|------|------|--------|
| 18.1 | Update REPL to use compileModule() | glp_repl.dart | ✅ |
| 18.2 | REPL registers module in ServiceRegistry | glp_repl.dart | ✅ |
| 18.3 | Execute via LoadedModule context | glp_repl.dart | ✅ |
| 18.4 | Implicit module name from filename | glp_repl.dart | ✅ |
| 18.5 | Verify all 60 REPL tests pass | run_repl_tests.sh | ✅ |
| 18.6 | Update dart unit tests to use module path | test/*.dart | ✅ |
| 18.7 | Verify all 64 dart tests pass | dart test | ✅ |
| 18.8 | Mark compile() as deprecated (only after all tests pass) | compiler.dart | [ ] |
| 18.9 | Remove old code (only when confident) | *.dart | [ ] |

---

## Phase 4: Service Hierarchy & Hot Reload

**Goal**: FCP-style hierarchical services and development workflow.

### 4.1 Service Hierarchy

```dart
/// Hierarchical module paths: utils.list → [utils, list]
class ServiceHierarchy {
  final ServiceNode root = ServiceNode('root');

  void register(String moduleName, LoadedModule module) {
    final path = moduleName.split('.');
    // Navigate/create path, register module
  }

  LoadedModule? lookup(String moduleName) {
    final path = moduleName.split('.');
    // Navigate path, return module
  }
}
```

### 4.2 Hot Reload

```dart
class ModuleLoader {
  final ServiceRegistry registry;

  /// Reload module from source
  LoadedModule reload(String name, String source) {
    final module = compiler.compileModule(source, moduleName: name);
    registry.reload(module);
    return module;
  }
}
```

### 4.3 Tasks

| # | Task | File | Status |
|---|------|------|--------|
| 19 | ServiceHierarchy class | service_hierarchy.dart | [ ] |
| 20 | Hierarchical path resolution | service_registry.dart | [ ] |
| 21 | ModuleLoader with reload | module_loader.dart | [ ] |
| 22 | Hot reload tests | hot_reload_test.dart | [ ] |

---

## Phase 5: Multi-Agent Serialization

**Goal**: Serialize modules for network transfer between agents.

### 5.1 Instruction Serialization

```dart
class ModuleSerializer {
  /// Serialize LoadedModule to bytes
  List<int> serialize(LoadedModule module) {
    // 1. Serialize metadata (name, exports, imports)
    // 2. Serialize each Op to bytes
    // 3. Serialize label table
    return bytes;
  }

  /// Deserialize bytes to LoadedModule
  LoadedModule deserialize(List<int> bytes) {
    // Reconstruct LoadedModule
  }
}
```

### 5.2 Opcode Encoding

Define numeric opcodes for serialization:

```dart
const opcodeMap = {
  'ClauseTry': 0x01,
  'Commit': 0x02,
  'Proceed': 0x03,
  'Spawn': 0x04,
  'CallRemote': 0x05,
  // ...
};
```

### 5.3 Tasks

| # | Task | File | Status |
|---|------|------|--------|
| 23 | Define opcode numbers | opcodes.dart | [ ] |
| 24 | ModuleSerializer class | module_serializer.dart | [ ] |
| 25 | Op → bytes encoding | module_serializer.dart | [ ] |
| 26 | bytes → Op decoding | module_serializer.dart | [ ] |
| 27 | Serialization tests | serialization_test.dart | [ ] |

---

## Implementation Order

```
Phase 1: Per-Module Compilation (Foundation) ✅
├── LoadedModule class
├── ServiceRegistry
├── generateModule() in CodeGenerator
└── compileModule() in GlpCompiler
    ↓
Phase 2: Module Syntax ✅
├── Lexer: # token, declaration keywords
├── AST: RemoteGoal, ModuleInfo
└── Parser: declarations, # operator
    ↓
Phase 3: Runtime Execution ✅
├── CodeGen: RemoteGoal → CallRemote
├── Opcodes: CallRemote
└── Runner: module context, CallRemote handler
    ↓
Phase 3.5: Transition to Module System ✅
├── Update REPL to use compileModule()
├── Implicit module name from filename
├── Deprecate old compile() path (pending)
└── All execution via LoadedModule
    ↓
Phase 4: Hierarchy & Hot Reload
├── ServiceHierarchy
└── ModuleLoader with reload
    ↓
Phase 5: Multi-Agent Serialization
├── Opcode numbers
└── ModuleSerializer
```

---

## Compatibility

- Programs without `-module` declaration become anonymous modules
- All procedures exported by default (no `-export` = export all)
- Existing single-file programs work unchanged

---

## Task Summary

| Phase | Tasks | Priority | Status |
|-------|-------|----------|--------|
| 1 | 1-5 | Must have | ✅ Complete |
| 2 | 6-12 | Must have | ✅ Complete |
| 3 | 13-18 | Must have | ✅ Complete |
| 3.5 | 18.1-18.7 | Must have | ✅ Complete |
| 4 | 19-22 | Should have | ⬜ Pending |
| 5 | 23-27 | For multi-agent | ⬜ Pending |

---

*Document Version: 3.1*
*Updated: Added Phase 3.5 - Transition to Module System*
*Following FCP/Logix: everything is a module, module system becomes the standard path*
