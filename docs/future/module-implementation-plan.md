# GLP Module System Implementation Plan

## Overview

This document provides a detailed implementation plan for adding modules to GLP. The implementation is divided into phases, with Phase 1 providing a complete static module system.

## Architecture Decision: Static Linking

For Phase 1, we use **static linking**:
- All modules compiled and linked at load time
- `Module # Goal` compiles to direct procedure call
- No runtime module resolution overhead
- Simple implementation

This is sufficient for most GLP programs. Dynamic loading can be added later.

---

## Phase 1: Static Module System

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
}
```

**Tasks**:
- [ ] Add `HASH` token recognition (`#`)
- [ ] Add keyword recognition for `module`, `export`, `import` after `-`
- [ ] Handle hierarchical module names (`utils.list` as single atom or dotted sequence)

**Implementation Notes**:
```dart
// In scanToken():
case '#':
  addToken(TokenType.HASH);
  break;

// After '-', check for declaration keywords:
if (_match('-')) {
  final word = _identifier();
  switch (word) {
    case 'module': addToken(TokenType.MODULE); break;
    case 'export': addToken(TokenType.EXPORT); break;
    case 'import': addToken(TokenType.IMPORT); break;
    default: // handle as regular minus + atom
  }
}
```

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
class Module extends ASTNode {
  final String name;
  final Set<String> exports;      // "name/arity" strings
  final List<String> imports;
  final List<Procedure> procedures;

  Module(this.name, this.exports, this.imports, this.procedures, int line, int col)
      : super(line, col);

  bool isExported(String procName, int arity) {
    return exports.isEmpty || exports.contains('$procName/$arity');
  }
}
```

**Tasks**:
- [ ] Add `ModuleDeclaration` class
- [ ] Add `ExportDeclaration` class
- [ ] Add `ImportDeclaration` class
- [ ] Add `ProcedureRef` class
- [ ] Add `RemoteGoal` class
- [ ] Add `Module` class
- [ ] Modify `Program` to hold list of `Module`

---

### 1.3 Parser Changes

**File**: `lib/compiler/parser.dart`

**New Parsing Methods**:

```dart
/// Parse a module file
Module parseModule() {
  String? moduleName;
  final exports = <ProcedureRef>[];
  final imports = <String>[];

  // Parse declarations at start of file
  while (_checkDeclaration()) {
    if (_match(TokenType.MINUS)) {
      if (_match(TokenType.MODULE)) {
        moduleName = _parseModuleDecl();
      } else if (_match(TokenType.EXPORT)) {
        exports.addAll(_parseExportDecl());
      } else if (_match(TokenType.IMPORT)) {
        imports.addAll(_parseImportDecl());
      }
    }
  }

  // Parse procedures
  final procedures = <Procedure>[];
  while (!_isAtEnd()) {
    procedures.add(_parseProcedure());
  }

  final name = moduleName ?? '_anonymous_${_fileId}';
  final exportSet = exports.map((e) => e.signature).toSet();

  return Module(name, exportSet, imports, procedures, 1, 1);
}

/// Parse: -module(name).
String _parseModuleDecl() {
  _consume(TokenType.LPAREN, 'Expected "(" after module');
  final name = _parseModuleName();
  _consume(TokenType.RPAREN, 'Expected ")" after module name');
  _consume(TokenType.DOT, 'Expected "." after module declaration');
  return name;
}

/// Parse hierarchical module name: foo or foo.bar.baz
String _parseModuleName() {
  final parts = <String>[];
  parts.add(_consume(TokenType.ATOM, 'Expected module name').lexeme);

  while (_match(TokenType.DOT) && _check(TokenType.ATOM)) {
    parts.add(_advance().lexeme);
  }

  return parts.join('.');
}

/// Parse: -export([name/arity, ...]).
List<ProcedureRef> _parseExportDecl() {
  _consume(TokenType.LPAREN, 'Expected "(" after export');
  _consume(TokenType.LBRACKET, 'Expected "[" in export list');

  final exports = <ProcedureRef>[];

  if (!_check(TokenType.RBRACKET)) {
    do {
      exports.add(_parseProcRef());
    } while (_match(TokenType.COMMA));
  }

  _consume(TokenType.RBRACKET, 'Expected "]" after export list');
  _consume(TokenType.RPAREN, 'Expected ")" after export');
  _consume(TokenType.DOT, 'Expected "." after export declaration');

  return exports;
}

/// Parse: name/arity
ProcedureRef _parseProcRef() {
  final name = _consume(TokenType.ATOM, 'Expected procedure name').lexeme;
  _consume(TokenType.SLASH, 'Expected "/" in procedure reference');
  final arity = int.parse(_consume(TokenType.INTEGER, 'Expected arity').lexeme);
  return ProcedureRef(name, arity);
}

/// Parse: -import([module, ...]).
List<String> _parseImportDecl() {
  _consume(TokenType.LPAREN, 'Expected "(" after import');
  _consume(TokenType.LBRACKET, 'Expected "[" in import list');

  final imports = <String>[];

  if (!_check(TokenType.RBRACKET)) {
    do {
      imports.add(_parseModuleName());
    } while (_match(TokenType.COMMA));
  }

  _consume(TokenType.RBRACKET, 'Expected "]" after import list');
  _consume(TokenType.RPAREN, 'Expected ")" after import');
  _consume(TokenType.DOT, 'Expected "." after import declaration');

  return imports;
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
- [ ] Add `_parseModuleDecl()` method
- [ ] Add `_parseModuleName()` method
- [ ] Add `_parseExportDecl()` method
- [ ] Add `_parseProcRef()` method
- [ ] Add `_parseImportDecl()` method
- [ ] Modify `_parseGoal()` to handle `#` operator
- [ ] Add `_checkDeclaration()` helper

---

### 1.4 Compiler Changes

**File**: `lib/compiler/codegen.dart`

**Module Compilation**:

```dart
class ModuleCompiler {
  final Map<String, Module> modules = {};
  final Map<String, CompiledModule> compiled = {};

  /// Compile multiple modules together
  CompiledProgram compileProgram(List<Module> modules) {
    // Register all modules
    for (final mod in modules) {
      if (this.modules.containsKey(mod.name)) {
        throw CompileError('Duplicate module: ${mod.name}');
      }
      this.modules[mod.name] = mod;
    }

    // Validate imports
    for (final mod in modules) {
      _validateImports(mod);
    }

    // Check for circular imports
    _checkCircularImports();

    // Compile each module
    for (final mod in modules) {
      compiled[mod.name] = _compileModule(mod);
    }

    // Link into single program
    return _link(compiled);
  }

  void _validateImports(Module mod) {
    for (final imp in mod.imports) {
      if (!modules.containsKey(imp) && !_isSystemModule(imp)) {
        throw CompileError(
          'Module ${mod.name} imports unknown module: $imp'
        );
      }
    }
  }

  bool _isSystemModule(String name) {
    return ['system', 'io', 'math'].contains(name);
  }

  CompiledModule _compileModule(Module mod) {
    // Validate exports
    for (final export in mod.exports) {
      final parts = export.split('/');
      final name = parts[0];
      final arity = int.parse(parts[1]);

      final found = mod.procedures.any(
        (p) => p.name == name && p.arity == arity
      );
      if (!found) {
        throw CompileError(
          'Module ${mod.name} exports undefined procedure: $export'
        );
      }
    }

    // Compile procedures
    final bytecode = <int>[];
    final procOffsets = <String, int>{};

    for (final proc in mod.procedures) {
      procOffsets['${proc.name}/${proc.arity}'] = bytecode.length;
      bytecode.addAll(_compileProc(proc, mod));
    }

    return CompiledModule(
      name: mod.name,
      exports: mod.exports,
      bytecode: bytecode,
      procOffsets: procOffsets,
    );
  }

  /// Compile a remote goal: Module # Goal
  List<int> _compileRemoteGoal(RemoteGoal rg, Module currentModule) {
    final targetModName = rg.moduleName;
    final goal = rg.goal;

    // Check import
    if (!currentModule.imports.contains(targetModName) &&
        !_isSystemModule(targetModName)) {
      throw CompileError(
        'Module ${currentModule.name} uses ${targetModName} but does not import it',
        rg.line, rg.column
      );
    }

    // Check export
    final targetMod = modules[targetModName];
    if (targetMod != null && !targetMod.isExported(goal.functor, goal.args.length)) {
      throw CompileError(
        '${goal.functor}/${goal.args.length} is not exported by $targetModName',
        rg.line, rg.column
      );
    }

    // For static linking: compile as call to qualified procedure name
    // The linker will resolve this to actual offset
    return _compileCall(
      '${targetModName}::${goal.functor}',
      goal.args,
      goal.arity,
    );
  }
}
```

**Tasks**:
- [ ] Add `ModuleCompiler` class
- [ ] Add `compileProgram()` method
- [ ] Add `_validateImports()` method
- [ ] Add `_checkCircularImports()` method
- [ ] Add `_compileModule()` method
- [ ] Add `_compileRemoteGoal()` method
- [ ] Add linker to resolve cross-module calls

---

### 1.5 Linker

**File**: `lib/compiler/linker.dart` (new file)

```dart
/// Links multiple compiled modules into a single executable
class Linker {
  /// Link modules into single program
  LinkedProgram link(Map<String, CompiledModule> modules) {
    final bytecode = <int>[];
    final globalOffsets = <String, int>{};  // "module::proc/arity" -> offset

    // Concatenate all bytecode, tracking offsets
    for (final entry in modules.entries) {
      final modName = entry.key;
      final mod = entry.value;
      final baseOffset = bytecode.length;

      // Add this module's bytecode
      bytecode.addAll(mod.bytecode);

      // Record global offsets for exported procedures
      for (final procEntry in mod.procOffsets.entries) {
        final localOffset = procEntry.value;
        final globalOffset = baseOffset + localOffset;
        globalOffsets['$modName::${procEntry.key}'] = globalOffset;
      }
    }

    // Patch cross-module call addresses
    _patchCalls(bytecode, globalOffsets);

    return LinkedProgram(bytecode, globalOffsets);
  }

  void _patchCalls(List<int> bytecode, Map<String, int> offsets) {
    // Walk through bytecode, find CALL instructions
    // with placeholder addresses, replace with actual offsets
    // Implementation depends on bytecode format
  }
}

class LinkedProgram {
  final List<int> bytecode;
  final Map<String, int> procedureOffsets;

  LinkedProgram(this.bytecode, this.procedureOffsets);
}
```

**Tasks**:
- [ ] Create `linker.dart`
- [ ] Implement `Linker` class
- [ ] Implement `link()` method
- [ ] Implement `_patchCalls()` method
- [ ] Define placeholder format for unresolved calls

---

### 1.6 Multi-File Loading

**File**: `lib/compiler/loader.dart` (new file)

```dart
/// Loads and compiles multiple GLP source files
class ModuleLoader {
  final Map<String, String> sources = {};  // moduleName -> source

  /// Load a single source file
  void loadFile(String path, String source) {
    // Quick parse to get module name
    final lexer = Lexer(source);
    final tokens = lexer.scanTokens();
    final moduleName = _extractModuleName(tokens) ?? _pathToModuleName(path);
    sources[moduleName] = source;
  }

  /// Load multiple files
  void loadFiles(Map<String, String> files) {
    for (final entry in files.entries) {
      loadFile(entry.key, entry.value);
    }
  }

  /// Compile all loaded modules
  CompiledProgram compile() {
    final modules = <Module>[];

    for (final entry in sources.entries) {
      final lexer = Lexer(entry.value);
      final tokens = lexer.scanTokens();
      final parser = Parser(tokens);
      final module = parser.parseModule();
      modules.add(module);
    }

    final compiler = ModuleCompiler();
    return compiler.compileProgram(modules);
  }

  String? _extractModuleName(List<Token> tokens) {
    // Look for -module(name) at start
    for (var i = 0; i < tokens.length - 3; i++) {
      if (tokens[i].type == TokenType.MINUS &&
          tokens[i+1].type == TokenType.MODULE &&
          tokens[i+2].type == TokenType.LPAREN &&
          tokens[i+3].type == TokenType.ATOM) {
        return tokens[i+3].lexeme;
      }
    }
    return null;
  }

  String _pathToModuleName(String path) {
    // Convert "path/to/file.glp" to "file"
    final name = path.split('/').last;
    return name.endsWith('.glp') ? name.substring(0, name.length - 4) : name;
  }
}
```

**Tasks**:
- [ ] Create `loader.dart`
- [ ] Implement `ModuleLoader` class
- [ ] Implement `loadFile()` method
- [ ] Implement `compile()` method
- [ ] Integrate with existing compiler entry points

---

### 1.7 System Modules

**File**: `lib/runtime/system_modules.dart` (new file)

```dart
/// Built-in system modules
class SystemModules {
  static final Map<String, CompiledModule> modules = {
    'system': _systemModule,
    'io': _ioModule,
    'math': _mathModule,
  };

  static final CompiledModule _mathModule = CompiledModule(
    name: 'math',
    exports: {':=/2'},  // Current assign.glp functionality
    // bytecode loaded from stdlib/assign.glp
  );

  // etc.
}
```

**Tasks**:
- [ ] Create `system_modules.dart`
- [ ] Define `system` module (meta-predicates)
- [ ] Define `io` module (I/O predicates)
- [ ] Integrate `math` module (existing assign.glp)

---

### 1.8 Integration

**File**: `lib/glp_runtime.dart` and others

**Tasks**:
- [ ] Update main compiler entry point to use `ModuleLoader`
- [ ] Update REPL to handle module declarations
- [ ] Update bytecode runner if needed
- [ ] Update error messages to include module context

---

## Phase 1 Testing Plan

### Unit Tests

**File**: `test/compiler/module_test.dart`

```dart
void main() {
  group('Module Lexer', () {
    test('recognizes # token', () { ... });
    test('recognizes -module declaration', () { ... });
    test('recognizes -export declaration', () { ... });
    test('recognizes -import declaration', () { ... });
  });

  group('Module Parser', () {
    test('parses module declaration', () { ... });
    test('parses export list', () { ... });
    test('parses import list', () { ... });
    test('parses remote goal', () { ... });
    test('parses hierarchical module name', () { ... });
  });

  group('Module Compiler', () {
    test('validates exports exist', () { ... });
    test('validates imports exist', () { ... });
    test('detects circular imports', () { ... });
    test('compiles remote goal', () { ... });
  });

  group('Module Linker', () {
    test('links two modules', () { ... });
    test('resolves cross-module calls', () { ... });
  });
}
```

### Integration Tests

**File**: `test/integration/module_integration_test.dart`

Test complete module programs:

```dart
test('simple module call', () {
  final sources = {
    'math.glp': '''
      -module(math).
      -export([double/2]).
      double(X?, Y) :- Y := X? * 2.
    ''',
    'main.glp': '''
      -module(main).
      -import([math]).
      test(R) :- math # double(5, R).
    ''',
  };

  final loader = ModuleLoader();
  loader.loadFiles(sources);
  final program = loader.compile();

  final result = execute(program, 'main', 'test', [_]);
  expect(result, equals(10));
});
```

---

## Phase 2: Future Enhancements

### 2.1 Separate Compilation

- Compile each module to `.glpc` file
- Link `.glpc` files at load time
- Incremental recompilation

### 2.2 Dynamic Loading

- Load modules on first use
- Module cache
- Hot reloading (development)

### 2.3 Service Model

- FCP-style stateful services
- Module as process
- Monitor modules with state

---

## Task Summary

### Must Have (Phase 1)

| # | Task | File | Status |
|---|------|------|--------|
| 1 | Add HASH token | lexer.dart | [ ] |
| 2 | Add module declaration tokens | lexer.dart | [ ] |
| 3 | Add ModuleDeclaration AST | ast.dart | [ ] |
| 4 | Add ExportDeclaration AST | ast.dart | [ ] |
| 5 | Add ImportDeclaration AST | ast.dart | [ ] |
| 6 | Add RemoteGoal AST | ast.dart | [ ] |
| 7 | Add Module AST | ast.dart | [ ] |
| 8 | Parse module declaration | parser.dart | [ ] |
| 9 | Parse export declaration | parser.dart | [ ] |
| 10 | Parse import declaration | parser.dart | [ ] |
| 11 | Parse remote goal (M # G) | parser.dart | [ ] |
| 12 | Module validation | codegen.dart | [ ] |
| 13 | Remote goal compilation | codegen.dart | [ ] |
| 14 | Create linker | linker.dart | [ ] |
| 15 | Create module loader | loader.dart | [ ] |
| 16 | System modules | system_modules.dart | [ ] |
| 17 | Integration | glp_runtime.dart | [ ] |
| 18 | Unit tests | module_test.dart | [ ] |
| 19 | Integration tests | module_integration_test.dart | [ ] |

### Nice to Have (Phase 2+)

| # | Task | Priority |
|---|------|----------|
| 20 | Separate compilation | Medium |
| 21 | Dynamic loading | Low |
| 22 | Module versioning | Low |
| 23 | Service model | Low |

---

## Dependencies

The implementation order should be:

```
1. Lexer (tokens)
   ↓
2. AST (node types)
   ↓
3. Parser (parsing logic)
   ↓
4. Compiler (code generation)
   ↓
5. Linker (cross-module resolution)
   ↓
6. Loader (multi-file support)
   ↓
7. Integration (tie it together)
   ↓
8. Testing
```

Each step can be tested independently before proceeding.
