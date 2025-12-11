# GLP Module System v1 - Design Specification

**Status:** Draft for Review
**Date:** 2025-12-11
**Based on:** FCP/Logix Module System (see `docs/fcp-module-system-analysis.md`)

---

## 1. Overview

### 1.1 Purpose

Enable GLP programs to be organized into modules with:
- Namespace separation
- Explicit interfaces (exports)
- Cross-module calls via RPC
- Dynamic module loading

### 1.2 Design Principle

**Follow FCP/Logix precisely.** This design replicates FCP's proven module system, adapted only where GLP's simpler semantics allow. Deviations are explicitly noted with rationale.

### 1.3 Why Stream-Based RPC?

In sequential Prolog, cross-module calls can be direct procedure jumps. In concurrent GLP, this doesn't work because:

1. **Callee may not be ready** — processing another request
2. **Results return asynchronously** — via variable binding, not call return
3. **Dynamic loading** — modules load on first use

FCP's solution: **streams**. Each module has an input stream. Cross-module calls are messages. This unifies module communication with GLP's existing stream/SRSW model.

### 1.4 Architecture Overview

```
Module A                              Module B (math)
─────────                             ────────────────

┌─────────────┐                       ┌─────────────┐
│ Procedures  │                       │ Procedures  │
│             │                       │ factorial/2 │
│ run(R) :-   │                       │ gcd/3       │
│   math #    │                       └──────┬──────┘
│   fact(5,R) │                              │
└──────┬──────┘                              │
       │                                     │
       ▼                                     │
┌─────────────┐                       ┌──────┴──────┐
│ Output      │   write_vector        │ Dispatcher  │
│ Handler     │──────────────────────▶│             │
└─────────────┘   stream message      └─────────────┘
       │                                     ▲
       ▼                                     │
┌─────────────┐      serve_import            │
│ Vector[1]   │──────────────────────────────┘
│ (to math)   │
└─────────────┘
```

---

## 2. Syntax

### 2.1 Module Declaration

```glp
-module(name).
```

- `name` — atom (lowercase identifier)
- Must be first declaration in file
- File without `-module` is anonymous main module

### 2.2 Export Declaration

```glp
-export([pred1/arity1, pred2/arity2, ...]).
```

- Lists procedures visible to other modules
- Unexported procedures are module-private
- Multiple `-export` declarations allowed (cumulative)

### 2.3 Import Declaration

```glp
-import([module1, module2, ...]).
```

- Lists modules this module will call
- Compiler assigns index to each (1, 2, 3, ...)
- Enables O(1) indexed dispatch
- **FCP behavior:** Import does NOT load the module — loading is lazy on first RPC

### 2.4 Remote Procedure Call

```glp
Module # Goal
```

- `Module` — atom (compile-time known) or variable (runtime resolved)
- `Goal` — any goal, arguments may include variables
- Results return via shared variable unification

### 2.5 Complete Example

```glp
%% File: math.glp
-module(math).
-export([factorial/2, gcd/3]).

factorial(0, 1).
factorial(N, F) :-
    N? > 0 |
    N1 := N? - 1,
    factorial(N1?, F1),
    F := N? * F1?.

gcd(A, 0, A?).
gcd(A, B, G) :-
    B? > 0 |
    R := A? mod B?,
    gcd(B?, R?, G).
```

```glp
%% File: main.glp
-module(main).
-import([math]).
-export([boot/1]).

boot(_Args) :-
    math # factorial(5, F),
    math # gcd(48, 18, G),
    print({factorial: F?, gcd: G?}).
```

---

## 3. Compiler Transformations

### 3.1 RPC Transformation

**Following FCP:** Transform `Module # Goal` at precompile time.

**Case 1: Module is atom (compile-time known)**

```glp
% Source
math # factorial(5, R)

% Transformed (FCP rpc.cp:164-175)
distribute # {1, factorial(5, R)}
```

Where `1` is math's index in import list.

**Rationale:** Index enables O(1) vector lookup. FCP calls this "distribute" — distributing a goal to a known target via indexed vector.

**Case 2: Module is variable (runtime value)**

```glp
% Source
M # factorial(5, R)

% Transformed
transmit # {M, factorial(5, R)}
```

**Rationale:** Target unknown at compile time; runtime resolves name. FCP calls this "transmit".

### 3.2 Import Index Assignment

```glp
-import([math, io, utils]).
```

Compiler builds import table:

| Index | Module |
|-------|--------|
| 1 | math |
| 2 | io |
| 3 | utils |

### 3.3 Export Validation

Compiler verifies each exported `pred/arity` is defined in the module. Error if not.

---

## 4. Runtime Architecture

### 4.1 Module Structure

Each loaded module has:

```
┌────────────────────────────────────────┐
│ Module: math                           │
├────────────────────────────────────────┤
│ Name: "math"                           │
│ Exports: {factorial/2, gcd/3}          │
│ Imports: []                            │
│ Import Vector: (empty)                 │
│ Input Channel: Stream (writer end)     │
│ Dispatcher: Process                    │
│ Procedures: Compiled bytecode          │
└────────────────────────────────────────┘
```

### 4.2 Import Vector

**Following FCP:** `make_vector(N)` creates paired streams.

```
make_vector(N) → (Vector, Tuple)

Vector[i] = writer end of stream i
Tuple[i]  = reader end of stream i
```

For a module with `imports = [math, io, utils]`:
- `Vector[1]` — writer to stream for math
- `Vector[2]` — writer to stream for io
- `Vector[3]` — writer to stream for utils

Each `Tuple[i]` is read by a `serve_import` process.

**Rationale:** Separates writing (caller side) from reading (bridge process). Streams provide message ordering and buffering.

### 4.3 serve_import Process

**Following FCP:** One bridge process per import (domain_server.cp:964-1013).

```
serve_import(StreamReader, ImportName, Registry):
    wait for first message on StreamReader
    Channel = Registry.lookupOrLoad(ImportName)  // lazy loading!
    for each Message in StreamReader:
        write(Message, Channel)
```

**Key behaviors:**
- **Lazy connection:** Target module looked up on FIRST message, not at creation
- **Lazy loading:** `lookupOrLoad` triggers module loading if not yet loaded
- **Buffering:** Messages queue on stream until target ready

**Rationale:** This is how FCP handles circular dependencies — modules load on demand, streams buffer messages.

### 4.4 Dispatcher Process

**Following FCP:** Each module has a dispatcher reading its input channel.

```
dispatcher(InputChannel, Module):
    for each export(CallInfo, Scope, Goal, CCC) in InputChannel:
        if Goal.functor/Goal.arity in Module.exports:
            call(Goal, Module.Procedures)
        else:
            report_error(unknown, Goal)
```

**v1 (trust mode):** CCC is ignored — just dispatch and call.

### 4.5 Message Format

**Following FCP:**

```
export(CallInfo, Scope, Goal, CCC)
```

| Field | Purpose |
|-------|---------|
| CallInfo | Caller identification (debugging/tracing) |
| Scope | Module path for hierarchical resolution |
| Goal | The goal to execute, e.g., `factorial(5, R)` |
| CCC | Computation Control Context (ignored in trust mode) |

**Rationale:** FCP format allows future modes (failsafe, interrupt) without message format change.

### 4.6 Result Return via Shared Variables

**Key insight:** No explicit return message needed.

```glp
% Caller
math # factorial(5, R)
% ... later use R? ...

% Callee
factorial(5, R) :- ... R = 120.
```

The variable `R` is shared between caller and callee. When callee binds `R = 120`, caller's `R` is bound via standard GLP unification.

**Rationale:** This is why stream-based RPC works elegantly in logic programming — bidirectional communication via shared variables, no explicit return protocol.

---

## 5. Module Loading

### 5.1 File Location

**Following FCP:** Context-relative hierarchical search (get_module.cp:67-84).

For module `math`:

| Priority | Path | Type |
|----------|------|------|
| 1 | `Bin/math.glpc` | Compiled bytecode (Bin subdir) |
| 2 | `math.glpc` | Compiled bytecode |
| 3 | `math.glp` | Source (compile if newer) |

**Directory structure convention:**
```
project/
  main.glp
  math.glp
  Bin/
    main.glpc
    math.glpc
  utils/
    list.glp
    Bin/
      list.glpc
```

### 5.2 Lazy Loading

**Following FCP:** Modules load on first RPC, not at import declaration.

```
1. Module A loads
2. Import vector created with entries for [math, io]
3. serve_import processes started, but IDLE (no messages yet)
4. A executes, hits: math # factorial(5, R)
5. Message written to Vector[1]
6. serve_import[1] receives first message
7. serve_import[1] calls Registry.lookupOrLoad("math")
8. math.glp loaded, compiled, registered
9. Message forwarded to math's input channel
10. math's dispatcher processes the goal
```

**Rationale:** Lazy loading naturally handles circular dependencies and avoids loading unused modules.

### 5.3 Circular Dependencies

**Following FCP:** Allowed, handled by lazy loading + stream buffering.

A imports B, B imports A:
1. A loads, creates import entry for B (B not loaded)
2. A calls `B # foo(...)` → first message triggers B loading
3. B loads, creates import entry for A
4. B calls `A # bar(...)` → A already loaded, message delivered
5. Stream buffering handles any message ordering

**No explicit circular dependency detection needed.**

### 5.4 Loading Modes

**v1:** Auto mode only.

| Mode | Behavior | v1 Support |
|------|----------|------------|
| auto | Compile source if newer than binary | ✓ |
| binary | Only load binary, error if missing | future |
| query | Ask user whether to compile | future |

---

## 6. Entry Point and Boot Sequence

### 6.1 Boot Protocol

**Following FCP:** Special `boot/1` entry point (simplified from FCP's `boot/2`).

```glp
-module(main).
-export([boot/1]).

boot(Args) :-
    % Args = command line arguments or caller-provided
    initialize(Args?),
    run_main_loop.
```

### 6.2 Startup Flow

```
1. GLP runtime starts
2. Load root module (specified by user or default "main")
3. Create module infrastructure (vector, dispatcher)
4. Call: root # boot(Args)
5. boot/1 spawns application computation
```

### 6.3 GLP Deviation from FCP

FCP's `boot/2` receives `[system | In]` where `system` is channel to system services.

**GLP v1 simplification:** `boot/1` receives just Args. System services accessed via `system # ...` RPC if needed.

---

## 7. Error Handling

### 7.1 Trust Mode Behavior

**Following FCP:** Errors fail immediately, don't suspend (domain_server.cp:574-601).

| Error | Cause | Behavior |
|-------|-------|----------|
| `no_service` | Module not found | Goal fails |
| `unknown` | Procedure not exported | Goal fails |
| `unrecognized` | Invalid goal format | Goal fails |

**Key insight:** Missing modules/procedures FAIL (don't suspend). Different from suspension on unbound variables.

### 7.2 Error Reporting

Errors reported to computation context. In trust mode (v1), this means:
- Goal fails
- Error logged (if debugging enabled)
- Computation continues (other goals unaffected)

---

## 8. Implementation Components

### 8.1 Lexer Extensions

**File:** `lib/compiler/lexer.dart`

New tokens:
```dart
enum TokenType {
  // ... existing ...
  HASH,    // #
  MODULE,  // module (after -)
  EXPORT,  // export (after -)
  IMPORT,  // import (after -)
}
```

### 8.2 AST Extensions

**File:** `lib/compiler/ast.dart`

```dart
/// Module declaration: -module(name).
class ModuleDeclaration extends AstNode {
  final String name;
  ModuleDeclaration(this.name, int line, int col);
}

/// Export declaration: -export([pred/arity, ...]).
class ExportDeclaration extends AstNode {
  final List<ProcRef> exports;
  ExportDeclaration(this.exports, int line, int col);
}

/// Import declaration: -import([module, ...]).
class ImportDeclaration extends AstNode {
  final List<String> imports;
  ImportDeclaration(this.imports, int line, int col);
}

/// Remote goal: Module # Goal
class RemoteGoal extends AstNode {
  final Term module;   // Atom or Variable
  final Goal goal;
  RemoteGoal(this.module, this.goal, int line, int col);
}

/// Complete module
class Module extends AstNode {
  final ModuleDeclaration? declaration;
  final List<ExportDeclaration> exports;
  final List<ImportDeclaration> imports;
  final List<Procedure> procedures;
}
```

### 8.3 Parser Extensions

**File:** `lib/compiler/parser.dart`

```dart
Module parseModule() {
  ModuleDeclaration? modDecl;
  final exports = <ExportDeclaration>[];
  final imports = <ImportDeclaration>[];
  final procedures = <Procedure>[];

  // Parse declarations
  while (_matchDeclaration()) {
    if (_check(MODULE)) modDecl = _parseModuleDecl();
    else if (_check(EXPORT)) exports.add(_parseExportDecl());
    else if (_check(IMPORT)) imports.add(_parseImportDecl());
  }

  // Parse procedures
  while (!_isAtEnd()) {
    procedures.add(_parseProcedure());
  }

  return Module(modDecl, exports, imports, procedures);
}

/// Parse: Module # Goal
Goal _parseGoal() {
  final term = _parseTerm();

  if (_match(HASH)) {
    final goal = _parseGoal();
    return RemoteGoal(term, goal, term.line, term.col);
  }

  // ... rest of goal parsing
}
```

### 8.4 Compiler Extensions

**File:** `lib/compiler/codegen.dart`

```dart
class ImportTable {
  final Map<String, int> _indices = {};
  int _nextIndex = 1;

  void addImport(String moduleName) {
    if (!_indices.containsKey(moduleName)) {
      _indices[moduleName] = _nextIndex++;
    }
  }

  int? getIndex(String moduleName) => _indices[moduleName];
  int get size => _indices.length;
}

/// Transform RemoteGoal to distribute/transmit
Goal transformRPC(RemoteGoal rg, ImportTable imports) {
  if (rg.module is ConstTerm) {
    // Static target: use indexed distribute
    final name = (rg.module as ConstTerm).value as String;
    final index = imports.getIndex(name);
    if (index == null) {
      throw CompileError('Module $name not in imports', rg.line, rg.col);
    }
    return Goal('distribute', [ConstTerm(index), rg.goal]);
  } else {
    // Dynamic target: use transmit
    return Goal('transmit', [rg.module, rg.goal]);
  }
}
```

### 8.5 Runtime: Vector Operations

**File:** `lib/runtime/import_vector.dart`

```dart
/// Import vector: array of streams, one per imported module
class ImportVector {
  final List<StreamController> _streams;

  ImportVector._(this._streams);

  /// Create vector of N streams (FCP make_vector)
  static (ImportVector, List<Stream>) make(int size) {
    final streams = <StreamController>[];
    final readers = <Stream>[];

    for (int i = 0; i < size; i++) {
      final controller = StreamController();
      streams.add(controller);
      readers.add(controller.stream);
    }

    return (ImportVector._(streams), readers);
  }

  /// Write message to stream at index (FCP write_vector)
  void write(int index, ExportMessage message) {
    _streams[index - 1].add(message);  // 1-indexed like FCP
  }
}
```

### 8.6 Runtime: Message Types

**File:** `lib/runtime/module_messages.dart`

```dart
/// RPC message format (FCP export message)
class ExportMessage {
  final CallInfo callInfo;
  final List<String> scope;
  final Goal goal;
  final Map<String, dynamic> ccc;  // ignored in trust mode

  ExportMessage(this.callInfo, this.scope, this.goal, this.ccc);
}

/// Call information for debugging/tracing
class CallInfo {
  final String sourceModule;
  final int line;
  final int col;

  CallInfo(this.sourceModule, this.line, this.col);
}
```

### 8.7 Runtime: serve_import Process

**File:** `lib/runtime/serve_import.dart`

```dart
/// Bridge process: reads from vector stream, forwards to target module
void serveImport(
  Stream<ExportMessage> reader,
  String targetName,
  ModuleRegistry registry,
) async {
  StreamWriter? targetChannel;

  await for (final message in reader) {
    // Lazy connection on first message (FCP behavior)
    targetChannel ??= await registry.lookupOrLoad(targetName);

    targetChannel.add(message);
  }
}
```

### 8.8 Runtime: Dispatcher

**File:** `lib/runtime/dispatcher.dart`

```dart
/// Module dispatcher: reads input channel, calls local procedures
void dispatcher(
  Stream<ExportMessage> inputChannel,
  LoadedModule module,
) async {
  await for (final message in inputChannel) {
    final goal = message.goal;
    final procKey = '${goal.functor}/${goal.arity}';

    if (!module.exports.contains(procKey)) {
      // Unknown procedure (FCP domain_server.cp:698)
      _reportError('unknown', goal, message.callInfo);
      continue;
    }

    // Execute goal in module context
    try {
      await module.execute(goal);
    } catch (e) {
      _reportError('failed', goal, message.callInfo, e);
    }
  }
}
```

### 8.9 Runtime: Module Registry

**File:** `lib/runtime/module_registry.dart`

```dart
/// Global registry of loaded modules
class ModuleRegistry {
  final Map<String, LoadedModule> _modules = {};
  final ModuleLoader _loader;

  ModuleRegistry(this._loader);

  /// Register a loaded module
  void register(String name, LoadedModule module) {
    _modules[name] = module;
  }

  /// Lookup module, return null if not loaded
  LoadedModule? lookup(String name) => _modules[name];

  /// Lookup or load module (lazy loading)
  Future<LoadedModule> lookupOrLoad(String name) async {
    var module = _modules[name];
    if (module == null) {
      module = await _loader.load(name);
      _modules[name] = module;
    }
    return module;
  }
}
```

### 8.10 Runtime: Module Loader

**File:** `lib/runtime/module_loader.dart`

```dart
/// Loads modules from files
class ModuleLoader {
  final String basePath;
  final ModuleRegistry registry;

  ModuleLoader(this.basePath, this.registry);

  /// Load module by name (FCP get_module.cp search order)
  Future<LoadedModule> load(String name) async {
    // Search order (FCP convention)
    final paths = [
      '$basePath/Bin/$name.glpc',  // Compiled in Bin/
      '$basePath/$name.glpc',       // Compiled
      '$basePath/$name.glp',        // Source
    ];

    for (final path in paths) {
      if (await File(path).exists()) {
        if (path.endsWith('.glpc')) {
          return _loadCompiled(path, name);
        } else {
          return _compileAndLoad(path, name);
        }
      }
    }

    throw ModuleNotFoundError(name);
  }

  Future<LoadedModule> _loadCompiled(String path, String name) async {
    final bytes = await File(path).readAsBytes();
    final bytecode = BytecodeProgram.deserialize(bytes);
    return _activate(name, bytecode);
  }

  Future<LoadedModule> _compileAndLoad(String path, String name) async {
    final source = await File(path).readAsString();
    final bytecode = GlpCompiler().compile(source);
    return _activate(name, bytecode);
  }

  /// Activate module: create vector, start processes
  LoadedModule _activate(String name, BytecodeProgram bytecode) {
    final module = LoadedModule(name, bytecode);

    // Create import vector
    final (vector, readers) = ImportVector.make(module.imports.length);
    module.importVector = vector;

    // Start serve_import for each import
    for (int i = 0; i < module.imports.length; i++) {
      serveImport(readers[i], module.imports[i], registry);
    }

    // Start dispatcher
    dispatcher(module.inputChannel.stream, module);

    return module;
  }
}
```

### 8.11 Built-in Goals: distribute and transmit

**File:** `lib/runtime/builtins.dart`

```dart
/// Handle distribute # {Index, Goal}
void executeDistribute(int index, Goal goal, ExecutionContext ctx) {
  final message = ExportMessage(
    CallInfo(ctx.moduleName, ctx.line, ctx.col),
    ctx.scope,
    goal,
    {},  // empty CCC in trust mode
  );

  ctx.module.importVector.write(index, message);
}

/// Handle transmit # {Target, Goal}
Future<void> executeTransmit(Term target, Goal goal, ExecutionContext ctx) async {
  // Resolve target (may suspend if unbound variable)
  final targetName = await resolve(target, ctx);

  // Get target channel (may trigger loading)
  final targetModule = await ctx.registry.lookupOrLoad(targetName);

  final message = ExportMessage(
    CallInfo(ctx.moduleName, ctx.line, ctx.col),
    ctx.scope,
    goal,
    {},
  );

  targetModule.inputChannel.add(message);
}
```

---

## 9. What v1 Does NOT Include

| Feature | Reason | Future Version |
|---------|--------|----------------|
| CCC threading | Trust mode sufficient for most programs | v2 |
| Failsafe mode | Needs exception design | v2 |
| Interrupt mode | Needs suspend/resume signals | v2 |
| Block compilation | Optimization, not core | v2 |
| Hierarchical paths | `a.b.c` adds complexity | v2 |
| PMT integration | Design separately | post-v1 |
| Binary-only loading | Auto mode sufficient | v2 |

---

## 10. Testing Strategy

### 10.1 Unit Tests

```dart
// Lexer
test('lexer recognizes HASH token');
test('lexer recognizes MODULE after dash');
test('lexer recognizes EXPORT after dash');
test('lexer recognizes IMPORT after dash');

// Parser
test('parser parses module declaration');
test('parser parses export declaration');
test('parser parses import declaration');
test('parser parses remote goal');

// Compiler
test('compiler assigns import indices');
test('compiler transforms static RPC to distribute');
test('compiler transforms dynamic RPC to transmit');
test('compiler rejects RPC to non-imported module');
```

### 10.2 Integration Tests

```glp
%% test_simple_rpc.glp
-module(test_simple).
-import([math]).
-export([test/1]).

test(R) :- math # factorial(5, R).
```

```glp
%% test_bidirectional.glp - results via shared variables
-module(test_bidir).
-import([counter]).
-export([test/1]).

test(R) :-
    counter # new(C),
    counter # inc(C?),
    counter # inc(C?),
    counter # get(C?, R).
```

```glp
%% test_circular.glp - circular dependency
-module(a).
-import([b]).
-export([from_a/1]).

from_a(R) :- b # from_b(X), R = {a_called_b: X?}.
```

```glp
-module(b).
-import([a]).
-export([from_b/1]).

from_b(42).
```

### 10.3 Error Tests

```dart
test('error: RPC to unknown module');
test('error: RPC to unexported procedure');
test('error: module file not found');
```

---

## 11. Implementation Phases

### Phase 1: Syntax and Parsing
- Lexer tokens
- AST nodes
- Parser for declarations and remote goals

### Phase 2: Compiler
- Import table
- RPC transformation (distribute/transmit)
- Export validation

### Phase 3: Runtime Infrastructure
- ImportVector
- ExportMessage
- ModuleRegistry
- ModuleLoader

### Phase 4: Runtime Processes
- serve_import
- dispatcher
- distribute/transmit builtins

### Phase 5: Integration
- Boot sequence
- End-to-end testing
- Error handling

---

## 12. References

- `docs/fcp-module-system-analysis.md` — Detailed FCP analysis with code references
- `/tmp/FCP/Savannah/Logix/domain_server.cp` — FCP module dispatch
- `/tmp/FCP/Savannah/Logix/system/compile/precompile/rpc.cp` — FCP RPC transformation
- `/tmp/FCP/Savannah/Logix/system/get_module.cp` — FCP module loading
