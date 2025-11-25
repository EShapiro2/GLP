# GLP Module System Design

**Based on FCP/Logix Module and Service Architecture**

## 1. Executive Summary

This document proposes a module system for GLP based on the FCP/Logix service model, adapted for GLP's simpler SRSW (Single-Reader/Single-Writer) semantics.

## 2. FCP/Logix Module System Analysis

### 2.1 Core Concepts

**Services as Processes**: In FCP, a module is a running process (service) that handles goals sent via the `#` operator:
```prolog
file # read(Name, Contents)   % sends read/2 goal to file service
```

**Hierarchy**: Services organized in tree structure with paths like `[processor]`, `[system, file]`.

**Module Declarations**:
```prolog
-export([proc1/2, proc2/3]).     % Public procedures
-import([file, math]).           % Pre-cached service channels
-language(compound).             % Language mode
-mode(trust).                    % Security mode
```

**Service Types**:
| Type | Description | State |
|------|-------------|-------|
| `procedures` | Simple exported predicates | Stateless |
| `monitor` | Server with internal state | Stateful (via stream) |
| `director` | Namespace/hierarchy node | Container |

### 2.2 Key Implementation Components in FCP

1. **hierarchy_server.cp**: Routes service requests, manages service tree
2. **domain_server.cp**: Handles goal dispatch within a domain
3. **computation_server.cp**: Manages computation state and control
4. **get_module.cp**: Module loading and compilation
5. **C Level**: `is_module` kernel, `activate/3` kernel, MdlType heap tag

### 2.3 FCP Communication Flow

```
Goal: math # factorial(5, R)
  → hierarchy_server: find [math] service
  → if not loaded: get_module → compile → activate
  → domain_server: export(CallInfo, Scope, Goal, UCC)
  → service process receives goal on input channel
  → reduces goal, results flow back via unification
```

## 3. GLP Module Design

### 3.1 Design Principles

1. **Simpler than FCP**: GLP's SRSW constraint eliminates shared streams
2. **Static linking preferred**: GLP programs typically small
3. **Incremental adoption**: Work without modules, add when needed
4. **Familiar syntax**: Follow FCP conventions where sensible

### 3.2 Module Syntax

**Module Declaration**:
```glp
%% File: math.glp
-module(math).
-export([factorial/2, gcd/3]).

factorial(0, 1).
factorial(N?, F) :-
  N? > 0 |
  N1 := N? - 1,
  factorial(N1?, F1),
  F := N? * F1?.

gcd(A?, 0, A?).
gcd(A?, B?, G) :-
  B? > 0 |
  R := A? mod B?,
  gcd(B?, R?, G).
```

**Module Usage**:
```glp
%% File: main.glp
-module(main).
-import([math]).

run(Result) :-
  math # factorial(5, F),
  math # gcd(48, 18, G),
  Result = {factorial: F?, gcd: G?}.
```

### 3.3 The `#` Operator

**Syntax**: `Module # Goal`

**Semantics**:
- Routes `Goal` to `Module` for reduction
- Module must export the goal's predicate
- Acts as a regular goal in the body
- Variables retain their reader/writer modes across module boundaries

**Multiple Goals**:
```glp
math # [factorial(5, F), gcd(48, 18, G)]  % Multiple goals to same module
```

### 3.4 Export/Import Rules

1. **Export**: Only exported predicates callable via `#`
2. **Import**: Declares compile-time dependencies
3. **Private**: Non-exported predicates only callable within module
4. **Implicit self**: Within a module, `factorial(5,R)` = `self # factorial(5,R)`

### 3.5 Module Resolution

**Static Resolution (Phase 1)**:
- All imports resolved at compile time
- Linked into single bytecode unit
- Simple, efficient, sufficient for most uses

**Dynamic Resolution (Future)**:
- Load modules on demand
- Service hierarchy like FCP

### 3.6 Namespacing

```glp
-module(utils.list).   % Hierarchical module name
-import([utils.math]). % Import sibling module

append(Xs, Ys, Zs) :- ...
```

### 3.7 System Modules

Pre-loaded modules available without explicit import:
- `system` - Process control, meta-operations
- `io` - Input/output operations
- `math` - Arithmetic (current assign.glp functionality)

## 4. Implementation Plan

### Phase 1: Static Module System

#### 4.1 Parser Extensions (lib/compiler/parser.dart)

```dart
// New token types
TokenType.HASH,        // #
TokenType.MODULE,      // -module
TokenType.EXPORT,      // -export
TokenType.IMPORT,      // -import

// Parse module declaration
ModuleDecl? _parseModuleDecl() {
  if (_match(TokenType.MINUS) && _match(TokenType.MODULE)) {
    _consume(TokenType.LPAREN, 'Expected "(" after module');
    final name = _consume(TokenType.ATOM, 'Expected module name');
    _consume(TokenType.RPAREN, 'Expected ")" after module name');
    _consume(TokenType.DOT, 'Expected "." after module declaration');
    return ModuleDecl(name.lexeme);
  }
  return null;
}

// Parse Module # Goal
Goal _parseRemoteGoal() {
  final module = _parseAtom();
  if (_match(TokenType.HASH)) {
    final goal = _parseGoal();
    return RemoteGoal(module.functor, goal);
  }
  return module; // Regular goal
}
```

#### 4.2 AST Extensions (lib/compiler/ast.dart)

```dart
class ModuleDeclaration extends ASTNode {
  final String name;
  final List<ProcedureRef> exports;
  final List<String> imports;

  ModuleDeclaration(this.name, this.exports, this.imports, int line, int col)
      : super(line, col);
}

class ProcedureRef {
  final String name;
  final int arity;
  ProcedureRef(this.name, this.arity);
}

class RemoteGoal extends Goal {
  final String module;
  final Goal goal;

  RemoteGoal(this.module, this.goal, int line, int col)
      : super(goal.functor, goal.args, line, col);
}

class Module {
  final ModuleDeclaration? declaration;
  final List<Procedure> procedures;

  Module(this.declaration, this.procedures);

  String get name => declaration?.name ?? '_main';
  bool exports(String proc, int arity) =>
    declaration?.exports.any((e) => e.name == proc && e.arity == arity) ?? true;
}
```

#### 4.3 Compiler Changes (lib/compiler/codegen.dart)

```dart
class ModuleCompiler {
  final Map<String, Module> modules = {};

  void compile(Module module) {
    // Validate exports exist
    for (final export in module.declaration?.exports ?? []) {
      if (!module.procedures.any((p) =>
          p.name == export.name && p.arity == export.arity)) {
        throw CompileError('Exported ${export.name}/${export.arity} not defined');
      }
    }

    // Compile procedures
    for (final proc in module.procedures) {
      _compileProc(proc, module);
    }
  }

  void _compileRemoteGoal(RemoteGoal goal, Module currentModule) {
    final targetModule = modules[goal.module];
    if (targetModule == null) {
      throw CompileError('Unknown module: ${goal.module}');
    }
    if (!targetModule.exports(goal.goal.functor, goal.goal.args.length)) {
      throw CompileError(
        '${goal.goal.functor}/${goal.goal.args.length} not exported by ${goal.module}');
    }
    // Emit call to target module's procedure
    _emitModuleCall(goal.module, goal.goal);
  }
}
```

#### 4.4 Bytecode Extensions

**Option A: Inline Linking** (Recommended for Phase 1)
- Merge all imported procedures into single bytecode
- `Module # Goal` becomes regular procedure call
- No runtime overhead
- Simple implementation

**Option B: Module Call Opcode** (For Phase 2)
```dart
enum Opcode {
  // ... existing opcodes ...
  CallModule,  // CallModule <moduleIndex> <procIndex> <arity>
}
```

#### 4.5 Runtime Changes

```dart
class ModuleRegistry {
  final Map<String, CompiledModule> modules = {};

  void register(String name, CompiledModule module) {
    modules[name] = module;
  }

  CompiledModule? lookup(String name) => modules[name];
}

class CompiledModule {
  final String name;
  final Set<String> exports;
  final List<int> bytecode;
  final Map<String, int> procOffsets;

  bool canCall(String proc, int arity) =>
    exports.contains('$proc/$arity');
}
```

### Phase 2: Multi-File Support

- Load multiple .glp files
- Resolve imports across files
- Link into single execution unit

### Phase 3: Dynamic Loading (Future)

- On-demand module loading
- Module cache
- Optional FCP-style service processes for stateful modules

## 5. SRSW Considerations

The SRSW constraint applies across module boundaries:

```glp
% Module A
a(X) :- b # proc(X).  % X is writer in A, can be bound by B

% Module B
proc(Y) :- Y = 42.    % Y receives writer, can bind it
```

**Rule**: Variable modes (reader/writer) are preserved across `#` calls. The called module receives the same mode as the caller provides.

## 6. Open Questions

1. **Circular imports**: Disallow or use forward declarations?
2. **Versioning**: Module versions for compatibility checking?
3. **Namespaces**: Flat (`math`) vs hierarchical (`utils.math`)?
4. **Re-export**: Allow modules to re-export imports?
5. **Default imports**: Auto-import system modules?

## 7. Migration Path

1. **No breaking changes**: Existing single-file programs work unchanged
2. **Optional modules**: `-module` declaration is optional
3. **Gradual adoption**: Add modules as codebases grow

## 8. Example: Complete Multi-Module Program

**File: stdlib/list.glp**
```glp
-module(list).
-export([append/3, length/2, reverse/3]).

append([], Ys?, Ys?).
append([X?|Xs?], Ys?, [X?|Zs]) :- append(Xs?, Ys?, Zs).

length([], 0).
length([_|Xs?], N) :- length(Xs?, N1), N := N1? + 1.

reverse(Xs?, Ys) :- rev(Xs?, [], Ys).
rev([], Acc?, Acc?).
rev([X?|Xs?], Acc?, Ys) :- rev(Xs?, [X?|Acc?], Ys).
```

**File: stdlib/math.glp**
```glp
-module(math).
-export([factorial/2, sum_list/2]).
-import([list]).

factorial(0, 1).
factorial(N?, F) :-
  N? > 0 |
  N1 := N? - 1,
  factorial(N1?, F1),
  F := N? * F1?.

sum_list([], 0).
sum_list([X?|Xs?], S) :-
  sum_list(Xs?, S1),
  S := X? + S1?.
```

**File: main.glp**
```glp
-module(main).
-import([list, math]).

run(Result) :-
  list # append([1,2], [3,4], L1),
  list # length(L1?, Len),
  math # factorial(5, F),
  math # sum_list(L1?, Sum),
  Result = {list: L1?, len: Len?, fact: F?, sum: Sum?}.
```

---

*Document Version: 1.0*
*Based on FCP/Logix analysis from github.com/EShapiro2/FCP*
