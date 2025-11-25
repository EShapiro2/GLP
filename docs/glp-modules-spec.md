# GLP Module System Design

**Based on FCP/Logix Module and Service Architecture**

## 1. Executive Summary

This document specifies a module system for GLP that follows the FCP/Logix service model. Modules are runtime entities with dynamic loading, supporting hot reloading and multi-agent distribution.

**Key Design Decision**: GLP follows FCP's runtime service model, NOT static linking. The `#` operator routes goals at runtime through a service hierarchy.

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

1. **Follow FCP**: Runtime service model with dynamic module resolution
2. **SRSW Simplified**: No shared streams, but variables still cross module boundaries
3. **Multi-Agent Ready**: Modules can be sent across agents, loaded dynamically
4. **Hot Reloading**: Support development workflow with module reloading
5. **Familiar Syntax**: Follow FCP conventions for declarations

### 3.2 Module Declarations

**Module Declaration**:
```glp
-module(math).
```

**Export Declaration**:
```glp
-export([factorial/2, gcd/3]).
```

**Import Declaration**:
```glp
-import([list, io]).
```

**Language Mode** (following FCP):
```glp
-language(glp).              % Default GLP mode
-language(compound).         % FCP compatibility mode (future)
```

**Security Mode** (following FCP):
```glp
-mode(trust).                % Full access
-mode(user).                 % Restricted access (default)
```

### 3.3 Service Types

Following FCP's service model:

| Type | Declaration | Description |
|------|-------------|-------------|
| `procedures` | `-service_type(procedures).` | Stateless exported predicates (default) |
| `monitor` | `-service_type(monitor).` | Stateful server with internal state |
| `director` | `-service_type(director).` | Namespace/hierarchy container |

**Default**: If no `-service_type` is specified, module is `procedures` type.

### 3.4 Module Syntax Example

```glp
%% File: math.glp
-module(math).
-export([factorial/2, gcd/3]).
-language(glp).
-mode(user).

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

### 3.5 The `#` Operator

**Syntax**: `Module # Goal`

**Runtime Semantics** (following FCP):
1. Resolve `Module` through service hierarchy at runtime
2. If module not loaded, load and compile dynamically
3. Route `Goal` to module's service process
4. Module reduces goal, results unify back to caller
5. Variable modes (reader/writer) preserved across boundary

**Multiple Goals**:
```glp
math # [factorial(5, F), gcd(48, 18, G)]  % Multiple goals to same module
```

### 3.6 Export/Import Rules

1. **Export**: Only exported predicates callable via `#`
2. **Import**: Declares dependencies, enables compile-time checking
3. **Private**: Non-exported predicates only callable within module
4. **Runtime Resolution**: Import is advisory; actual module resolved at runtime

### 3.7 Module Resolution (Runtime)

Unlike static linking, GLP resolves modules at runtime:

```
Goal: math # factorial(5, R)

1. ServiceRegistry.lookup("math")
   → if cached: return service handle
   → if not loaded: load_module("math")

2. load_module("math"):
   → find source (filesystem, network, agent message)
   → compile to bytecode
   → create service entry in registry
   → return service handle

3. Route goal to service:
   → verify factorial/2 is exported
   → execute goal in module's context
   → results unify to caller's variables
```

### 3.8 Namespacing

```glp
-module(utils.list).   % Hierarchical module name
-import([utils.math]). % Import sibling module

append(Xs, Ys, Zs) :- ...
```

Hierarchical names map to service hierarchy like FCP.

### 3.9 System Modules

Pre-loaded modules available without explicit import:
- `system` - Process control, meta-operations
- `io` - Input/output operations
- `math` - Arithmetic (`:=` predicate)

## 4. Service Architecture

### 4.1 Service Registry

Runtime component managing loaded modules:

```
ServiceRegistry
├── modules: Map<String, LoadedModule>
├── hierarchy: ServiceHierarchy (tree structure)
└── methods:
    ├── lookup(name) → LoadedModule?
    ├── load(name, source) → LoadedModule
    ├── reload(name) → LoadedModule  (hot reload)
    └── unload(name)
```

### 4.2 Service Hierarchy (following FCP)

```
[root]
├── [system]
│   ├── [io]
│   └── [file]
├── [math]
└── [user]
    ├── [utils]
    │   ├── [list]
    │   └── [math]
    └── [app]
```

Resolution follows FCP path semantics:
- `math # goal` → searches `[math]`
- `utils.list # goal` → searches `[utils, list]`

### 4.3 Monitor Modules (Stateful Services)

For stateful services (following FCP monitor pattern):

```glp
-module(counter).
-service_type(monitor).
-export([increment/1, get/1]).

% Monitor has internal state stream
server(State?, Requests?) :-
  Requests? = [Req? | Rest?] |
  handle(State?, Req?, NewState),
  server(NewState?, Rest?).

handle(N?, increment(Delta?), NewN) :- NewN := N? + Delta?.
handle(N?, get(Value), N?) :- Value = N?.
```

## 5. Multi-Agent Distribution

### 5.1 Design for Distribution

GLP modules must support:
1. **Serialization**: Module bytecode can be serialized and sent
2. **Remote Loading**: Load module from another agent
3. **Distributed Resolution**: `#` can route to remote agent's modules

### 5.2 Remote Module Reference

```glp
% Local module call
math # factorial(5, R)

% Remote agent module call (future)
agent(remote) # math # factorial(5, R)
```

### 5.3 Module Transfer

When agent A sends module to agent B:
1. A serializes module (bytecode + metadata)
2. B receives and registers in its ServiceRegistry
3. B can now use `module # goal` locally

## 6. SRSW Considerations

The SRSW constraint applies across module boundaries:

```glp
% Module A
-module(a).
-export([caller/1]).

caller(X) :- b # proc(X).  % X is writer in A, can be bound by B

% Module B
-module(b).
-export([proc/1]).

proc(Y) :- Y = 42.    % Y receives writer, can bind it
```

**Rule**: Variable modes (reader/writer) are preserved across `#` calls. The called module receives the same mode as the caller provides.

## 7. Hot Reloading

For development workflow:

```glp
% In REPL or system module
system # reload(math)  % Reload math module from source
```

Reloading:
1. Compiles new version
2. Replaces entry in ServiceRegistry
3. New calls use new version
4. In-flight calls complete with old version

## 8. Implementation Phases

### Phase 1: Runtime Module Loading
- ServiceRegistry implementation
- Dynamic module compilation
- Runtime `#` operator resolution
- Basic service types (procedures)

### Phase 2: Full Service Model
- Service hierarchy (FCP-style paths)
- Monitor modules (stateful)
- Director modules (namespaces)
- Hot reloading

### Phase 3: Multi-Agent
- Module serialization
- Remote module loading
- Distributed service resolution

## 9. Comparison: Static vs Runtime

| Aspect | Static (NOT chosen) | Runtime (chosen) |
|--------|---------------------|------------------|
| Resolution | Compile time | Runtime |
| Loading | All at start | On demand |
| Hot reload | No | Yes |
| Multi-agent | Difficult | Natural |
| FCP alignment | Diverges | Follows |
| Complexity | Lower | Higher |

**Decision**: Runtime model chosen because:
1. Follows FCP design
2. Required for multi-agent distribution
3. Enables hot reloading
4. Natural for dynamic systems

## 10. Open Questions

1. **Module Versioning**: How to handle version conflicts?
2. **Dependency Resolution**: Automatic transitive loading?
3. **Security Sandboxing**: How does `-mode` restrict access?
4. **Remote Trust**: Trust model for modules from other agents?

---

*Document Version: 2.0*
*Based on FCP/Logix analysis from github.com/EShapiro2/FCP*
*Updated to follow FCP runtime service model per design requirement*
