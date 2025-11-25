# GLP Module Language Specification

**Version 2.0**

## 1. Overview

This specification defines the syntax and semantics for modules in GLP. A module is a named collection of procedures with explicit exports and imports, following the FCP/Logix service model.

**Key Principle**: Modules are runtime entities resolved dynamically, NOT statically linked.

## 2. Module Declarations

### 2.1 Syntax

```
ModuleDecl     ::= '-module(' ModuleName ').'
ModuleName     ::= Atom | Atom '.' ModuleName

ExportDecl     ::= '-export([' ExportList ']).'
ExportList     ::= ProcRef | ProcRef ',' ExportList
ProcRef        ::= Atom '/' Integer

ImportDecl     ::= '-import([' ImportList ']).'
ImportList     ::= ModuleName | ModuleName ',' ImportList

LanguageDecl   ::= '-language(' LanguageMode ').'
LanguageMode   ::= 'glp' | 'compound'

ModeDecl       ::= '-mode(' SecurityMode ').'
SecurityMode   ::= 'trust' | 'user'

ServiceDecl    ::= '-service_type(' ServiceType ').'
ServiceType    ::= 'procedures' | 'monitor' | 'director'
```

### 2.2 Declarations Reference

| Declaration | Purpose | Default |
|------------|---------|---------|
| `-module(name).` | Names the module | Anonymous |
| `-export([...]).` | Public procedures | All (if no declaration) |
| `-import([...]).` | Module dependencies | None |
| `-language(mode).` | Language mode | `glp` |
| `-mode(security).` | Security mode | `user` |
| `-service_type(type).` | Service type | `procedures` |

### 2.3 Module Declaration

Names the module. Must appear first if present.
```glp
-module(math).
-module(utils.list).      % Hierarchical name
```

### 2.4 Export Declaration

Lists procedures callable from other modules via `#`.
```glp
-export([factorial/2, gcd/3]).
```

### 2.5 Import Declaration

Declares module dependencies. Enables compile-time checking but resolution is at runtime.
```glp
-import([list, io]).
-import([utils.list, utils.math]).  % Hierarchical imports
```

### 2.6 Language Declaration (FCP)

Specifies language mode for parsing and compilation.
```glp
-language(glp).       % Default GLP syntax
-language(compound).  % FCP compound syntax (future)
```

### 2.7 Mode Declaration (FCP)

Specifies security/trust level.
```glp
-mode(trust).  % Full system access
-mode(user).   % Restricted access (default)
```

### 2.8 Service Type Declaration (FCP)

Specifies how the module operates.
```glp
-service_type(procedures).  % Stateless predicates (default)
-service_type(monitor).     % Stateful server
-service_type(director).    % Namespace container
```

### 2.9 Declaration Rules

1. At most one `-module` declaration per file
2. `-module` must appear first if present
3. Multiple `-export` declarations allowed (merged)
4. Multiple `-import` declarations allowed (merged)
5. At most one `-language`, `-mode`, `-service_type` each
6. Module name must be unique in the service registry
7. Exported procedures must be defined in the module
8. If no `-module` declaration, the file defines an anonymous module

## 3. The Remote Call Operator (`#`)

### 3.1 Syntax

```
RemoteGoal     ::= ModuleName '#' Goal
                 | ModuleName '#' '[' GoalList ']'
GoalList       ::= Goal | Goal ',' GoalList
```

### 3.2 Semantics

The `#` operator routes a goal to another module for reduction **at runtime**.

```glp
math # factorial(5, F)      % Call factorial/2 in module math
list # [append(A,B,C), length(C,N)]  % Multiple goals to list module
```

### 3.3 Runtime Resolution

When `M # G` is encountered:
1. Runtime looks up module `M` in ServiceRegistry
2. If not loaded, module is loaded and compiled dynamically
3. Goal `G` is verified to be exported by `M`
4. Goal is executed in `M`'s context
5. Results unify back to caller's variables

### 3.4 Rules

1. Target module resolved at runtime (not compile time)
2. The goal's predicate must be exported by the target module
3. Variable modes (reader/writer) are preserved across the call
4. The remote call is a body goal (not allowed in guards)
5. Import declaration is advisory; actual resolution is dynamic

### 3.5 Reduction

A remote call `M # G` reduces as follows:
1. Resolve module `M` via ServiceRegistry
2. If not loaded: load_module(M) → compile → register
3. Verify `G`'s predicate is exported by `M`
4. Reduce `G` using `M`'s procedures
5. Unifications affect the caller's variables

## 4. Visibility Rules

### 4.1 Exported Procedures

Exported procedures can be called from any module via `#`.

```glp
% In module A
-module(a).
-export([public/1]).

public(X) :- private(X).  % OK: internal call
private(X) :- X = 42.

% In module B
-module(b).
-import([a]).

test(R) :- a # public(R).   % OK: public is exported
test2(R) :- a # private(R). % ERROR: private not exported
```

### 4.2 Private Procedures

Procedures not listed in `-export` are private to the module.

### 4.3 Self Reference

Within a module, procedures call each other directly (no `#` needed).

```glp
-module(math).
-export([factorial/2]).

factorial(0, 1).
factorial(N?, F) :-
  N? > 0 |
  N1 := N? - 1,
  factorial(N1?, F1),   % Direct call, not self # factorial(...)
  F := N? * F1?.
```

### 4.4 No Export Declaration

If no `-export` declaration, all procedures are exported (legacy mode).

## 5. System Modules

System modules are pre-loaded and available without explicit import.

| Module | Purpose |
|--------|---------|
| `system` | Process control, meta-predicates |
| `io` | Input/output operations |
| `math` | Arithmetic operations (`:=` predicate) |

System modules can be used directly:
```glp
run(X) :-
  io # write("Hello"),
  X := 2 + 3.           % math is implicit for :=
```

## 6. Service Types

### 6.1 Procedures (Default)

Stateless exported predicates. Each call is independent.

```glp
-module(math).
-service_type(procedures).  % Optional, this is default
-export([factorial/2]).

factorial(0, 1).
factorial(N?, F) :- ...
```

### 6.2 Monitor

Stateful server maintaining internal state across calls.

```glp
-module(counter).
-service_type(monitor).
-export([increment/1, get/1, reset/0]).

% Monitor server loop (FCP pattern)
server(State?, Requests?) :-
  Requests? = [Req? | Rest?] |
  handle(State?, Req?, NewState),
  server(NewState?, Rest?).

handle(N?, increment(Delta?), NewN) :- NewN := N? + Delta?.
handle(N?, get(Value), N?) :- Value = N?.
handle(_, reset, 0).
```

### 6.3 Director

Namespace container for organizing other modules.

```glp
-module(utils).
-service_type(director).
% Contains utils.list, utils.math, etc.
```

## 7. SRSW Across Module Boundaries

GLP's Single-Reader/Single-Writer constraint applies across modules.

### 7.1 Mode Preservation

Variable modes are preserved when crossing module boundaries:

```glp
% Module A
-module(a).
-export([caller/1]).

caller(X) :- b # callee(X).  % X is writer, passed to B

% Module B
-module(b).
-export([callee/1]).

callee(Y) :- Y = 42.  % Y receives writer mode, can bind it
```

### 7.2 Reader Arguments

```glp
% Module A
caller(X?) :- b # lookup(X?, R).  % X? is reader, R is writer

% Module B
lookup(Key?, Value) :- ...  % Key receives reader, Value is writer
```

## 8. Module File Structure

A module file has this structure:

```glp
% Module declaration (first if present)
-module(name).

% Other declarations (any order)
-export([proc1/arity1, proc2/arity2]).
-import([module1, module2]).
-language(glp).
-mode(user).
-service_type(procedures).

% Procedure definitions
proc1(...) :- ...
proc2(...) :- ...
```

## 9. Hierarchical Module Names

Module names can be hierarchical using dot notation:

```glp
-module(utils.list).
-module(utils.math).
-module(app.handlers.http).
```

Import uses the full name:
```glp
-import([utils.list, utils.math]).
```

Usage:
```glp
run(R) :-
  utils.list # append([1,2], [3,4], R).
```

Hierarchical names map to service hierarchy paths:
- `utils.list` → `[utils, list]`
- `app.handlers.http` → `[app, handlers, http]`

## 10. Examples

### 10.1 Simple Module

```glp
-module(greeting).
-export([hello/1, goodbye/1]).

hello(Name?) :-
  io # write("Hello, "),
  io # writeln(Name?).

goodbye(Name?) :-
  io # write("Goodbye, "),
  io # writeln(Name?).
```

### 10.2 Module with Dependencies

```glp
-module(statistics).
-export([mean/2, sum/2]).
-import([list]).

sum([], 0).
sum([X?|Xs?], S) :-
  sum(Xs?, S1),
  S := X? + S1?.

mean(List?, Avg) :-
  sum(List?, Total),
  list # length(List?, Count),
  Avg := Total? / Count?.
```

### 10.3 Using Multiple Modules

```glp
-module(main).
-import([statistics, list]).

analyze(Data?, Result) :-
  list # length(Data?, N),
  statistics # mean(Data?, Avg),
  statistics # sum(Data?, Total),
  Result = {count: N?, average: Avg?, total: Total?}.
```

### 10.4 Monitor Module (Stateful)

```glp
-module(cache).
-service_type(monitor).
-export([get/2, put/2, clear/0]).

% Server with state (Map of key-value pairs)
server(Cache?, Requests?) :-
  Requests? = [Req? | Rest?] |
  handle(Cache?, Req?, NewCache),
  server(NewCache?, Rest?).

handle(Cache?, get(Key?, Value), Cache?) :-
  map # lookup(Cache?, Key?, Value).
handle(Cache?, put(Key?, Value?), NewCache) :-
  map # insert(Cache?, Key?, Value?, NewCache).
handle(_, clear, {}).
```

## 11. Error Conditions

| Error | Description |
|-------|-------------|
| `unknown_module` | Module not found in service registry |
| `not_exported` | Remote call to non-exported procedure |
| `undefined_export` | Export lists undefined procedure |
| `duplicate_module` | Two modules with same name |
| `circular_import` | Module A imports B which imports A |
| `invalid_service_type` | Unknown service type |

## 12. Compatibility

Programs without module declarations work unchanged. They define an anonymous module where all procedures are implicitly exported.

```glp
% No -module declaration - legacy/simple mode
factorial(0, 1).
factorial(N?, F) :- ...

% Can still use system modules
run(X) :- X := factorial(5).
```

## 13. Hot Reloading

Modules can be reloaded at runtime for development:

```glp
system # reload(math)     % Reload math module
system # unload(cache)    % Unload cache module
```

Reloading:
1. New version compiled
2. ServiceRegistry entry replaced
3. New calls use new version
4. In-flight calls complete with old version

---

*Document Version: 2.0*
*Updated to include FCP declarations (-language, -mode, -service_type)*
*Runtime resolution model, not static linking*
