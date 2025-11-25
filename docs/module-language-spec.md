# GLP Module Language Specification

**Version 1.0**

## 1. Overview

This specification defines the syntax and semantics for modules in GLP. A module is a named collection of procedures with explicit exports and imports.

## 2. Module Declaration

### 2.1 Syntax

```
ModuleDecl     ::= '-module(' ModuleName ').'
ModuleName     ::= Atom | Atom '.' ModuleName

ExportDecl     ::= '-export([' ExportList ']).'
ExportList     ::= ProcRef | ProcRef ',' ExportList
ProcRef        ::= Atom '/' Integer

ImportDecl     ::= '-import([' ImportList ']).'
ImportList     ::= ModuleName | ModuleName ',' ImportList
```

### 2.2 Semantics

**Module Declaration**: Names the module. Must appear first if present.
```glp
-module(math).
```

**Export Declaration**: Lists procedures callable from other modules.
```glp
-export([factorial/2, gcd/3]).
```

**Import Declaration**: Declares module dependencies.
```glp
-import([list, io]).
```

### 2.3 Rules

1. At most one `-module` declaration per file
2. Module name must be unique across the program
3. Exported procedures must be defined in the module
4. Imports must be resolvable at compile time
5. If no `-module` declaration, the file defines an anonymous module

## 3. The Remote Call Operator (`#`)

### 3.1 Syntax

```
RemoteGoal     ::= ModuleName '#' Goal
                 | ModuleName '#' '[' GoalList ']'
GoalList       ::= Goal | Goal ',' GoalList
```

### 3.2 Semantics

The `#` operator routes a goal to another module for reduction.

```glp
math # factorial(5, F)      % Call factorial/2 in module math
list # [append(A,B,C), length(C,N)]  % Multiple goals to list module
```

### 3.3 Rules

1. The target module must be imported or be a system module
2. The goal's predicate must be exported by the target module
3. Variable modes (reader/writer) are preserved across the call
4. The remote call is a body goal (not allowed in guards)

### 3.4 Reduction

A remote call `M # G` reduces as follows:
1. Resolve module `M` to its compiled code
2. Verify `G`'s predicate is exported by `M`
3. Reduce `G` using `M`'s procedures
4. Unifications affect the caller's variables

## 4. Visibility Rules

### 4.1 Exported Procedures

Exported procedures can be called from any module that imports the exporting module.

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

## 6. SRSW Across Module Boundaries

GLP's Single-Reader/Single-Writer constraint applies across modules.

### 6.1 Mode Preservation

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

### 6.2 Reader Arguments

```glp
% Module A
caller(X?) :- b # lookup(X?, R).  % X? is reader, R is writer

% Module B
lookup(Key?, Value) :- ...  % Key receives reader, Value is writer
```

## 7. Module File Structure

A module file has this structure:

```glp
% Optional module declaration (first if present)
-module(name).

% Optional export declaration
-export([proc1/arity1, proc2/arity2]).

% Optional import declarations
-import([module1, module2]).

% Procedure definitions
proc1(...) :- ...
proc2(...) :- ...
```

## 8. Hierarchical Module Names

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

## 9. Examples

### 9.1 Simple Module

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

### 9.2 Module with Dependencies

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

### 9.3 Using Multiple Modules

```glp
-module(main).
-import([statistics, list]).

analyze(Data?, Result) :-
  list # length(Data?, N),
  statistics # mean(Data?, Avg),
  statistics # sum(Data?, Total),
  Result = {count: N?, average: Avg?, total: Total?}.
```

## 10. Error Conditions

| Error | Description |
|-------|-------------|
| `unknown_module` | Import references undefined module |
| `not_exported` | Remote call to non-exported procedure |
| `undefined_export` | Export lists undefined procedure |
| `duplicate_module` | Two modules with same name |
| `circular_import` | Module A imports B which imports A |

## 11. Compatibility

Programs without module declarations work unchanged. They define an anonymous module where all procedures are implicitly exported.

```glp
% No -module declaration - legacy/simple mode
factorial(0, 1).
factorial(N?, F) :- ...

% Can still use system modules
run(X) :- X := factorial(5).
```
