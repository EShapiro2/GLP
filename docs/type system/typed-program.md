# Typed GLP Program

**Paper Reference**: Definition 4.1, Section 4.1

## Definition 4.1 (Typed GLP Program)

> A typed GLP program P = (Cs, D) has GLP clauses Cs and a GLP type D defining the type of every procedure in Cs.

## Type Definitions

Types are specified using BNF rules with alternatives separated by `;`:

```
Stream ::= [] ; [_|Stream].
CounterCall ::= add ; clear ; read(Integer?).
```

The duality operator `?` is implicit: defining `Stream` also defines `Stream?`.

## Procedure Declarations

Each procedure has a type declaration:

```
procedure merge(Stream?, Stream?, Stream).
procedure monitor(Integer?, Stream(CounterCall)?).
```

Arguments marked with `?` are inputs (consumed); unmarked arguments are outputs (produced).

## Primitive Types

| Type | Dual | Description |
|------|------|-------------|
| `_` | `_?` | Any produced/consumed term (wildcard) |
| `Integer` | `Integer?` | Any integer literal |
| `Real` | `Real?` | Any real literal |
| `Number` | `Number?` | Any numeric literal |
| `String` | `String?` | Any string literal |

## Type Classification

| Classification | Definition | Example |
|----------------|------------|---------|
| Output type | No `?` in definition | `Stream ::= [] ; [_\|Stream]` |
| Input type | Dual of output type | `Stream?` |
| Interactive type | Contains internal `?` | `HollowStream ::= [] ; [_?\|HollowStream]` |

## Program Structure and Declaration Ordering

A well-formed typed GLP program must follow these ordering rules:

### Type Declarations

Type declarations (`T ::= ...`) may appear **anywhere in the file before first use**. A type must be declared before it appears in:
- Another type definition
- A procedure declaration
- A clause

### Procedure Declarations

Procedure declarations (`procedure p(...)`) must appear **immediately before** the first clause of that procedure. Specifically:

1. The procedure declaration must precede all clauses for that procedure
2. No other clauses or declarations may appear between the procedure declaration and its first clause
3. Comments and blank lines are permitted between the declaration and first clause
4. All clauses for a procedure must be **contiguous** (no other procedure's clauses may intervene)

### Example: Well-formed Program

```glp
%% Type used by merge
Stream ::= [] ; [_|Stream].

%% Procedure declaration immediately before clauses
procedure merge(Stream?, Stream?, Stream).
merge([], Ys, Ys?).
merge(Xs, [], Xs?).
merge([X|Xs], [Y|Ys], [X?|Zs?]) :- merge(Xs?, [Y?|Ys?], Zs).
merge([X|Xs], [Y|Ys], [Y?|Zs?]) :- merge([X?|Xs?], Ys?, Zs).

%% Another type, declared before use
Counter ::= counter(Integer).

%% Another procedure
procedure increment(Counter?, Counter).
increment(counter(N), counter(N1?)) :- N1 := N? + 1.
```

### Example: Ill-formed Program (rejected)

```glp
procedure merge(Stream?, Stream?, Stream).
Stream ::= [] ; [_|Stream].   %% ERROR: Type declaration between procedure declaration and clause
merge([], Ys, Ys?).
```

```glp
procedure merge(Stream?, Stream?, Stream).
merge([], Ys, Ys?).
procedure increment(Counter?, Counter).  %% Another procedure declaration
merge(Xs, [], Xs?).           %% ERROR: Non-contiguous clauses for merge
```

### Untyped Procedures

If a procedure appears in clauses but has no procedure declaration, it is **untyped**. The type checker reports an error for untyped procedures. GLP programs may be typed or untyped, but the type checker only validates typed programs.

## Parameterized Types

**Paper Reference**: Section 8, Definition 8.1

Parameterized types are syntactic sugar. A parameterized type definition is a template; each use with concrete type arguments is expanded into a fresh monomorphic type definition before type automaton construction. After expansion, the existing type checking and subtype checking machinery apply without modification.

### Parameterized Type Definitions

A parameterized type definition introduces type parameters, written as uppercase identifiers in parentheses after the type name:

```
Stream(X) ::= [] ; [X | Stream(X)].
Pair(A, B) ::= pair(A, B).
Channel(In, Out) ::= ch(In, Out?).
```

Type parameters are identifiers that may appear in place of type names within the alternatives. A parameterized type definition is a template: it does not itself denote a type. Only instantiations yield types.

Mode annotations within the template (e.g., `Out?` in `Channel`) are preserved during expansion.

**Finiteness rule.** Where a parameterized type refers to itself, directly or transitively, no parameter may occur as a *proper subterm* of an argument **of the self-referential occurrence**. `Stream(X) ::= [] ; [X | Stream(X)]` is admissible — the self-reference's argument is the bare parameter `X`. `StreamBox(X) ::= [Box(X) | StreamBox(X)]` is also admissible — the self-reference `StreamBox(X)` has the bare parameter `X`; the `Box(X)` is a *sibling* element, not an argument of the self-reference, and `StreamBox(Msg)` expands finitely (it needs only `StreamBox(Msg)` and `Box(Msg)`). `Bad(X) ::= leaf ; node(Bad(Box(X)))` is rejected — in the self-reference `Bad(Box(X))`, `X` occurs as a proper subterm of the argument `Box(X)`, so the reachable types would grow without bound (`Bad(Box(Box(…)))`). The restriction applies only to arguments of the self-referential occurrence, not to other occurrences of the parameter in the body. This rule bounds the types reachable from a program, so expansion terminates. It is enforced statically, at the parsing/expansion stage, before any expansion runs.

### Instantiation

An instantiation supplies a concrete type for each parameter:

```
Stream(Integer)                  % stream of integers
Stream(AgentMsg)                 % stream of agent messages
Channel(FriendMsg, FriendMsg)    % bidirectional friend channel
Stream(Pair(Integer, String))    % stream of pairs (nested)
```

Instantiating `Stream(_)` recovers the original unparameterized definition `Stream ::= [] ; [_ | Stream]`.

### Parameterized Procedure Declarations

A procedure declaration may use type parameters to express uniform behaviour:

```
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
```

The parameter `X` is implicitly universally quantified. When the procedure is called, the type checker infers the instantiation from the call context by structural matching: each argument type in the declaration is matched against the corresponding concrete type at the call site, and each type parameter is bound to the concrete type that occupies its position. A parameter may be bound either from an argument that **is** a bare parameter at a top-level argument position — e.g. the message type `X` in `send(X?, Channel(Y, Stream(X))?, ...)`, bound from the first argument — or from a parameter **nested** within a parameterized argument type, e.g. `X` in `Stream(X)`. If different argument positions yield conflicting bindings for the same parameter, the type checker reports an error.

A parameterized procedure declaration is a **clause template**. A call in a clause body to a parameterized procedure induces an instantiation of the callee: the caller's bindings make the call's argument types concrete, and matching them against the callee's declaration binds its parameters. The **instantiations** of a program form the least set containing the instantiation of every call in a monomorphic clause and **closed under this induction**, except that **recursion is monomorphic**: a call to a procedure already being instantiated on the current cycle is checked at *that* instantiation rather than inducing a new one, and a call that would require a *different* instantiation of such a procedure is a type error. Recursion thus induces no instantiation, and the non-recursive calls induce finitely many (the reachable types are finite by the Finiteness rule), so the set is finite. For example, `ploop(S) :- wrap(S?, W), ploop(W?)` with `wrap(Stream(X)?, Stream(Box(X)))` is ill-typed: at the clause's instantiation `X = Msg`, `W : Stream(Box(Msg))`, but the recursive `ploop(W?)` is checked at the enclosing instantiation `ploop(Stream(Msg)?)`, and `Stream(Box(Msg))` is not `Stream(Msg)`. For each instantiation in the set, the instantiated procedure's clauses are checked by Well-Typed Clause (Definition 5.7) against the monomorphic declaration that instantiation produces. A clause may destructure a parameter: at an argument position whose type is a parameter, the clause is checked against the constructors the instantiation supplies, and an instantiation whose argument lacks a destructured constructor is rejected at that instantiation. Procedures that pass a parameter opaquely (`merge`, `send`) check identically at every instantiation; a procedure that destructures a parameter — e.g., a `lib/` router whose clause matches `user_output(...)` — requires this per-instantiation reading.

Multiple parameters are supported:

```
procedure relay(Stream(X)?, Stream(X), Channel(X, X)?).
```

### Programs and Modules

Parameterized type definitions and parameterized procedure declarations are syntactic sugar; they have no well-typing of their own. Well-typing (Well-Typed Program, Definition 5.10) is defined only after expansion, on a typed GLP program `(Cs, D)` in which **no free type parameter remains** — every type reached is a concrete user-defined or primitive type. **The unit of type checking is the program**: a checker expands it (collecting the instantiation set above and replacing every parameterized reference by its expanded monomorphic name) and applies Definition 5.10 to the resulting `(Cs, D)`. A fragment that still contains a free type parameter after expansion is **not a program and is not type-checked**.

Checking a parameterized procedure with its parameter left free — equivalently, treating the parameter as the wildcard `_` — is **unsound**, because a clause that inspects the parameter is then accepted vacuously. A parameterized procedure acquires concrete types, and is checked, only within a program that instantiates it; with every type then concrete, the variable-pair conditions of Well-Typed Clause (Definition 5.7) apply, so a writer/reader mode mismatch in an instantiated clause is rejected.

In the modular setting (`../modules/glp-module-system-spec.md`), a **program** is a finite set of modules scoped as a hierarchy with one or more concrete initial goals; its **linked program** is the typed GLP program obtained by parameterized-type expansion, procedure instantiation, and linking, defined only when expansion leaves no free type parameter. A program is **well-typed** if its linked program is well-typed. Soundness (type soundness) is a property of the linked program, hence of the program, and makes no claim about a module checked outside a program.

### Modular Checking via Abstract Parameters

The per-instantiation rule above checks a parameterized procedure once per concrete type its program supplies, so a procedure with **no caller** in its program goes unchecked. A procedure that never **inspects** its parameters can instead be checked **once, for all instantiations at once** (paper: `sec:abstract-parameters`). Treating the parameter as the wildcard `_` for this is unsound (above); treating it as an **abstract type** is sound and enforces the parametricity discipline.

**Abstract type.** An *abstract type* is a type-automaton state with **no outgoing transitions**, distinct from the wildcard `_`. By the leaf-consistency definition, a moded term path is consistent with an abstract type at a position exactly when the term there is a **variable of the position's mode**: a variable is consistent with any same-mode type symbol, whereas a functor or constant has no transition to take and matches no wildcard, so it is **inconsistent**. In the implementation an abstract type is a synthesized type definition with **zero alternatives**; its automaton is therefore empty, which delivers exactly this behaviour (variable consistent; functor/constant inconsistent) with no special-casing of the DFA state.

**Abstract instance.** The *abstract instance* of a parameterized procedure declaration replaces each type parameter by a **distinct abstract type** and expands the result like any instantiation (e.g. `pconsumer(Stream(X)?)` becomes `pconsumer(Stream<$0>?)` with `$0` an abstract type and `Stream<$0>` its expanded monomorphic type).

**Parametrically well-typed.** A parameterized procedure is *parametrically well-typed* if, **by its abstract instance**, (1) its clauses are well-typed (Well-Typed Clause, Definition 5.7) and (2) every input path is accepted by some clause (input-accepting coverage, the contravariance condition). Coverage is part of the property: the abstract instance has the same outer alternatives as every concrete instance (only the parameter positions differ, and there a variable covers everything), so coverage transfers to every instantiation.

**Parametricity.** If a parameterized procedure is parametrically well-typed **and no type parameter occurs as a top-level alternative of a type definition**, then for every instantiation the instantiated clauses are well-typed and input-accepting. Such a procedure is **certified**: checked once and certified for every instantiation; the linked program need not re-check its clauses.

**Routing.** Before checking, a parameterized procedure is routed structurally:

- It **inspects a parameter** if some clause head places a functor or constant at a parameter position (e.g. `pconsumer([befriend(From, Resp?)|Rest])` places `befriend/2` at the `Stream(X)` element position). Such a procedure is **not parametrically well-typed** and takes the **per-instantiation route** (above): never instantiated ⇒ not checked; instantiated ⇒ checked per instantiation.
- A **parameter used as a top-level alternative of a type definition** (e.g. `Box(X) ::= X ; empty`) likewise takes the per-instantiation route, its determinism resting on the instantiation.
- Otherwise the procedure takes the **abstract route**: its abstract instance is checked (clause well-typing + input coverage) by running it through the instantiation closure seeded with that abstract instance, so any type its body induces is materialized and a type-changing recursive call is checked under monomorphic recursion (a non-monomorphic recursion is then a genuine duality clash against the materialized type). Only the abstract instance's own verdict is reported; the callee instantiations it induces are discarded here (they belong to the program closure when the procedure is actually instantiated). Any failure is a **genuine error** (e.g. a coverage gap, a mode mismatch on a concrete position, or a non-monomorphic recursion), reported once. The abstract route is a **commitment**: the procedure is certified whether the check passes or fails, so the program closure never re-reports it.

**Closure interaction.** A certified procedure is excluded from per-instantiation clause re-checking: when the instantiation closure reaches a concrete instantiation of a certified procedure, its clause well-typing and coverage are **suppressed** (already guaranteed by the abstract-instance verdict), while the callee instantiations its body induces are **still propagated** to the closure, so non-certified callees are checked. A callee that a procedure instantiates at a concrete type (independent of the parameter) is thus checked through the closure exactly when that procedure is actually instantiated — not on the procedure's behalf during certification.

### Expansion Algorithm

Expansion runs as a preprocessing step, after parsing and before type automaton construction.

**Expansion rule.** Given a parameterized type definition `T(X₁, ..., Xₖ) ::= A₁ ; ... ; Aₙ` and an instantiation `T(S₁, ..., Sₖ)`, the expansion is:

```
T<S₁,...,Sₖ> ::= A₁[S₁/X₁, ..., Sₖ/Xₖ] ; ... ; Aₙ[S₁/X₁, ..., Sₖ/Xₖ]
```

where `T<S₁,...,Sₖ>` is a fresh type name and `Aᵢ[Sⱼ/Xⱼ]` denotes simultaneous substitution. Recursive references to `T(X₁,...,Xₖ)` in the body are replaced by `T<S₁,...,Sₖ>`.

**Steps:**

1. **Collect templates.** Scan all type definitions. A definition whose name is followed by a parenthesized parameter list is a template; all others are monomorphic. Templates are recorded but not added to the type environment.

2. **Collect instantiations.** Scan all type definitions, procedure declarations (including `imported` and `exported` declarations), and type definition bodies for parameterized type references `T(S₁, ..., Sₖ)`. Each distinct instantiation is recorded. A type definition body may reference a different parameterized type; such cross-references are collected here. Scan also every clause body for calls to a parameterized procedure; each call, with the enclosing clause's parameters bound, records an instantiation of the callee. An instantiated procedure's body may call further parameterized procedures, so repeat the clause-body scan over each new procedure instantiation until none is added; the types reachable from a program are finite (Finiteness rule), so this terminates.

3. **Expand.** For each recorded instantiation, generate a fresh monomorphic type definition by substituting parameters and renaming. Recursive references within the template body are expanded to the same fresh name. Nested instantiations are expanded inside-out.

4. **Replace references.** In all type definitions and procedure declarations, replace every parameterized type reference `T(S₁, ..., Sₖ)` with its expanded name `T<S₁, ..., Sₖ>`.

5. **Remove templates.** Remove parameterized type definitions from the type environment. Only the expanded monomorphic definitions remain.

After this step, the program contains only monomorphic type definitions. A parameterized procedure declaration with a free parameter is not expanded here; it is monomorphized per inferred instantiation during well-typing (see Parameterized Procedure Declarations). Type automaton construction, well-typing analysis, and subtype checking proceed without modification.

### Expansion Example

From `Stream(X) ::= [] ; [X | Stream(X)]`, the instantiation `Stream(Integer)` expands to:

```
Stream<Integer> ::= [] ; [Integer | Stream<Integer>].
```

From `Channel(In, Out) ::= ch(In, Out?)`, the instantiation `Channel(FriendMsg, FriendMsg)` expands to:

```
Channel<FriendMsg,FriendMsg> ::= ch(FriendMsg, FriendMsg?).
```

### Subtyping

Since parameterized types expand to monomorphic types, subtyping is handled entirely by the existing DFA-based subtype check. No additional subtyping rules are needed. Variance (covariant, contravariant, invariant) is determined automatically by the subtype check on the expanded types — no explicit variance annotations are required.

### Interaction with Modules

A module that imports a parameterized procedure instantiates its type parameters to the specific message type used locally:

```
imported procedure merge(Stream(CounterCall)?, Stream(CounterCall)?, Stream(CounterCall)).
```

The defining module's parameterized declaration is instantiated at the importing site. Type checking of the importing module uses the expanded monomorphic type. Because type identity is structural, independently expanded types are compatible if their type automata are equivalent.

## Guards

Guards are procedure calls with predefined signatures:

```
procedure integer(Integer?).
procedure number(Number?).
procedure string(String?).
procedure ground(_?).
procedure Exp? < Exp?.
procedure Exp? > Exp?.
...
```

For type checking, the guard separator `|` is treated as conjunction: `H :- G | B` is checked as `H :- G, B`.
