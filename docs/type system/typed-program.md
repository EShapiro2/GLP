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

The parameter `X` is implicitly universally quantified. When the procedure is called, the type checker infers the instantiation from the call context by structural matching: each parameterized type in the declaration is matched against the corresponding concrete type at the call site, and each type parameter is bound to the concrete type that occupies its position. If different argument positions yield conflicting bindings for the same parameter, the type checker reports an error.

Multiple parameters are supported:

```
procedure relay(Stream(X)?, Stream(X), Channel(X, X)?).
```

### Expansion Algorithm

Expansion runs as a preprocessing step, after parsing and before type automaton construction.

**Expansion rule.** Given a parameterized type definition `T(X₁, ..., Xₖ) ::= A₁ ; ... ; Aₙ` and an instantiation `T(S₁, ..., Sₖ)`, the expansion is:

```
T<S₁,...,Sₖ> ::= A₁[S₁/X₁, ..., Sₖ/Xₖ] ; ... ; Aₙ[S₁/X₁, ..., Sₖ/Xₖ]
```

where `T<S₁,...,Sₖ>` is a fresh type name and `Aᵢ[Sⱼ/Xⱼ]` denotes simultaneous substitution. Recursive references to `T(X₁,...,Xₖ)` in the body are replaced by `T<S₁,...,Sₖ>`.

**Steps:**

1. **Collect templates.** Scan all type definitions. A definition whose name is followed by a parenthesized parameter list is a template; all others are monomorphic. Templates are recorded but not added to the type environment.

2. **Collect instantiations.** Scan all type definitions, procedure declarations (including `imported` and `exported` declarations), and type definition bodies for parameterized type references `T(S₁, ..., Sₖ)`. Each distinct instantiation is recorded. A type definition body may reference a different parameterized type; such cross-references are collected here.

3. **Expand.** For each recorded instantiation, generate a fresh monomorphic type definition by substituting parameters and renaming. Recursive references within the template body are expanded to the same fresh name. Nested instantiations are expanded inside-out.

4. **Replace references.** In all type definitions and procedure declarations, replace every parameterized type reference `T(S₁, ..., Sₖ)` with its expanded name `T<S₁, ..., Sₖ>`.

5. **Remove templates.** Remove parameterized type definitions from the type environment. Only the expanded monomorphic definitions remain.

After this step, the program contains only monomorphic type definitions and procedure declarations. Type automaton construction, well-typing analysis, and subtype checking proceed without modification.

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
