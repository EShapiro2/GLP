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
