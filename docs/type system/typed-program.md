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
