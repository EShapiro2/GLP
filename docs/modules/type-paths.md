# Module: type-paths

**Version**: 0.2  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Lines 30-35, 213-228

## Purpose

Defines paths(D) — the regular set of moded paths defined by a GLP type D.

## Dependencies

- `moded-term` — Mode enum, path representation

## Paper Definitions

### Type Paths (line 213)

> "A defined GLP type D also defines a regular set of moded paths, denoted paths(D), in which every symbol corresponds to a type and the last symbol is a primitive type."

### Vocabulary Difference (lines 214-226)

Type paths and term paths have different vocabularies at their leaves:

| Primitive Term | Primitive Type |
|----------------|----------------|
| `1` (integer) | `Integer`, or `1` |
| `c` (string) | `String`, or `c` |
| `X?` (reader) | `_?` (consumed) |
| `X` (writer) | `_` (produced) |

### DFA Correspondence (line 30)

> "We impose standard restrictions on GLP type definitions so that they correspond to a DFA in which every state is a defined type, except for final states which are primitive types."

### Determinism Requirement (lines 31-35)

> "the requirement that the BNF be deterministic applies also to modes, so the following declarations are illegal:"
> ```
> Any ::= _ ; _?.
> AnyOne ::= 1 ; 1?.
> ```

## Formal Definition

### Type Path

A **type path** is a sequence:
```
(0,m₀) → T₀ --(i₁,m₁)--> T₁ --(i₂,m₂)--> ... --(iₙ,mₙ)--> Tₙ
```

Where:
- Each `Tₖ` is a type (defined type name or primitive type)
- Each `(iₖ,mₖ)` is an edge label with argument index `iₖ` and mode `mₖ ∈ {↓,↑}`
- `Tₙ` is a **primitive type**: `_`, `_?`, `Integer`, `String`, or a constant

### Primitive Types

| Primitive | Mode | Meaning |
|-----------|------|---------|
| `_` | ↑ (produce) | Any produced term |
| `_?` | ↓ (consume) | Any consumed term |
| `Integer` | — | Any integer constant |
| `String` | — | Any string constant |
| constant | — | Specific value (e.g., `[]`, `0`) |

### Mode Propagation

For a type reference `T` or `T?` at position with parent mode `m`:
- `T` (no complement): child mode = `m`
- `T?` (complement): child mode = `m̄` (complement of m)

## Examples

### Example: paths(Stream) where `Stream ::= [] ; [_|Stream]`

From the produce perspective (↑):
```
Path 1: (0,↑) → Stream --(1,↑)--> []
Path 2: (0,↑) → Stream --(1,↑)--> [|] --(1,↑)--> _
Path 3: (0,↑) → Stream --(1,↑)--> [|] --(2,↑)--> Stream → ...
```

### Example: paths(Stream?) — complemented

From the consume perspective (↓):
```
Path 1: (0,↓) → Stream? --(1,↓)--> []
Path 2: (0,↓) → Stream? --(1,↓)--> [|] --(1,↓)--> _?
Path 3: (0,↓) → Stream? --(1,↓)--> [|] --(2,↓)--> Stream? → ...
```

### Example: Type paths for merge(Stream?, Stream?, Stream)

Paper lines 254-264:

**Argument 1 (Stream?):**
```
(0,↓) → merge --(1,↓)--> Stream? --(1,↓)--> "."/2 --(1,↓)--> _?
```

**Argument 3 (Stream):**
```
(0,↓) → merge --(3,↑)--> Stream --(1,↑)--> "."/2 --(1,↑)--> _
```

Note: Root mode is ↓ because procedure head is being matched (consumed).

## Interface

### `paths(D: Type) → Set<TypePath>`

Returns the (conceptually infinite) regular set of moded paths for type D.

**Implementation note:** Since types are recursive (e.g., Stream), paths(D) is infinite. The DFA representation captures this finitely — cycles in the DFA represent infinite path families.

### `inputPaths(D) = {p ∈ paths(D) | p starts with ↓}`

### `outputPaths(D) = {p ∈ paths(D) | p starts with ↑}`

## Relationship to DFA

Per line 30, type D corresponds to a DFA where:
- States = defined type names
- Final states = primitive types  
- Transitions labelled by (argIndex, mode)

The DFA **accepts** exactly paths(D).

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.2 | 2025-01-07 | Simplified to paper definitions only |
