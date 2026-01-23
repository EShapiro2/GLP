# Well-Typed Program

**Paper Reference**: Definition 5.10

## Definition 5.10 (Well-typed GLP program)

> A typed GLP program P = (Cs, D) is **well-typed** if:
>
> 1. **Output conformance**: Every clause C ∈ Cs is well-typed by D.
>
> 2. **Input coverage**: Every input path in every procedure type in D has a clause C ∈ Cs that accepts it.

## Output Conformance

Every clause must satisfy all three conditions of Definition 5.7 (well-typed clause). This ensures the program produces terms within declared types.

## Input Coverage

For every procedure with input arguments, every possible input (as specified by the type) must be accepted by at least one clause head. This ensures the program can consume all values permitted by input types.

Coverage is checked by traversing the type automaton's input paths and verifying each is accepted by some clause.

## Wildcard Types and Coverage

From Definition 5.11: The states `_` and `_?` are **final states** with no outgoing transitions. A procedure argument typed `_?` requires no coverage checking — the type simply declares "any consumed term is acceptable" without requiring clauses to enumerate alternatives.

## Error Reporting

Errors are reported as simple strings. Example error messages:

- "Clause 3 of merge/3 not well-typed: ..."
- "Input coverage gap: merge/3 argument 1, uncovered alternative 'foo/2'"
