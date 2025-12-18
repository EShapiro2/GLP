# GLP Programming Knowledge

Accumulated knowledge and techniques for GLP programming.

## Mode Inversion Rule

**When calling a predicate, the modes in the goal are the precise inverse of the modes in the head of the clause.**

This applies to all predicate calls, including defined guards.

### Example: Channel Creation

**Unit clause** (defines new_channel as a guard):
```prolog
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

Head modes: `ch(reader, writer), ch(reader, writer)`

**Correct call** (modes inverted):
```prolog
new_channel(ch(PQIn, PQOut?), ch(QPIn, QPOut?))
```

Call modes: `ch(writer, reader), ch(writer, reader)`

### Rationale

The head reader positions receive values from the caller (so caller provides writers). The head writer positions produce values for the caller (so caller provides readers to receive them).

### Common Error

```prolog
% WRONG - all writers in call
new_channel(ch(A, B), ch(C, D))

% CORRECT - modes inverted from head
new_channel(ch(A, B?), ch(C, D?))
```

---

## Change Log

- 2025-12-18: Added Mode Inversion Rule based on friend_introduction.glp analysis
