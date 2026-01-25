# Handover: Social Graph Protocol Type Checking

**Date**: 2026-01-24  
**Status**: In Progress — One type error remaining  
**File**: `/programs/typed_book/social_graph/social_graph_protocol.glp`

---

## Objective

Integrate working GLP social graph programs into a single self-contained file with proper type declarations that pass both SRSW checking and type checking.

---

## Approach

The integration started from programs known to pass the type checker independently:

- `merge_simple.glp` — Fair merge of two streams (passes)
- `channel.glp` — Channel operations: send, receive, new_channel (passes)

Additional components were added incrementally: tag_stream, lookup, update, response handling, bind_response, agent initialization, and the social_graph main loop.

---

## Current State

The integrated file passes SRSW checking but has one remaining type error:

```
Body atom 1 (handle_response) is not well-typed:
  Variable mode mismatch: reader requires ↓ (consume), got ↑ (produce)
  Path: (accept/1, 0, input) → (OurCh?, 1, output) at line 93
    in: bind_response(7 args) :- 2 goals.
```

---

## Root Cause Analysis

The error occurs in the `bind_response` clause:

```prolog
bind_response(yes, From, accept(TheirCh?), Fs, Fs1?, In, In1?) :-
    new_channel(OurCh, TheirCh),
    handle_response(accept(OurCh?), From?, Fs?, Fs1, In?, In1).
```

The current type declaration is `Response ::= accept(Channel?) ; no`.

When `Response?` is consumed (as in `handle_response(Response?, ...)`), the inner `Channel?` flips to `Channel` (produce mode ↑). Therefore the position expects a writer, but the code has `OurCh?` (a reader).

Changing to `OurCh` (writer) would create an SRSW violation since `OurCh` is already produced by `new_channel`.

The fundamental tension is that the same channel value must be both produced by `new_channel` and then passed to another procedure. SRSW requires each writer occurrence be unique, but the type system's mode rules require a writer at that position given the current type declaration.

---

## Possible Solutions

**Option 1: Change the type declaration.** Modify the Response type from `Response ::= accept(Channel?) ; no` to `Response ::= accept(Channel) ; no` (without mode complement). This changes the semantics: when Response is produced, Channel is produced; when Response? is consumed, Channel is consumed (becomes Channel?). Then `handle_response(accept(OurCh?), ...)` would have `OurCh?` at a consume position, which matches the reader form.

**Option 2: Restructure the program.** Avoid passing the channel through the `accept(...)` wrapper in the internal call. Handle the channel separately from the response.

**Option 3: Use an intermediate variable.** Use explicit assignment to convert the writer to a reader before passing.

---

## Testing Command

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && echo "../programs/typed_book/social_graph/social_graph_protocol.glp" | dart run bin/glp_repl.dart --typecheck 2>&1 | tee /tmp/integrated_check.txt
```

---

## Recommended Next Step

Try Option 1: Change the type declaration from `Response ::= accept(Channel?) ; no` to `Response ::= accept(Channel) ; no` and test whether this resolves the mode mismatch while preserving correct semantics for both `bind_response` (produces Response) and `handle_response` (consumes Response?).

---

## Files Modified

Only `/programs/typed_book/social_graph/social_graph_protocol.glp` was modified during this session.

## Related Documentation

- Paper: Section 4 (Typed GLP), Section A.6 (Bidirectional Channel example)
- Spec: `/docs/type system/typed-program.md`
