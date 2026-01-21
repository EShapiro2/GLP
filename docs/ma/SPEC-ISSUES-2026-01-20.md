# irmaGLP Specification Issues and Clarifications Needed

**Date:** 2026-01-20  
**Author:** Claude  
**Spec Version Reviewed:** 2.5  
**Status:** Issues identified during implementation audit

---

## Overview

During the comprehensive audit of the irmaGLP implementation, several issues were identified in the specification itself. These range from ambiguities that led to implementation confusion, to missing definitions, to internal inconsistencies.

---

## Issue S1: Heap Representation of Imported Variables Not Specified

### Location

Section 3.1.2 (Variable Table V_p)

### Current Text

"For imported readers, V_p serves as the 'virtual writer' that holds suspensions since there is no local writer cell."

### Problem

The spec states there is "no local writer cell" for imported readers, but does not specify what heap structure DOES exist. The implementation must allocate something to represent the imported variable in the local heap (for use in terms, unification, etc.), but the spec provides no guidance on:

1. Whether imported readers have a heap cell at all
2. What the cell's tag should be (RoTag?)
3. What the cell's content should contain (VariableEntry? Pointer? null?)
4. How dereferencing should handle imported variable cells

### Recommendation

Add a new section "Heap Representation of Imported Variables" specifying:
- Imported readers: single RoTag cell with VariableEntry as content
- Imported writers: single WrtTag cell with VariableEntry as content
- Dereferencing rules for cells containing VariableEntry
- Binding rules for imported variables (how bindImportedReader transforms the cell)

---

## Issue S2: abandon(Y) Parameter Type Ambiguous

### Location

Section 4.1 (routine abandon(Y))

### Current Text

"The abandon helper notifies other agents when variable Y becomes unreachable."

The cases then discuss:
- "If (Y, q, s) ∈ V_p where q ≠ p" — imported variable
- "If (Y, p, s) ∈ V_p and s ≠ ⊥" — created with requester

### Problem

The spec uses Y generically, implying it could be either a reader or writer. However:

1. In GLP semantics, can a writer be abandoned? Writers are bound, not read, so "abandonment" (giving up on ever receiving a value) seems reader-specific.

2. The implementation restricts abandon() to readers only, with the comment: "An agent can only abandon a READER, which causes its dual writer to be abandoned at the remote agent."

3. The notation "Y' = Y? if Y ∈ V, else Y' = Y if Y ∈ V?" suggests converting between reader and writer notation, but the semantics are unclear.

### Recommendation

Clarify whether:
- Only readers can be abandoned (implementation's interpretation), or
- Both readers and writers can be abandoned (spec's apparent interpretation)

If only readers: rewrite Section 4.1 to use "X?" notation consistently.
If both: explain the semantics of writer abandonment.

---

## Issue S3: export_reader/2 Implementation Not Specified

### Location

Section 4.3 (routine export(T))

### Current Text

"add export_reader(Y, Z) to A'_p"

And later:

```prolog
export_reader(Y?, Z) :- Z = Y?.
```

### Problem

The spec defines export_reader as a GLP goal to be spawned, but:

1. Where is this predicate defined? Is it a built-in or must it be in the user's module?
2. The implementation uses heap callbacks instead of spawning a GLP goal. Is this equivalent?
3. If it's a GLP goal, it consumes scheduler resources and adds to the goal queue. The callback approach is more efficient but differs from spec.

### Recommendation

Either:
- Specify that export_reader is a built-in predicate handled specially by the runtime, or
- Explicitly permit callback-based implementation as an optimization, or
- Remove the GLP goal approach and specify callbacks directly

---

## Issue S4: V_p Entry Lifecycle Not Fully Specified

### Location

Section 3.1.2 and Section 5.3

### Current Text

Section 3.1.2: "When p exports a term, the export helper function updates V_p accordingly"

Section 5.3 Type 1 (Imported reader): "Remove (X?, r, s) from V'_q"

### Problem

The spec describes when entries are added (export, import) and when some are removed (after assignment to imported reader), but does not comprehensively specify:

1. When is a created writer entry removed? (After the writer is bound and value sent?)
2. When is a created reader entry removed? (After forwarding the value to requester?)
3. What happens to entries when variables become garbage-collectible locally?
4. Can entries transition between states, or are they immutable once created?

### Recommendation

Add a section "V_p Entry Lifecycle" with a state diagram showing:
- Entry creation triggers
- State transitions (e.g., created reader: ⊥ → requester → value)
- Entry removal triggers
- Interaction with local garbage collection

---

## Issue S5: Suspension Storage Location Ambiguous for Imported Readers

### Location

Section 3.1.2

### Current Text

"The fourth component Σ ∈ 𝒮* is a list of local suspension records for goals waiting on this variable. For imported readers, there is no local writer cell, so V_p serves as the 'virtual writer' that holds the suspension list."

### Problem

The spec says suspensions go in V_p (the Σ component), but:

1. Does this mean suspensions are ONLY in V_p, or ALSO in the heap cell?
2. If suspensions are in V_p, how does the heap's suspendOnReader() find them?
3. The current implementation has `VariableEntry.suspensions` (in the object attached to the heap cell), not a separate Σ in the V_p tuple structure.

### Recommendation

Clarify that for imported readers:
- The VariableEntry object serves as both the V_p entry AND the suspension holder
- The VariableEntry is stored in the heap cell's content field
- V_p maps VarKey → VariableEntry (same object reference)
- Suspensions are accessed via entry.suspensions, not a separate V_p field

---

## Issue S6: Global Variable ID Format Underspecified

### Location

Section 8.1 (Implementation Notes)

### Current Text

"Format: creator:localId"

### Problem

This is too brief. Questions that arise:

1. Is the colon literal? What if agent names contain colons?
2. Is localId the writer address or the varId (which may differ for readers)?
3. For imported variables being re-exported, whose ID is used?
4. What is the serialization byte format (length-prefixed? null-terminated?)

### Recommendation

Add detailed specification:
- Formal grammar for global ID string format
- Agent ID character restrictions (no colons, no whitespace)
- localId is always the creator's writer address (not reader address)
- Serialization format (e.g., length-prefixed UTF-8)

---

## Issue S7: Communicate Transaction Missing Imported Writer Case

### Location

Section 5.3 (Communicate Transaction)

### Current Text

Type 1 Assignment cases:
- Created reader with pending request
- Created reader, no request yet
- Imported reader

### Problem

There is no case for imported WRITER receiving an assignment. Per the spec, when Alice binds an imported writer (created by Bob), she sends an assignment to Bob. But what does Bob do when he receives it?

Bob has a created READER entry (the paired reader of the writer he sent to Alice). The spec's "Created reader with pending request" case handles this, but it's not obvious that an assignment to an imported writer at Alice becomes an assignment to a created reader at Bob.

### Recommendation

Add explicit text connecting the flow:
- Agent A binds imported writer W (created by B)
- A sends assignment (W?:=T) to B (the creator)
- B receives assignment for W?
- B looks up W? as created reader (since B created the W/W? pair)
- B handles per "Created reader" case

---

## Issue S8: Section 8.4 Should Be Normative

### Location

Section 8.4 (Scheduler-IRMA Integration)

### Current Text

"*Note: This section contains implementation guidance not derived from the paper.*"

### Problem

Section 8.4 describes critical behavior: the scheduler must provide blocking readers to the IRMA layer so read requests can be sent. Without this, the system cannot function (as demonstrated by the bidirectional stream test failure).

Marking this as non-normative "implementation guidance" led to it being overlooked.

### Recommendation

Either:
- Move Section 8.4 content into Section 5.2 (Reduce Transaction) as normative requirements, or
- Remove the "implementation guidance" disclaimer and make it normative, or
- Create a clear division between "MUST" requirements and "SHOULD" guidance

---

## Issue S9: reactivate() Return Type Inconsistent

### Location

Section 4.4 (routine reactivate(X?))

### Current Text

"reactivate(X?) returns R" where "R = {G : (G, W) ∈ S'_p, X? ∈ W}"

### Problem

The spec says reactivate returns a set of goals R, but in Section 5.2 Case 1:
"Let R = ⋃_{X? ∈ V_σ̂?} reactivate(X?)"

This takes a union over multiple reactivate calls, suggesting R is a set. But Section 5.3 Type 1 says:
"Let R = reactivate(X?) for agent q"

And then:
"A'_q := (A_q · R){X?:=T}"

This concatenates R to the active queue, suggesting R is a sequence/list.

### Recommendation

Clarify whether reactivate returns:
- A set of goals (unordered)
- A list of goals (ordered, for queue concatenation)
- Goal references (GoalRef with PC) vs goal atoms

---

## Issue S10: "Not Local" Check Definition Missing

### Location

Section 4.2 (routine request(X?))

### Current Text

"If (X?, q, ⊥) ∈ V'_p and q ≠ p then..."

The README mentions: "'Not local' check: (Y, ·, ·) ∉ V_p"

### Problem

There are two different "locality" concepts:

1. A variable is "in V_p" — has an entry
2. A variable was "created by p" — creator field equals p

The spec uses "q ≠ p" to mean "imported" but earlier text says V_p contains variables "whose paired counterparts are non-local." This conflates:
- Variables WE created but exported (counterpart is non-local)
- Variables OTHERS created that we imported (creator is non-local)

### Recommendation

Define precisely:
- "Local variable": created by this agent (creator == agentId)
- "Imported variable": created by another agent (creator != agentId)
- "Exported variable": local variable whose counterpart is held by another agent
- "In V_p": has an entry in variable table (could be local or imported)

---

## Issue S11: Network Transaction vs Message Queue Relationship Unclear

### Location

Section 5.4 (Network Transaction) vs Section 5.2/5.3

### Current Text

Section 5.4: "when a new msg(q, X) appears in p's network output stream"

Sections 5.2/5.3: "Add ... to M'_p"

### Problem

Section 5.4 describes a "network output stream" while 5.2/5.3 describe a "message queue M_p". Are these the same thing? The Network Transaction seems to be about GLP-level msg/2 predicates (application messages), while M_p contains protocol messages (assignments, requests, abandons).

### Recommendation

Clarify the distinction:
- M_p: Protocol-level message queue (assignments, read requests, abandons)
- Network streams: Application-level message channels (msg(q, X) predicates)
- How they interact (do protocol messages also go through network streams?)

---

## Summary

The specification has 11 issues requiring clarification or correction. The most critical for implementation correctness are:

- **S1**: Heap representation of imported variables (fundamental to implementation)
- **S5**: Suspension storage location (critical for goal reactivation)
- **S8**: Scheduler-IRMA integration should be normative (critical for message flow)

These spec issues directly contributed to implementation bugs identified in the companion audit document.

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-20 | Claude | Initial specification issues report |
