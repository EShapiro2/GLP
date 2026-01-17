# irmaGLP Paper Issues and Resolutions

**Document Type**: Errata and Clarifications  
**Date**: 2026-01-17  
**Source Paper**: GLP-2025, Appendix "Smartphone Implementation-ready Multiagent Transition System for GLP"  
**Status**: RESOLVED - Ready for paper revision

---

## Purpose

This document catalogs all ambiguities, errors, and unclear passages discovered during specification writing for irmaGLP implementation. Each issue includes the resolution decision for incorporation into the paper.

---

## Issue 1: Variable Table Writer Entry Creator Field

**Severity**: Medium (Clarity)  
**Location**: Definition of V_p, Writer Entry  
**Status**: RESOLVED

### Current Text
```
Writer: Y ∈ V, s ∈ 𝒯 is the value of Y, else s=⊥
```

### Problem
The definition doesn't explicitly state that for writer entries (Y, q, s) where Y ∈ V, the creator q must always equal p (the current agent holding the writer).

### Why This Matters
Writers are only created locally - an agent never imports a writer variable. Only readers can be imported. The invariant "q = p for all writer entries" is implied but should be explicit.

### Resolution
**Add explicit constraint**: For writer entries (Y, q, s) where Y ∈ V: q = p always (writers are always created locally, never imported).

### Recommended Paper Change
```
Writer: Y ∈ V (writer variable)
  - q ∈ Π: agent who created Y (always q = p for writers)
  - s ∈ 𝒯 ∪ {⊥}: current value (⊥ if unbound)
  - Paired reader Y? is non-local
```

---

## Issue 2: abandon() Paired Variable Notation

**Severity**: High (Correctness)  
**Location**: Helper routine abandon(Y), paired variable definition  
**Status**: RESOLVED

### Current Text
```
where Y' = Y? if Y ∈ V, else Y' = Y if Y ∈ V? (the paired variable)
```

### Problem
The notation is confusing and potentially incorrect. If Y is already a reader (Y ∈ V?), then:
- Y is notation like "X?" (already has the ? marker)
- The paired writer should be X (without the ?)
- But the formula says Y' = Y, which would give "X?" again

This doesn't make sense. The paired variable of a reader "X?" is the writer "X", not "X?" itself.

### Resolution
**Clarify the pairing operation**:
- If Y is a writer (Y ∈ V), then Y' = Y? (add ? to get reader)
- If Y is a reader (Y ∈ V?), then Y' = the writer part (conceptually remove ?, but in implementation this is just the varId without the isReader flag)

### Recommended Paper Change
```
where Y' is the paired counterpart:
  - If Y ∈ V (writer): Y' = Y? (the paired reader)
  - If Y ∈ V? (reader): Y' denotes the paired writer
```

**Implementation Note**: In practice, varId remains the same; only the reader/writer flag changes.

---

## Issue 3: Reduce Transaction Variable Table Update (W in Success Case)

**Severity**: Critical (Ambiguity)  
**Location**: Definition of IRmaGLP-reduce, Reduce (success) case, step 3  
**Status**: RESOLVED

### Current Text
```
Update V'_p: for each X? ∈ W where (X?, q, ⊥) ∈ V'_p, update to (X?, q, q)
```

### Problem
This appears in the **success** case of Reduce, but W is defined as the suspension set in the **suspend** case. The meaning of W in the success case is unclear.

### Author Clarification
W = {readers that got instantiated in σ̂?}

If σ̂? contains {X?:=T}, then X? ∈ W. The intention is: if a reader is assigned, then its dual writer is also assigned.

### Resolution
**Clarify that W = domain(σ̂?)**: The set of readers that receive values in this reduction.

**Purpose**: Mark imported readers that need request messages sent to their creators.

### Recommended Paper Change
```
Update V'_p: Let W = {X? : {X?:=T} ∈ σ̂?} be the set of readers assigned by this reduction.
For each X? ∈ W where (X?, q, ⊥) ∈ V'_p:
  Update to (X?, q, q) in V'_p
  // Marks that we need values from creator q
```

**Additional Note**: Add explanation that when a reader gets a value, its paired writer is implicitly bound, so we need to track which imported readers now require read requests.

---

## Issue 4: export() Forwarding Goal Semantics

**Severity**: High (Undefined Behavior)  
**Location**: Helper routine export(T), requested reader case  
**Status**: RESOLVED

### Current Text
```
add relay(Y, Z) to A'_p
```

### Problem
The predicate `relay/2` was not originally defined. What does this goal do?

### Author Clarification
```prolog
relay(Y, Z) :- known(Z?) | Y = Z?.
```

This is a forwarding goal: wait until Z? (the relay reader) is bound, then unify the original writer Y with Z?.

### Resolution
**Define the forwarding semantics inline**:

The forwarding goal ensures that when the relay reader Z? receives a value, the original writer Y gets bound to the same value, maintaining the request chain.

### Recommended Paper Change

Add after the export routine definition:

```
The forwarding goal relay(Y, Z) is defined as:
  relay(Y, Z) :- known(Z?) | Y = Z?.

This ensures that when relay reader Z? receives a value V, the original 
writer Y is bound to V, maintaining the request relationship across 
export boundaries.
```

---

## Issue 5: Variable Abandonment: Y vs Y'

**Severity**: High (Correctness)  
**Location**: Definition of Variable Abandonment in Reduction vs Reduce Transaction  
**Status**: RESOLVED

### Current Text

**Abandonment Definition**: "a variable Y is abandoned if its paired variable Y' satisfies all three conditions: Y' occurs in A, Y' is not instantiated by σ̂ or σ̂?, and Y' does not occur in B."

**Reduce Transaction**: "Call abandon(Y) for each abandoned variable Y"

### Problem
If Y' is the variable that disappeared, should we call abandon(Y') or abandon(Y)?

The definition says Y is abandoned when Y' disappears, but then we call abandon(Y). This is confusing about which part of the pair to pass.

### Author Clarification
An agent can only abandon a **reader**, which causes its dual writer to be abandoned.

When a reader Y? disappears from computation without being instantiated, we call abandon(Y?) to notify the remote agent holding writer Y.

### Resolution
**Clarify that abandonment is detected on readers**:

When a reader Y? in the head doesn't appear in the body and isn't instantiated, that reader is abandoned. We call abandon(Y?) which notifies the creator of writer Y.

### Recommended Paper Change

**Variable Abandonment Definition**:
```
When reducing atom A with clause C yielding body B and substitution σ̂, 
a reader Y? is abandoned if all three conditions hold:
  1. Y? occurs in A
  2. Y? is not instantiated by σ̂?
  3. Y? does not occur in B

For each abandoned reader Y?, call abandon(Y?) to notify the creator 
holding the paired writer Y.
```

**Reduce Transaction**:
```
Call abandon(Y?) for each abandoned reader Y?
```

---

## Issue 6: "Not Already Local" Definition

**Severity**: Medium (Precision)  
**Location**: Multiple locations - Communicate and Network transactions  
**Status**: RESOLVED

### Current Text
```
For each variable Y in T not already local to q...
```

### Problem
"Not already local" is ambiguous. Does it mean:
1. Y doesn't occur anywhere in R_q (resolvent)?
2. (Y, ·, ·) ∉ V_q (not in variable table)?

These are different! A variable can be in the resolvent but also in V_q if its counterpart is remote.

### Author Clarification
- **Local** means (Y, ·, ·) ∉ V_q
- **Not already local** means (Y, ·, ·) ∈ V_q

The variable table V_q contains exactly those variables whose paired counterparts are non-local.

### Resolution
**Use precise language**: "For each variable Y in T where (Y, ·, ·) ∉ V_q..."

### Recommended Paper Change

Replace all instances of "not already local to q" with:
```
For each variable Y in T where (Y, ·, ·) ∉ V_q (Y is fully local to q):
  If Y created by agent r:
    Add (Y, r, ⊥) to V'_q
```

**Add earlier in paper**: "A variable Y is **fully local** to agent p if (Y, ·, ·) ∉ V_p, meaning both Y and its paired counterpart are in p's resolvent."

---

## Issue 7: Missing irGLP Reference

**Severity**: Low (Completeness)  
**Location**: Paper introduction, reference to "Section~\ref{appendix:irGLP}"  
**Status**: RESOLVED

### Current Text
```
This section combines the implementation-ready structure of irGLP 
(Section~\ref{appendix:irGLP}) with the multiagent framework...
```

### Problem
The appendix doesn't include the irGLP section defining (A_p, S_p, F_p) and deterministic scheduling.

### Resolution
**Option 1**: Include irGLP appendix section before irmaGLP  
**Option 2**: Define (A_p, S_p, F_p) inline in irmaGLP

### Recommended Paper Change

Add before the irmaGLP section:

```
\subsection{Implementation-Ready Single-Agent GLP (irGLP)}

The implementation-ready resolvent R_p = (A_p, S_p, F_p) partitions 
goals into three categories:

- A_p ∈ 𝒜*: FIFO queue of active goals
- S_p ⊆ 𝒜 × 2^(V?): Suspended goals paired with blocking reader sets  
- F_p ⊆ 𝒜: Failed goals (terminal)

Goals are selected deterministically from the head of A_p. When a 
suspended goal's blocking readers receive values, the goal reactivates 
and moves to the tail of A_p.
```

---

## Issue 8: Initial Configuration Notation Inconsistency

**Severity**: Low (Style)  
**Location**: Definition of Implementation-Ready maGLP Transition System, initial configuration  
**Status**: RESOLVED

### Current Text
```
R_p = ([\texttt{agent}(p, \texttt{ch}(_?, _), \texttt{ch}(_?, _))], ∅, ∅)
```

### Problem
Mixes formal notation (∅) with code notation (\texttt). Style should be consistent.

### Resolution
**Use formal notation consistently**: Either all formal (∅) or all code (\texttt{empty}).

### Recommended Paper Change
```
R_p = ([agent(p, ch(_, _), ch(_, _))], ∅, ∅)
```

Or in pure LaTeX:
```
R_p = ([\text{agent}(p, \text{ch}(\_, \_), \text{ch}(\_, \_))], \emptyset, \emptyset)
```

---

## Issue 9: Global Variable ID Format Not Formal

**Severity**: Medium (Completeness)  
**Location**: Scattered in implementation notes  
**Status**: RESOLVED

### Current Text
Global variable IDs mentioned informally as "creator:localId" in various places.

### Problem
The encoding format for cross-agent variable identity should be in formal definitions, not relegated to implementation notes.

### Resolution
**Add formal definition**: Include global variable identity in Section 2 (Definitions).

### Recommended Paper Change

Add to Definitions section:

```
\subsubsection{Global Variable Identity}

Variables crossing agent boundaries require globally unique identifiers.

**Format**: creator:localId where:
- creator ∈ Π: agent who allocated this variable
- localId: unique integer within creator's heap

**Example**: alice:1042 identifies variable with local ID 1042 created by agent alice.

When serializing terms for inter-agent transport, local variable IDs 
are replaced with global IDs to enable routing through variable tables.
```

---

## Summary of Changes Required

### High Priority (Correctness)
1. **Issue 2**: Fix paired variable notation in abandon()
2. **Issue 3**: Define W in Reduce success case as domain(σ̂?)
3. **Issue 4**: Define relay/2 forwarding semantics
4. **Issue 5**: Clarify abandonment is detected on readers

### Medium Priority (Clarity)
1. **Issue 1**: Explicit constraint q=p for writer entries
6. **Issue 6**: Replace "not already local" with precise V_q notation
9. **Issue 9**: Formalize global variable ID format

### Low Priority (Completeness/Style)
7. **Issue 7**: Add irGLP section or inline definition
8. **Issue 8**: Consistent notation style

---

## Implementation Impact

These resolutions affect the specification and implementation as follows:

1. **Variable Table Implementation**: Must enforce q=p for writer entries
2. **abandon() Implementation**: Takes reader as parameter, computes paired writer
3. **Reduce Transaction**: Must compute W = domain(σ̂?) for V_p updates
4. **relay/2 Clause**: Must be defined in runtime or standard library
5. **Serialization**: Must implement creator:localId global ID format

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-17 | Claude + Udi | Initial issues catalog with resolutions |

---

## Next Steps

1. Paper author (Udi) reviews and approves resolutions
2. Separate Claude session revises paper LaTeX incorporating these changes
3. Updated paper becomes source of truth for implementation
4. Implementation proceeds from corrected specification
