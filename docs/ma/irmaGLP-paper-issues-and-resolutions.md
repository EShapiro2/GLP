# irmaGLP Paper Issues and Resolutions

**Document Type**: Errata and Clarifications  
**Date**: 2026-01-18  
**Source Paper**: GLP-ICLP-2026 (`~/Grassroots/GLP-ICLP-2026/GLP_for_ICLP.pdf`), Appendix "Smartphone Implementation-ready Multiagent Transition System for GLP"  
**Status**: Active tracking document

---

## Resolved Issues

### Issue 1: Imported Writer Notification (RESOLVED)

**Problem**: The original paper's Reduce transaction only sent assignment messages for created readers:
> "add (X?:=T, r) for each {X?:=T} ∈ σ̂? where (X?, p, r) ∈ V'_p, r ≠ ⊥"

This meant that when an agent binds an imported writer (received via introduction), the creator would never be notified, breaking the routing.

**Resolution**: Paper updated (2026-01-18) to add:
1. **Imported Writer** role in V_p definition
2. **New rule in Reduce**: "add (X?:=T, q) for each {X:=T} ∈ σ̂ where (X, q, ·) ∈ V'_p, q ≠ p"
3. **Updated Communicate/Assignment**: Three cases for creator routing
4. **Example scenarios**: Full introduction protocol trace

**Implementation** (2026-01-18):
- `VariableRole` enum split: `createdWriter` vs `importedWriter`
- `registerImportedWriter()` with heap callback
- Three-case `handleAssignment()` for creator routing
- Value-first logic in `handleReadRequest()`
- 151 unit tests passing

See spec v2.1, paper's "Example Scenarios" subsection, and `HANDOVER-imported-writer-2026-01-18.md`.

---

## Historical Notes

This document was created during spec writing but initially contained several incorrect assumptions and "hallucinated" clarifications that were not in the original paper. 

The spec has been revised to version 2.1 to faithfully match the (now updated) paper.

Key items that were **incorrectly added** in earlier spec versions and have been removed:

1. **Writer invariant "q = p"**: The spec claimed writers are always created locally. This is NOT in the paper and would break the introduction protocol where writers can be transferred between agents.

2. **Global Variable ID format**: The `creator:localId` format was added as implementation detail, not paper content. Now clearly marked as implementation notes.

---

## Genuine Questions for Paper Author

The following are genuine questions about the paper that may need clarification:

### Q1: What is W in the Reduce success case?

The paper says: "Update V'_p: for each X? ∈ W where (X?, q, ⊥) ∈ V'_p, update to (X?, q, q)"

But W is only defined in the Suspend case. In the success case, should W be the set of readers assigned by σ̂? (i.e., domain(σ̂?))? Or something else?

### Q2: What is export_reader/2?

The export routine adds `export_reader(Y, Z)` to A'_p but this predicate is not defined in the paper. What are its semantics?

### Q3: Variable abandonment - Y or Y'?

The definition says "a variable Y is abandoned if its paired variable Y' satisfies all three conditions" but then we "Call abandon(Y)". Should we call abandon(Y) or abandon(Y')?

---

## Document History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-17 | Initial issues catalog |
| 2.0 | 2026-01-18 | Marked obsolete, retained genuine questions only |
