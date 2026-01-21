# irmaGLP Specification Issues and Clarifications Needed

**Date:** 2026-01-20  
**Author:** Claude  
**Spec Version Reviewed:** 2.5 → 3.0  
**Status:** ALL RESOLVED

---

## Overview

During the comprehensive audit of the irmaGLP implementation, several issues were identified in the specification itself. All issues have been resolved in spec v3.0.

---

## Issue S1: Heap Representation of Imported Variables Not Specified

**Status:** ✅ RESOLVED

**Resolution:** Added Section 3.2 "Heap Representation" to spec v3.0, including:
- Section 3.2.1: Variable Identity (MUST NOT use address arithmetic)
- Section 3.2.2: Local Variables (two-cell allocation)
- Section 3.2.3: Imported Variables (single-cell allocation with VariableEntry)
- Section 3.2.4: Binding Imported Readers (ValueTag cell allocation)

---

## Issue S2: abandon(Y) Parameter Type Ambiguous

**Status:** ✅ RESOLVED

**Resolution:** Changed Section 4.1 from `abandon(Y)` to `abandon(X?)`. Only readers can be abandoned. Updated definition and explanation accordingly.

---

## Issue S3: export_reader/2 Implementation Not Specified

**Status:** ✅ RESOLVED

**Resolution:** Added implementation note to Section 4.3: "The forwarding behavior may be implemented via runtime heap callbacks rather than spawning a GLP goal, provided the semantics are preserved."

---

## Issue S4: V_p Entry Lifecycle Not Fully Specified

**Status:** ✅ RESOLVED (deferred)

**Resolution:** Added note to Section 3.1.2: "The precise rules for when entries are added and removed for each role (created writer, imported writer, created reader, imported reader) are to be further clarified if needed."

---

## Issue S5: Suspension Storage Location Ambiguous for Imported Readers

**Status:** ✅ RESOLVED

**Resolution:** Simplified V_p to 3-tuple (Y, q, s), removing Σ component. Clarified that "the VariableEntry object holds suspensions in its `suspensions` field."

---

## Issue S6: Global Variable ID Format Underspecified

**Status:** ✅ RESOLVED

**Resolution:** Expanded Section 8.1 with:
- Formal format: `<creator>:<localId>`
- Character restrictions: ASCII alphanumeric plus underscore, no colons
- localId defined as creator's writer address
- Serialization: Length-prefixed UTF-8 encoding

---

## Issue S7: Communicate Transaction Missing Imported Writer Case

**Status:** ✅ RESOLVED

**Resolution:** Added note to Section 5.3 Type 1: "When agent A binds an imported writer W (created by agent B), A sends (W?:=T, B) to creator B. Agent B receives this as an assignment to their created reader W? and handles it per the cases below."

---

## Issue S8: Section 8.4 Should Be Normative

**Status:** ✅ RESOLVED

**Resolution:** Changed Section 5.2 Case 2 from "Call request(X?)" to "The runtime MUST call request(X?)". Added: "This requires the scheduler to provide the set of blocking readers W to the IRMA layer after suspension."

---

## Issue S9: reactivate() Return Type Inconsistent

**Status:** ✅ RESOLVED

**Resolution:** Changed Section 4.4 from set notation `{G:...}` to list notation `[G:...]` with note: "ordered list, preserving suspension order" and "The returned list R is appended to the active queue A_p, preserving goal ordering."

---

## Issue S10: "Not Local" Check Definition Missing

**Status:** ✅ RESOLVED

**Resolution:** Added Section 1.3 "Definitions" with precise definitions:
- Local variable: creator == agentId
- Imported variable: creator != agentId
- Exported variable: local variable sent to another agent (has V_p entry)
- Reader/Writer: identified by cell tag, not address arithmetic

---

## Issue S11: Network Transaction vs Message Queue Relationship Unclear

**Status:** ✅ RESOLVED

**Resolution:** Removed Section 5.4 (Network Transaction) entirely. It described application-level concerns (GLP `msg/2` predicates) that are out of scope for irmaGLP, which only specifies the infrastructure layer (V_p, M_p, protocol messages).

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-20 | Claude | Initial specification issues report |
| 2.0 | 2026-01-20 | Claude | All issues resolved in spec v3.0 |
