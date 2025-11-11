# Compiler vs Hand-Written Bytecode Comparison Summary

## Completed Comparisons

### 1. ✅ simple_body_test.dart - `p(a). forward(X) :- p(X).`

**Hand-written source**: `forward(X) :- p(X).` (SRSW violation)
**Compiled source**: `forward(X) :- p(X?).` (SRSW-compliant)

**Bytecode difference**:
- Hand-written: `PutWriter(0, 0)` - forwards writer variable
- Compiled: `PutReader(0, 0)` - passes reader variable

**Conclusion**:
- Hand-written tests VM capability (writer forwarding) that violates SRSW
- Cannot be expressed in valid GLP source
- Compiler correctly rejects SRSW violation
- SRSW-compliant version has different semantics (suspends vs binds)

**Test**: `test/compiler/compare_simple_body.dart` ✅

---

### 2. ✅ simple_p_q_test.dart - `p(a). q(a).`

**Source**: `p(a). q(a).` (identical)

**Bytecode differences**:
- Label naming: `p_end` vs `p/1_end` (compiler uses full signature)
- `SuspendEnd` vs `NoMoreClauses` (semantically equivalent)
- Two separate programs vs single program (both supported by VM)

**Conclusion**:
- Semantically equivalent
- Minor cosmetic differences only
- Compiled version works correctly

**Test**: `test/compiler/compare_simple_pq.dart` ✅
**Execution**: `test/compiler/verify_simple_pq_test.dart` ✅

---

### 3. ✅ merge_test.dart base case - `merge([],[],[]).`

**Source**: `merge([],[],[]).` (identical)

**Bytecode differences**:
- Hand-written: `HeadConstant(null, N)` for empty lists
- Compiled: `HeadNil(N)` for empty lists (optimization)
- Label naming: `merge/3_start` vs `merge/3`

**Conclusion**:
- Semantically equivalent
- Compiler optimization (`HeadNil` is better than `HeadConstant(null)`)
- Compiled version works correctly

**Test**: `test/compiler/compare_merge_base.dart` ✅
**Execution**: `test/compiler/verify_merge_base_test.dart` ✅

---

### 4. ⚠️ merge_test.dart clause 1 - `merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).`

**Source**: Identical 2-clause merge program

**Bytecode differences**:
- Hand-written: `HeadStructure('.', 2, slot) + HeadWriter + HeadWriter`
- Compiled: `HeadList(slot) + UnifyWriter + UnifyWriter` (optimization)

**Comparison result**: ✅ Bytecode structurally correct

**Execution result**: ❌ **VM BUG**
- Hand-written bytecode: Works correctly, binds Z to `[a]`
- Compiled bytecode: Fails (all clauses exhausted)
- Problem: VM doesn't handle `HeadList + Unify*` pattern correctly

**Conclusion**:
- Compiler generates correct/optimized bytecode
- VM has bug with list instruction execution
- Need to fix VM's HeadList/UnifyWriter/UnifyReader handling

**Test**: `test/compiler/compare_run_merge_clause1.dart` ⚠️

---

## Summary Statistics

- **Total tests compared**: 4
- **Fully working**: 3 (75%)
- **VM bugs found**: 1 (25%)
  - HeadList + UnifyWriter/UnifyReader pattern

---

## Key Findings

### Compiler Optimizations
1. `HeadNil` instead of `HeadConstant(null)` for empty lists
2. `HeadList` instead of `HeadStructure('.', 2, slot)` for list patterns
3. `Unify*` instructions for structure traversal

### VM Bugs Discovered
1. **HeadList with UnifyWriter/UnifyReader**: Compiled merge clauses fail where hand-written equivalents succeed

### SRSW Constraints
1. Some hand-written tests use patterns that violate SRSW (e.g., `forward(X) :- p(X).`)
2. These patterns cannot be expressed in valid GLP source
3. Compiler correctly rejects SRSW violations

---

## Next Steps

1. Fix VM bug with HeadList + Unify* pattern
2. Continue comparing remaining ~36 hand-compiled tests
3. Focus on simpler tests (facts, basic unification) until VM bugs are fixed
4. Build comprehensive test suite of verified compilations
