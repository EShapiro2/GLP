# Union Alias Expansion (v0.8) - Handover Report

**Date:** 2025-01-17
**Feature:** Union alias expansion for type system
**Status:** IN PROGRESS - Core implementation done, testing in progress

---

## Summary

Implementing union aliases that combine multiple type definitions into a single type name:
```glp
Msg ::= NetMsg ; UserMsg.  % Expands to all alternatives from both types
```

---

## Completed Work

### 1. Paper Update (`/Users/udi/Grassroots/Moded-Types/sections/well-typing.tex`)
- Extended "Type aliases" section distinguishing simple vs union aliases
- Documented expansion semantics and constraints

### 2. Spec Update (`/Users/udi/Grassroots/GLP/docs/type system/type-environment.md`)
- Version bumped to v0.8
- Added `isSimpleAlias()` and `isUnionAlias()` functions
- Documented two-phase alias resolution algorithm
- Added `AliasExpansionError` for validation failures

### 3. Implementation (`type_environment_builder.dart`)
- Added `AliasExpansionError` exception class
- Split `_isTypeAlias()` into `_isSimpleAlias()` and `_isUnionAlias()`
- Rewrote `_resolveAliases()` for two-phase processing
- Added `_applyComplementToAlt()` for recursive complement application

### 4. Test Files Created
- `valid/union_alias_basic.glp` - Basic union of two types
- `valid/union_alias_three.glp` - Union of three types  
- `valid/union_alias_simple.glp` - Minimal test (PASSES)
- `invalid/union_alias_overlap.glp` - Overlapping functors error
- `invalid/union_alias_refs_alias.glp` - Reference to alias error

---

## Bugs Found and Fixed

### Bug 1: Alias resolution before prelude merge
**Problem:** Union aliases in user code couldn't reference prelude types because alias resolution happened during `_buildEnvironmentFromModule`, before prelude was merged.

**Fix:** Restructured `buildTypeEnvironment()`:
```dart
TypeEnvironment buildTypeEnvironment(ast.Module module) {
  // Load prelude (with aliases resolved)
  final preludeEnv = buildPreludeEnvironment();

  // Build user environment WITHOUT resolving aliases yet
  final userEnv = _buildEnvironmentFromModule(module, 
      checkRedefinitions: true, resolveAliasesNow: false);

  // Merge: prelude first, then user
  final merged = preludeEnv.merge(userEnv);

  // NOW resolve aliases on merged environment
  final types = Map<String, TypeDef>.from(merged.types);
  final procedures = Map<String, ProcDecl>.from(merged.procedures);
  _resolveAliases(types, procedures);

  return TypeEnvironment(types, procedures);
}
```

### Bug 2: Prelude's `Constant` misclassified as union alias
**Problem:** `Constant ::= Number ; String.` was being treated as a union alias because it has multiple TypeRef alternatives. But `Number` and `String` are predefined primitives, not user-defined types to expand.

**Fix:** Updated `_isUnionAlias()` to exclude types referencing predefined primitives:
```dart
bool _isUnionAlias(TypeDef def) {
  if (def.alternatives.length < 2) return false;

  for (final alt in def.alternatives) {
    if (alt is! TypeRef) return false;
    // If it references a predefined type, it's not a union alias
    final typeName = (alt as TypeRef).name;
    if (isPredefinedType(typeName)) {
      return false;  // Not a union alias
    }
  }
  return true;
}
```

---

## Current Test Status

| Test | Status | Notes |
|------|--------|-------|
| `union_alias_simple.glp` | ✅ PASS | Minimal test with user-defined types only |
| `union_alias_basic.glp` | ❓ PENDING | Updated to use `_` instead of `Constant` |
| `union_alias_three.glp` | ❓ PENDING | Not yet run |
| `union_alias_overlap.glp` | ❓ PENDING | Should fail with determinism error |
| `union_alias_refs_alias.glp` | ❓ PENDING | Should fail with alias-refs-alias error |

---

## Next Steps

1. **Run updated tests:**
   ```bash
   cd /Users/udi/Grassroots/GLP/glp_runtime
   dart run bin/glpc.dart --type-check ../test/analysis/type_checker/valid/union_alias_basic.glp
   dart run bin/glpc.dart --type-check ../test/analysis/type_checker/valid/union_alias_three.glp
   dart run bin/glpc.dart --type-check ../test/analysis/type_checker/invalid/union_alias_overlap.glp
   dart run bin/glpc.dart --type-check ../test/analysis/type_checker/invalid/union_alias_refs_alias.glp
   ```

2. **Run full test suite** to check for regressions:
   ```bash
   cd /Users/udi/Grassroots/GLP/test && ./run_type_checker_tests.sh
   ```

3. **Update test script** (`run_type_checker_tests.sh`) if not already done - add union alias test sections

4. **Consider edge cases:**
   - Union alias with complemented references: `Msg ::= NetMsg? ; UserMsg?`
   - Mixed predefined and user-defined types in same union (should NOT be union alias)

---

## Key Files

| File | Purpose |
|------|---------|
| `type_environment_builder.dart` | Main implementation |
| `type-environment.md` | Spec (v0.8) |
| `well-typing.tex` | Paper documentation |
| `prelude.dart` | Defines `isPredefinedType()` |

---

## Design Decisions

1. **Union aliases cannot reference other aliases** - prevents complex dependency chains
2. **Union aliases expanded in-place** - unlike simple aliases which are removed
3. **Determinism check on expanded union** - catches overlapping alternatives early
4. **Predefined type references don't make a union alias** - `Constant ::= Number ; String` is a regular type definition, not an alias
