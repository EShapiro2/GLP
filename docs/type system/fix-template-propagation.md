# Fix: Template Propagation for Parameterized Type Expansion

## Problem

Two bugs in `param_expansion.dart` block Step 2.4 (parameterizing `self.glp` procedures and removing monomorphic types):

### Bug 1: Nested template refs in `_collectInstantiationsInTemplate`

When a template body or parameterized proc decl contains `Channel(In, Stream(X))` where `In` and `X` are type params, the function checks if ALL args are bare param refs. `Stream(X)` fails this check (it's a parameterized TypeRef, not a bare name), so the code falls into the `!allParamRefs` branch — which calls `_collectInstantiations` (the **non-template-aware** version) on nested args. This causes `Stream(X)` to be recorded as a concrete instantiation, producing a bogus `Stream<X>` type definition with bare `X` in its alternatives.

**Fix**: One-line change. In `_collectInstantiationsInTemplate`, the `!allParamRefs` branch (around line 323) calls `_collectInstantiations`. Change it to `_collectInstantiationsInTemplate` to preserve template param awareness through nested refs.

### Bug 2: Downstream modules can't find prelude templates

When monomorphic `Stream` is removed from `self.glp`, downstream files like `merge_simple.glp` that declare `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).` can't expand `Stream(X)` because `expandParameterizedTypes` only sees templates defined in the module being expanded. The prelude's `Stream(X)` template was removed during the prelude's own expansion. There's no mechanism to pass prelude templates to downstream expansions.

**Fix**: Add an `externalTemplates` parameter to `expandParameterizedTypes` and thread prelude templates through the pipeline.

## Detailed Changes

### File 1: `param_expansion.dart`

#### Change 1a: Fix `_collectInstantiationsInTemplate`

In the `!allParamRefs` branch, change `_collectInstantiations` to `_collectInstantiationsInTemplate`:

```dart
// BEFORE (around line 320-325):
      if (!allParamRefs) {
        // Contains concrete types — collect any nested instantiations
        for (final arg in expr.typeArgs) {
          _collectInstantiations(arg, templates, instantiations);
        }
      }

// AFTER:
      if (!allParamRefs) {
        // Contains concrete types — collect any nested instantiations
        // Use template-aware version to preserve param awareness through nesting
        for (final arg in expr.typeArgs) {
          _collectInstantiationsInTemplate(arg, templates, instantiations, templateParams);
        }
      }
```

#### Change 1b: Add `externalTemplates` parameter

Change the function signature:

```dart
// BEFORE:
ast.Module expandParameterizedTypes(ast.Module module, {Set<String> knownTypeNames = const {}}) {

// AFTER:
ast.Module expandParameterizedTypes(ast.Module module, {
    Set<String> knownTypeNames = const {},
    Map<String, TypeDef> externalTemplates = const {},
}) {
```

In Step 1, merge external templates into the local templates map (after building local templates):

```dart
  // Merge external templates (from prelude/ancestor scopes).
  // Local templates take precedence over external ones.
  for (final entry in externalTemplates.entries) {
    templates.putIfAbsent(entry.key, () => entry.value);
  }
```

No other changes to `param_expansion.dart`. The rest of the expansion logic already uses the `templates` map, so merging external templates into it makes everything work automatically — `_substituteTypeExpr`, `_replaceParamRefs`, `_collectInstantiations`, and the worklist all consult `templates`.

### File 2: `type_ast.dart`

Add `typeTemplates` field to `TypeEnvironment`:

```dart
class TypeEnvironment {
  final Map<String, TypeDef> types;
  final Map<String, ProcDecl> procedures;
  final Map<String, ProcDecl> paramProcDecls;
  final Map<String, TypeDef> typeTemplates;  // NEW: parameterized type templates
  
  TypeEnvironment(this.types, this.procedures, {
      this.paramProcDecls = const {},
      this.typeTemplates = const {},
  });
  
  factory TypeEnvironment.empty() => TypeEnvironment({}, {});
  
  TypeEnvironment merge(TypeEnvironment other) {
    return TypeEnvironment(
      {...types, ...other.types},
      {...procedures, ...other.procedures},
      paramProcDecls: {...paramProcDecls, ...other.paramProcDecls},
      typeTemplates: {...typeTemplates, ...other.typeTemplates},
    );
  }
  // ... rest unchanged
}
```

### File 3: `type_environment_builder.dart`

In `buildPreludeEnvironment`, extract templates BEFORE expansion and store them in the returned environment:

```dart
TypeEnvironment buildPreludeEnvironment() {
  // ... parse module ...
  
  // Extract templates before expansion removes them
  final preludeTemplates = <String, TypeDef>{};
  for (final td in module.typeDefs) {
    if (td.isParameterized) {
      preludeTemplates[td.name] = td;
    }
  }
  
  final expandedModule = expandParameterizedTypes(module);
  // ... build environment from expandedModule ...
  
  return TypeEnvironment(types, procs,
      paramProcDecls: paramProcDeclMap,
      typeTemplates: preludeTemplates,
  );
}
```

### File 4: `type_checker.dart`

In `checkModule`, pass `externalTemplates`:

```dart
final expandedModule = expandParameterizedTypes(module,
    knownTypeNames: baseEnv.types.keys.toSet(),
    externalTemplates: baseEnv.typeTemplates,
);
```

### File 5: `module_hierarchy.dart`

In `assembleTypeScope` (or wherever `expandParameterizedTypes` is called for self.glp files), pass `externalTemplates`:

```dart
final expandedModule = expandParameterizedTypes(selfModule,
    knownTypeNames: env.types.keys.toSet(),
    externalTemplates: env.typeTemplates,
);
```

Also: when building scope from an expanded self.glp, extract its templates and merge them into the environment so they chain to descendant modules. If self.glp defines its own parameterized types (e.g., `cssg_modules/self.glp` might in the future), those templates need to flow to modules in that directory.

### File 6: `glp_engine.dart`

In `_mergeModuleIntoEnv` or wherever `expandParameterizedTypes` is called for ancestor self.glp files, pass `externalTemplates`:

```dart
final expandedModule = expandParameterizedTypes(selfModule,
    knownTypeNames: env.types.keys.toSet(),
    externalTemplates: env.typeTemplates,
);
```

## Execution Order

1. Run baseline tests — `bash test/run_all_tests.sh` — must show 390 pass. Commit baseline.
2. **Fix Bug 1**: One-line change in `_collectInstantiationsInTemplate`. Run tests — all 390 must pass (this bug only manifests when prelude procs use nested parameterized types like `Channel(In, Stream(X))`, which hasn't happened yet, so no test should break).
3. **Fix Bug 2**: Add `externalTemplates` parameter and thread through all call sites. Run tests — all 390 must pass (again, no test exercises this yet since monomorphic types still exist in `self.glp`).
4. Commit both fixes.
5. **Now proceed with Step 2.4**: Parameterize `self.glp` procedures and remove monomorphic types per `parameterized-types-plan.md` Step 2.4. Run tests after changes.

## Testing

After the infrastructure fixes (steps 2-3), no existing test should break — the fixes are prerequisites for Step 2.4 but don't change behavior for existing code.

After Step 2.4 (parameterizing `self.glp` and removing monomorphic types), many downstream files will break because they reference removed types. Those files were already converted in Steps 2.1-2.3, so most should work. Any remaining references to monomorphic `Stream`, `Channel`, etc. need conversion.

## Key Principle

These are TWO SEPARATE COMMITS:
1. Infrastructure fixes (Bug 1 + Bug 2) — all tests pass, no behavior change
2. Step 2.4 (parameterize self.glp) — this is the breaking change that exercises the fixes
