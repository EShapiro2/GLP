# CSSG Modules Validation — Corrected Instructions

**Date:** 2026-02-23
**Branch:** `claude/module-phase1` (Phases 1-3 complete)
**Discipline:** `docs/DISCIPLINE.md`

---

## Context

The first attempt bypassed partial evaluation (PE), causing false type errors.
The real pipeline in `GlpEngine.loadSource` is:

```dart
// 1. Parse
final module = parser.parseModule();
// 2. Partial evaluation — eliminates defined guards from guard position
final ast = Program(module.procedures, module.line, module.column);
final partialEvaluator = PartialEvaluator();
final transformedAst = partialEvaluator.transformDefinedGuards(ast);
// 3. Type check — uses PE-transformed procedures
final typeResult = checkModule(module, transformedProcedures: transformedAst.procedures);
```

The test must follow the same pipeline, adding `ancestorScope` for module hierarchy.

---

## Step 1: Delete and rewrite the test

Delete `glp_runtime/test/module/cssg_modules_test.dart` and rewrite it.

For each module file, the test should:

1. `discoverSelfChain(targetFile, rootDir)` → self.glp chain
2. Parse the target file into a `Module` AST
3. Build ancestor scope from the chain (prelude + self.glp files, **without** the target module — because `checkModule` via `buildTypeEnvironment` will add the module's own definitions)
4. Run PE: `PartialEvaluator().transformDefinedGuards(Program(module.procedures, ...))`
5. Call `checkModule(module, transformedProcedures: transformedAst.procedures, ancestorScope: ancestorEnv)`
6. Assert: no type errors

### Building ancestor scope (step 3)

Read `assembleTypeScope` in `module_hierarchy.dart` — it builds prelude + chain + module. But we need prelude + chain WITHOUT the module (because `checkModule` adds the module). So either:

- Call `assembleTypeScope` and strip the module part (awkward)
- Build it manually: start with `buildPreludeEnvironment()`, then for each self.glp in the chain, parse it, build a scope using the same approach as `_buildScopeFromModule`, and merge

The `_buildScopeFromModule` helper is private. But its logic is simple — iterate `module.typeDefs` and `module.procDeclarations`, build a `TypeEnvironment`. Replicate this in the test or make it accessible.

Alternatively: check if `checkModule` with `ancestorScope` param handles merging correctly. Read `type_checker.dart`'s `checkModule` and `type_environment_builder.dart`'s `buildTypeEnvironment` to see exactly how `ancestorScope` is used. The key question: does `buildTypeEnvironment(module, ancestorScope: X)` add the module's own types on top of X? If yes, then pass prelude+chain as ancestorScope and it works.

### Test cases

```dart
group('cssg_modules end-to-end', () {
  test('self.glp parses and type-checks', () { ... });
  test('agent.glp type-checks with PE and ancestor scope', () { ... });
  test('ui/mediator.glp type-checks with PE and ancestor scope', () { ... });
  test('ui/actors.glp type-checks with PE and ancestor scope', () { ... });
  test('boot.glp parses (untyped orchestration)', () { ... });
});
```

For `self.glp`: types-only, no procedures — just verify parsing.
For `boot.glp`: no procedure declarations → no type checking. Just verify parsing. The imported declarations exist in the AST but boot has no typed procedures to check.
For agent, mediator, actors: full pipeline — PE then type check with ancestor scope.

---

## Step 2: Run

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/module/cssg_modules_test.dart
```

If all pass, run full regression:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh
```

---

## Step 3: Commit

```bash
git add programs/cssg_modules/ glp_runtime/test/module/cssg_modules_test.dart
git commit -m "test(modules): validate cssg_modules project end-to-end with PE + ancestor scope"
```

---

## Rules

- **Do NOT modify** any `.glp` files under `programs/cssg_modules/`.
- **Do NOT modify** any runtime/compiler code.
- The test MUST run PE before type checking — matching `GlpEngine.loadSource`.
- Report failures exactly — do not try to fix them.
