# Task: Parametric Types Refactoring for CSSG v2

Apply the same parametric types refactoring to `programs/cssg_modules_v2/` that was already done for `programs/cssn_modules_v2/`.

## What to read

1. `/Users/udi/Grassroots/GLP/CLAUDE.md` — skip to "GLP Programming" and "Core Rules"
2. `/Users/udi/Grassroots/GLP/programs/cssn_modules_v2/self.glp` — the REFERENCE: this is what the result should look like
3. `/Users/udi/Grassroots/GLP/programs/cssg_modules_v2/self.glp` — the file to refactor

## What to do

Remove all concrete stream/channel type definitions from `cssg_modules_v2/self.glp` and replace with `Stream(X)` and `Channel(In, Out)` from the prelude. Update all procedure declarations in all files in `cssg_modules_v2/` to use parametric types. Try moving duplicated `merge` to `self.glp`.

Use `cssn_modules_v2/self.glp` as the model — it was already refactored.

## How

1. Baseline: `cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh`
2. Refactor cssg_modules_v2 files
3. Test again. All tests must pass. Commit.
