# Task: Replace "prelude" with "root self.glp" in the manual

## File to edit

`docs/typed-glp-manual.md`

## Replacements (6 occurrences, exact strings)

1. In Section 8.1, replace:
   `The prelude defines several single-unit-clause procedures. The PE automatically includes them when processing any program, so user programs do not need to redefine them. User programs may override a prelude unit clause by defining a procedure with the same name/arity.`
   with:
   `The root self.glp defines several single-unit-clause procedures. The PE automatically includes them when processing any program, so user programs do not need to redefine them. User programs may override a root self.glp unit clause by defining a procedure with the same name/arity.`

2. In Section 8.1, replace:
   `Examples from the prelude:`
   with:
   `Examples from root self.glp:`

3. In Section 14 (first paragraph), replace:
   `The prelude's generic procedures`
   with:
   `The root self.glp's generic procedures`

4. In Section 18.2, replace:
   `from the root prelude:`
   with:
   `from root self.glp:`

5. In Section 18.3, replace:
   `1. **System builtins** in `self.glp``
   This one is fine — it already says self.glp, not prelude. Skip.

6. In Section 19.6, replace:
   `The root `programs/self.glp` is the prelude: it defines all predefined types`
   with:
   `The root `programs/self.glp` defines all predefined types`

## Verification

After editing, run:
```bash
grep -n -i "prelude" docs/typed-glp-manual.md
```
Must return zero matches.

## Also check cheat sheet

```bash
grep -n -i "prelude" docs/glp-cheat-sheet.md
```
Should already return zero matches. If any found, replace similarly.

## Do NOT change anything else in either file.
