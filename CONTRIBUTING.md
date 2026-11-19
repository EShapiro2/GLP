# Contributing to GLP

How a collaborator changes this repo: by a pull request, which Code takes or declines with the reason (Udi, 2026-11-19).  It extends to every directory the procedure for phones in `phones/docs/contributors.md`, "Sending a fix", which still holds for phones.

## What you read first

1. `CLAUDE.md`, the rules every change to the repo follows.
2. `servers/peoplesweb/tools/directories.py`, which says who answers for each directory and which paper specifies it.  peoplesweb.org prints it under Directories.
3. The paper that specifies what you change.  The paper is the specification: what the code does is never evidence of what it should do.

## A pull request

1. Make one pull request for each change, from a branch of your fork or of this repo, off the current `main`.
2. Say in its description:
   - what it changes and why;
   - the paper and section it follows, or the issue it closes;
   - which tests you ran, with their result.
3. Touch only the directories the change needs.  A change that spans several owners is better split into one pull request for each owner.
4. Declare every GLP procedure, with no `_` or `_?` in a type (`docs/typed-glp-manual.md`, section 18).  Never mark a test as expected to fail, and never write a workaround.  A test is never removed.
5. Write a contract as `programs/<owner>/<NAME>.txt`.  Its schemas, `<NAME>.glp`, and its record, `cascade/<NAME>/`, are printed by `calculus/tools/adopt.py` and `cascade.sh`, never written by hand.
6. Commit only generated files that `CLAUDE.md` says are committed.

## What happens to it

1. Code tells Udi of every new or updated pull request.
2. Code sends it to the owner of each directory it touches, by `directories.py`, for its content:
   - A change that brings the code to its paper needs no further approval.
   - A change that goes beyond the paper, changes a contract's text, or changes the language or its engine waits until the owner's paper says it, with Udi's approval.
3. Code tests it: the sections of the suite the change touches, on the pull request merged with `main`.
4. Code takes it and merges it to `main`, or declines it and says why on the pull request.
   - What helps but does not fit as it stands is ported into the owner's code, and the pull request says where.
   - A pull request is never left without an answer.
5. Only Code merges to `main`.

## Faults

Report a fault as a GitHub issue on this repo, one issue for each fault.  Say:

- what you ran, at which commit;
- what you saw, against what the paper says.

Label a phone fault `phones`, as `phones/docs/contributors.md` says.
