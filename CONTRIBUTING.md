# Contributing to GLP

This is the one document that explains how a collaborator contributes to the Grassroots project's code.  Code, the thread that keeps this repo, updates it whenever the procedure changes (Udi, 2026-11-19).

## Where to send a change

A collaborator opens a pull request in one of two places (Udi, 2026-11-19):

1. **To this repo, `EShapiro2/GLP`**, for the project they work on.
2. **To the Audit repo, `udi-shapiro/Grassroots-Audit`**, for the project as a whole, or for an area they do not work on.  The Audit repo's own `README.md` and `CONTRIBUTING.md` say how to do that.

A fault you find is reported as an issue, not as a pull request (see "Faults" below).

## Owning a project

A collaborator may own a project and its paper as Udi does, under the same rules (Udi, 2026-11-19):

1. The project has its paper's repo, its directories in this repo (recorded in `directories.py`), its contracts under the Calculus's names (`calculus/NAMES.txt`), and its place on peoplesweb.org.
2. The collaborator approves the revisions of their own paper and contracts, as Udi approves his.
3. The collaborator's own Claude reads the same rules, in `organisation/` of this repo, and sends the work as pull requests.  Code tests them and merges them, as it merges a thread's branch.
4. Udi's approval is still needed for a change to the language, to the Calculus, or to a contract of another project.

## What you read first

1. `CLAUDE.md`, the rules every change to this repo follows.
2. `servers/peoplesweb/tools/directories.py`.  It says which project answers for each directory and which paper specifies it.  peoplesweb.org prints it under Directories.
3. The paper that specifies what you change.  The paper is the specification: what the code does is never evidence of what it should do.  A gap between the paper and the code is reported, not worked around.
4. For GLP code: `docs/typed-glp-manual.md`, section 18, and `docs/glp-cheat-sheet.md`.
5. For phones: `phones/docs/contributors.md`, which still holds for phone testing.

## Making a pull request

1. Fork this repo.  Branch off the current `main`, and merge `main` into the branch again before you ask for review.
2. Make one pull request for each change.  A change that spans several owners is better split into one pull request for each owner.
3. Touch only the directories the change needs.  Commit only the files you changed; never commit generated files except the ones `CLAUDE.md` says are committed.
4. In the description, say:
   - what it changes and why;
   - the paper and section it follows, or the issue it closes;
   - which tests you ran, at which commit, with their result.
5. Declare every GLP procedure, with no `_` or `_?` in a type (section 18 of the typed-GLP manual).  Never mark a test as expected to fail, never remove a test, and never write a workaround.

## Contracts

1. A contract is `programs/<owner>/<NAME>.txt`: its clauses, then its pragmas.  The words of a contract change only with Udi's approval, so a pull request that changes them says so in its title.
2. Its schemas, `<NAME>.glp`, and its record, `cascade/<NAME>/`, are printed by `calculus/tools/adopt.py` and `cascade.sh`, never written by hand.  Code prints the records again once a day.  A pull request need not include them.
3. A new contract is checked by GSC's checker, `check/2` in `programs/gsc`.  Give its verdict in the description.  At present new contracts go as far as checked schemas; nothing new is compiled to a platform.
4. A new contract appears first on peoplesweb.org/next, and on the live site after Udi approves.

## Testing

From the repo root (see `CLAUDE.md`, "Test suites", for the full list):

- `GLP_SECTIONS="<sections>" bash test/run_all_tests.sh` runs the sections your change touches.
- `cd glp_runtime && dart test` runs the Dart unit tests.  The Dart SDK is 3.13.7.
- `cd glp_runtime && bin/glpc` is the REPL.  Loading a `.glp` file type-checks it and runs it.
- For the site: `cd servers/peoplesweb && python3 -I tools/build.py`, which must exit 0.

The full suite runs every night on `main`.  You need not run it.

## What happens to your pull request

1. Code checks this repo every hour for new and updated pull requests, and answers each on GitHub.  Udi hears of one only if it changes a paper or a contract, since that needs his approval.
2. Code sends it to the owner of each directory it touches, by `directories.py`, for its content:
   - A change that brings the code to its paper needs no further approval.
   - A change that goes beyond the paper, changes a contract's words, or changes the language or its engine waits until the owner's paper says it, with Udi's approval.
3. Code tests it, on the pull request merged with `main`.
4. Every pull request gets an answer within a day:
   - **Taken:** Code merges it to `main`.  What helps but does not fit as it stands is ported into the owner's code, and the pull request says where.
   - **Declined:** the pull request says why.
   - **Waiting:** the pull request says on whom and for what.
5. Only Code merges to `main`.

## Faults

Report a fault as a GitHub issue on this repo, one issue for each fault.  Say:

- what you ran, at which commit;
- what you saw, against what the paper says.

Label a phone fault `phones`.  The owner answers on the issue.
