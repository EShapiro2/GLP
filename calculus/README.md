# The contracts of the Grassroots Contracts Calculus

This directory is published by the project Calculus.  It holds grassroots contracts in the language Udi Shapiro approved, and for each of them every artefact of Figure 1 of the paper Grassroots Contracts Calculus (in preparation): the contract, its speech-act schemas, the contract and the schemas with the relations among contracts applied, and the program the abstraction cascade compiles from them.  `MIGRATION.md` says what each project does with it.

## A contract's directory

`NAME/` has:

- `contract.txt`: the contract, a text for people;
- `logic-pragmas.txt`: its Logic pragmas: its vocabulary, what each phrase means, and a sentence in a restricted English for each act of each clause;
- `schemas.txt`: its speech-act schemas, compiled from the Logic pragmas;
- `full-contract.txt`: the full contract, with every clause it has from the contracts it is written from, each labelled by its origin, as GKK:1;
- `full-pragmas.txt` and `flat.txt`: the Logic pragmas' sentences and the flat text with the same operations applied;
- `full-schemas.txt`: the schemas with the same operations applied;
- `term.glp.txt`: the full schemas as the term GLP's checker of speech-act schemas reads;
- `verdict.txt`: the checker's verdict;
- `vmat.tex`, `interface.txt`, `platform.vglp`, `platform.glp`: the VMAT form, the Interface pragmas printed from the acts, the platform in vGLP and the program in GLP.

## The rest

- `STATE.txt`: a line for each contract: its name, its owner, and yes or no for certified, compiled and adopted by its owner.
- `LAYERS.txt`: the layers, a line for each with its number and its contracts in order: GKK is layer 0, and the layer of any other contract is one more than the highest layer among the contracts it is written from.  It is the order of adoption.
- `OWNERS.md`: for each contract its owner, the owner's text in this repository, and the lines where the text here differs from it.
- `STATUS.txt`, `VERDICTS.txt`: the cascade's line and the checker's verdict for each contract.
- `tools/`, `tests/`: the programs that print all of the above, in Python 3 with its standard library, and their test cases.

## Adopting a contract

`tools/adopt.py` is the owner's command, run from `calculus/tools/` in a checkout of GLP, which it finds itself, or with `GLP_DIR` naming the checkout:

- `adopt.py --write NAME DIR` writes the adopted `NAME.txt` into `DIR`: the owner's `%` lines; the lines of `contract.txt`; a line `Logic pragmas`; the lines of `logic-pragmas.txt` after its title; and, where the owner has Interface pragmas, a line `Interface pragmas` and the owner's lines.
- `adopt.py --paths` writes `PATHS.txt`, the file of each contract: its adopted file, `programs/<dir>/NAME.txt` with a `% Owner` line and a line `Logic pragmas`, or else `calculus/NAME/`.  Without `PATHS.txt` the commands below work the same paths out themselves.
- A contract not adopted is read from `calculus/NAME/` of the checkout or, where the checkout has none, as on the branches `migration` and `main`, from `calculus/NAME/` of the branch `Calculus` of the clone (`git -C GLP fetch origin Calculus` fetches it; without it, `adopt.py` names the contract as missing and gives that command).
- `adopt.py NAME` prints `NAME.glp` beside the adopted file, a module with `contract_named(name, Term)`, the term being the full schemas, complete in itself; and into `cascade/NAME/` the record's part that is the Calculus's: `NAME.txt`, a copy of the adopted file, `NAME.glp`, `schemas.txt`, `full-contract.txt`, `full-pragmas.txt`, `flat.txt` and `full-schemas.txt`.  The rest of the record is `cascade.sh`'s.
- The contract's introduction predicate is a line `Introduction: PRED.` under the heading `Implementation pragmas`, among the owner's lines after `Interface pragmas`, PRED a basic state's predicate, as `knows`; `adopt.py NAME` then prints in `NAME.glp`, after `contract_named`, the type `Intro`, `procedure introduction_named(Constant?, Intro)` and `introduction_named(name, introduction(PRED))`, which `cascade.sh` and the `compile.sh` of `programs/cssn` and `programs/apps` read; with no such line it prints none, and they read `no_introduction`.
- `adopt.py --check NAME` exits 0 if those files are what it would print, and otherwise names each that differs and exits 1.
- `--out DIR`, after `NAME` or `--check NAME`, writes or checks the files under `DIR` in place of the checkout, so a check writes nothing in it.

## The relations

A contract's first line says which contracts it is written from and how: it extends, restricts, strengthens, instantiates or composes them.  No contract amends another.

## Changing a contract

No word of a contract here changes without Udi Shapiro's approval of that change.  An owner who disagrees with a text, a relation or an artefact of its contract discusses it with Calculus (`Calculus_inbox.md`); a change to a contract goes to Udi as the marked words of the clause.
