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
- `OWNERS.md`: for each contract its owner, the owner's text in this repository, and the lines where the text here differs from it.
- `STATUS.txt`, `VERDICTS.txt`: the cascade's line and the checker's verdict for each contract.
- `tools/`, `tests/`: the programs that print all of the above, in Python 3 with its standard library, and their test cases.

## The relations

A contract's first line says which contracts it is written from and how: it extends, restricts, strengthens, instantiates or composes them.  No contract amends another.

## Changing a contract

No word of a contract here changes without Udi Shapiro's approval of that change.  An owner who disagrees with a text, a relation or an artefact of its contract discusses it with Calculus (`Calculus_inbox.md`); a change to a contract goes to Udi as the marked words of the clause.
