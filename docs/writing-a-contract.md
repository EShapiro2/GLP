# Writing a Grassroots Contract

What a contract writer needs, in one place (Udi, 2026-11-01: "everything that contract writers / developers need should be in GLP").  Coordination writes this file; the language is GSC's paper (arXiv 2609.06231), the method is `speech-act-schemas-guide.md` beside this file, and where this file and a paper differ the paper is right.

## What a contract is

A grassroots contract is defined in GSC's paper (arXiv 2609.06231), and its speech-act schemas are written by GSC's guide, `speech-act-schemas-guide.md` beside this file; GSC and the owners write the guide, committing it path-limited on `main` as they do a contract, and an owner adds to it whatever its contract teaches (Udi, 2026-10-24).  The contract is the source of all truth for its platform and its app: the schemas and the pragmas are compiled down the abstraction cascade to a running app with no manual intervention.

## The two files

A contract named NAME lives in one copy, in its owner's directory under `programs/`, as two files.

1. `NAME.txt`, the clauses.  Lines beginning with `%` are meta-information: the owner, the paper responsible for the contract, `% Paper: <acronym>, <title>, <link>` --- its acronym, its title, no authors, and its arXiv link if it is there or `in preparation` if not (Udi, 2026-11-05) --- what it extends, what it defines, when it last changed, and where its schemas are.  The rest is the contract verbatim as its paper prints it in a box: the title line, `NAME: <name in full>`; where the contract extends another, a line of its own saying so, `GDO extends GSG`, ending with a reference to the paper of the contract it extends (Udi, 2026-11-05) --- in a paper's box a citation where that paper is on arXiv, and where it is not its acronym and title and then `, in preparation`, as `GDO-L extends GDO (GRH, Grassroots Ride-Hailing, in preparation).`; in `NAME.txt` the acronym, the title and the arXiv link or `in preparation`, in parentheses; no reference where the extended contract is the same paper's; until Code's and vGLP's readers take this line, a `NAME.txt` keeps what it extends on the names line; then every definition that is part of the contract, as GSG defines trust, since a contract is self-contained, and a definition outside the box is a formal definition the box cites explicitly (Udi, 2026-11-07, to GSC #13; GSC's Section 2); after the definitions, the declaration of its names, `Alice, Bob and Carol are persons.`, a formal part of the contract from which the schemas and the pragmas take their names, the names coming last before the clauses (Udi, 2026-11-09, to GO #2, reported by GO: 'i want the people names last, before contract text'); the clauses, numbered from 1, each opening with its legal modality, as GSC's paper defines them --- `■` enforced (U+25A0), `◆` attested (U+25C6), `○` undertaken (U+25CB) --- then its title in one or two words and its text; and after the clauses the pragmas, under the heading line `Interface pragmas`, the tabs, buttons and questions, and `Implementation pragmas`, what the app does beyond its screens, a heading `Pragmas` alone heading the interface ones (Udi, 2026-11-07, supporting GSA #1's proposal, reported by GSC #13; GSC's paper defines the two kinds); until Code's and vGLP's readers take the two headings, a `NAME.txt` keeps the one line `Pragmas`.

2. `NAME.glp`, the speech-act schemas, the term `schema(Name, Names, Vars, Roles)` for each act the clauses describe, written by the guide.  The term is the one source; a run of the cascade prints the notation from it.

A clause is cited as `GKK:1`.  A contract built on another cites the other's clauses and does not restate them.  Its relation to the other is a relation of GSC's paper, Section 9, Relations among Contracts, which defines them and says what the checker redoes for each, and is stated in those terms in its paper and in its header, as `GSN amends GSG`, never as 'replaces' or 'extends with changes' (Udi, 2026-11-06, reported by GSC #11).  Every contract's clauses use Alice and Bob; two names are two persons, and a clause that admits one person in both says "Alice and Bob, possibly one person".  A clause states an obligation, a permission or a prohibition, and nothing else: no explanation inside the box.

## The pragmas

The pragmas specify the app, one line per person: a line per tab, naming it and what it lists; then, for each clause, a line naming it, the person who proposes, a button for each act they propose in brackets, and any field they fill in angle brackets; an indented line for each other proposer, written the same way; and an indented line for each other person asked, their name, a colon, the question.  A pragma's name is the schema's role of that name.  A pragma is readable, carries no act schema, and is abstract.  Every word the app shows is in its contract.  `programs/gsc/GKK.txt` has pragmas of every kind.

## The contracts and their owners

The contracts are the files `programs/<dir>/<NAME>.txt`, each with its `<NAME>.glp`; `grep -rl --include='*.txt' '^% Owner:' programs` lists them.  Each is the one record of its text, its owner, its paper and its relation, and no other file holds a copy or a list of them (Udi, 2026-11-06, verbatim: "any content that is in the repo, point to it and not replicate it!  it will always be behind and out of date.  NEVER duplicate content or structure.  only point to it.").

A contract's name and its owner are each a ruling of Udi's, never read off a project's name, and the owner writes the ruling in the `% Owner` line (Udi, 2026-10-18).  The default, when he rules, is the owner's acronym and the paper of the platform the contract specifies, but neither is automatic.  A contract is named by a letter acronym, as a project is; a project with several contracts names them XYZ1, XYZ2, never a bare XYZ, and a project with several papers uses the paper's acronym, as GC and GB.  The name goes everywhere: the files `<NAME>.txt` and `<NAME>.glp`, the checker's name for it, the acronym in lower case, and its record, `cascade/<NAME>/<date>`; the program directories keep their names.  A contract extends GSG when its clauses require friendship, and not otherwise.

## How a contract reaches the repository

1. Ask Udi for the contract's name and its owner before writing.
2. Write the two files in the owner's directory, and nothing else: `cascade/`, `cascade.sh`, `test/` and `test/OPEN_TYPES` are Code's.
3. Check it: `bash cascade.sh <name> --check`, `<name>` the acronym in lower case, runs GSC's checker, prints the VMAT form, compiles the platform and runs it with scripted persons into a scratch directory.  The checker's verdict, `syntactically_grassroots`, is what certifies the contract.
4. An owner's Cowork commits the two files path-limited on `main` and pushes, then tasks Code to check and run; anyone else opens a pull request with the two files, and Code merges it, records the run under `cascade/<NAME>/<date>` and enters the compiled platform's open types in `test/OPEN_TYPES` until vGLP's compiler prints none.

## Rules of the language

A construct or a condition enters the language only when a paper says why, and no change to the language, its checker or its compiler beyond what GSC's paper says is made before Udi approves it.  No new GLP code is written untyped or with open types.
