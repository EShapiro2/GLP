# Writing a Grassroots Contract

What a contract writer needs, in one place (Udi, 2026-11-01: "everything that contract writers / developers need should be in GLP").  Coordination writes this file; the language is GSC's paper (arXiv 2609.06231), the method is `speech-act-schemas-guide.md` beside this file, and where this file and a paper differ the paper is right.

## What a contract is

A grassroots contract, a social contract that runs, is a finite list of clauses its agents undertake towards one another, written in prose, and its speech-act schemas, written in GLP's term form, which say what each clause permits and what it changes.  The contract is the source of all truth for the platform and its app: the schemas and the pragmas are compiled down the abstraction cascade to a running app with no manual intervention.

## The two files

A contract named NAME lives in one copy, in its owner's directory under `programs/`, as two files.

1. `NAME.txt`, the clauses.  Lines beginning with `%` are meta-information: the owner, the paper responsible for the contract --- its title, no authors, with its arXiv link if it is there and `in preparation` if not --- what it extends, what it defines, when it last changed, and where its schemas are.  The rest is the contract verbatim as its paper prints it in a box: the title line, `NAME: <name in full>`; the declaration of its names, `Alice, Bob and Carol are persons.`, a formal part of the contract from which the schemas and the pragmas take their names, saying also what the contract extends; the clauses, numbered from 1, each opening with its legal modality --- `■` enforced (U+25A0), no act of the contract can breach it; `◆` attested (U+25C6), an agent may breach it and the contract records the breach; `○` undertaken (U+25CB), its performance lies outside the contract --- then its title in one or two words and its text; and after the clauses the line `Pragmas` and the pragmas.

2. `NAME.glp`, the speech-act schemas, the term `schema(Name, Names, Vars, Roles)` for each act the clauses describe, written by the guide.  The term is the one source; a run of the cascade prints the notation from it.

A clause is cited as `GKK:1`.  A contract that extends another has the other's acts and its own, cites the other's clauses and does not restate them, and replaces an act of the other that its clauses change.  Every contract's clauses use Alice and Bob; two names are two persons, and a clause that admits one person in both says "Alice and Bob, possibly one person".  A clause states an obligation, a permission or a prohibition, and nothing else: no explanation inside the box.

## The pragmas

The pragmas specify the app, one line per person: a line per tab, naming it and what it lists; then, for each clause, a line naming it, the person who proposes, a button for each act they propose in brackets, and any field they fill in angle brackets; an indented line for each other proposer, written the same way; and an indented line for each other person asked, their name, a colon, the question.  A pragma's name is the schema's role of that name.  Every word the app shows is in its contract.

## The contracts and their owners

A contract's name and its owner are each a ruling of Udi's, never read off a project's name.  The rulings so far, each in `programs/<dir>/`:

| Contract | Name in full | Extends | Owner | Directory |
|---|---|---|---|---|
| GKK | Grassroots Keys and Knowledge | --- (the base every contract extends) | GSC | `gsc` |
| GSG | Grassroots Social Graph | GKK | GSC | `gsc` |
| GSN | Grassroots Social Network | GSG | GSC | `gsc` |
| GnW | Guardians 'n' Wards | GSG | GAI | `gai` |
| GAI | Accountable AI Agents | GnW | GAI | `gai` |
| GSNP | Grassroots Social Network with Provenance | GSN, GAI | GAI | `gai` |
| CSSN | Child-Safe Social Network | GSN, GnW | CSSN | `cssn` |
| GFF | Grassroots Feeds and Followers | GSG | GFF | `gff` |
| GD | Grassroots Distrust | GSG | GGGA | `ggga` |
| GC | Grassroots Coins | GKK | Currencies | `currencies/coins` |
| GB | Grassroots Bonds | GC | Currencies | `currencies/bonds` |
| DGC | Denominated Grassroots Currencies | GC, GB | Currencies | `currencies/sovereign` |
| GDC | Grassroots Denominated Coins | GC | Currencies | `currencies/gdc` |
| GCC | Grassroots Community Currencies | GDC | Currencies | `currencies/gcc` |
| BCC | Backed Community Currencies | GCC, GDC | Currencies | `currencies/bcc` |
| AGORA | Grassroots Barter | GB | Currencies, until it has a paper | `currencies/agora` |
| LBM | Location-Based Matching | GSG | not ruled | --- (not in the repository) |
| GDO | Grassroots Directed Overlay | GSG | LBM | `lbm` |
| GRA | Grassroots Ride Auction | GDO | LBM | `lbm` |
| GFWC | the grassroots federation | --- | GFWC | `federation` (schemas only) |

A contract extends GSG when its clauses require friendship, and not otherwise.  The current text of every contract is Coordination Appendix E, `/Grassroots/Coordination/sections/E-contracts.tex`, which follows the repository's copies.

## How a contract reaches the repository

1. Ask Udi for the contract's name and its owner before writing.
2. Write the two files in the owner's directory, and nothing else: `cascade/`, `cascade.sh`, `test/` and `test/OPEN_TYPES` are Code's.
3. Check it: `bash cascade.sh <name> --check`, `<name>` the acronym in lower case, runs GSC's checker, prints the VMAT form, compiles the platform and runs it with scripted persons into a scratch directory.  The checker's verdict, `syntactically_grassroots`, is what certifies the contract.
4. An owner's Cowork commits the two files path-limited on `main` and pushes, then tasks Code to check and run; anyone else opens a pull request with the two files, and Code merges it, records the run under `cascade/<NAME>/<date>` and enters the compiled platform's open types in `test/OPEN_TYPES` until vGLP's compiler prints none.

## Rules of the language

A construct or a condition enters the language only when a paper says why, and no change to the language, its checker or its compiler beyond what GSC's paper says is made before Udi approves it.  No new GLP code is written untyped or with open types.
