# Calculus migration plan

Draft 7, Calculus #6, 2026-11-15.  Agreed by Code (12:47 UTC), by Coordination (12:48 UTC) and by Website for its section (12:46 UTC); for Udi to confirm.  It has Website's corrections (12:40 UTC), Coordination's five changes and Code's six (12:42 UTC), all of which Udi approved taking in ("1 yes 2 currencies 3 done, go ahead").  Udi, 2026-11-15, to Calculus #6: "i want you to write a "Calculus migration plan" which explains what each project needs to do, push it to where Code and Coordination can read, and once the three of you agree, i confirm and we go public."

## What is migrated

The Calculus holds 62 grassroots contracts, each in the language Udi approved, and for each every artefact of Figure 1 of its paper (Grassroots Contracts Calculus, in preparation):

1. the contract, a text for people: `contract.txt`;
2. its Logic pragmas, which say in a restricted English what each clause says: `logic-pragmas.txt`;
3. its speech-act schemas, compiled from the Logic pragmas by a program: `schemas.txt`;
4. the full contract, with every clause it has from the contracts it is written from: `full-contract.txt`, and the full schemas, `full-schemas.txt`;
5. what the abstraction cascade makes of the full schemas: the verdict of GLP's checker, the VMAT form, the platform in vGLP and the program in GLP.

One program, `tools/gcsas.py`, compiles a contract to its schemas and back, applies the relations on the text and on the schemas, and checks that the two agree; `tools/toglp.py` and `tools/cascade.py` give the schemas to GLP's checker and compilers.  The relations among contracts are five: extends, restricts, strengthens, instantiates, composes.  No contract amends another.

Today each of 61 of these contracts has an owner's text in GLP, `programs/<dir>/NAME.txt`, with schemas the owner wrote by hand, `NAME.glp`; all 61 differ from the Calculus text (`OWNERS.md` lists, for each, the lines that differ).  SGSG has no owner text in GLP.  Migration is each owner taking the Calculus text and its artefacts as its contract.

## The standard

Udi, 2026-11-15: "i want publication to include all artefacts in the figure, contracts and schemas, original and expended, and all woners to maintain this standard."

For every contract, its owner keeps the artefacts 1 to 5 above, printed by the tools and never written by hand but for the contract and its pragmas.  A change to a contract is a change to its text or to its Logic pragmas, after which the owner runs the tools and commits what they print.

Udi, 2026-11-15: "any disagreement on the new contracts language should be brought for my approval."  No word of a contract's text changes without Udi's approval of that change, shown him as the marked words of the clause.  Udi, 2026-11-15, to Coordination #16: "Calculus is the authority on the relations among contracts and on the contracts; but this does not mean that owners of the contracts cannot push back, discuss, disagree if they think something is wrong"; and "except I must approve ANY change to a contract as current written in Calculus".  A change to Logic pragmas or to Interface pragmas needs no approval of his.

Adopting a Calculus text revises a contract's clauses, and for seven contracts, GSN, GnW, GSNW, GSNP, MCS, ORH and DGC, its first line, which no longer says "amends".  A revision of a contract and a change of a dependency among contracts take Udi's express consent (`organisation/claude.md`, Papers).  Udi approved the text of each contract of the Calculus, its first line included, and an owner adopts only that text.  Udi's confirmation of this plan is his consent to each owner's adoption of the Calculus text as it stands at the published commit; a change beyond that text is shown him, marked.

## Before going public

1. Each contract published is certified by GLP's checker (Udi, 2026-11-15: "i want to publish as soon as the contracts are all checked"; and, of waiting for the programs, "we agreed we dont wait for compile").  A contract with no certificate is held out of the publication until it has one: at going public that is ORH, on which the checker gives no verdict in half an hour (the second mend of item 2).  The other 61 are certified, and all 61 compile to GLP.
2. Three faults of GLP's tools, found on the way, are mended on branches of GLP and are with Code for their owners: vGLP's compiler takes time exponential in the listed roles of a schema (`helper-7/vglp-locality`, `e90af04`: the same output, GnW in 6 s for 121 s, the organisation contracts in two minutes each for no answer in an hour); GLP's reader of Interface pragmas keeps the quotes of a quoted schema name (`helper-2/pragmas-quoted-name`, to come); GSC's checker takes time exponential in the predicates of arity zero of a contract (`helper-4/gsc-wards-search`, to come).  A fourth, reported of the checker and the order of a body, was a misreading and is withdrawn (Helper #1 and GSC #17, 12:50 UTC).  Code merges each mend when its owner takes it and the sections TY, VG, APPS, CAS and GSC of the suite stand; the compiler's mend, staged over main, passes TY, VG, APPS and CAS with no new failure (Code #30, 12:43 UTC).
3. One run of the contracts published at one commit of the Calculus and one commit of GLP, its results the published ones.
4. Code, Coordination and Calculus agree this plan, Website its own section, and Udi confirms.

## Going public

Calculus pushes to a branch `Calculus` of GLP one directory, `calculus/`: a directory for each contract with the artefacts above, `OWNERS.md`, `STATE.txt`, `STATUS.txt`, `VERDICTS.txt`, this plan, `tools/` and `tests/`.  No file of an owner is touched.  The paper stays private until Udi says.  Calculus mails Code the branch and the commit, and Coordination announces it.

Of `calculus/` only the tools and their tests go into GLP's main at going public.  The contracts stay on the branch `Calculus`: main has one text of each contract, its owner's, until that owner adopts.

## What each project does

### Every owner of a contract

The owners and their contracts, from `OWNERS.md`:

- GO (`programs/go`): CB, CMB, CnC, GAN, GBCC, GBank, GCoop, GCorp, GDAO, GLDAO, GNG, GNL, GNM, GNO, GO, MCS, ORH, SC, SCB, SPLLC, TC, WC.
- GCE (`programs/gce`): DAO, FDAO, JOINT, LDAO, LIB, REC, STAY.
- Currencies (`programs/currencies`): GC, GB, GDC, DGC, GCC, and AGORA (Udi, 2026-11-15, to Calculus #6: "currencies"; its owner line in GLP names Talmon, and Currencies corrects it).
- GFF (`programs/gff`): GDO-L, GFF, GFF-FR, GFF-FR-BC, GFF-PS.
- GSC (`programs/gsc`): GKK, GSG, GSN, GnW, GFP.
- GRH (`programs/lbm`): GDO, GDO-LB, GRA, GRA-SS.
- GSA (`programs/social/graph`): GPE, GPD, GAS.
- GNA, a paper of GSA (`programs/examples/rendezvous`): GSV, GRV, GDS.
- CSSN (`programs/cssn`): GSNW, CSSN.
- GAI (`programs/gai`): GAI, GSNP.
- GGGA (`programs/ggga`): GD, GD-S.
- SPM: SGSG, which has no text in GLP yet.

For each of its contracts the owner:

1. Reads `calculus/NAME/contract.txt` against its own `NAME.txt`, by the lines of `OWNERS.md`, and against its paper.
2. Reads `calculus/NAME/logic-pragmas.txt` against the contract: each sentence of the pragmas says what its clause says.  This check is by reading, and it is the owner's; no program makes it.
3. May push back on any text, relation or artefact of its contracts: it discusses it with Calculus.  Any change to a contract as currently written in the Calculus, whoever proposes it and whether or not the two agree on it, goes to Udi as the marked words of the clause, and is made only on his approval.  A fault of the pragmas or of an artefact it mails to Calculus.  It changes no file of `calculus/`.
4. Adopts, the text being one Udi approved: in one commit on its branch, its `programs/<dir>/NAME.txt` becomes the Calculus contract, with the Logic pragmas after its clauses and the owner's own Interface pragmas after them, and, where the platform is compiled with an introduction predicate, a heading `Implementation pragmas` and under it the line `Introduction: PRED.` (PRED a basic state's predicate, as `knows`); its `NAME.glp` is no longer written by hand: the tools print it from the Logic pragmas, with `introduction_named(name, introduction(PRED))` where that line is, it is committed, and a check of the suite holds it to what they print.  It adopts a contract only after the contracts it is written from are adopted: the order is that of Table 1 of the paper, by layers, GKK first.
5. Recompiles what is compiled from the contract, its platform, its app's screens and its simulation, pushes its branch, and tasks Code to check and merge.
6. Brings its paper to the contract when it next edits it: the contract's box, the first line in the five relations, and the paper's claims about the contract.

An owner with no session running is started by Code at once, at the layer where its contracts come up (Udi, 2026-11-15, to Code #30: "any project we wait on must be started to respond; your respinsibility!"); Udi sets its model.

### Code

1. Merges the three tool mends into GLP's main, each when its owner takes it and the sections TY, VG, APPS, CAS and GSC stand.
2. At going public merges the tools and the tests of `calculus/` into main, and adds to the suite a section that runs `python3 tools/gcsas.py` and `python3 tools/tests.py`.
3. Gathers the owners' branches on one branch, `migration`, off main, layer by layer in the order of Table 1; the helpers run the sections each layer touches; Code merges a layer into main only when its sections stand as on main, so that main is never broken.  A contract reaches main at its adoption: its text and pragmas as the owner's `NAME.txt`, its `NAME.glp` and its printed artefacts in `cascade/NAME/`, held by a check to what the tools print.
4. Decomposes the adoption into owner tasks, starts the sessions of the owners that need one, keeps the list of what is adopted in its `docs/work.md`, and tells Calculus each merge, for `OWNERS.md` and `STATE.txt`.
5. Brings `cascade.sh` to the standard, after Coordination has written the new record of a run: a run of the cascade starts from the Logic pragmas, and its record has the artefacts above.
6. Reports what the adoption breaks in the suite, the apps and the simulations, each to its owner.

### Coordination

1. Records the standard in `organisation/claude.md` and in `docs/writing-a-contract.md`: a contract is its text, its Logic pragmas and its Interface pragmas; its artefacts are printed by the tools; the relations are the five of the Calculus, and no contract amends another; a disagreement on a contract's language goes to Udi.
2. Asks Website to record in `servers/peoplesweb/tools/directories.py`, Website's file, that Calculus answers for `calculus/`.
3. Records that the Calculus paper defines the relations among contracts that a first line states, in `organisation/claude.md`, the style guide's 36, Appendix C and `docs/writing-a-contract.md`, which point to GSC's paper today; and writes in `organisation/claude.md`, Code, "A run of the cascade is recorded whole", the new record of a run, with Udi's yes, before Code brings `cascade.sh` to it.
4. Announces the publication and this plan to all.

### Calculus

1. Keeps the tools, the tests and the 62 contracts' artefacts until each is adopted; answers each owner's finding; shows Udi each proposed change of a clause, marked.
2. After each approved change runs the tools and the cascade on the contract and on those written from it, and pushes.
3. Keeps `OWNERS.md` and the status of the adoption current.

### GSC and vGLP

GSC owns the language of speech-act schemas and its checker, vGLP the compiler and the reader of Interface pragmas.  The Calculus paper defines the relations among contracts (Udi, 2026-11-15, to Coordination #16: "Calculus is the authority"), and GSC brings its paper's relations among contracts to the Calculus's.  The Calculus defines the five relations that a first line states; "implements", which is of what a contract's runs do and not an operation on its text, stays GSC's.  GSC takes the two mends of its checker, vGLP those of its compiler and its reader of pragmas, or each mends its tool otherwise.  vGLP's reader of Interface pragmas still asks a person who "must consent"; vGLP brings it to the Calculus's grammar, where such a person is not asked.

### Website

Udi, 2026-11-15: "i want the web to include for each contract a button you press 'full contract' that show the full contract"; and "the plan should also explain what the web should do when (if different from ordinary sweep)".  The ordinary sweep (Website #2, 2026-11-15): every hour the server pulls GLP's main, rebuilds the site from it and publishes it live, and then builds /test from the branch `Website-site` over main; a sweep at once, a deploy and the server's log are Code's; every change to what the site shows goes first to /test and goes live on Udi's word.  What differs, at each stage:

1. At going public the contracts of the Calculus are on the branch `Calculus` and not on main, so the ordinary sweep shows nothing of them.  The site reads them from the branch: for each contract, the proposed text and the button Full contract, which shows `calculus/NAME/full-contract.txt` as it is, and the state of the 62 from `calculus/STATE.txt`, a line for each contract: its name, its owner, and yes or no for certified, compiled and adopted.  This is outside the ordinary sweep, which reads main, and is Website's to build: it writes the fetch of the branch in its `deploy/sweep.sh`, and Code merges it and sees that the first sweep on the server runs it; it goes first to /test and goes live on Udi's word.
2. Until its owner adopts a contract, the site has two texts of it: the owner's, from main, as the contract in force, and the Calculus's, from the branch, as the text proposed, with the button on the proposed one.  SGSG has no owner text: the site lists it from the branch as a proposed text, with nothing in force, until SPM adopts it.
3. When an owner adopts a contract and Code merges its layer, the ordinary sweep takes it from main: its page shows the one text, and the button reads `cascade/NAME/full-contract.txt` (Where a contract is kept, below).  Nothing else differs from the ordinary sweep.
4. When every contract is adopted the site reads nothing from the branch.
5. The paper is not on the site until Udi says and it is on arXiv; `servers/peoplesweb/tools/papers.py` gains it then.
6. The relations.  The site's globe draws, and its `tools/certify.py` checks, the relations of the first line of each `NAME.txt` as GSC's paper has them: extends, restricts, amends, instantiates, implements, composes.  The Calculus has five, strengthens among them and amends not.  The Calculus paper defines the relations; when the adopted texts state them, Website gives the globe a mark for strengthens and the check its rule.

## Where a contract is kept

`organisation/claude.md` has a contract's one record in its owner's directory, and no second copy.  So main never has two texts of a contract: before its adoption a contract of the Calculus is on the branch `Calculus` only; at its adoption its text and pragmas are the owner's `NAME.txt` on main, with its `NAME.glp`, and its printed artefacts are the cascade's record of that contract, `cascade/NAME/`.  When every contract is adopted, main has of `calculus/` the tools, the tests and the table of owners, and the branch is closed.

## After the migration

The audit is on hold until the migration is done (Udi, 2026-11-15, to Code #30: "put audit on hold; i want to finish migration first").
