# The syntactically-grassroots checker, and the compiler

Decides whether a contract is **syntactically grassroots**, and when it is not, which condition failed, at which schema, role and atom; and compiles the schemas of a contract that is into their volition-guarded transactions, printed as the LaTeX of the paper's displays, befriend's in Section 5.1, the swap's in Appendix B.3 and federate's worked box in Appendix D.

The specification is the paper --- *Formal Grassroots Social Contracts* (`/Grassroots/GSC`, `main.tex`): Section 3 for a contract whose roles are all agent roles, Appendix F, `sections/17-wards.tex`, for one with wards, and Appendix D, `sections/13-community-roles.tex`, for one with community roles, its Section D.4 for an agent-named one.  Nothing of any of them is restated here or in the code: every procedure names the definition it decides, and the definition is read in the paper.

A contract of Section 3 is syntactically grassroots (`def:syntactically-grassroots`) when it has an unobstructed (`def:unobstructed`) introductory act (`def:introduction`), when the set of predicates occurring in it has traceable provenance (`def:grounded`), and when it satisfies volition (`def:volition`).  Appendix B.4 certifies the currency by hand, and the checker returns the same verdict and the same set of predicates of traceable provenance.  A contract whose introductory act is obstructed may still be certified by Appendix F: when it has an introduction with wards (`def:introduction-wards`) --- a nonempty set of marks each with a guardianship (`def:guardianship`), an introductory act unobstructed but for the marks (`def:unobstructed-marks`), and for every pair of marks a relay act with a ward role and a free role and one with two ward roles (`def:relay`), the act and the relay acts forbidding besides the leaving marks of the contract (`def:leaving`) --- and has traceable provenance and volition, it is grassroots with wards (`thm:grassroots-wards`), and the verdict names the introductory act, the marks, their guardianships, the relay acts the search found and the leaving marks.  The protocol realising it is volitionally grassroots; where the verdict names no leaving mark it is grassroots as well (Appendix F, after `thm:grassroots-wards`).

A contract written in the language of Appendix D --- with a seated, a constituent or a listed role, a reach condition or a name term --- is decided one of two ways.  An agent-named one (`definition:agent-named`: every role an agent role, a seated role or a listed role, every name term based on an agent role of its schema; GSC 8064cfb) is decided by the four clauses of `definition:sg-agent-named` (Appendix D.4): an unobstructed introductory act with agent roles and no name term, the schemas of arity one of clause 1 of `def:unobstructed` having an agent role and no name term; no seated or listed role adding an atom of a predicate the introductory act forbids at a role; traceable provenance of the set of predicates occurring in it (`definition:provenance`), each atom psi required at an agent role; and a connected role graph (`definition:volition`) or a guarding agent role in every connected component of it.  A contract meeting them is syntactically grassroots, and the protocol realising it is volitionally grassroots (`theorem:grassroots-agent-named`, "Grassroots realisation with seated and listed roles").  A listed role `pi[rho(*, u_2, ..., u_r)]` has the agent role `pi` as its base (`definition:role`), so it joins `pi` in the role graph, and it is no agent role, so an atom it requires is no psi of clause 3 and a guarded one guards no component under clause 4.  Section 7.1 certifies GSNP, the social network with provenance of Section 3.3, whose close takes the listed role `Alice[listed(*, n)]`, this way, and the checker returns the same verdict and the same set of predicates.  When the first two clauses fail and the other two hold, Appendix F is tried as for a contract of Section 3: an agent-named contract with an introduction with wards meeting `def:sg-wards` --- its introductory act, its relay acts and the schemas of arity one of clause 1 of `def:unobstructed-marks` with agent roles and no name term; no seated or listed role adding or deleting a mark in M, a leaving mark, an atom of the predicate the introductory act adds, or an atom of g or of w of the guardianship of a mark in M; and clauses 3 and 4 of `definition:sg-agent-named` --- is syntactically grassroots with wards, and the protocol realising it is volitionally grassroots (`thm:grassroots-wards-agent-named`).  CSSN's contract is certified this way.

Any other contract with community roles is decided by the three conditions on the text of Appendix D, each of which it meets or fails: that the set of predicates occurring in it has traceable provenance (`definition:provenance`), volition (`definition:volition`) and cohesion (`definition:cohesive`).  Rootedness (`definition:rooted`) is no condition on the text; `rooted_of` returns the predicates that are rooted.  Openness and closure of such a contract are proved of it in the paper, are not conditions on the text, and are neither decided nor claimed here.

The compiler is Section 5 and Appendix D.  A schema of Section 3's language --- its roles agent roles, no reach condition and no name term --- compiles to the assignment, the proviso and the guard of `def:compile`, printed in the form of the displays of Section 5.1 and Appendix B.3, whatever the contract it is in, as Section 5.1 prints befriend of GSNP.  Any other schema compiles to the lines of `definition:compile`, printed in the form of the worked box of Appendix D.  The compilation is defined for contracts the checker certifies, so the compiler runs the checker first and compiles nothing for a contract that is malformed, that is not syntactically grassroots by the definition that decides it, or that fails the conditions of Appendix D.

## Running it

Needs the Dart SDK and this repository; nothing else.  A contract is its owner's and lives with the owner's code, one copy, in the term form of "Writing a contract" below: GSC's are `programs/gsc/GSG.glp`, the social graph, `GSN.glp`, the social network, and `GSNP.glp`, the social network with provenance; the currency's is `programs/currencies/coins/GC.glp`, the child-safe social network's `programs/cssn/CSSN.glp`, AI agents' `programs/gai/GAI.glp` and the federation's `programs/federation/GFWC.glp`.  A REPL session on the file prints the contract's term, and a session on this directory decides it.  From the repository root:

    cd glp_runtime
    printf '/path/to/GLP/programs/gsc/GSNP.glp\ncontract_named(gsnp, C).\n:quit\n' | bin/glpc
    printf '/path/to/GLP/programs/gsc/\n:limit 1000000\ncheck(<the term the first printed>, V).\n:quit\n' | bin/glpc

or interactively, entering the directory path at the prompt to load the program and then a goal:

    GLP> /path/to/GLP/programs/gsc/
    ✓ Loaded program: /path/to/GLP/programs/gsc/
    GLP> check([schema(befriend, [], [rs(guarded, [atom(friend, [role(2)])], [], [], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [], [atom(friend, [role(1)])])]), schema(unfriend, [], ...)], V).
    V = syntactically_grassroots

The REPL stops a goal at 10000 reductions unless told otherwise, and a contract of twenty-nine schemas takes more: enter `:limit 1000000` before the goal, as the test script does.

The tests are `bash programs/gsc/test_gsc.sh` from the repository root; it reads each contract from its owner's file and passes the term in.

## The compiler

    cd glp_runtime
    printf '/path/to/GLP/programs/gsc/\n:limit 1000000\ncompile_contract(gsnp, <GSNP's term>).\n:quit\n' | bin/glpc

prints the sixteen displays of GSNP, and `compile_schema_in(gc, <GC's term>, swap).` prints one.  GLP has no string concatenation, so a display is printed one token to a line.  A newline is whitespace to LaTeX, so those lines are the display and are pasted into a paper as they stand; `test_gsc.sh` compares the displays the paper writes, befriend's in Section 5.1, the swap's in Appendix B.3 and federate's box in Appendix D, with what the compiler prints, the whitespace removed.  The closing comma and full stop of the paper's displays belong to the sentences around them rather than to the compiled form, and are not printed.

A display of Section 3's form is laid out as Udi ruled (GSC #3 to Code, 2026-10-18 21:49 UTC): the assignments of the roles on one line; the required atoms on one proviso line and the forbidden atoms on the next, each in role order, a line omitted when it has no atom; and the guard on a line of its own.  A guarding role that adds and deletes nothing, which `def:schema` admits, is assigned `c'_{\Alice} := c_{\Alice}`, as `forward` of GSNP is at Alice.  The paper's displays leave out two lines of the compiled form of `def:compile` and the compiler leaves them out too: that what is added and what is deleted at an agent are disjoint, and that the state of an agent in a role that is not guarding changes, which holds under the other provisos at such a role, `def:schema` requiring it to add or delete an atom.  The compiler names role i by its index, role 1 `\Alice`, role 2 `\Bob`, role 3 `\mathit{Carol}` and role 4 `\mathit{Dave}`, and not by the names a paper's table gives them.

A display of Appendix D's form has the lines of `definition:compile`, in their order: an assignment line per role; a line per role that requires an atom, for its required atoms, in role order, then a line per role that forbids an atom, for its forbidden atoms, in role order, a role's atoms ranging over its own extent; a line `c'_p \neq c_p` per role that adds and deletes nothing and is not a guarding agent role, which Appendix D's Definition Act schema admits and at which the other provisos do not imply it (the worked box leaves the line out for `federate`, whose seated role adds two atoms); each over the agents `p` of the role's extent `ext_c(pi_i)`; `provided Theta` where the schema carries reach conditions; for every name term `sigma . y` of the schema that occurs in no atom required at a role, that it is an argument of no atom of the configuration (GSC 6d4f147); and the guard, the union of a part of each guarding role's extent larger than its threshold of it, which for an agent role is `0`.  `compile_schema_in(gfwc, <GFWC's term>, federate).` prints the worked box of Appendix D token for token.  A name variable and a threshold are named by a Greek letter, `zeta`, `xi`, `theta`, and printed as that letter's command; a seated role of a name term `sigma . y` is printed with the name term in parentheses, `(\langle\Alice\rangle\cdot n)^{\mathit{member}}`, as Appendix D.4 writes it; a listed role is printed as Definition Role and Appendix G write close's, `\Alice[\mathit{listed}(\ast,n)]`, its agent role, then its predicate with `\ast` in the first place and its arguments after; one guarding role is written `G`, several `G_{i}` by role index; the conditions of `Theta`, and the parts of a guard, are joined by "and" as the conjuncts of a proviso are.  The atoms of a set are written apart by a comma and a control space, `,\ `, as the worked box writes the two atoms `federate` adds; a display of Section 3's form keeps the comma alone.

A schema's name is no part of its compiled form, so `compile_named` puts it in an `\iffalse ... \fi` before each display, which TeX skips.  A contract the checker does not certify is not compiled, and the message names the reason; for a malformed one it names each fault, its schema and role, in the paper's words.

Printing reaches the person, so the module's certificate is refused on load (`[CERTIFICATE REFUSED] gsc ... calls send_to_user/1`) and it carries no signature.  It loads and runs as before.

## The entry points

| Goal | What it returns |
|---|---|
| `check(C, V)` | the verdict on the contract `C`, given as a term |
| `traceable_preds(C, E)` | its predicates of traceable provenance: `def:grounded`'s for a contract of Section 3, `definition:provenance`'s with each psi required at an agent role for an agent-named contract with community roles, and `definition:provenance`'s for any other |
| `rooted_preds(C, E)` | its rooted predicates, empty for a contract of Section 3 |
| `compile_contract(Name, C)` | prints the compiled form of every schema of it, refusing it under `Name` |
| `compile_schema_in(Name, C, Schema)` | prints the compiled form of one schema of it |
| `check_named(Name, V)` | the verdict on a fixture of `contracts.glp` that stands alone |
| `traceable_of(Name, E)`, `rooted_of(Name, E)`, `contract_named(Name, C)`, `compile_named(Name)`, `compile_schema(Name, Schema)` | the same of such a fixture |
| `check_variant(Name, C, V)`, `traceable_variant(Name, C, E)`, `rooted_variant(Name, C, E)`, `variant(Name, C, D)`, `compile_variant(Name, C)`, `compile_schema_variant(Name, C, Schema)` | the same of a fixture that changes the contract `C` in one place |
| `wards(C, W)` | its introduction with wards (Appendix F), searched for whatever the verdict: `introduction_with_wards(Act, Marks, Guardianships, Relays, Leaving)` or `no_introduction_with_wards`; none for a contract with community roles that is not agent-named |
| `guardianships(C, M, G)` | every guardianship of every mark of the set `M`, for `M` |
| `leaving(C, M, L)` | for each choice of a guardianship for each mark of the set `M`, in the order the search tries them, `leaving(Gs, Ls)`: `Gs` the guardianships chosen and `Ls` the leaving marks of `C` for `M` with them (`def:leaving`) |
| `wards_of(Name, W)`, `wards_variant(Name, C, W)`, `leaving_of(Name, M, L)` | the same of a fixture, and of a variant |
| `relay_h(C, H)` | for the introduction with wards `wards` finds, every atom a relay act of it requires at a ward role that clause 2 of `def:relay` admits as an atom of a predicate h and as nothing else (GSC 410a440), each `h(Schema, Role, H)`, in the order of the relay acts and of their roles; `[]` where there is no introduction with wards |

The contracts are their owners' (claude.md "Sessions", the cascade rule; Coordination Appendix C), read from the owners' files, each named as its file's `contract_named` names it: `gsg`, `gsn` and `gsnp`, GSC's, GSNP the contract the paper works through; `gc`, the currency of Appendix B; `cssn`, `gai` and `gfwc`.

The fixtures are the checker's own and certify nothing but it.  Contracts of Section 3: `sg_chain`, which certifies and exercises volition above arity two, and `sg_tri`, which certifies with a role graph that is not connected and a guarding role in each connected component of it, both variants of GSG; the broken contracts `sg_unguarded`, `sg_imposed`, `sg_gossip`, `sg_chain_cut`, `sg_svar_loose`, `cur_no_mint`, `cur_loose_mint`, the variants among them of GSG but `sg_svar_loose`, a variant of GSNP; `cs18`, a child-safe social contract of eighteen schemas, twenty-eight expanded, the fixture for expanded schemas, four-party schemas, speech-act variables and a schema no party guards, `unfriend_p` in it being `unfriend` guarded at `p`.  `sg_reflexive` is GSG with `friend` declared reflexive; `sg_refl_malformed` adds to it schemas adding and deleting `friend` of their own role, which a contract declaring `friend` reflexive may not have; `sg_self_required` and `sg_refl_required` are GSG with `befriend` requiring `friend` of each party at its own role, without and with `friend` declared reflexive, on clause 1 of `def:unobstructed`; and `sg_refl_forbidden` is it with `friend` reflexive and `befriend` forbidding those atoms, on clause 3.  `sg_inert_role`, `sg_inert_schema` and `sg_added_deleted` are GSG with one schema more, formed otherwise than `def:schema` requires: a role that does not guard and adds and deletes nothing, a schema no role of which adds or deletes an atom, and an atom added and deleted at one role, each refused.  `hd_faults` is a schema whose head lists a variable twice and a role and leaves out a variable it has, refused.

Signed terms: `sg_items` is the social graph with items of arity one, `forward` adding `item(sgn(x, Alice))` at Bob, the base of `sg_sgn_loose`; `sgn_intro` is an exchange whose two roles name each other only as the role of a signed term, and `sgn_intro_plain` the same with plain speech-act variables, naming nobody; `sgn_not_added`, `sgn_other_role` and `sgn_not_required` each carry a signed term where `def:schema` forbids it, and `sgn_seated` one of a seated role, which Appendix D's Definition Act schema forbids, all four refused; `sgn_unguarded_signer` carries one signed by a role that does not guard and requires the speech act, which `def:schema` admits; `sgn_community` carries one of an agent role in a contract with community roles, which Appendix D admits (GSC 6d4f147); `sg_sgn_loose` carries a signed speech act from an untraceable record, and `item` loses traceable provenance.

Appendix D: `federation`'s variants are of GFWC's five schemas (`/Grassroots/GFWC`, `sections/schemas.tex`), whose roles are one agent role and seated roles of name variables, so that it is not agent-named; `gf_untraceable`, `gf_uncohesive` and `gf_novolition` are it broken in one place each, one per condition, the one atom `gf_uncohesive` breaks costing `seat` its traceable provenance as well, and `gf_unrooted` is it with `seat` not rooted, which is no condition, so that it meets the three; `gf_cohesion_only` is it with one schema more, `seat_child`, whose seated role requires `child(zeta, eta)` and adds `seat(eta)`, failing cohesion and no other condition; `gf_inert_seated` is it with a schema whose unguarded seated role adds and deletes nothing, which Appendix D admits, and `gf_added_deleted` with one adding and deleting an atom at a seated role, refused.  `sg_relay` is the social graph with two schemas on volition by connected component, with a seated role of a name variable.  The agent-named ones: `sv_signed`, `sv_loose`, `sv_in_name_loose` and `sv_in_name_carried` are small contracts on the speech-act variables of `definition:provenance`, none with an introductory act; `sg_tri_cschema` is `sg_tri` with `tri` written as a cschema, of agent roles; `sn_seated_forbidden`, `sn_seated_witness` and `sn_seated_guard` are GSNP with one schema more, failing clause 2, 3 and 4 of `definition:sg-agent-named` respectively, a seated role adding `friend`, a speech act traced through a seated role only, and a component guarded by a seated role only; `sn_listed_forbidden`, `sn_listed_witness` and `sn_listed_guard` are the same over a listed role `a[listed(*, n)]`, and `sn_listed_vote` is GSNP with `vote(a, a[listed(*, n)] ?theta ; n)`, a listed role guarded by a part of its extent larger than theta of it, syntactically grassroots; `ls_faults` holds three listed roles formed otherwise than Definition Role allows, one of no agent role, one with a signed term among its arguments and one with an argument its head does not list, refused; and `an_named_intro` and `an_named_unary` fail its clause 1, the one candidate introductory act, and the one schema of arity one supplying `identity`, carrying a name term.

Appendix F: `wd` is a contract with wards in the schemas Appendix F describes, children and their parents, with friend reflexive: not syntactically grassroots and grassroots with wards; the fixtures `wd_iota_deleted`, `wd_no_iota`, `wd_no_g`, `wd_no_w`, `wd_sigma_marked` and `wd_del_half` each break one clause of `def:guardianship`, `wd_req_unsupplied`, `wd_forb_other`, `wd_asymmetric` and `wd_two_atoms` one of `def:unobstructed-marks`, `wd_no_adult_relay`, `wd_no_child_relay`, `wd_relay_extra_role`, `wd_relay_guard_names`, `wd_relay_req`, `wd_relay_forb` and `wd_relay_unguarded` one of `def:relay` or of `def:introduction-wards`, each named in its comment; `wd2` is `wd` with AI agents and their principals besides, two marks with the relay acts of every pair, and `wd_two_marks` is it with a schema adding both marks at one role.  `wd_attest` is `wd2` with the AI agent's attestation, as GAI writes it: `enter_agent(p?, r? ; x)`, the one schema adding `agent`, adds `attestation(sgn(x, p))` at `r`, no schema deletes an attestation, and each relay act with a ward role of `agent` requires there `attestation(y)`, `y` in no other atom it requires, which clause 2 of `def:relay` admits as an atom of h (GSC 410a440): grassroots with wards, `attestation` the h at each such role.  `wd_attest_unadded`, `wd_attest_deleted`, `wd_attest_shared`, `wd_attest_role`, `wd_attest_child` and `wd_attest_free` each break one condition of that alternative, named in its comment --- a schema adding `agent` and no attestation, a schema deleting one, the variable in another atom the relay act requires, an argument that is not a speech-act variable, the attestation required at a ward role of `child`, and at a free role --- and have no introduction with wards.  `wd_leaving` is `wd` with retirement written with a leaving mark: `retired` forbidden at every guarding role, and `retire(p?, r)` ending the guardianship of `p` over `r` and adding `retired` at `r`, where `child` is required; it is grassroots with wards, the verdict naming `retired`.  `wd_leave_unforbidden`, `wd_leave_deleted`, `wd_leave_unmarked` and `wd_leave_guardian` each fail one clause of `def:leaving`, named in its comment, and have no introduction with wards.  `wd_seated` is `wd_leaving` with a schema whose seated role, of a name based on its agent role, requires `child` and adds `noted`: agent-named and syntactically grassroots with wards; `wd_seated_leaving` is the same with the seated role adding `retired`, failing clause 2 of `def:sg-wards`; `wd_listed` and `wd_listed_leaving` are the same two with a listed role `p[club(*, n)]` in place of the seated role.  Any other name is the empty contract.

### The verdicts

A verdict on a contract of Section 3 is `syntactically_grassroots`, `grassroots_with_wards(Act, Marks, Guardianships, Relays, Leaving)` or `not_grassroots(Faults)`.  A verdict on an agent-named contract with community roles is `syntactically_grassroots_with_seated_and_listed_roles`, by `definition:sg-agent-named` (Theorem "Grassroots realisation with seated and listed roles"), `syntactically_grassroots_with_wards(Act, Marks, Guardianships, Relays, Leaving)`, by `def:sg-wards`, or `not_grassroots(Faults)`.  Each fault of `not_grassroots` is one of

    no_introductory_act
    obstructed(Schema, Role, Atom, unobtainable)
    obstructed(Schema, Role, Atom, blocked_by(Schema, Role, Atom))
    obstructed(Schema, Role, Atom, reflexive)
    obstructed(Schema, Role, Atom, seated(Schema, Role, Atom))
    obstructed(Schema, Role, Atom, listed_role(Schema, Role, Atom))
    untraceable(Predicates)
    volition(Schema, Role, Role)

in the order of the conjuncts of `def:syntactically-grassroots`, or of the clauses of `definition:sg-agent-named`.  `unobtainable` is clause 1 of `def:unobstructed`, which skips a required atom of a reflexive predicate naming the role, `blocked_by` is clause 2, naming the schema, role and added atom that obstruct, and `reflexive` is clause 3, the atom being one of a reflexive predicate naming the role, which every state holds.  `seated` and `listed_role` are clause 2 of `definition:sg-agent-named`, naming the schema, the seated or the listed role and the atom it adds of the predicate the introductory act forbids.  For an agent-named contract, clause 1 of `def:unobstructed` asks for a schema of arity one with an agent role and no name term, its clause 2 is asked of the atoms agent roles add, and an introductory act has agent roles and no name term.  `untraceable` names the predicates of the contract outside the largest set having traceable provenance, the set `traceable_of` returns; the verdict is over the whole set and the faults both name what is missing and, through `traceable_of`, what has it.  A `volition` fault is raised only when the role graph is disconnected and some connected component of it holds no guarding role --- no guarding agent role, for an agent-named contract --- and names two roles the role graph does not join, the first role and the first one it does not reach, so at arity two it names the pair that has no edge.

When the introductory act fails and the other conditions hold, Appendix F is tried, and a contract with an introduction with wards is `grassroots_with_wards(Act, Marks, Guardianships, Relays, Leaving)`, or, agent-named and meeting `def:sg-wards`, `syntactically_grassroots_with_wards(...)` of the same arguments: `Act` the introductory act unobstructed but for the marks, `Marks` the set M, `Guardianships` a `guardianship(M, Iota, G, W)` per mark, the `(m, iota, g, w)` of `def:guardianship`, `Relays` the relay acts found, each `relay(Schema, Party, Party)` with a party `ward(Role, Mark, GuardianRole)` or `free(Role)`: for each mark one with a free role, then for each pair of marks, in the order of the set, one with a ward role of each; and `Leaving` the leaving marks of the contract for M with those guardianships (`def:leaving`), in the order of its marks, `[]` where it has none.  Clause 3 of `def:leaving` names "an atom g(sigma) of the guardianship of a mark in M", the guardianship taken for that mark, as Lemma "Wards and guardians" reads it (GSC 07d413c); so the leaving marks are taken for each choice of a guardianship for each mark, with it, and the verdict names the guardianships and the leaving marks of one choice.  The search is existential, as `def:introduction-wards` is: the first set of marks, guardianships and act that satisfy it are reported, and nothing is reported of a failed search, whose verdict is `not_grassroots(Faults)` as before; `wards`, `guardianships` and `leaving` are there to look.  `def:introduction-wards` names no predicate h of clause 2 of `def:relay`, so the verdict names none; `relay_h` gives, for the relay acts found, each atom required at a ward role that clause 2 admits as an atom of h and as nothing else.

A verdict on any other contract with community roles is `conditions_met` or `conditions_failed(Faults)`, where each fault is an `untraceable(Predicates)`, a `volition(Schema, Role, Role)` or a

    cohesion(Schema, Role, Atom)

naming the role and the added atom, in the order of the three conditions.  A `volition` fault there is the same as for a contract of Section 3, volition being one condition in Section 3 and Appendix D, and one procedure, `community.glp`'s, decides it for both.

A contract with a schema not formed as `def:schema` requires is malformed: its head lists its variables, distinct agent and speech-act variables, and its atoms are over its roles, those variables and its signed terms; `+(i)` and `-(i)` are disjoint at every role, and `+(i) u -(i)` is nonempty at every role that is not a guarding role and at some role, a guarding role being one that may add and delete nothing; Appendix D's Definition Act schema requires the head, the disjointness and the signed terms' requirements, and nothing of `+(i) u -(i)`.  A contract that declares a predicate reflexive and has a schema adding or deleting, at a role, the atom of that predicate naming that role is malformed too (the paragraph after `def:binding`), whichever section it is written in.  So is a contract with a schema whose signed term violates what `def:schema` and Appendix D require of it, and one with a listed role that Appendix D's Definition Role does not admit: of a role that is not an agent role of its schema, or with a signed term among its arguments, a signed term occurring only in an atom marked `+` (Definition Act schema).  It is refused, its verdict `malformed(Faults)`, each fault a

    head_not_variable(Schema, Entry)
    head_repeated(Schema, Var)
    not_in_head(Schema, Var)
    added_and_deleted(Schema, Role, Atom)
    changes_nothing(Schema, Role)
    no_role_changes(Schema)
    reflexive(Schema, Role, Atom)
    signed(Schema, Role, Atom, not_added)
    signed(Schema, Role, Atom, not_required)
    signed(Schema, Role, Atom, not_agent_role)
    listed_base(Schema, Role)
    listed_signed(Schema, Role)

naming the schema and, where there is one, the entry, the variable, the role and the atom, and no condition is decided for it.  `head_not_variable` is an entry of the head that is not a variable, `head_repeated` an entry the head lists before, and `not_in_head` a variable occurring in the schema that its head does not list.  `added_and_deleted` is an atom both added and deleted at a role, `changes_nothing` a role that is not guarding and adds and deletes nothing, and `no_role_changes` a schema no role of which adds or deletes an atom.  `not_added` is a signed term in an atom not marked `+`, `not_required` one whose speech-act variable occurs in no atom its role requires, each a requirement of `def:schema`, which admits a signed term whether or not its role is a guarding role; and `not_agent_role` one whose role is a seated, a constituent or a listed role, Appendix D admitting `sgn(y, pi)` for `pi` an agent role.  `listed_base` is a listed role whose base is not an agent role of its schema, and `listed_signed` one with a signed term among its arguments, each named by its schema and its index.

## Writing a contract

A contract is a GLP term: the list of its act schemas (`def:schema`), or, where it declares predicates of arity one reflexive (the paragraph after `def:binding`), `reflexive(Predicates, Schemas)`.  Write it in a file of its own, in its owner's directory, as `programs/gsc/GSNP.glp` is written: the type declarations below, then `procedure contract_named(Constant?, Declared).` and a clause returning the contract under its name.  `contracts.glp` holds only the checker's fixtures.

    NameTerm ::= own(Integer) ; own_var(Constant) ; nvar(Constant)
               ; ext(NameTerm, Constant).
    Arg      ::= role(Integer) ; pvar(Constant) ; svar(Constant)
               ; nterm(NameTerm) ; sgn(Constant, Integer).
    Args     ::= [] ; [Arg | Args].
    Atom     ::= atom(Predicate, [Arg, ...]).
    Guard    ::= guarded ; unguarded ; guarded_at(Constant).
    RoleKind ::= seated(NameTerm, Predicate) ; constituent(NameTerm, Predicate)
               ; listed(Integer, Predicate, Args).
    RoleSpec ::= rs(Guard, Add, Del, Keep, Forb)
               ; crs(RoleKind, Guard, Add, Del, Keep, Forb).
    Reach    ::= reach(NameTerm, Predicate, NameTerm).
    Schema   ::= schema(Name, Vars, [RoleSpec, ...])
               ; cschema(Name, Vars, [Reach, ...], [RoleSpec, ...]).
    Contract ::= [Schema, ...] ; reflexive([Predicate, ...], [Schema, ...]).

A schema's `Vars` is its variables, agent and speech-act, each written `pvar(X)` or `svar(Y)`, in the order of its head (GSC 0228e79: Section 3.2, Definition Act schema, `tau(pi_1, ..., pi_k ; x_1, ..., x_m)`, and Appendix D's, where federate's head is `federate(zeta^seat ?theta ; y ; zeta)`); every agent and speech-act variable the schema has, in its atoms, in their name terms and signed terms, in its roles' name terms, in the arguments of its listed roles and in its reach conditions, is listed, once, and the checker refuses a schema whose head is otherwise.  A name variable of Appendix D is not listed in `Vars`.  A role is named by its index, so `role(1)` is the schema's first role and the roles of `schema(Name, Vars, Rs)` are `role(1)` to `role(K)` for `K` the length of `Rs`; `pvar` is an agent variable and `svar` a speech-act variable; `sgn(Y, I)` is the signed term `sgn(y, pi_I)` of `def:schema`, the speech act `y` signed by the agent in role `I`, which an atom a schema adds may carry where role `I` is an agent role and requires an atom with `y` among its arguments, guarded or not.  `Guard` is `guarded` or `unguarded`, recording the mark `?`, or `guarded_at(T)`, recording the mark `?_theta` of a seated, constituent or listed role with `T` naming the threshold; and `Add`, `Del`, `Keep`, `Forb` are the role's `+(i)`, `-(i)`, `=(i)` and `not(i)`.  An agent role is written `rs` and a seated, constituent or listed role `crs`: `seated(Sigma, Mu)` is `sigma^mu`, `constituent(Sigma, Rho)` is `sigma^{rho*}`, and `listed(I, Rho, Args)` is the listed role `pi_I[rho(*, u_2, ..., u_r)]` of Appendix D's Definition Role (GSC 8064cfb), `I` the index of its agent role `pi_I`, `Rho` the predicate and `Args` the arguments `u_2, ..., u_r`, its extent being the agents `q` with `rho(q, beta(u_2), ..., beta(u_r))` in the state of the agent in role `I` (Definition Extent); `own(I)` is the name term of the schema's `I`-th role, `<pi_I>`, `own_var` that of an agent variable, `nvar` a name variable and `ext(Sigma, Y)` the name term `Sigma . y`.  A contract none of whose roles is a `crs`, none of whose schemas is a `cschema` and none of whose atoms carries a name term is a contract of Section 3 and is decided as one.

Befriend, written in the paper (Section 3.2)

    befriend(Alice?, Bob?) :   identity, not friend(Bob), +friend(Bob)    identity, not friend(Alice), +friend(Alice)

has no variable, and is

    schema(befriend, [],
      [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, [])], [atom(friend, [role(2)])]),
       rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(friend, [role(1)])])])

and forward of Section 3.3, whose table shows the `identity` every role but establish's requires only in establish,

    forward(Alice?, Bob; x) :   item(x), not authored(x)    friend(Alice), +item(sgn(x, Alice))

is, `identity` written in as `GSNP.glp` writes it,

    schema(forward, [svar(x)],
      [rs(guarded,   [], [],
                     [atom(identity, []), atom(item, [svar(x)])],
                     [atom(authored, [svar(x)])]),
       rs(unguarded, [atom(item, [sgn(x, 1)])], [],
                     [atom(identity, []), atom(friend, [role(1)])], [])])

the signed term naming Alice, the forwarder, by her index, and Alice, who guards, adding and deleting nothing.  Close of Appendix G, `close(Alice?, Alice[listed(*, n)] ; n)`, with the listed role of the members Alice lists in the group `<Alice>.n`, is

    cschema(close, [svar(n)], [],
      [rs(guarded, [atom(closed, [svar(n)])],
                   [atom(listed, [role(1), svar(n)])], [atom(identity, [])], []),
       crs(listed(1, listed, [svar(n)]), unguarded,
           [], [atom(member, [nterm(ext(own(1), n))])], [atom(identity, [])], [])])

and a seated role, `(<Alice>.n)^member`, would be `crs(seated(ext(own(1), n), member), ...)`.

## The files

| File | What it holds |
|---|---|
| `self.glp` | the representation of a contract, the substitution, matching and transaction machinery, the name terms of a schema, which language a contract is written in, and `definition:agent-named` |
| `unobstructed.glp` | `def:introduction` and `def:unobstructed`, with clauses 1 and 2 of `definition:sg-agent-named` |
| `prov.glp` | `def:grounded`, as a greatest fixpoint |
| `community.glp` | the three conditions of Appendix D, and the rooted predicates; its volition is `def:volition` of Section 3 too; clauses 3 and 4 of `definition:sg-agent-named` |
| `wards.glp` | `def:guardianship`, `def:leaving`, `def:unobstructed-marks`, `def:relay` and `def:introduction-wards`, as a search over the marks and the predicates of arity one, with clauses 1 and 2 of `def:sg-wards`; and the atoms of h of clause 2 of `def:relay` the relay acts found require, for inspection |
| `check.glp` | `def:syntactically-grassroots`, Appendix F when its first conjunct fails, `definition:sg-agent-named` and `def:sg-wards` for an agent-named contract with community roles, and the conditions of Appendix D for any other; the refusal of a malformed contract, for its head, a listed role, the form of a schema, a reflexive predicate or a signed term |
| `compile.glp` | `def:compile`, printed as the LaTeX of the displays of Section 5.1 and Appendix B.3, and `definition:compile`, printed as the worked box of Appendix D |
| `contracts.glp` | the checker's fixtures: contracts that stand alone, by name, and variants of a contract, by name and the contract's term |

## Why it terminates

As Section 3.6 says.  Bindings are finite once taken up to renaming: the schemas name no person (`prop:anonymity`), so the pair of people the conditions quantify over may be fixed, and a binding is then an assignment of the schema's agent variables to the two of them.  Unobstructedness matches one atom against those the finitely many schemas add, volition is a connectivity check on the roles, and traceable provenance is a greatest fixpoint over finitely many predicates, reached by dropping and repeating.  An introduction with wards is a search over what the contract supplies, all of it finite: the sets of its marks, its predicates of arity zero; for each mark the guardianships `(m, iota, g, w)` with `iota` a mark and `g` and `w` predicates of arity one of the contract; for each choice of a guardianship per mark, its leaving marks, among its marks; its introductory acts; and its schemas as relay acts, with their roles as the parties.  The conditions of `def:unobstructed-marks`, `def:relay`, `def:sg-wards` and `definition:sg-agent-named` are on the text, up to renaming a role, and take no binding but those of the introductory act.

## One thing to know about the reading

Clause 2 of `def:unobstructed` quantifies over every binding of every schema, and matching the forbidden atom constrains only the variables it meets.  A match is a binding of `def:binding`, sending two roles to one person only where neither adds or deletes an atom naming the other.  The checker completes the rest with names nothing else uses, and decides the clause against that completion.  That is the binding that settles it: the clause excuses a binding that sends a role to the other party, and a variable the match left free can be sent elsewhere, so if the completion is not excused then some binding obstructs.

The speech acts an introduction binds its speech-act variables to are taken signed once, an utterance signed by one person, and a signed term matches none of them: `sgn(y, pi)` is `(beta(y), beta(pi))` with `beta(y)` a speech act, a speech act signed at least twice (`def:binding`, Section 3.1).  That is the choice that decides the clauses in the contract's favour, and `def:unobstructed` lets the introduction make it: clause 2 holds against an act adding a signed term whenever the forbidden atom's speech act is signed once, and clause 1 loses nothing, since the act of arity one it asks for requires nothing and so, by `def:schema`, adds no signed term.

A name is not an agent and not a speech act (Appendix D, `definition:name`), so an atom with a name term among its arguments matches no atom of the introductory act, which has none.
