# The syntactically-grassroots checker, and the compiler

Decides whether a contract is **syntactically grassroots**, and when it is not,
which condition failed, at which schema, role and atom; and compiles the
schemas of a contract that is into their volition-guarded transactions,
printed as the LaTeX of the paper's displays, befriend's in Section 5.1 and
the swap's in Appendix B.3.

The specification is the paper — *Formal Grassroots Social Contracts*
(`/Grassroots/GSC`, `main.tex`), Section 3 for a contract whose roles are all
party roles, Appendix F, `sections/17-wards.tex`, for one with wards, and the
appendix on community roles, `sections/13-community-roles.tex`, for one with
community roles (Appendix D since the paper's restructuring of 2026-10-18, Section 8 before).
Nothing of any of them is restated here or in the code: every
procedure names the definition it decides, and the definition is read in the
paper.  A contract is syntactically grassroots
(`def:syntactically-grassroots`) when it has an unobstructed
(`def:unobstructed`) introductory act (`def:introduction`), when the set of
predicates occurring in it has traceable provenance (`def:grounded`), and when
it satisfies volition (`def:volition`), which rests on traceable provenance
too.  Section 7.1 certifies GSNP, the social network with provenance of
Section 3.3, by hand, and Appendix B.4 the currency; the checker returns the
same verdict for both, and the same sets of predicates of traceable
provenance.  A contract whose introductory act is obstructed may
still be certified by Appendix F: when it has an introduction with wards
(`def:introduction-wards`) --- a nonempty set of marks each with a
guardianship (`def:guardianship`), an introductory act unobstructed but for
the marks (`def:unobstructed-marks`), and for every pair of marks a relay act
with a ward role and a free role and one with two ward roles (`def:relay`)
--- and has traceable provenance and volition, it is grassroots with wards
(`thm:grassroots-wards`), and the verdict names the introductory act, the
marks, their guardianships and the relay acts the search found.  The
compiler is Section 5: a schema compiles to the assignment,
the proviso and the guard of `def:compile`, printed in the form of the
paper's displays.  Section 5 defines the compilation for syntactically grassroots
contracts, and Appendix F takes the realisation of a contract grassroots with
wards by the same definition, so the compiler runs the checker first and
compiles nothing for a contract that fails either.

A contract with community roles is decided by the three conditions on the
text of Appendix D, each of which it meets or fails: that the set of predicates
occurring in it has traceable provenance (`definition:provenance`), volition
(`definition:volition`) and cohesion (`definition:cohesive`).  The first rests
on the largest set of predicates having traceable provenance, which
`traceable_of` gives.  Rootedness (`definition:rooted`) is no condition on the
text; `rooted_of` gives the predicates that are rooted.  Openness and closure
are proved of a contract in the paper, are not conditions on the text, and are
neither decided nor claimed here.  The
compiled form of a schema of such a contract is Definition Compilation of
Appendix D (`definition:compile`), and its worked box, the display for
`federate`, is the form printed.

## Running it

Needs the Dart SDK and this repository; nothing else.  A contract is its
owner's and lives with the owner's code, one copy, in the term form of
"Writing a contract" below: GSC's are `programs/gsc/GSG.glp`, the social
graph, `GSN.glp`, the social network, and `GSNP.glp`, the social network with
provenance; the currency's is `programs/currencies/coins/GC.glp`, the
child-safe social network's `programs/cssn/CSSN.glp` and the federation's
`programs/federation/GFWC.glp`.  A REPL session on the file prints the
contract's term, and a session on this directory decides it.  From the
repository root:

    cd glp_runtime
    printf '/path/to/GLP/programs/gsc/GSNP.glp\ncontract_named(gsnp, C).\n:quit\n' | bin/glpc
    printf '/path/to/GLP/programs/gsc/\ncheck(<the term the first printed>, V).\n:quit\n' | bin/glpc

or interactively, entering the directory path at the prompt to load the program
and then a goal:

    GLP> /path/to/GLP/programs/gsc/
    ✓ Loaded program: /path/to/GLP/programs/gsc/
    GLP> check([schema(befriend, [rs(guarded, [atom(friend, [role(2)])], [], [], [atom(friend, [role(2)])]), rs(guarded, [atom(friend, [role(1)])], [], [], [atom(friend, [role(1)])])]), schema(unfriend, ...)], V).
    V = syntactically_grassroots

The REPL stops a goal at 10000 reductions unless told otherwise, and a contract
of twenty-eight schemas takes more: enter `:limit 1000000` before the goal, as
the test script does.

The tests are `bash programs/gsc/test_gsc.sh` from the repository root; it
reads each contract from its owner's file and passes the term in.

## The compiler

    cd glp_runtime
    printf '/path/to/GLP/programs/gsc/\ncompile_contract(gsnp, <GSNP's term>).\n:quit\n' | bin/glpc

prints the twelve displays of GSNP, and
`compile_schema_in(gc, <GC's term>, swap).` prints one.  GLP has no string
concatenation, so a display is printed one token to a line.  A newline is
whitespace to LaTeX, so those lines are the display and are pasted into a
paper as they stand; `test_gsc.sh` compares the two displays the paper writes
by hand, befriend's in Section 5.1 and the swap's in Appendix B.3, with what
the compiler prints, the whitespace removed.  The closing comma and full stop
of the paper's two displays belong to the sentences around them rather than
to the compiled form, and are not printed.

A schema's name is no part of its compiled form, so `compile_named` puts it
in an `\iffalse ... \fi` before each display, which TeX skips.

The compilation is defined for syntactically grassroots contracts (Section 5),
so the compiler runs the checker first; `compile_named(sg_gossip).` prints
that the contract is not compiled and nothing else, and `check_named` gives
the faults.  A malformed contract is not compiled either, and the message
names each fault the checker found, its schema and role, in the paper's words.

A contract with community roles compiles to the lines of Definition
Compilation of Appendix D, one form for the whole contract, its party roles
included: an assignment line per role and, where the role requires or forbids
an atom, a proviso line, each over the agents `p` of the role's extent
`ext_c(pi_i)`; `provided Theta` where the schema carries reach conditions;
for every name term `sigma . y` of the schema, that it is an argument of no
atom of the configuration; and the guard, the union of a part of each guarding
role's extent larger than its threshold of it, which for a party role is `0`.
`compile_schema_in(gfwc, <GFWC's term>, federate).` prints the worked box of Appendix D
token for token, and `test_gsc.sh` compares them.  A name variable and a
threshold are named by a Greek letter, `zeta`, `xi`, `theta`, and printed as
that letter's command; one guarding role is written `G`, several `G_{i}` by
role index; the conditions of `Theta`, and the parts of a guard, are joined by
"and" as the conjuncts of a proviso are.  The atoms of a set are written apart
by a comma and a control space, `,\ `, as the worked box writes the two atoms
`federate` adds; the displays of Section 5.1 and Appendix B.3 write no set of
two, and a display in their form keeps the comma alone.  A contract that fails the conditions of Appendix D is
not compiled, as one that is not syntactically grassroots is not.

Printing reaches the person, so the module's certificate is refused on load
(`[CERTIFICATE REFUSED] gsc ... calls send_to_user/1`) and it carries no
signature.  It loads and runs as before.

## The entry points

| Goal | What it gives |
|---|---|
| `check(C, V)` | the verdict on the contract `C`, given as a term |
| `traceable_preds(C, E)` | its predicates of traceable provenance |
| `rooted_preds(C, E)` | its rooted predicates, empty for a contract of Section 3 |
| `compile_contract(Name, C)` | prints the compiled form of every schema of it, refusing it under `Name` |
| `compile_schema_in(Name, C, Schema)` | prints the compiled form of one schema of it |
| `check_named(Name, V)` | the verdict on a fixture of `contracts.glp` that stands alone |
| `traceable_of(Name, E)`, `rooted_of(Name, E)`, `contract_named(Name, C)`, `compile_named(Name)`, `compile_schema(Name, Schema)` | the same of such a fixture |
| `check_variant(Name, C, V)`, `traceable_variant(Name, C, E)`, `rooted_variant(Name, C, E)`, `variant(Name, C, D)`, `compile_variant(Name, C)`, `compile_schema_variant(Name, C, Schema)` | the same of a fixture that changes the contract `C` in one place |
| `wards(C, W)` | its introduction with wards (Appendix F), searched for whatever the verdict: `introduction_with_wards(Act, Marks, Guardianships, Relays)` or `no_introduction_with_wards` |
| `guardianships(C, M, G)` | every guardianship of every mark of the set `M`, for `M` |
| `wards_of(Name, W)`, `wards_variant(Name, C, W)` | the same of a fixture, and of a variant |

The contracts are their owners' (claude.md "Sessions", the cascade rule;
Coordination Appendix C), read from the owners' files, each named as its
file's `contract_named` names it: `gsg`, `gsn` and `gsnp`, GSC's, GSNP the
contract the paper works through; `gc`, the currency of Appendix B; `cssn`
and `gfwc`.  The
fixtures are the checker's own and certify nothing but it:
`sg_chain`, which certifies and exercises volition above arity two; `sg_tri`,
which certifies with a role graph that is not connected and a guarding role in
each connected component of it; and the
seven broken contracts `sg_unguarded`, `sg_imposed`, `sg_gossip`,
`sg_chain_cut`, `sg_svar_loose`, `cur_no_mint`, `cur_loose_mint`; `cs18`, a
child-safe social contract of eighteen schemas, twenty-eight expanded, the
fixture for expanded schemas, four-party schemas, speech-act variables and a
schema no party guards.  `federation`, GFWC's five schemas
(`/Grassroots/GFWC`, `sections/schemas.tex`), has roles that are one party role
and seated roles, none of them a constituent role; `gf_untraceable`,
`gf_uncohesive` and `gf_novolition` are it broken in one place each, one per
condition, the one atom `gf_uncohesive` breaks costing `seat` its traceable
provenance as well, and `gf_unrooted` is it with `seat` not rooted, which is
no condition, so that it meets the three; `gf_cohesion_only` is it with one
schema more, `seat_child`, whose seated role requires `child(zeta, eta)` and
adds `seat(eta)`, failing cohesion alone.  `sv_signed`, `sv_loose`,
`sv_in_name_loose` and `sv_in_name_carried` are small contracts on the
speech-act variables of `definition:provenance`, `sg_relay` is the social
graph with two schemas on volition by connected component, and
`sg_tri_cschema` is `sg_tri` with `tri` written as a cschema, all in the
language of Appendix D.  `sg_items` is the social graph with items of arity
one, `forward` adding `item(sgn(x, Alice))` at Bob, without identity and
groups, the base of `sg_sgn_loose`; `sgn_intro` is an exchange whose two roles
name each other only as the role of a signed term, and `sgn_intro_plain` the
same with plain speech-act variables, naming nobody; `sgn_not_added`,
`sgn_other_role` and `sgn_not_required` each carry a signed term where
`def:schema` forbids it, `sgn_community` one in a contract with community
roles, and all four are refused; `sgn_unguarded_signer` carries one signed by
a role that does not guard and requires the speech act, which `def:schema`
admits; `sg_sgn_loose` carries a signed speech act from an untraceable
record, and `item` loses traceable provenance.  `wd` is a contract with wards
in the schemas Appendix F describes, children and their parents, with friend
reflexive: not syntactically grassroots and grassroots with wards; the
fixtures `wd_iota_deleted`, `wd_no_iota`, `wd_no_g`, `wd_no_w`,
`wd_sigma_marked` and `wd_del_half` each break one clause of
`def:guardianship`, `wd_req_unsupplied`, `wd_forb_other`, `wd_asymmetric`
and `wd_two_atoms` one of `def:unobstructed-marks`, `wd_no_adult_relay`,
`wd_no_child_relay`, `wd_relay_extra_role`, `wd_relay_guard_names`,
`wd_relay_req`, `wd_relay_forb` and `wd_relay_unguarded` one of `def:relay`
or of `def:introduction-wards`, each named in its comment; `wd2` is `wd` with
AI agents and their principals besides, two marks with the relay acts of
every pair, and `wd_two_marks` is it with a schema adding both marks at one
role.  `sg_reflexive` is the social graph with `friend`
declared reflexive; `sg_refl_malformed` adds to it schemas adding and
deleting `friend` of their own role, which a contract declaring `friend`
reflexive may not have; `sg_self_required` and `sg_refl_required` are the
social graph with `befriend` requiring `friend` of each party at its own role,
without and with `friend` declared reflexive, on clause 1 of
`def:unobstructed`; and `sg_refl_forbidden` is it with `friend` reflexive and
`befriend` forbidding those atoms, on clause 3.  `cssn` declares `friend`
reflexive, as CSSN's paper does, and is its eighteen schemas expanded to
twenty-eight, `unfriend`, `child_unfriend`, `leave`, `child_leave` and
`child_leave_2` each written as one schema per role, guarded at that role and
named by it: `unfriend_p` is `unfriend` guarded at `p`.  Any other name is the
empty contract.

A verdict on a contract of Section 3 is `syntactically_grassroots`,
`grassroots_with_wards(Act, Marks, Guardianships, Relays)` or
`not_grassroots(Faults)`, where each fault is one of

    no_introductory_act
    obstructed(Schema, Role, Atom, unobtainable)
    obstructed(Schema, Role, Atom, blocked_by(Schema, Role, Atom))
    obstructed(Schema, Role, Atom, reflexive)
    untraceable(Predicates)
    volition(Schema, Role, Role)

in the order of the three conjuncts of `def:syntactically-grassroots`.
`unobtainable` is clause 1 of `def:unobstructed`, which skips a required atom
of a reflexive predicate naming the role, `blocked_by` is clause 2, naming
the schema, role and added atom that obstruct, and `reflexive` is clause 3,
the atom being one of a reflexive predicate naming the role, which every
state holds.  `untraceable` names the
predicates of the contract outside the largest set having traceable provenance,
the set `traceable_of` gives; the verdict is over the whole set and the faults
both name what is missing and, through `traceable_of`, what has it.  A
`volition` fault is raised only when the role graph is disconnected and some
connected component of it holds no guarding role, and names two roles the
role graph does not join — the first role and the first one it does not reach
— so at arity two it names the pair that has no edge.  When the first conjunct
fails and the other two hold, Appendix F is tried, and a contract with an
introduction with wards is `grassroots_with_wards(Act, Marks, Guardianships,
Relays)`: `Act` the introductory act unobstructed but for the marks, `Marks`
the set M, `Guardianships` a `guardianship(M, Iota, G, W)` per mark, the
`(m, iota, g, w)` of `def:guardianship`, and `Relays` the relay acts found,
each `relay(Schema, Party, Party)` with a party `ward(Role, Mark,
GuardianRole)` or `free(Role)`: for each mark one with a free role, then for
each pair of marks, in the order of the set, one with a ward role of each.
The search is existential, as `def:introduction-wards` is: the first set of
marks, guardianships and act that satisfy it are reported, and nothing is
reported of a failed search, whose verdict is `not_grassroots(Faults)` as
before; `wards` and `guardianships` are there to look.

A verdict on a contract with community roles is `conditions_met` or
`conditions_failed(Faults)`, where each fault is an `untraceable(Predicates)`,
a `volition(Schema, Role, Role)` or a

    cohesion(Schema, Role, Atom)

naming the role and the added atom, in the order of the three conditions.
`untraceable` names the predicates of the contract outside the largest set
having traceable provenance, as for a contract of Section 3.  A `volition`
fault there is the same as for a contract of Section 3, volition being one
condition in Section 3 and Appendix D, and one procedure, `community.glp`'s, decides
it for both.

A contract that declares a predicate reflexive and has a schema adding or
deleting, at a role, the atom of that predicate naming that role is
malformed (the paragraph after `def:binding`), whichever section it is
written in.  So is a contract with a schema whose signed term violates what
`def:schema` requires of it.  It is refused, its verdict `malformed(Faults)`,
each fault a

    reflexive(Schema, Role, Atom)
    signed(Schema, Role, Atom, not_added)
    signed(Schema, Role, Atom, not_required)
    signed(Schema, Role, Atom, community)

naming the schema, the role and the atom, and no condition is decided for it.
`not_added` is a signed term in an atom not marked `+`, and `not_required` one
whose speech-act variable occurs in no atom its role requires, each a
requirement of `def:schema`, which admits a signed term whether or not its
role is a guarding role; and `community` is a signed term in a contract with community
roles, whose schemas (Appendix D, Definition Act schema) are written over
roles, variables and name terms and admit none.

## Writing a contract

A contract is a GLP term: the list of its act schemas (`def:schema`), or,
where it declares predicates of arity one reflexive (the paragraph after
`def:binding`), `reflexive(Predicates, Schemas)`.  Write it in a file of its
own, in its owner's directory, as `programs/gsc/GSNP.glp` is
written: the type declarations below, then `procedure contract_named(Constant?,
Declared).` and a clause giving the contract under its name.  `contracts.glp`
holds only the checker's fixtures.

    NameTerm ::= own(Integer) ; own_var(Constant) ; nvar(Constant)
               ; ext(NameTerm, Constant).
    Arg      ::= role(Integer) ; pvar(Constant) ; svar(Constant)
               ; nterm(NameTerm) ; sgn(Constant, Integer).
    Atom     ::= atom(Predicate, [Arg, ...]).
    Guard    ::= guarded ; unguarded ; guarded_at(Constant).
    RoleKind ::= seated(NameTerm, Predicate) ; constituent(NameTerm, Predicate).
    RoleSpec ::= rs(Guard, Add, Del, Keep, Forb)
               ; crs(RoleKind, Guard, Add, Del, Keep, Forb).
    Reach    ::= reach(NameTerm, Predicate, NameTerm).
    Schema   ::= schema(Name, [RoleSpec, ...])
               ; cschema(Name, [Reach, ...], [RoleSpec, ...]).
    Contract ::= [Schema, ...] ; reflexive([Predicate, ...], [Schema, ...]).

A role is named by its index, so `role(1)` is the schema's first role and the
roles of `schema(Name, Rs)` are `role(1)` to `role(K)` for `K` the length of
`Rs`; `pvar` is a party variable and `svar` a speech-act variable; `sgn(Y, I)`
is the signed term `sgn(y, pi_I)` of `def:schema`, the speech act `y` signed by
the agent in role `I`, which an atom a schema adds may carry where role `I`
requires an atom with `y` among its arguments, guarded or not.  `Guard` is
`guarded` or `unguarded`, recording the mark `?`, or `guarded_at(T)`, recording
the mark `?_theta` with `T` naming the threshold; and `Add`, `Del`, `Keep`,
`Forb` are the role's `+(i)`, `-(i)`, `=(i)` and `not(i)`.  A party role is
written `rs` and a seated or constituent role `crs`; `own(I)` is the name term
of the schema's `I`-th role, `own_var` that of a party variable, `nvar` a name
variable and `ext` a name term extended by a speech-act variable.  A contract
none of whose roles is a `crs` and none of whose schemas is a `cschema` is a
contract of Section 3 and is decided as one.  Befriend, written in
the paper (Section 3.2)

    befriend(Alice?, Bob?) :   identity, not friend(Bob), +friend(Bob)    identity, not friend(Alice), +friend(Alice)

is

    schema(befriend,
      [rs(guarded, [atom(friend, [role(2)])], [], [atom(identity, [])], [atom(friend, [role(2)])]),
       rs(guarded, [atom(friend, [role(1)])], [], [atom(identity, [])], [atom(friend, [role(1)])])])

and the two schemas of Section 3.3 that carry the speech-act variable `x`,
sign and forward, written in its table, which shows the `identity` every role
but establish's requires only in establish,

    sign(Alice?; x) :   +item(x)
    forward(Alice?, Bob; x) :   item(x), +sent(x, Bob)    friend(Alice), +item(sgn(x, Alice))

are, `identity` written in as `GSNP.glp` writes it,

    schema(sign,
      [rs(guarded, [atom(item, [svar(x)])], [], [atom(identity, [])], [])])
    schema(forward,
      [rs(guarded,   [atom(sent, [svar(x), role(2)])], [],
                     [atom(identity, []), atom(item, [svar(x)])], []),
       rs(unguarded, [atom(item, [sgn(x, 1)])], [],
                     [atom(identity, []), atom(friend, [role(1)])], [])])

the signed term naming Alice, the forwarder, by her index.  The checker
takes the contract as given and does not test it against the
well-formedness `def:schema` requires of a schema (`+(i)` and `-(i)` disjoint
and not both empty).  It does test what `def:schema` requires of a signed
term, and what the paragraph after `def:binding`
requires of a contract declaring a predicate reflexive, and refuses a
contract that fails either as malformed.

## The files

| File | What it holds |
|---|---|
| `self.glp` | the representation of a contract, and the substitution, matching and transaction machinery |
| `unobstructed.glp` | `def:introduction` and `def:unobstructed` |
| `prov.glp` | `def:grounded`, as a greatest fixpoint |
| `community.glp` | the three conditions of Appendix D, and the rooted predicates; its volition is `def:volition` of Section 3 too |
| `wards.glp` | `def:guardianship`, `def:unobstructed-marks`, `def:relay` and `def:introduction-wards`, as a search over the marks and the predicates of arity one |
| `check.glp` | `def:syntactically-grassroots`, the two halves together, Appendix F when its first conjunct fails, and the conditions of Appendix D for a contract with community roles; the refusal of a malformed contract, for a reflexive predicate or a signed term |
| `compile.glp` | `def:compile`, printed as the LaTeX of the displays of Section 5.1 and Appendix B.3, and `definition:compile`, printed as the worked box of Appendix D |
| `contracts.glp` | the checker's fixtures: contracts that stand alone, by name, and variants of a contract, by name and the contract's term |

## Why it terminates

As Section 3.6 says.  Bindings are finite once taken up to renaming: the
schemas name no person (`prop:anonymity`), so the pair of people the conditions
quantify over may be fixed, and a binding is then an assignment of the schema's
party variables to the two of them.  Unobstructedness matches one atom against
those the finitely many schemas add, volition is a connectivity check on the
roles, and traceable provenance is a greatest fixpoint over finitely many
predicates, reached by dropping and repeating.  An introduction with wards is
a search over what the contract supplies, all of it finite: the sets of its
marks, its predicates of arity zero; for each mark the guardianships
`(m, iota, g, w)` with `iota` a mark and `g` and `w` predicates of arity one of
the contract; its introductory acts; and its schemas as relay acts, with
their roles as the parties.  The conditions of `def:unobstructed-marks` and
`def:relay` are on the text, up to renaming a role, and take no binding.

## One thing to know about the reading

Clause 2 of `def:unobstructed` quantifies over every binding of every schema,
and matching the forbidden atom constrains only the variables it meets.  A
match is a binding of `def:binding`, sending two roles to one person only
where neither adds or deletes an atom naming the other.  The
checker completes the rest with names nothing else uses, and decides the clause
against that completion.  That is the binding that settles it: the clause
excuses a binding that sends a role to the other party, and a variable the match
left free can be sent elsewhere, so if the completion is not excused then some
binding obstructs.

The speech acts an introduction binds its speech-act variables to are taken
signed once, an utterance signed by one person, and a signed term matches
none of them: `sgn(y, pi)` is `(beta(y), beta(pi))` with `beta(y)` a speech
act, a speech act signed at least twice (`def:binding`, Section 3.1).  That
is the choice that decides the clauses in the contract's favour, and
`def:unobstructed` lets the introduction make it: clause 2 holds against an
act adding a signed term whenever the forbidden atom's speech act is signed
once, and clause 1 loses nothing, since the act of arity one it asks for
requires nothing and so, by `def:schema`, adds no signed term.
