# The syntactically-grassroots checker, and the compiler

Decides whether a contract is **syntactically grassroots**, and when it is not,
which condition failed, at which schema, role and atom; and compiles the
schemas of a contract that is into their volition-guarded transactions,
printed as the LaTeX of Section 5.2.

The specification is the paper — *Formal Grassroots Social Contracts*
(`/Grassroots/Jurix`, `main.tex`), Section 3 for a contract whose roles are all
party roles and Appendix B, `sections/13-community-roles.tex`, for one with
community roles.  Nothing of either is restated here or in the code: every
procedure names the definition it decides, and the definition is read in the
paper.  A contract is syntactically grassroots
(`def:syntactically-grassroots`) when it has an unobstructed
(`def:unobstructed`) introductory act (`def:introduction`), when the set of
predicates occurring in it has traceable provenance (`def:grounded`), and when
it satisfies volition (`def:volition`), which rests on traceable provenance
too.  Section 7 certifies the two contracts of Sections 3.3 and 3.4 by hand;
the checker returns the same verdict for both, and the same sets of predicates
of traceable provenance.  The compiler is Section 5: a schema compiles to the assignment,
the proviso and the guard of `def:compile`, and Section 5.2 is the form
printed.  Section 5 defines the compilation for syntactically grassroots
contracts, so the compiler runs the checker first and compiles nothing for a
contract that fails.

A contract with community roles is decided by the conditions on the text of
Appendix B: which predicates are rooted (`definition:rooted`), which have
traceable provenance (`definition:provenance`), volition
(`definition:volition`) and cohesion (`definition:cohesive`).  The first two
are sets, given by `rooted_of` and `traceable_of`; the last two the contract
meets or fails.  Openness and closure are proved of a contract in the paper,
are not conditions on the text, and are neither decided nor claimed here.  The
compiled form of a schema of such a contract is Definition Compilation of
Appendix B (`definition:compile`), and its worked box, the display for
`federate`, is the form printed.

## Running it

Needs the Dart SDK and this repository; nothing else.  From the repository root:

    cd glp_runtime
    printf '/path/to/GLP/programs/jurix/\ncheck_named(social_graph, V).\n:quit\n' | bin/glpc

or interactively, entering the directory path at the prompt to load the program
and then a goal:

    GLP> /path/to/GLP/programs/jurix/
    ✓ Loaded program: /path/to/GLP/programs/jurix/
    GLP> check_named(social_graph, V).
    V = syntactically_grassroots

The REPL stops a goal at 10000 reductions unless told otherwise, and CSSN's
eighteen schemas take more: enter `:limit 1000000` before the goal, as the
test script does.

The tests are `bash programs/jurix/test_jurix.sh` from the repository root.

## The compiler

    cd glp_runtime
    printf '/path/to/GLP/programs/jurix/\ncompile_named(social_graph).\n:quit\n' | bin/glpc

prints the four displays of the social graph's contract, and
`compile_schema(currency, swap).` prints one.  GLP has no string
concatenation, so a display is printed one token to a line.  A newline is
whitespace to LaTeX, so those lines are the display and are pasted into a
paper as they stand; the two the paper writes by hand, in Section 5.2, come
back token for token, and `test_jurix.sh` compares them with the whitespace
removed.  The closing comma and full stop of the paper's two displays belong
to the sentences around them rather than to the compiled form, and are not
printed.

A schema's name is no part of its compiled form, so `compile_named` puts it
in an `\iffalse ... \fi` before each display, which TeX skips.

The compilation is defined for syntactically grassroots contracts (Section 5),
so the compiler runs the checker first; `compile_named(sg_gossip).` prints
that the contract is not compiled and nothing else, and `check_named` gives
the faults.

A contract with community roles compiles to the lines of Definition
Compilation of Appendix B, one form for the whole contract, its party roles
included: an assignment line per role and, where the role requires or forbids
an atom, a proviso line, each over the agents `p` of the role's extent
`ext_c(pi_i)`; `provided Theta` where the schema carries reach conditions;
for every name term `sigma . y` of the schema, that it is an argument of no
atom of the configuration; and the guard, the union of a part of each guarding
role's extent larger than its threshold of it, which for a party role is `0`.
`compile_schema(federation, federate).` prints the worked box of Appendix B
token for token, and `test_jurix.sh` compares them.  A name variable and a
threshold are named by a Greek letter, `zeta`, `xi`, `theta`, and printed as
that letter's command; one guarding role is written `G`, several `G_{i}` by
role index; the conditions of `Theta`, and the parts of a guard, are joined by
"and" as the conjuncts of a proviso are.  A contract that fails the conditions
of Appendix B is not compiled, as one that is not syntactically grassroots is
not.

Printing reaches the person, so the module's certificate is refused on load
(`[CERTIFICATE REFUSED] jurix ... calls send_to_user/1`) and it carries no
signature.  It loads and runs as before.

## The entry points

| Goal | What it gives |
|---|---|
| `check(C, V)` | the verdict on the contract `C` |
| `check_named(Name, V)` | the verdict on one of the contracts of `contracts.glp` |
| `traceable_of(Name, E)` | its predicates of traceable provenance |
| `rooted_of(Name, E)` | its rooted predicates, empty for a contract of Section 3 |
| `contract_named(Name, C)` | the contract itself |
| `compile_named(Name)` | prints the compiled form of every schema of it |
| `compile_schema(Name, Schema)` | prints the compiled form of one schema |

The names are `social_graph` and `currency`, the two the paper works through;
`sg_chain`, which certifies and exercises volition above arity two; and the
seven broken contracts `sg_unguarded`, `sg_imposed`, `sg_gossip`,
`sg_chain_cut`, `sg_svar_loose`, `cur_no_mint`, `cur_loose_mint`.
`federation` is GFWC's five schemas
(`/Grassroots/GFWC`, `sections/act-schemas.tex`), and `gf_unrooted`,
`gf_untraceable`, `gf_uncohesive` and `gf_novolition` are it broken in one place
each, one per condition of Appendix B.  Any other name is the empty contract.

A verdict on a contract of Section 3 is `syntactically_grassroots` or
`not_grassroots(Faults)`, where each fault is one of

    no_introductory_act
    obstructed(Schema, Role, Atom, unobtainable)
    obstructed(Schema, Role, Atom, blocked_by(Schema, Role, Atom))
    untraceable(Predicates)
    volition(Schema, Role, Role)

in the order of the three conjuncts of `def:syntactically-grassroots`.
`unobtainable` is clause 1 of `def:unobstructed` and `blocked_by` is clause 2,
naming the schema, role and added atom that obstruct.  `untraceable` names the
predicates of the contract outside the largest set having traceable provenance,
the set `traceable_of` gives; the verdict is over the whole set and the faults
both name what is missing and, through `traceable_of`, what has it.  A
`volition` fault names two roles the role graph does not join — the first role
and the first one it does not reach — so at arity two it names the pair that
has no edge.

A verdict on a contract with community roles is `conditions_met` or
`conditions_failed(Faults)`, where each fault is a `volition(Schema, Role,
Role)` or a

    cohesion(Schema, Role, Atom)

naming the role and the added atom.  A `volition` fault there is raised only
when the role graph is disconnected and some role of the schema is joined to no
guarding role, and names the first role that role one does not reach.

## Writing a contract

A contract is a GLP term: the list of its act schemas (`def:schema`).  Write it
in a file of its own, or add a clause to `contracts.glp`.

    NameTerm ::= own(Integer) ; own_var(Constant) ; nvar(Constant)
               ; ext(NameTerm, Constant).
    Arg      ::= role(Integer) ; pvar(Constant) ; svar(Constant)
               ; nterm(NameTerm).
    Atom     ::= atom(Predicate, [Arg, ...]).
    Guard    ::= guarded ; unguarded ; guarded_at(Constant).
    RoleKind ::= seated(NameTerm, Predicate) ; constituent(NameTerm, Predicate).
    RoleSpec ::= rs(Guard, Add, Del, Keep, Forb)
               ; crs(RoleKind, Guard, Add, Del, Keep, Forb).
    Reach    ::= reach(NameTerm, Predicate, NameTerm).
    Schema   ::= schema(Name, [RoleSpec, ...])
               ; cschema(Name, [Reach, ...], [RoleSpec, ...]).
    Contract ::= [Schema, ...].

A role is named by its index, so `role(1)` is the schema's first role and the
roles of `schema(Name, Rs)` are `role(1)` to `role(K)` for `K` the length of
`Rs`; `pvar` is a party variable and `svar` a speech-act variable.  `Guard` is
`guarded` or `unguarded`, recording the mark `?`, or `guarded_at(T)`, recording
the mark `?_theta` with `T` naming the threshold; and `Add`, `Del`, `Keep`,
`Forb` are the role's `+(i)`, `-(i)`, `=(i)` and `not(i)`.  A party role is
written `rs` and a seated or constituent role `crs`; `own(I)` is the name term
of the schema's `I`-th role, `own_var` that of a party variable, `nvar` a name
variable and `ext` a name term extended by a speech-act variable.  A contract
none of whose roles is a `crs` and none of whose schemas is a `cschema` is a
contract of Section 3 and is decided as one.  Befriend, written in
the paper

    befriend(p?, q?) :   not friend(q), +friend(q)    not friend(p), +friend(p)

is

    schema(befriend,
      [rs(guarded, [atom(friend, [role(2)])], [], [], [atom(friend, [role(2)])]),
       rs(guarded, [atom(friend, [role(1)])], [], [], [atom(friend, [role(1)])])])

The checker takes the contract as given and does not test it against the
well-formedness `def:schema` requires of a schema (`+(i)` and `-(i)` disjoint
and not both empty).

## The files

| File | What it holds |
|---|---|
| `self.glp` | the representation of a contract, and the substitution, matching and transaction machinery |
| `unobstructed.glp` | `def:introduction` and `def:unobstructed` |
| `prov.glp` | `def:grounded`, as a greatest fixpoint |
| `volition.glp` | `def:volition` |
| `community.glp` | the four conditions of Appendix B |
| `check.glp` | `def:syntactically-grassroots`, the two halves together, and the conditions of Appendix B for a contract with community roles |
| `compile.glp` | `def:compile`, printed as the LaTeX of Section 5.2, and `definition:compile`, printed as the worked box of Appendix B |
| `contracts.glp` | the contracts to run on |

## Why it terminates

As Section 3.6 says.  Bindings are finite once taken up to renaming: the
schemas name no person (`prop:anonymity`), so the pair of people the conditions
quantify over may be fixed, and a binding is then an assignment of the schema's
party variables to the two of them.  Unobstructedness matches one atom against
those the finitely many schemas add, volition is a connectivity check on the
roles, and traceable provenance is a greatest fixpoint over finitely many
predicates, reached by dropping and repeating.

## One thing to know about the reading

Clause 2 of `def:unobstructed` quantifies over every binding of every schema,
and matching the forbidden atom constrains only the variables it meets.  The
checker completes the rest with names nothing else uses, and decides the clause
against that completion.  That is the binding that settles it: the clause
excuses a binding that sends a role to the other party, and a variable the match
left free can be sent elsewhere, so if the completion is not excused then some
binding obstructs.
