# The syntactically-grassroots checker

Decides whether a contract is **syntactically grassroots**, and when it is not,
which condition failed, at which schema, role and atom.

The specification is the paper — *Formal Grassroots Social Contracts*
(`/Grassroots/Jurix`, `main.tex`), Section 3.  Nothing of it is restated here or
in the code: every procedure names the definition it decides, and the definition
is read in the paper.  A contract is syntactically grassroots
(`def:syntactically-grassroots`) when it has an unobstructed
(`def:unobstructed`) introductory act (`def:introduction`) and satisfies volition
(`def:volition`), which rests on traceable provenance (`def:grounded`).  Section
7 certifies the two contracts of Sections 3.3 and 3.4 by hand; the checker
returns the same verdict for both, and the same sets of predicates of traceable
provenance.

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

The tests are `bash programs/jurix/test_jurix.sh` from the repository root.

## The entry points

| Goal | What it gives |
|---|---|
| `check(C, V)` | the verdict on the contract `C` |
| `check_named(Name, V)` | the verdict on one of the contracts of `contracts.glp` |
| `traceable_of(Name, E)` | its predicates of traceable provenance |
| `contract_named(Name, C)` | the contract itself |

The names are `social_graph` and `currency`, the two the paper works through;
`sg_chain`, which certifies and exercises volition above arity two; and the five
broken contracts `sg_unguarded`, `sg_imposed`, `sg_gossip`, `sg_chain_cut`,
`cur_no_mint`, `cur_loose_mint`.  Any other name is the empty contract.  A
verdict is `syntactically_grassroots` or `not_grassroots(Faults)`, where each
fault is one of

    no_introductory_act
    obstructed(Schema, Role, Atom, unobtainable)
    obstructed(Schema, Role, Atom, blocked_by(Schema, Role, Atom))
    volition(Schema, Role, Role)

`unobtainable` is clause 1 of `def:unobstructed` and `blocked_by` is clause 2,
naming the schema, role and added atom that obstruct.  A `volition` fault names
two roles the role graph does not join — the first role and the first one it
does not reach — so at arity two it names the pair that has no edge.

## Writing a contract

A contract is a GLP term: the list of its act schemas (`def:schema`).  Write it
in a file of its own, or add a clause to `contracts.glp`.

    Arg      ::= role(Integer) ; pvar(Constant) ; svar(Constant).
    Atom     ::= atom(Predicate, [Arg, ...]).
    RoleSpec ::= rs(Guard, Add, Del, Keep, Forb).
    Schema   ::= schema(Name, [RoleSpec, ...]).
    Contract ::= [Schema, ...].

A role is named by its index, so `role(1)` is the schema's first role and the
roles of `schema(Name, Rs)` are `role(1)` to `role(K)` for `K` the length of
`Rs`; `pvar` is a party variable and `svar` a speech-act variable.  `Guard` is
`guarded` or `unguarded`, recording the mark `?`, and `Add`, `Del`, `Keep`,
`Forb` are the role's `+(i)`, `-(i)`, `=(i)` and `not(i)`.  Befriend, written in
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
| `check.glp` | `def:syntactically-grassroots`, the two halves together |
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
