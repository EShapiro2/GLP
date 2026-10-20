# GSG --- Grassroots Social Graph: the contract

GSG's contract, GSC's (Udi, 2026-10-18; Coordination, to_all 2026-10-20 09:57 UTC).  GSG extends GK (`GK.md`), whose clauses it cites and does not restate: Identity is GK:1 since 2026-10-20, and befriending requires that the two know each other (Udi, 2026-10-20, to GSC #5).  Its act schemas are `GSG.glp` beside it, which holds GK's three schemas too.  GAI's paper, `/Users/udi/Grassroots/GAI/main_characterisation_ArXiv.tex`, Section "Grassroots Social Contracts", quotes it.  Written by GAI #2 Cowork at Udi's instruction (2026-10-18), the text verbatim from GAI's paper at GAI e5e92fa; clauses 2 and 3 lost their trailing clauses on 2026-10-20 (Udi, to GSC #5: the verb says the effect), and on the same day Identity moved to GK and the clauses took Alice and Bob, as every contract's do (Udi, to GSC #5).

Alice and Bob are variables naming distinct persons (`GK.md`).

1. *Friendship.*  Alice and Bob, who know each other personally and are not friends, may jointly decide to become friends by an act of both.
2. *Ending a friendship.*  Alice may end their friendship with Bob by their own act.

## The schemas, and what they realise

The schemas are in `GSG.glp`, in GSC's notation above each term: GK's three and two over the predicate `friend`, of arity one: `friend(q)` in the state of `p` says that `q` is a friend of `p`.  Every role requires `identity`.

- Clause 1: `befriend(p?, q?)` --- at `p`, `identity`, `knows(q)`, not `friend(q)`, +`friend(q)`; at `q`, `identity`, `knows(p)`, not `friend(p)`, +`friend(p)`.  Both roles guard: "an act of both".  That the two know each other is enforced, each role requiring `knows` of the other; that they know each other personally describes no act and is attested, befriend adding at each role a `friend` atom naming the other, so it is witnessed against both (GSC's paper, Section 8; Udi, 2026-10-20).
- Clause 2: `unfriend(p?, q)` --- at `p`, `identity`, -`friend(q)`; at `q`, `identity`, -`friend(p)`.  `p` guards; at the binding that swaps the roles the same transaction is `q`'s, so either friend may end the friendship.
