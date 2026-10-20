# GSG --- Grassroots Social Graph: the contract

GSG's contract, GSC's (Udi, 2026-10-18; Coordination, to_all 2026-10-20 09:57 UTC).  Clauses 2 and 3 lost their trailing clauses, "after which they are friends" and "and then the friendship ends", on 2026-10-20 (Udi, to GSC #5: the verb says the effect).  Its act schemas are `GSG.glp` beside it.  GAI's paper, `/Users/udi/Grassroots/GAI/main_characterisation_ArXiv.tex`, Section "Grassroots Social Contracts", quotes it.  Written by GAI #2 Cowork at Udi's instruction (2026-10-18 20:25 local: "Write all contracts to disc, where they should be, according to the Coordination ruling"; "write act schemas only for GSG and GSN"), the text verbatim from GAI's paper at GAI e5e92fa.

A *person* is a legally culpable entity: a natural person of age, also referred to as an *adult*, or a legal person of good standing, such as a business.  A person takes the pronoun 'they'.

1. *Identity.*  A person may act to establish their identity by choosing their own key pair.  They identify themselves with the public key, sign their acts with the private key, and are accountable for every act signed with it.
2. *Friendship.*  Two persons who know each other personally and are not friends may jointly decide to become friends by an act of both.
3. *Ending a friendship.*  A person may end their friendship with another person by their own act.

## The schemas, and what they realise

The schemas are in `GSG.glp`, in GSC's notation above each term.  Three schemas over two predicates: `identity`, of arity zero, in the state of `p` says that `p` has established their identity; `friend`, of arity one: `friend(q)` in the state of `p` says that `q` is a friend of `p`.  Every role but establish's requires `identity`.

- Clause 1: `establish(p?)` --- at `p`, +`identity`; it requires and forbids nothing.  Being accountable for every act signed with the key describes no act.
- Clause 2: `befriend(p?, q?)` --- at `p`, `identity`, not `friend(q)`, +`friend(q)`; at `q`, `identity`, not `friend(p)`, +`friend(p)`.  Both roles guard: "an act of both".  That the two know each other personally describes no act, and is attested: befriend adds at each role a `friend` atom naming the other, so it is witnessed against both (GSC's paper, Section 8; CSSN #6's request, approved by Udi, 2026-10-19).
- Clause 3: `unfriend(p?, q)` --- at `p`, `identity`, -`friend(q)`; at `q`, `identity`, -`friend(p)`.  `p` guards; at the binding that swaps the roles the same transaction is `q`'s, so either friend may end the friendship.
