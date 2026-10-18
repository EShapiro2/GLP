# GSG --- Grassroots Social Graph: the contract

GSG's contract, GSC's: GSC owns every contract that other contracts extend (Udi, 2026-10-18; Coordination, to_all 2026-10-18 19:23 UTC).  Its act schemas are `GSG.glp` beside it.  GAI's paper, `/Users/udi/Grassroots/GAI/main_characterisation_ArXiv.tex`, Section "Grassroots Social Contracts", quotes it.  Written by GAI #2 Cowork at Udi's instruction (2026-10-18 20:25 local: "Write all contracts to disc, where they should be, according to the Coordination ruling"; "write act schemas only for GSG and GSN"), the text verbatim from GAI's paper at GAI e5e92fa.

A *person* is a legally culpable entity: a natural person of age, also referred to as an *adult*, or a legal person of good standing, such as a business.  A person takes the pronoun 'they'.

1. *Identity.*  A person chooses their own key pair, identifies themselves with the public key, signs their acts with the private key, and is accountable for every act signed with it.
2. *Friendship.*  Two persons who are not friends may jointly decide to become friends by an act of both, after which they are friends.
3. *Ending a friendship.*  A person may end their friendship with another person by their own act, and then the friendship ends.

## The schemas, and what they realise

The schemas are in `GSG.glp`, in GSC's notation above each term.  Two schemas over one predicate, `friend`, of arity one: `friend(q)` in the state of `p` says that `q` is a friend of `p`.

- Clause 1 describes no act: a person's key pair is theirs before any act, and every act is signed with its private key.
- Clause 2: `befriend(p?, q?)` --- at `p`, not `friend(q)`, +`friend(q)`; at `q`, not `friend(p)`, +`friend(p)`.  Both roles guard: "an act of both".
- Clause 3: `unfriend(p?, q)` --- at `p`, -`friend(q)`; at `q`, -`friend(p)`.  `p` guards; at the binding that swaps the roles the same transaction is `q`'s, so either friend may end the friendship.
