# GSG --- the grassroots social graph: the contract

GSG's contract (Coordination Appendix C), GSC's by Udi's ruling of 2026-10-18; its act schemas are `GSG.glp` beside it.  The paper, `/Users/udi/Grassroots/GSC/main.tex`, Section 2.1, quotes it, and every other contract extends it.  Written by GSC #1 Cowork, 2026-10-18.

A *party* is a person, natural or legal, with the gender-neutral pronoun `they'; Alice and Bob are variables naming distinct parties.

1. *Friendship.*  Alice and Bob may jointly decide to become friends, if neither is the friend of the other.  Each is thereafter a friend of the other.
2. *Ending a friendship.*  Alice may decide to end their friendship with Bob, and it then ends for both.

## The schemas, and what they realise

The schemas are in `GSG.glp`, in GSC's notation above each term.  Two schemas over one predicate, `friend`, of arity one: `friend(Bob)` in the state of Alice says that Bob is a friend of Alice.

- Clause 1: `befriend(Alice?, Bob?)` --- at Alice, not `friend(Bob)`, +`friend(Bob)`; at Bob, not `friend(Alice)`, +`friend(Alice)`.  Both roles guard: "jointly decide".
- Clause 2: `unfriend(Alice?, Bob)` --- at Alice, -`friend(Bob)`; at Bob, -`friend(Alice)`.  Alice guards; at the binding that swaps the roles the same transaction is Bob's, so either friend may end it.

Befriend is an unobstructed introductory act, `friend` has traceable provenance, and the contract satisfies volition (GSC, Section 7.1), so the contract is syntactically grassroots.
