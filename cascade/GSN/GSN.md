# GSN --- the grassroots social network: the contract

GSN's contract (Coordination Appendix C), GSC's by Udi's ruling of 2026-10-18: GSG with authorship, forwarding and confidence, the five clauses of the paper's Section 2.1; its act schemas are `GSN.glp` beside it.  The paper, `/Users/udi/Grassroots/GSC/main.tex`, Sections 2.1 and 3.3, quotes it and certifies it in Section 7.1.  Written by GSC #1 Cowork, 2026-10-18.

A *party* is a person, natural or legal, with the gender-neutral pronoun `they'; Alice and Bob are variables naming distinct parties.  The parties to this contract are the members of a social network.  Its first two clauses are GSG:1 and GSG:2, friendship and ending a friendship; its own are these.

1. *Authorship.*  Alice may decide to sign an item of their own.
2. *Forwarding.*  Alice may decide to forward an item they hold to their friend Bob, and warrants that they know personally the party they received the item from.
3. *Confidence.*  Alice shall disclose an item they hold only by forwarding it.

GSG:1, GSG:2, clause 1 and clause 2 describe acts and determine the contract's transactions.  The warranty in clause 2 describes no act of its own, and neither does clause 3; both bind the parties.  The warranty is attested, clause 3 undertaken, the rest enforced (GSC, Section 8).

## The schemas, and what they realise

The schemas are in `GSN.glp`, in GSC's notation above each term: GSG's `befriend` and `unfriend`, and two over `item`, of arity one, and `sent`, of arity two.  `item(x)` in the state of Alice is a speech act Alice holds, signed by Alice if Alice is its author and by the friend it came from otherwise, the author and every earlier forwarder signed inside it; `sent(x, Bob)` says that its holder forwarded `x` to Bob.

- Clause 1: `sign(Alice?; x)` --- at Alice, +`item(x)`; `x` is a speech act signed by Alice, the guarding role (GSC, Definition Binding, admissible).
- Clause 2: `forward(Alice?, Bob; x)` --- at Alice, `item(x)`, +`sent(x, Bob)`; at Bob, `friend(Alice)`, +`item(sgn(x, Alice))`.  Alice guards; Bob receives what Alice held, signed by Alice, so the item carries its provenance inside (GSC, Section 3.1, the signed term).
- Clause 3 and the warranty describe no act: the forward leaves `item(sgn(x, Alice))` in Bob's hands, signed by the party that passed the item on, which is the record of both.

Befriend is an unobstructed introductory act, `{friend, item, sent}` has traceable provenance, and the contract satisfies volition (GSC, Section 7.1), so the contract is syntactically grassroots.
