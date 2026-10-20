# GK --- Grassroots Keys: the contract

GK's contract, GSC's, the base contract every other contract extends (Udi, 2026-10-20, to GSC #5).  Its act schemas are `GK.glp` beside it.  GSG extends it (`GSG.md`); Identity was GSG:1 until 2026-10-20.  Written by GSC #5 Cowork at Udi's instruction, 2026-10-20, the clauses as he approved them in chat.

A *person* is a legally culpable entity: a natural person of age, or a legal person of good standing, such as a business.  A person takes the pronoun 'they'; Alice, Bob and Carol are variables naming distinct persons.  A person *knows* another when they hold the other's public key.

1. *Identity.*  Alice may act to establish their identity by choosing their own key pair.  They identify themselves with the public key, sign their acts with the private key, and are accountable for every act signed with it.
2. *Meeting.*  Alice and Bob may come to know each other by an act of both.
3. *Introduction.*  If Alice and Bob know each other and Alice knows Carol, then an act of Alice and Bob can introduce Carol to Bob, following which Bob knows Carol.

## The schemas, and what they realise

The schemas are in `GK.glp`, in GSC's notation above each term.  Three schemas over two predicates: `identity`, of arity zero, in the state of `p` says that `p` has established their identity; `knows`, of arity one: `knows(q)` in the state of `p` says that `p` holds `q`'s public key.  Every role but establish's requires `identity`.  Nothing deletes or forbids `knows`; a second copy means the same.

- Clause 1: `establish(p?)` --- at `p`, +`identity`; it requires and forbids nothing.  Being accountable for every act signed with the key describes no act.
- Clause 2: `meet(p?, q?)` --- at `p`, `identity`, +`knows(q)`; at `q`, `identity`, +`knows(p)`.  Both roles guard: "an act of both".  It is the introductory act of every contract extending GK (GSC's paper, Definitions Introductory act and Unobstructed): it requires only `identity`, which establish adds, and forbids nothing.
- Clause 3: `introduce(p?, q? ; c)` --- at `p`, `identity`, `knows(q)`, `knows(c)`; at `q`, `identity`, `knows(p)`, +`knows(c)`.  Both roles guard; `c` is an agent variable, the one introduced, who takes no part: `knows(c)` at `q` is traced through `knows(c)` required at `p` (traceable provenance), and the two roles are joined by `knows(q)` at `p` (volition).
