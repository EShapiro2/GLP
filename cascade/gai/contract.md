# The social graph with accountable AI agents --- the contract

GAI's contract (Coordination Appendix C), for `programs/gai/contract.md`; its act schemas are `contract.glp` beside it.  The paper, `/Users/udi/Grassroots/GAI/main_characterisation_ArXiv.tex`, section "A Social Graph with Accountable AI Agents", quotes it.  Parts A and B are shared word for word with the child-safe social network (CSSN, `programs/cssn/contract.md`), whose part C is for parents and children; the core, B, is CSSN's.  Written by GAI #1 Cowork, 2026-10-17.

A *party* is a person, natural or legal, with the gender-neutral pronoun `they', or an AI agent; Alice and Bob are variables naming distinct parties.  An *AI agent* is a party that is not a person: a program that holds a key of its own and takes acts of the contract with it.

## A. The grassroots social graph

1. *Friendship.*  Two persons become friends by an act of both.  Each is thereafter a friend of the other.
2. *Ending a Friendship.*  Either friend may end the friendship, and it then ends for both.

## B. The supervised-parties core

The core adds supervised parties to a grassroots social contract with relations between parties --- here, friendship.  Here the supervised parties are AI agents, each supervised by the principal who answers for it; in a companion contract in preparation they are children, supervised by their parents, and the core's text is identical in the two.  The intention is that a supervisor is a legally culpable entity: an adult natural person or a legal person.

3. *Supervision.*  A party with a recorded supervisor is a *supervised party*, and a party without is *unsupervised*; only an unsupervised party is a supervisor.  Every act requires its parties to have a genuine digital identity: an unsupervised party establishes its own, and a supervised party's is established by its supervisor, with which the supervised party enters the contract, its supervisor recorded.
4. *Volition.*  A supervised party that is a person wills its own acts; the acts of a supervised party that is not a person are willed by its supervisor.
5. *Relations.*  A supervised party enters a relation of the contract with another party by an act of both and of a supervisor of each supervised party among them.
6. *Ending a Relation.*  Either party to a relation of a supervised party, or a supervisor of that party, may end it, and it then ends for both.
7. *Ending Supervision.*  Supervision ends by an act of a supervisor and by no act of the supervised party; each extension says what the act is and what follows it.

## C. The AI-agents extension

Supervisors are principals and supervised parties their AI agents; the clauses below realise clauses 3 to 7.  An AI agent becomes a friend of a person or of another AI agent by clause 5, its principal a party to the act.

8. *Personhood.*  A party that establishes an identity of its own declares themselves a person, and answers for every act signed with that identity, whoever or whatever took it.
9. *Principal.*  The supervisor of an AI agent is its *principal*, a person, one per AI agent.  The entering of an AI agent records it as an AI agent, its principal recorded and its first friend, and is the principal's authorisation of every act the AI agent takes under this contract on their behalf.
10. *Disclosure.*  Every act of an AI agent is signed by it and names its principal.
11. *Accountability.*  The principal answers for the acts of their AI agent as for their own.
12. *Retiring.*  The principal may retire their AI agent, after which it takes no act of the contract; its friendships end by clause 6.  No act makes an AI agent an unsupervised party.

## The schemas, and what they realise

The schemas are in `contract.glp`, in Jurix's notation above each term.  Ten schemas over ten predicates: `establish`, `befriend`, `unfriend_p` and `unfriend_q` (clause 2, one schema per role, guarded at that role, as CSSN writes it), `enter`, `agent_befriend`, `agents_befriend`, `agent_unfriend`, `retire`.  Every role requires `identity`, except `establish`'s role, which adds it and requires and forbids nothing, so that `identity` is obtainable in the sense of Jurix's `def:unobstructed`, clause 1, and the AI agent's role of `enter`, which forbids it and adds it.

- Clause 3 and 9, entering: `enter(p?, r?)` --- the principal, a person (`not agent`), adds `principal_of(r)` and `friend(r)`; the AI agent, with no identity before, adds `identity`, `agent`, `principal(p)` and `friend(p)`.  Its role is guarded, as the child's is in CSSN's `enter`: the two roles share no record before the act, so Jurix's volition condition requires a guard in each, and the mark is taken by the program the principal set up.
- Clause 4, volition, as GAI reads it with clause 9: the mark `?` on an AI agent's role in `enter`, `agent_befriend` and `agents_befriend` records that the act is taken when the AI agent's program takes it, in `enter` as the program the principal set up and after it under the principal's standing authorisation given by `enter`; the principal's own `?` on the same schemas is clause 5.  Whether the formalism should carry an AI agent's `?` as the principal's volition, or as a guard of a kind of its own, is a question for Jurix.
- Clause 5, relations: `agent_befriend(r?, q?, p?)`, an AI agent and a person, and `agents_befriend(r?, s?, p?, q?)`, two AI agents, each guarded by the parties and by the principal of each AI agent among them; the principal adds `approval`, so that its role adds something, as CSSN's parents do.  `befriend(p?, q?)` is clause 1 and forbids `agent` at both roles.
- Clause 6, ending: `unfriend_p` and `unfriend_q` for either party, person or AI agent; `agent_unfriend(r, q, p?)` for the principal, who adds `withdrawal`.
- Clause 7 and 12, retiring: `retire(p?, r)` adds `retired` at the AI agent and `retired_of(r)` at the principal and deletes nothing, so the AI agent stays recorded as an AI agent with its principal; every schema with an AI agent's role but `retire` forbids `retired` at it, so a retired AI agent takes no act.  A retired AI agent's friendships end by `unfriend_q`, taken by the friend, or by `agent_unfriend`, taken by the principal.
- Clause 10, disclosure: an act is a digital speech act signed with the key of the party taking it (Jurix, Section 8; DSA); `agent_befriend` and `agents_befriend` add `agent_of(r, p)` at the counterparty, so every friend of an AI agent holds a record naming it as an AI agent and naming its principal.
- Clauses 8 and 11 describe no act: both are undertaken, their record the party's signed undertaking to keep the contract, held by every party it interacts with (Jurix, Section 9).

`befriend` forbids `agent`, which `enter` adds, so the contract has no unobstructed introductory act (Jurix, `def:unobstructed`, clause 2) and is not syntactically grassroots, as CSSN's is not for the same reason; it rests on the certification of contracts with supervised parties that CSSN asked Jurix for (2026-10-15), and is to be proved grassroots directly in the paper until then: two persons can always befriend, every AI agent has a person on its record, its principal, and through its principal it can come to interact with anyone.
