# GAI --- the social network with accountable AI agents and digital provenance: the contract

GAI's contract, named GAI (Coordination Appendix C), the one copy, written by its owner (claude.md "Sessions"); its act schemas are `GAI.glp` beside it.  The paper, `/Users/udi/Grassroots/GAI/main_characterisation_ArXiv.tex`, sections "A Social Graph with Accountable AI Agents" and "A Social Network with Digital Provenance", quotes it.  Clauses 1 and 2 are GSG:1 and GSG:2, GSC's (`programs/gsc/GSG.md`); clauses 3 to 7 are the wards core, CSSN's, shared word for word with the child-safe social network (CSSN, `programs/cssn/CSSN.md`), whose own extension is for parents and children; clauses 13 to 15 are GSNP:1 to GSNP:3, GSC's (`programs/gsc/GSN.md`); clauses 8 to 12 are GAI's own.  Written by GAI #1 Cowork, 2026-10-17, revised 2026-10-18.

A *party* is a person, natural or legal, with the gender-neutral pronoun `they', or an AI agent; Alice and Bob are variables naming distinct parties.  An *AI agent* is a party that is not a person: a program that holds a key of its own and takes acts of the contract with it.

## The grassroots social graph

1. Friendship.  Alice and Bob may jointly decide to become friends, if neither is the friend of the other.  Each is thereafter a friend of the other.
2. Ending a friendship.  Alice may decide to end their friendship with Bob, and it then ends for both.

## The wards core

The core adds wards to a grassroots social contract with relations between parties --- here, friendship.  Here the wards are AI agents, the guardian of each the principal answerable for it; in a companion contract in preparation the wards are children and their guardians their parents, and the core's text is identical in the two.  The intention is that a guardian is a legally culpable entity: an adult natural person or a legal person.

3. Guardianship.  A party with a recorded guardian is a ward; only a party that is not a ward is a guardian.  Every act requires its parties to have a genuine digital identity: a party that is not a ward establishes its own, and a ward's is established by its guardian, with which the ward enters the contract, its guardian recorded.
4. Volition.  A ward that is a person wills its own acts; the acts of a ward that is not a person are willed by its guardian.
5. Relations.  A ward enters a relation of the contract with another party by an act of both and of a guardian of each ward among them.
6. Ending a Relation.  Either party to a relation of a ward, or a guardian of that ward, may end it, and it then ends for both.
7. Ending Guardianship.  Guardianship ends by an act of a guardian and by no act of the ward; each extension says what the act is and what follows it.

## The AI-agents extension

Guardians are principals and wards their AI agents; the clauses below realise clauses 3 to 7.  An AI agent becomes a friend of a person or of another AI agent by clause 5, its principal a party to the act.

8. *Personhood.*  A party that establishes an identity of its own declares themselves a person, and answers for every act signed with that identity, whoever or whatever took it.
9. *Principal.*  The guardian of an AI agent is its *principal*, a person, one per AI agent.  The entering of an AI agent records it as an AI agent, its principal recorded and its first friend, and is the principal's authorisation of every act the AI agent takes under this contract on their behalf.
10. *Disclosure.*  Every act of an AI agent is signed by it and names its principal.
11. *Accountability.*  The principal answers for the acts of their AI agent as for their own.
12. *Retiring.*  The principal may retire their AI agent, after which it takes no act of the contract; its friendships end by clause 6.  No act makes an AI agent a party that is not a ward.

## The social network with provenance (GSNP)

GSNP:1 to GSNP:3, word for word (`programs/gsc/GSN.md`).  Alice and Bob are any two parties, persons or AI agents alike; an AI agent's acts are authorised by clause 9.  An *item* is a speech act, content signed by its author and, when forwarded, signed over by each forwarder in order.

13. Authorship.  Alice may decide to sign an item of their own.
14. Forwarding.  Alice may decide to forward an item they hold to their friend Bob, and warrants that they know personally the party they received the item from.
15. Confidence.  Alice shall disclose an item they hold only by forwarding it.

## The schemas, and what they realise

The schemas are in `GAI.glp`, in GSC's notation above each term.  Eleven schemas over twelve predicates: `establish`, `befriend`, `unfriend_p` and `unfriend_q` (clause 2, one schema per role, guarded at that role, as CSSN writes it), `enter`, `agent_befriend`, `agents_befriend`, `agent_unfriend`, `retire`, `sign`, `forward`.  Every role requires `identity`, except `establish`'s role, which adds it and requires and forbids nothing, so that `identity` is obtainable in the sense of GSC's `def:unobstructed`, clause 1, and the AI agent's role of `enter`, which forbids it and adds it.

- Clause 3 and 9, entering: `enter(p?, r?)` --- the principal, a person (`not agent`), adds `principal_of(r)` and `friend(r)`; the AI agent, with no identity before, adds `identity`, `agent`, `principal(p)` and `friend(p)`.  Its role is guarded, as the child's is in CSSN's `enter`: the two roles share no record before the act, so GSC's volition condition requires a guard in each, and the mark is taken by the program the principal set up.
- Clause 4, volition, as GAI reads it with clause 9: the mark `?` on an AI agent's role in `enter`, `agent_befriend` and `agents_befriend` records that the act is taken when the AI agent's program takes it, in `enter` as the program the principal set up and after it under the principal's standing authorisation given by `enter`; the principal's own `?` on the same schemas is clause 5.  Whether the formalism should carry an AI agent's `?` as the principal's volition, or as a guard of a kind of its own, is a question for GSC.
- Clause 5, relations: `agent_befriend(r?, q?, p?)`, an AI agent and a person, and `agents_befriend(r?, s?, p?, q?)`, two AI agents, each guarded by the parties and by the principal of each AI agent among them; the principal adds `approval`, so that its role adds something, as CSSN's parents do.  `befriend(p?, q?)` is clause 1 and forbids `agent` at both roles.
- Clause 6, ending: `unfriend_p` and `unfriend_q` for either party, person or AI agent; `agent_unfriend(r, q, p?)` for the principal, who adds `withdrawal`.
- Clause 7 and 12, retiring: `retire(p?, r)` adds `retired` at the AI agent and `retired_of(r)` at the principal and deletes nothing, so the AI agent stays recorded as an AI agent with its principal; every schema with an AI agent's role but `retire` forbids `retired` at it, so a retired AI agent takes no act.  A retired AI agent's friendships end by `unfriend_q`, taken by the friend, or by `agent_unfriend`, taken by the principal.
- Clause 10, disclosure: an act is a digital speech act signed with the key of the party taking it (GSC, Section 8; DSA); `agent_befriend` and `agents_befriend` add `agent_of(r, p)` at the counterparty, so every friend of an AI agent holds a record naming it as an AI agent and naming its principal.
- Clauses 13 to 15, items: `sign(p?; x)` adds `item(x)` at the author, `x` a speech act signed by `p`, and `forward(p?, q; x)` adds `sent(x, q)` at the forwarder and `item(sgn(x, p))` at the friend, the item signed over by the forwarder --- GSNP's `sign` and `forward` (`programs/gsc/GSN.glp`) with `identity` required and `retired` forbidden at the acting role, so that an AI agent signs and forwards under clause 9 and not when retired.  The warranty of clause 14 is attested and clause 15 undertaken, as in GSNP (GSC, Section 8).
- Clauses 8 and 11 describe no act: both are undertaken, their record the party's signed undertaking to keep the contract, held by every party it interacts with (GSC, Section 9).

`befriend` forbids `agent`, which `enter` adds, so the contract has no unobstructed introductory act (GSC, `def:unobstructed`, clause 2) and is not syntactically grassroots, as CSSN's is not for the same reason; it rests on the certification of contracts with wards that CSSN asked GSC for (2026-10-15), approved in concept by Udi on 2026-10-18 and going into GSC's paper and then its checker, and is to be proved grassroots directly in the paper until then: two persons can always befriend, every AI agent has a person on its record, its principal, and through its principal it can come to interact with anyone.
