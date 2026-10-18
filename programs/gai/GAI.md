# GAI --- the social network with accountable AI agents and digital provenance: the contract

GAI's contract, named GAI (Coordination Appendix C), the one copy, written by its owner (claude.md "Sessions"); its act schemas are `GAI.glp` beside it.  The paper, `/Users/udi/Grassroots/GAI/main_characterisation_ArXiv.tex`, sections "A Social Graph with Accountable AI Agents" and "A Social Network with Digital Provenance", quotes it.  Each contract is numbered from 1 and a clause is cited by its contract's name (Udi, 2026-10-18): GSG:1 and GSG:2 are GSC's (`programs/gsc/GSG.md`); Wards:1 to Wards:5 are the wards core, CSSN's, shared word for word with the child-safe social network (CSSN, `programs/cssn/CSSN.md`), whose own extension is for parents and children; GSNP:1 to GSNP:3 are GSC's (`programs/gsc/GSN.md`); GAI:1 to GAI:5 are this contract's own.  Written by GAI #1 Cowork, 2026-10-17, revised 2026-10-18.

An *agent* is a natural agent, a *person*, natural or legal, with the gender-neutral pronoun `they', or a computational agent, an *AI agent*: a program that holds a key of its own and takes acts of the contract with it.  We say agent of both, since a person and an AI agent alike hold a key, sign, and take the acts of the contract; a clause applies to both unless it says person, and an AI agent takes the pronoun `it'.  Alice and Bob are variables naming distinct agents.

## GSG: Grassroots Social Graph

1. Friendship.  Alice and Bob may jointly decide to become friends, if neither is the friend of the other.  Each is thereafter a friend of the other.
2. Ending a friendship.  Alice may decide to end their friendship with Bob, and it then ends for both.

## Wards: The Wards Core

The core adds wards to a grassroots social contract with relations between agents --- here, friendship.  Here the wards are AI agents, the guardian of each the principal answerable for it; in a companion contract in preparation the wards are children and their guardians their parents, and the core's text is identical in the two.  The intention is that a guardian is a legally culpable entity: an adult natural person or a legal person.

1. Guardianship.  An agent with a recorded guardian is a ward; only an agent that is not a ward is a guardian.  Every act requires its agents to have a genuine digital identity: an agent that is not a ward establishes its own, and a ward's is established by its guardian, with which the ward enters the contract, its guardian recorded.
2. Volition.  A ward that is a person wills its own acts; the acts of a ward that is not a person are willed by its guardian.
3. Relations.  A ward enters a relation of the contract with another agent by an act of both and of a guardian of each ward among them.
4. Ending a Relation.  Either agent in a relation of a ward, or a guardian of that ward, may end it, and it then ends for both.
5. Ending Guardianship.  Guardianship ends by an act of a guardian and by no act of the ward; each extension says what the act is and what follows it.

## GAI: Accountable AI Agents

Guardians are principals and wards their AI agents; the clauses below realise Wards:1 to Wards:5.  An AI agent becomes a friend of a person or of another AI agent by Wards:3, its principal a party to the act.

1. Personhood.  An agent that establishes an identity of its own declares themselves a person, and answers for every act signed with that identity, whoever or whatever took it.
2. Principal.  The guardian of an AI agent is its *principal*, a person, one per AI agent.  The entering of an AI agent records it as an AI agent, its principal recorded and its first friend, and is the principal's authorisation of every act the AI agent takes under this contract on their behalf.
3. Disclosure.  Every act of an AI agent is signed by it and names its principal.
4. Accountability.  The principal answers for the acts of their AI agent as for their own.
5. Retiring.  The principal may retire their AI agent, after which it takes no act of the contract; its friendships end by Wards:4.  No act ends the guardianship of an AI agent.

## GSNP: Grassroots Social Network with Provenance

GSNP:1 to GSNP:3, word for word (`programs/gsc/GSN.md`).  Alice and Bob are any two agents, persons or AI agents alike; an AI agent's acts are authorised by GAI:2.  An *item* is a speech act, content signed by its author and, when forwarded, signed over by each forwarder in order.

1. Authorship.  Alice may decide to sign an item of their own.
2. Forwarding.  Alice may decide to forward an item they hold to their friend Bob, and warrants that they know personally the agent they received the item from.
3. Confidence.  Alice shall disclose an item they hold only by forwarding it.

## The schemas, and what they realise

The schemas are in `GAI.glp`, in GSC's notation above each term.  Eleven schemas over twelve predicates: `establish`, `befriend`, `unfriend_p` and `unfriend_q` (GSG:2, one schema per role, guarded at that role, as CSSN writes it), `enter`, `agent_befriend`, `agents_befriend`, `agent_unfriend`, `retire`, `sign`, `forward`.  Every role requires `identity`, except `establish`'s role, which adds it and requires and forbids nothing, so that `identity` is obtainable in the sense of GSC's `def:unobstructed`, clause 1, and the AI agent's role of `enter`, which forbids it and adds it.

- Wards:1 and GAI:2, entering: `enter(p?, r?)` --- the principal, a person (`not agent`), adds `principal_of(r)` and `friend(r)`; the AI agent, with no identity before, adds `identity`, `agent`, `principal(p)` and `friend(p)`.  Its role is guarded, as the child's is in CSSN's `enter`: the two roles share no record before the act, so GSC's volition condition requires a guard in each, and the mark is taken by the program the principal set up.
- Wards:2, volition, as GAI reads it with GAI:2: the mark `?` on an AI agent's role in `enter`, `agent_befriend` and `agents_befriend` records that the act is taken when the AI agent's program takes it, in `enter` as the program the principal set up and after it under the principal's standing authorisation given by `enter`; the principal's own `?` on the same schemas is Wards:3.  Whether the formalism should carry an AI agent's `?` as the principal's volition, or as a guard of a kind of its own, is a question for GSC.
- Wards:3, relations: `agent_befriend(r?, q?, p?)`, an AI agent and a person, and `agents_befriend(r?, s?, p?, q?)`, two AI agents, each guarded by the two and by the principal of each AI agent among them; the principal adds `approval`, so that its role adds something, as CSSN's parents do.  `befriend(p?, q?)` is GSG:1 and forbids `agent` at both roles.
- Wards:4, ending: `unfriend_p` and `unfriend_q` for either agent, person or AI agent; `agent_unfriend(r, q, p?)` for the principal, who adds `withdrawal`.
- Wards:5 and GAI:5, retiring: `retire(p?, r)` adds `retired` at the AI agent and `retired_of(r)` at the principal and deletes nothing, so the AI agent stays recorded as an AI agent with its principal; every schema with an AI agent's role but `retire` forbids `retired` at it, so a retired AI agent takes no act.  A retired AI agent's friendships end by `unfriend_q`, taken by the friend, or by `agent_unfriend`, taken by the principal.
- GAI:3, disclosure: an act is a digital speech act signed with the key of the agent taking it (GSC, Section 8; DSA); `agent_befriend` and `agents_befriend` add `agent_of(r, p)` at the counterparty, so every friend of an AI agent holds a record naming it as an AI agent and naming its principal.
- GSNP:1 to 3, items: `sign(p?; x)` adds `item(x)` at the author, `x` a speech act signed by `p`, and `forward(p?, q; x)` adds `sent(x, q)` at the forwarder and `item(sgn(x, p))` at the friend, the item signed over by the forwarder --- GSNP's `sign` and `forward` (`programs/gsc/GSN.glp`) with `identity` required and `retired` forbidden at the acting role, so that an AI agent signs and forwards under GAI:2 and not when retired.  The warranty of GSNP:2 is attested and GSNP:3 undertaken, as in GSNP (GSC, Section 8).
- GAI:1 and GAI:4 describe no act: both are undertaken, their record the agent's signed undertaking to keep the contract, held by every party it interacts with (GSC, Section 9).

`befriend` forbids `agent`, which `enter` adds, so the contract has no unobstructed introductory act (GSC, `def:unobstructed`, clause 2) and is not syntactically grassroots, as CSSN's is not for the same reason; it rests on the certification of contracts with wards that CSSN asked GSC for (2026-10-15), approved in concept by Udi on 2026-10-18 and going into GSC's paper and then its checker, and is to be proved grassroots directly in the paper until then: two persons can always befriend, every AI agent has a person on its record, its principal, and through its principal it can come to interact with anyone.
