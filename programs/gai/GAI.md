# GAI --- Accountable AI Agents: the contract

GAI's contract (Coordination Appendix C; to_all 2026-10-18 19:23 UTC).  GAI extends Wards, whose clauses it cites and does not restate: Wards:1 to Wards:4 (`programs/gsc/Wards.md`), with GSG and GSN beneath it (`programs/gsc/GSG.md`, `GSN.md`).  GAI's paper, `/Users/udi/Grassroots/GAI/main_characterisation_ArXiv.tex`, Section "A Social Graph with Accountable AI Agents", quotes it.  Written by GAI #2 Cowork at Udi's instruction (2026-10-18 20:25 local: "Write all contracts to disc, where they should be, according to the Coordination ruling"; "write act schemas only for GSG and GSN"), the text verbatim from GAI's paper at GAI e5e92fa.

The terms AI agent, developer and principal are GAI's (Coordination, to_all 2026-10-18 19:23 UTC); AI agent is defined in `programs/gsc/GSN.md`, developer and principal in the clauses below.

1. *Developer.*  Every AI agent is attested by its *developer*, a person, and shall provide the attestation on request.
2. *Principal.*  The guardian of an AI agent is its *principal*.  Every act of the AI agent names its principal, and the principal knows the developer of the AI agent.
3. *Accountability.*  The developer of an AI agent is accountable for its defects, and its principal for operating it recklessly.
4. *Retiring.*  A principal may retire their AI agent, after which it takes no act.

## The schemas

`GAI.glp` beside this file holds the schemas, in GSC's notation above each term: for friendship, GSG's `befriend` and `unfriend` with `identity` required (`unfriend` as `unfriend_p` and `unfriend_q`, one per role); `establish`, a person choosing their key pair (GSG:1); `enter`, a person creating an AI agent and becoming its principal and friend (Wards:1, GAI:2); `agent_befriend` and `agents_befriend`, an AI agent entering a friendship by an act of both and of the principal of each AI agent among them (Wards:2), the counterparty recording the AI agent's principal (GAI:2); `agent_unfriend`, the principal ending it (Wards:3); `retire` (GAI:4); and GSNP's `sign` and `forward`.  GAI:1, GAI:3 and the principal knowing the developer (GAI:2) describe no act.  A ward's membership of a group (Wards:2 with GSN:3) is not in the term.
