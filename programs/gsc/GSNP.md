# GSNP --- Grassroots Social Network with Provenance: the contract

GSNP's contract, GSC's: GSC owns every contract that other contracts extend (Udi, 2026-10-18; Coordination, to_all 2026-10-18 19:23 UTC); CSSN extends it.  GSNP extends GSN, whose clauses it cites and does not restate: GSN:1 to GSN:5 (`GSN.md`).  Its act schemas are `GSNP.glp` beside it, written by GSC #2 Cowork on 2026-10-18; GSC's paper, Section 3.3, quotes them.  GAI's paper, `/Users/udi/Grassroots/GAI/main_characterisation_ArXiv.tex`, Section "A Social Network with Digital Provenance", quotes it.  Written by GAI #2 Cowork at Udi's instruction (2026-10-18 20:25 local: "Write all contracts to disc, where they should be, according to the Coordination ruling"; "write act schemas only for GSG and GSN"), the text verbatim from GAI's paper at GAI e5e92fa.

1. *Authorship.*  An agent signs the content they create.
2. *Forwarding.*  An agent forwards content they received by sending it with the signatures it has and adding their own, and warrants that they know personally the agent they received it from.
3. *Confidence.*  An agent shall disclose content they received only by forwarding it.

## The schemas, and what they realise

The schemas are in `GSNP.glp`, in GSC's notation above each term: GSG's `establish`, `befriend` and `unfriend`; `sign` and `forward`; and GSN's group schemas, with a group message carried as an item.  `item(x)` in the state of `p` is a speech act `x` that `p` holds, signed by `p` if `p` is its author and otherwise by the agent who forwarded it to `p`, its author and every earlier forwarder signed inside it; `sent(x, q)` says that its holder forwarded `x` to `q`.  A message (GSN:1) is a forward of an item, and a group's leader delivers a member's message (GSN:5) by forwarding it, signed by the leader (Udi, 2026-10-18).  Every role but establish's requires `identity`, and the list below leaves it out.

- Clause 1: `sign(p? ; x)` --- at `p`, +`item(x)`.
- Clause 2, and GSN:1: `forward(p?, q ; x)` --- at `p`, `item(x)`, +`sent(x, q)`; at `q`, `friend(p)`, +`item(sgn(x, p))`.  The warranty describes no act.
- Clause 3 describes no act: every act of the contract that discloses an item is a forward.
- GSN:2 to GSN:4: `create`, `join`, `leave` and `close`, as in `GSN.glp`.
- GSN:5, with clause 2: `post_own(a? ; x, n)` --- at `a`, `item(x)`, `member(a, n)`, not `posted(x, n)`, +`posted(x, n)`; `post(p?, a ; x, n)` --- at `p`, `item(x)`, `member(a, n)`, +`sent(x, a)`; at `a`, `member(a, n)`, `listed(p, n)`, +`posted(sgn(x, p), n)`; and `deliver(a, q ; y, n)` --- at `a`, `member(a, n)`, `posted(y, n)`, `listed(q, n)`, not `delivered(y, q, n)`, +`delivered(y, q, n)`; at `q`, `member(a, n)`, +`item(sgn(y, a))`.  Delivery is not guarded: the leader undertook it on creating the group, and signs it.

