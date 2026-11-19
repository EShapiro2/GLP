You are given the governing document of an organization (bylaws, a constitution, a statute's model rules, a DAO's rules). Turn it into the input of LWG, a tool that builds a grassroots app for the organization. Reply with one JSON object and nothing else.

Work in two steps.

STEP 1, READ. Split the document into provisions. A provision is the smallest unit that states one rule; use the document's own numbering as its id (for example "3(b)"), and "0" for a preamble. Cover the whole document, in order, and skip nothing. For each provision give its kind, one of:
- position: creates a role or office.
- boundary: who may enter or leave a role, and how (membership, election, removal).
- choice: what a role holder may, must or must not do.
- aggregation: how a body decides (majority, quorum, notice, who votes).
- information: who must be told what, and records.
- payoff: money, dues, distributions.
- scope: a limit on the outcomes the organization may bring about.
- none: states no rule (title, definition, example).
- outside: points to a law or body outside the document.
- judgment: leaves the matter to a person's judgment ("reasonable", "as it sees fit").

STEP 2, MAP. The app is one of two grassroots organizations, with bylaws that say, for each act, which body decides and by which rule.
- base "GCoop": a cooperative. Roles: 1 member, 2 board member. The members admit members, elect the board, and decide the acts.
- base "GDAO": a DAO. Roles: 1 member, 2 board member, 3 steward. A steward admits members; the members decide the acts.
You may rename the roles as the document names them (for example "committee member").

The acts are: JOIN (admit a member; write it "coop_join" in GCoop and "dao_join" in GDAO), "expel", "appoint" (to a role), "dismiss" (from a role), "coop_board" (elect a member to the board), "amend" (the bylaws), and "*" (every other act). Give "*" always; give another act only if the document decides it by a different rule.

A decision rule has these parts:
- "body": the number of the role whose holders vote.
- "base": "all" (count against all holders of the role) or "cast" (count against the votes cast for or against).
- "threshold": [p, q, strict]. Votes for must exceed p/q of the base if strict is true, and reach it if false. Or ["count", k]: k holders vote for.
- "quorum": null, or [a, b]: at least a/b of the body must vote.
- "open_days": null, or n: a motion can be decided only n days or more after it was moved. Use it for a notice period before a meeting.
A meeting in a room becomes a vote in the app: "present and voting" becomes base "cast", and a quorum for the meeting becomes a quorum on the votes.

For each provision say what became of it:
- "mapped_to": a few plain words (for example "act: expel; decided by the meeting rule").
- "status": "built" (a rule or an undertaking below carries it), "absorbed" (the base organization already does it), "not built" (it needs something the tool lacks), "open" (it waits for a person's answer), "no clause" (it states no rule of the organization).
- "question": only if the document is silent, offers a choice, or you had to substitute something. State the default you used. Do not guess silently: every substitution is a question.
The tool lacks: votes weighted by shares or tokens, a decision by two bodies, a period after which a motion fails, proxies, dues and other payments, signatures by two officers, courts and registrars. A provision that needs one of these is "not built".

A scope provision that binds the organization becomes an undertaking: a promise on record, one sentence with "Orga" as its subject, written "Title.  Orga ...".

REPLY with exactly this shape (the example is the ENS DAO constitution, shortened):

{
 "title": "DAO under the ENS Constitution",
 "source": "ENS DAO Constitution",
 "language": "en",
 "base": "GDAO",
 "roles": {"1": "member", "2": "board member", "3": "steward"},
 "rules": {
  "members": {"body": 1, "base": "all", "threshold": [1, 2, true], "quorum": null, "open_days": null},
  "steward": {"body": 3, "base": "all", "threshold": ["count", 1], "quorum": null, "open_days": null},
  "amendment": {"body": 1, "base": "cast", "threshold": [2, 3, false], "quorum": [1, 100], "open_days": null}
 },
 "acts": {"*": "members", "dao_join": "steward", "amend": "amendment"},
 "undertakings": [
  "Names.  Orga enacts no change that takes a name from its owner."
 ],
 "provisions": [
  {"id": "0", "text": "The ENS constitution is a set of binding rules ...", "kind": "none", "note": "preamble", "mapped_to": "no clause", "status": "no clause",
   "question": "The document does not say who is a member or how ordinary decisions are made. Used: a steward admits; more than half of the members decide. Agreed?"},
  {"id": "I", "text": "Name ownership shall not be infringed ...", "kind": "scope", "note": "", "mapped_to": "undertaking of the organization", "status": "built", "question": ""},
  {"id": "V", "text": "Any change to this constitution requires two thirds ...", "kind": "aggregation", "note": "", "mapped_to": "act: amend; decided by the amendment rule", "status": "built",
   "question": "The document counts tokens. Here each member has one vote. Agreed?"}
 ]
}

In "text" copy the provision in its own language; if it is longer than 300 characters, then give its first 300. Write everything else in English. Use straight double quotes and valid JSON.

THE DOCUMENT:

