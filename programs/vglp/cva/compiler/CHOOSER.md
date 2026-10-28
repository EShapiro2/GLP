# The chooser: what vGLP's paper would say, for the experiment GFF-chooser

This note states what vGLP's paper would have to say for the experiment of the branch GFF-chooser, written as if the paper said it.  The experiment was approved by Udi on 2026-10-27 (reported by GFF #1 Cowork, Code_inbox.md, 2026-10-27 20:25 UTC, and by Code #16).  It is carried out on the branch GFF-chooser only, off GLP main 0a3d7997; vGLP's paper, /Users/udi/Grassroots/vGLP at acd3e74, is not changed, and the branch is not merged until Udi, GFF and vGLP decide from it how to proceed.  Written by a subagent of Code #16, 2026-10-27 20:41 UTC.

## Where the compiled agent takes the unguarded parts today

The canonical platform does not say which enabled transaction is taken first.  An opener proposes an unguarded part "whenever the opener can carry out its effect" (Section 3.2, "The protocol"), and where several are enabled at once any of them may be taken.

The compilation takes them as the agent's own transactions.  Section 5.4, "Triggers": the compiled agent takes "Propose, Vote, Commit, Wound and Introduce peers, its own transactions, one at a time, each on an own trigger that the agent writes on a stream merged fairly with its other triggers, when it starts, after another trigger and after each own transaction it takes, until none holds".

Section 5.4, "The compilation": "On an own trigger it puts up the Propose of each unguarded part the agent opens, ... the Vote on each answered question pending at it, the Commit of each attempt it initiated with a yes of every other participant recorded, and the Wound of each start it holds waiting, or answered question, blocked by an attempt over which it has priority, and the Introduce peers of each pair the introduction rule gives it at its machine state; the dispatch takes, among those whose condition holds, one of the class that has waited longest --- since it was last taken or, never taken, since its condition was first found holding --- ties broken in a fixed order, and counts it; where none holds, it takes none."

Section 5.4, after Theorem "Compilation of the Canonical Platform": "Completeness is not claimed: the compilation takes the agent's own transactions as soon as their conditions hold, in a fixed order, which P does not require."

So today the compiled agent takes, among the enabled unguarded parts, the one that has waited longest, and no contract can say otherwise.

## The seam

On an own trigger the agent puts up the Propose of each unguarded part it opens whose condition holds, as today, and orders these parts by how long each has waited, the longest first, ties in today's fixed order.  It calls one procedure, the chooser, with these parts, each by its class term, and with its machine state, the list of atoms its view shows.  The chooser returns one of the parts, take(P), or none_yet.

The dispatch then takes, among the part the chooser returned and the agent's other own transactions whose condition holds --- the Proposes of guarded parts, the Votes, the Commits, the Wounds and the Introduce peers --- one of the class that has waited longest, as today.  On none_yet it takes one of the others, or none.  Where it takes none it writes no own trigger, and the agent calls the chooser again after its next trigger, as today.

The default chooser returns the first part, and none_yet where there is none.  The parts come to it longest-waited first, so an agent with the default chooser takes the own transaction it takes today.

A chooser restricts which enabled Propose of an unguarded part is taken, and when.  Every transaction the agent takes is a transaction of the canonical platform whose condition holds.

The chooser sees the state at each call and keeps nothing between calls, so it cannot tell from the state whether an act that changes nothing at its opener was taken since the state last changed.  The seam therefore also gives it the every_so_often parts the agent has committed as their initiator, newest first, each once, a part committed again moving to the front; the agent keeps them in its index and notes a part when its Commit takes it (text.glp, note_often).  A part is every_so_often where a role of its schema carries the mark every_so_often (below, "For GSC"); it is put up to the chooser as an unguarded part is, and taken only when the chooser returns it.  The default chooser takes it as any other part, the first.  A part of one participant, applied at its Propose, is not noted.

The chooser's types, in the platform's terms: `Chosen ::= take(Class) ; none_yet.` and `procedure choose(List(Class)?, List(Atom)?, List(Class)?, Chosen).`, the third argument the every_so_often parts committed.

## A contract's Algorithms section

A contract's pragmas have two sections, UI and Algorithms (GFF's box 'Pragmas for GFF', Udi, 2026-10-27, reported by GFF #1).  The UI section is the pragmas vGLP reads today.  The Algorithms section says, in the contract's words, which of the enabled unguarded acts is taken.

The Algorithms section is not compiled.  The contract's owner writes a chooser from it, in GLP, in the app: programs/apps/<app>/chooser.glp, the module chooser, exporting choose/3 at the types above.  A realisation of a contract is its compiled platform with a chooser: the owner's, where the contract has an Algorithms section, and the default, where it has none.

In NAME.txt the Algorithms section is the %-lines after the pragmas, the first of them beginning '% Algorithms', until GSC's paper writes the pragmas in two sections (GFF.txt at 0a3d7997).  The reader of the pragmas, pragmas.sh, notes the section, and the compiler then prints the platform calling the app's chooser in place of the default.  A contract extending one with an Algorithms section takes the section, as it takes the clauses it extends: GFF1, which extends GFF, is compiled calling its app's chooser.

## What changes in vGLP's paper

1. Section 3.2, "The protocol", gains one sentence: which of the unguarded parts an opener can propose it proposes first is the choice of the realisation, made by its chooser.

2. Section 3.2, the paragraph after Definition "Strongly Fair, Fairness": the compiled agent secures strong fairness with a fair chooser.  A chooser is fair if every part put up to it in infinitely many own triggers is returned in infinitely many of them, or infinitely many attempts of its class are decided.  The default chooser is fair; whether another chooser is fair is for its owner to show.

3. Section 5.4, "Triggers" and "The compilation": the sentence on the own transactions puts up the Proposes of the unguarded parts through the chooser, as in "The seam" above.

4. Section 5.4, Theorem "Compilation of the Canonical Platform": it holds for every fair chooser; the remark after it reads "in an order the chooser and the waiting fix".

5. Section 5.4, "Speech-act schemas": a contract carries, beside its UI pragmas, an Algorithms section, from which its owner writes the app's chooser.

## What does not change

1. Definition "Canonical CVA Platform" and its transactions, the protocol's two-phase commit, Theorem "CVA Implementation" and its fairness condition.

2. The speech-act schemas, GSC's checker, and the classes and parts the compiler computes from the schemas.

3. The reading of the UI pragmas, the widget declarations, the view, and the drawing (Section 6.2, "Widgets"; the construct family).

4. The canonical compilation of vGLP into GLP (Section 6.3).

## For GSC

GFF marks a role whose act recurs, "Every so often, Carol updates their friend Alice" (GFF:5; Udi, 2026-10-28, reported by GFF #1 Cowork): the role's guard kind is every_so_often in place of unguarded.  On the branch GFF-chooser, GSC's checker and printer take the mark as follows, for GSC to put in its paper or to refuse.

1. programs/gsc/self.glp line 106: `Guard ::= guarded ; unguarded ; every_so_often.`; line 1130, `guard_marked(every_so_often, no)`: the mark is no guard; lines 1132 to 1145, `often_marked/2`, `is_often/2` and `n_is_often/2`; line 306, the fault `listed_often(Schema, Role)`.

2. programs/gsc/check.glp lines 224 to 236: a role marked every_so_often may add and delete nothing, as a guarding role may, so changes_nothing is not raised for it (`role_inert`, `either/3`); line 441: a listed role carrying the mark is the fault listed_often, as a listed role carrying ? is listed_guarded.

3. programs/gsc/compile.glp line 1068: the line printed for listed_often.

4. programs/gsc/notation.glp lines 268 to 298: a schema a role of which carries the mark is printed with "every so often" before its head, as `every so often update(Carol, Alice; b) :`.

vGLP's compiler on the branch takes the mark as no guard (self.glp lines 32, 187 and 191; cvac.glp line 1907; text.glp lines 74, 3508 and 3825), refuses a contract with listed_often as it does one with listed_guarded (cvac.glp line 152), and keeps the every_so_often parts committed for the chooser (text.glp lines 166, 1299 to 1300 and 2212 to 2242).  cascade.sh counts a head marked every so often among the schemas.

GFF also marks a schema's second role received (Udi, 2026-10-28, reported by GFF #1 Cowork): a delivery is a sending, one act of the clause and one schema, whose second role requires nothing but identity and only adds.  On the branch GSC's checker and printer take it as follows.

5. programs/gsc/self.glp line 107: `Guard ::= guarded ; unguarded ; every_so_often ; received.`, the mark no guard (`guard_marked(received, no)`), so the three conditions of def:syntactically-grassroots take a received role as unguarded; `received_marked/2` and `n_is_received/2`; line 308, the fault `received_form(Schema, Role)`.

6. programs/gsc/check.glp lines 247 to 266: a role marked received is the second role, requires `identity` alone, deletes nothing and forbids nothing, and otherwise is the fault received_form (`received_fault/5`); line 461, a listed role carrying the mark is that fault too.

7. programs/gsc/notation.glp lines 170 to 182: the role is printed `received` before its name, as `deliver_own(Bob, received Alice; x) :`.

8. programs/gsc/compile.glp line 1138: the line printed for received_form.

## For VMAT

VMAT gains CVA's send and receive, lifted, CVA being a special case of VMAT (Udi, 2026-10-28, reported by GFF #1 Cowork).  A sending, a schema of two roles whose second carries the mark received, compiles to two transactions, each guarded by none (programs/gsc/compile.glp lines 949 to 1020, `latex_sending`):

1. The sender's, at Alice: the assignment of the first role, its proviso and also that Alice knows Bob, `\mathit{knows}(\Bob) \in c_{\Alice}`, and the line "sends ⟨x, ...⟩ to Bob", the act being the binding of the schema's variables.

2. The recipient's, at Bob: "on ⟨x, ...⟩ received from Alice", and the assignment of the second role, which only adds.

A recipient that is down holds no sender: the sender's transaction is unary.

## For vGLP

A received role is realised by CVA's send and a receive, with no two-phase commit (vGLP, Section 3.1, Definition "App Transaction, Effect": an app transaction adds messages to known peers to its outbox and deletes the messages it has read from its inbox; Definition "CVA Platform", Communicate).  On the branch:

1. The class of a sending is the sender's alone: its Propose, by the opener, the sender, applies the sender's effect at once, as for a class of one participant, and sends the recipient the cargo `given(C)`, C the class term (text.glp lines 1972 to 2015, `propose_app`, `sending_class`, `given_to`); Propose's condition still requires that the sender know every other participant, here the recipient.

2. The recipient takes the arriving `given(C)` in, where its state is in the domain of its effect in the part, and applies the effect, CVA's receive; otherwise it drops it (text.glp line 930, `message_cond(given(_), ...)`; line 1112, `dispatch_message(mgiven, ...)`; `receive_effect`).

3. `Cargo` gains `given(Class)` (text.glp line 40); the mark is no guard everywhere the compiler reads a guard (compiler self.glp lines 33, 190 and 195; cvac.glp line 1909; text.glp line 75, `guard_roles`, `spec_unguarded`), and received_form refuses a contract (cvac.glp line 153).

## For vGLP, 6.2

What Section 6.2 would say: a tab's pragma line is what the tab lists, up to its first semicolon; after it, the line may say what a row shows under its entry.  A row shows under its entry the atoms of the state naming the entry by other predicates, each by its other arguments; an item such an atom names is shown once, however many atoms name it, and under it the signatures of each of its paths, the signers of each speech act an atom of the state pairs with it, as `path(x, y)`.  GFF's Feeds line, "the agents Alice follows; under an agent followed, the friend Alice follows them through, the friends who follow them, and the items of their feed, each once, the signatures of each of its paths under it", so lists `follows`, and a row `follows(Bob, Carol)` shows Carol, each `friend_follows(D, Bob)` by D, and each item of Bob's feed, `item(x, Bob)`, once, with under it each `path(x, y)` by the signers of y.  `from(x, c)` names x and no entry, so no row shows it.  Posts lists `feed`, Alice's own items, as before.

On the branch: programs/vglp/cva/compiler/pragmas.sh reads a tab's predicate from the part of its line before the first semicolon (`$listed`, line 284); glp_multiagent/lib/ui_runtime/named_widgets.dart, `_wordedRow` (lines 2460 to 2485) and `_pathsOf` (line 2528), draw an item once with its paths under it.  A row is still one atom of the tab's predicate, so an agent followed through two friends has two rows, each showing the same items.

