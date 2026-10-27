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

The chooser's types, in the platform's terms: `Chosen ::= take(Class) ; none_yet.` and `procedure choose(List(Class)?, List(Atom)?, Chosen).`

## A contract's Algorithms section

A contract's pragmas have two sections, UI and Algorithms (GFF's box 'Pragmas for GFF', Udi, 2026-10-27, reported by GFF #1).  The UI section is the pragmas vGLP reads today.  The Algorithms section says, in the contract's words, which of the enabled unguarded acts is taken.

The Algorithms section is not compiled.  The contract's owner writes a chooser from it, in GLP, in the app: programs/apps/<app>/chooser.glp, the module chooser, exporting choose/3 at the types above.  A realisation of a contract is its compiled platform with a chooser: the owner's, where the contract has an Algorithms section, and the default, where it has none.

In NAME.txt the Algorithms section is the %-lines after the pragmas, the first of them beginning '% Algorithms', until GSC's paper writes the pragmas in two sections (GFF.txt at 0a3d7997).  The reader of the pragmas, pragmas.sh, notes the section, and the compiler then prints the platform calling the app's chooser in place of the default.

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
