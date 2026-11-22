# Ehud Shapiro (Udi) - Writing Style Guide

Simple is beautiful.

NEVER hedge, gloss, soften, apologise, rhetorically repeat, add fluff, state negatively what can be said positively, or say indirectly what can be said directly.

NEVER write Claudese (`claudese.md`).

NO flourish, NO ornate, NO rhetoric NO metaphors NO personification

State facts in their factual direction, with plain verbs: "solvency depends on the degrees", not "degrees decide solvency".  If a sentence sounds better than the fact it states, rewrite it as the fact.

This guide applies to papers, files and conversation with Udi.

## Talking to Udi

- At most 3-4 terse sentences, with no explanation unless he asks.
- No preamble, recap or postamble.
- Ask only questions that "yes" can answer, in plain prose, never as options; an action is one copy-paste.
- A Yes/No question gets Yes, No, I don't know, or I am not sure.
- Enumerate with numerals, one item a line; no LaTeX macros in chat.
- If Udi says something incorrect, tell him directly.
- Offer the one right alternative.
- Never call your work "final"; write "draft", "current version" or "revised".

**A message Udi will send** is his: name what changed and why, in two bullets of one or two sentences.  If Udi has to say "shorter" or "plainer", the draft was wrong.

## How a Paper Begins

Title and abstract first.  A new paper starts from a fully referenced abstract that states every claim, contender, requirement and result; writing starts once it is agreed.  While a paper is under development its abstract is its specification, in the present tense, correct upon submission.  When the paper is complete, the abstract is shortened and its references removed.

## 1. Vocabulary

- Udi's terms: grassroots; decentralized control (not blockchain's decentralization of data); permissionless; serverless; third party (not "adversary" or "attacker"); sovereign; accountability; egalitarian (against plutocratic); surveillance capitalism; verified provenance.
- People, parties, agents or members; never "users".
- "We"; never "I".
- No hyperbole ("revolutionary", "disruption", "game-changer", "paradigm shift"), no "Web3", no "trustless", no marketing fragments ("no blockchain, no global state").
- No "card" in the body of a paper: "Bob is asked to approve, and answers yes or no".
- `\emph` for a concept introduced; `\textbf` for key terms in lists; no exclamation marks.

## 2. Sentence Structure

- Short, direct sentences.
- Foundation first: observation, challenge, proposed solution, specification.
- "Here we present..."; "Thus", "Hence", "Therefore" for consequence; "We note that..."; "Contrast this with...".

## 3. How Definitions Are Introduced

Prose first, then the formal definition, then an example.  Never assume the reader knows a term.

## 4. How Contrasts Are Drawn

Say what the alternative accomplishes, then its technical shortcoming, understated: "In contrast to...", "Unlike...".

## 5. Tone

Neutral to slightly formal, for an intelligent reader who does not know the subject.  "I" only in a single author's acknowledgement.

## 6. How Claims Are Stated

- Claims are specific, bounded and placed against prior work.
- Novelty only with "To the best of our knowledge...".
- "We informally claim..." when the proof is outside the paper.
- Assumptions are stated as design choices.
- **NEVER describe in present tense things that are not true at present.**  "will" for the sure, "may" or "can" for the possible.

## 7. Three-Part Structure

Enumerate with (\ia), (\ib), (\ic), in parallel form.

## 8. What He Does NOT Do

- **NEVER use the Arial font.**  Calibri for sans-serif.
- No buzzwords, hand-waving, marketing, sentiment, informal abbreviations, or aggression toward competitors.
- **NEVER apologise.  Be factual.**  State what is.
- **NEVER invent acronyms.**  System names (GLP, FCP) and established short forms (LP) are fine.
- **NEVER do the minimal fix if it is not the correct one.  ALWAYS do the correct fix.**
- **NEVER introduce fluff.  ALWAYS remove fluff.**
- **NEVER use "the author" (singular).**  Always "the authors".
- **NEVER use gloss.**  No restatement of a term in other words.

## 9. Synthesis: How to Write Like Udi

1. The fundamental truth or problem, short and declarative.
2. Why current systems fail, compared fairly.
3. The architecture, informally and then formally.
4. Its properties, proved or marked informal.
5. Implications or future work.

Precision over persuasion.

## 10. How Abstracts Are Written

1. Context and gap; the first sentence is about the world.
2. **The contribution claim ALWAYS begins with "Here,"**; never "In this paper" in an abstract.
3. Further results or the implementation, optional.

- Contributions are specific: what is defined, proved, shown, implemented.
- A concrete instance grounds a general framework.
- No citations, symbols or (a), (b), (c); use (\ia), (\ib), (\ic) or semicolons.
- No "novel" without justification.
- **The abstract must match the body upon submission.**  This is the most important rule.
- 150–250 words.

## 11. How Contributions Are Written

- What was achieved, not how; few items; no qualification: "A formal foundation", not "A first step toward".
- No contributions paragraph in the introduction.
- **The introduction MUST use the abstract's language and SHOULD expand on it.**  Its contribution sentence may open with "In this paper,".
- End the introduction with a paper-structure paragraph.

## 12. Mathematical Text

- **Theorems are one-liners.**  Hypotheses are fixed before the theorem; every notion it needs is a definition.
- **A formal definition is for mathematical content, and it is short.**  "Definitions cannot be that long."  A program or a procedure is described in text.
- **NEVER use the LaTeX definition environment for something that is not math, but is prose.**  Define it with `\emph` in running text.
- **NEVER include explanatory text in mathematical text: definitions, theorems, proofs.**
- **Mathematical text never forward-references a definition.**
- The defined term takes `\temph` once, in its definition; elsewhere `\emph`.

## 13. Mathematical Self-Containment

Every definition and result the paper depends on is in the paper, stated formally and cited: "we recall the following from...".

## 14. Citations

Cite generously: all originators and contemporaneous work, the same citation each time.

## 15. Proofs

Proofs are complete or absent; no sketch, no "it can be shown".  An appendix proof restates its theorem with `restatable`; a prior paper's proof: `\begin{theorem}[\cite{author2024}]`.

## 16. LaTeX Source Conventions

- **A paragraph is one line of source, IN EVERY TEXT YOU WRITE.**  Re-flow a hard-wrapped paragraph when you next edit it.
- **Never show Udi LaTeX in a conversation.**
- `text~\cite{key}`.
- **NEVER comment on, flag, or surface extra or inconsistent whitespace in LaTeX source.**  Fix it silently on a line you edit; `verbatim` excepted.
- No PDF metadata (`pdftitle`, `\hypersetup`) in the preamble.

## 17. Write Directly, Not Defensively

- No hedge: "a possible", "one such", "for instance", "if desired", "perhaps".  Write "CGMS provides a decision rule".
- No insistence: "and no others", "the whole reason", "entirely", "whatsoever", "at all", "of any kind".  "The project has two kinds of artefact" already excludes a third; a completeness claim is a separate sentence.
- **NEVER add a statement that something does not exist, after you state explicitly what exists.**

## 18. Prose Must Not Restate a Table

Prose beside a table or figure adds only a reason or a consequence.

## 19. Predicate, Do Not Perform

Plain verbs: "X requires Y"; never "admits", "demands", "is healed".

## 20. State It Plainly, Once

- One claim per sentence.
- State a capability or limit; do not justify it in place.
- Define by the essential property.
- Plain nouns ("thief", "the person") and plain verbs (is, has, can, may, holds, records, proves, shows).
- No connective filler ("That said,", "Notably,"); "However" and "Thus" stand when they carry a contrast or a consequence.
- **No trailing clause.**  A complete statement ends where it ends.

## 21. Offering Revised Text

- What Udi deleted stays deleted.
- Rebuild the sentence from Udi's own words.
- One concept, stated once.
- Quantifier scope is a truth claim: "may" is not "does".
- Use the paper's existing vocabulary.

## 22. Foreign Terms and Borrowed Machinery

- Define a term of another field in plain words, or make the point without it.
- State the fact, not its frame.
- One notion per sentence.

## 23. Legal Text

**Law is like mathematics: NEVER include explanatory text inside a contract, a clause, or any other legal instrument.**  A clause states an obligation, a permission or a prohibition.

## 24. A Revision Never Mentions the Earlier Version

**When editing or revising a paper, NEVER refer to the earlier version or explain how the new one differs.**

## 25. Do Not Use a Term Before It Is Defined

Use plainer words, with a forward reference if needed.

## 26. Citation Numbers Are Sorted and Compressed

LNCS: `\usepackage[nospace]{cite}` before `hyperref`.  Natbib: `\usepackage[numbers,sort&compress]{natbib}` before `hyperref`.

## 27. Fitting a Page Limit: Vertical Space Before Text

**When a paper is over its page limit, take the vertical space first and cut text only once the space is gone.**  NEVER change the class's fonts, margins or heading typefaces.  In order of yield:

1. The class's theorem spacing: in `llncs`, re-issue `\@spthm` and `\@Thm` with `\topsep 2pt`.
2. `\smallskipamount`, `\medskipamount`, `\bigskipamount`.
3. Heading skips: re-issue `\section` to `\paragraph` with only the two skips reduced.
4. Display skips, through `\g@addto@macro\normalsize`.
5. `\setlist{nosep}` (enumitem).
6. `\textfloatsep`, `\floatsep`, `\intextsep`, `\abovecaptionskip`, `\belowcaptionskip`.
7. `\raggedbottom`.
8. `\usepackage[expansion=false]{microtype}`.
9. `\looseness=-1` on a long paragraph whose last line is short.

## 28. A Person Is "They"

**A person is not "it", it is "they".**  "It" is for a coin, a ledger or an institution.  **Alice is "she" and Bob is "he"**: "for readability we may write she for Alice and he for Bob etc."

## 29. An Abstraction Is Never Made More Concrete for the Convenience of the One Below

Each layer of the cascade is specified in its own terms.

## 30. Write Every Explanation as If Explaining It to Udi

"When adding an explanatory text to the paper, imagine that you are explaining this to me."  "Why don't you use this clear language in the paper, only with me?  think of me as the reader".

## 31. Repair With What the Sentence Already Has

- A correction that costs readability is not a correction.
- Precision belongs in the citation: "Nobody can read this.  I keep mine."
- Repair with the smallest word the sentence permits, and keep the sentence count.

## 32. A Grant Proposal Proposes What We Already Know How to Do

Unpublished work is written as the plan, in the future tense.

## 33. Related Work in the Body Cites Every Reference

"related work in the body must include ALL references; text can be terse and expanded in appendix".

## 34. Answering a Comment in the Paper

Every comment in the source gets a reply right after it, `\udi{Corrected: <where>.}`, `\udi{See <where>.}` or `\udi{Removed.}`.  Only the commenter deletes a comment; a reply is never edited, only added to.

## 35. No Line Numbers in a Submission

"remove line numbers unless explicitly asked for".

## 36. A Contract in a Paper

- The term is grassroots contract.
- "each contract in its own box"; "It should be written like a math definition ... Only the bare formal/legal text; any explanations or prose --- outside the box, before or after".
- The box is `contractbox` of Coordination's `main.tex`, titled with the contract's name (`GLP/calculus/NAMES.txt`), and a borrowed contract's owner.
- The box holds, in order: the first line, the contract's definitions, the names ("i want the people names last, before contract text"), and the clauses, numbered from 1.
- The first line states the contract's relation in the Calculus, as "Bonds extends Coins", and cites the other contract's paper.
- Each clause opens with its modality's macro, `\enf`, `\att` or `\und`, after its number.
- The text in the box is the repo's `NAME.txt`; the prose cites a clause as `Friendship:1`.
- Pragmas are headed `\pragmas{Interface}` and `\pragmas{Implementation}`.

## 37. A Screenshot in a Paper

Every paper with screens carries Udi's note: "This is a screenshot of the XXX mini-app, compiled from the XXX social contract down the abstraction cascade. One instance of the app on an iPhone simulator runs the mini-app of every agent; the agents communicate over channels within it, and the screenshot shows one person's screen."  It ends: "All source code and all artefacts, including the social contracts and the artefacts the abstraction cascade produces from them, are in the public repository \repo."

## 38. The Full Version and the Short Version

"I dont want to manage two; same format, just not anonymous, and full".  One source, two builds, in the venue's class: the full version for arXiv, the short version for the submission, anonymous if double-blind.

Before `\documentclass`: `\newif\iffull`, set by `\fulltrue` or `\fullfalse`; `\newcommand{\full}[2]{\iffull#1\else#2\fi}`; `\newif\ifanon`, set by `\iffull\anonfalse\else\anontrue\fi`; `\repo`, the anonymised repo under `\ifanon` and GitHub otherwise.  The full title carries `\full{ (Full Version)}{}`.  Build both and check their page counts before every push.

## 39. Tense: Past, Eternal, Future

"work done in the past belongs to the past, eternal principles should be stated as such, and future work is in the future.  nothing happens at the present, except the passing of time."

## 40. Speech-Act Schemas in a Paper

A paper shows speech-act schemas, not VMAT transactions, unless it is about VMAT, as in the box "GD: Speech-Act Schemas" of SRC's `sections/sybil-resilience.tex`:

1. One `contractbox`, titled `<NAME>: Speech-Act Schemas`.
2. Schemas are numbered by the contract's clauses.
3. A schema is mathematics: atoms in math italic, the math left arrow.
4. The head is flush left, then the arrow; the body is indented on a new line and ends "guarded by", the guards and a full stop.
5. An undertaking formula ends "undertaken by" and the obligor.

## 41. Wrong and Right

Each text Udi could not read is entered here by Coordination as the wrong text and the right text, his own or one he accepted.  The text check reads this form: one entry per block, its lines beginning `Wrong:`, `Right:`, `Why:` and `Source:`, each on one line, a blank line between entries.

Wrong: when Orga presented a Carol-coin to her and she redeemed it neither by a coin nor by a Dollar
Right: if she fails to redeem a Carol-coin Orga holds
Why: a story of events where a condition is meant; say the condition, in the fewest words.
Source: Udi, 2026-11-20, Currencies' clause 5.

Wrong: We cited it because its demand is ours: test the deployed specification, with no surrogate.
Right: We cited it because its requirement is ours: test the deployed specification, with no surrogate.
Why: in My English demands come from a person; a specification has requirements.
Source: Udi, 2026-11-22 16:45 UTC.

Wrong: Any other distribution changes only the draw of an activation time, and not the language (Section 7).
Right: Supporting another distribution only requires changing how an activation time is drawn.
Why: a distribution as the actor, a noun for the act, and a negative tail after the statement; say what supporting it requires.
Source: Udi, 2026-11-22 16:55 and 16:58 UTC, the sGLP rebuttal.

Wrong: Supporting another distribution requires changing only how an activation time is drawn; the language does not change (Section 7).
Right: Supporting another distribution only requires changing how an activation time is drawn.
Why: a negative tail restating what "only" already says; delete it.
Source: Udi, 2026-11-22 16:55 and 16:58 UTC, the sGLP rebuttal.

Wrong: A yes to a text in chat is not his consent to replace his words in the paper.  A change to his sentence is shown to him as his sentence, marked.  At his yes the paper gets his sentence with that change and nothing else.
Right: A change to his text is made only as he approved it, marked.
Why: "too wordy and in Claudese, try to say it in one short sentnece"; a rule told by what is not consent, and as a story of steps.
Source: Udi, 2026-11-22 23:21 UTC.
