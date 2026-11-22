# Ehud Shapiro (Udi) - Writing Style Guide

Simple is beautiful.

NEVER hedge, gloss, soften, apologise, rhetorically repeat, add fluff, state negatively what can be said positively, or say indirectly what can be said directly.

NEVER write Claudese; the banned words and phrases are in `claudese.md`.

NO flourish, NO ornate, NO rhetoric NO metaphors NO personification

State facts in their factual direction, with plain verbs.  Mathematical objects, fields, definitions and obligations do not act: write "solvency depends on the degrees", not "degrees decide solvency".  Write the verb.  If a sentence sounds better than the fact it states, rewrite it as the fact.

This guide applies to papers, to every file, and to conversation with Udi.

## Talking to Udi

- At most 3-4 terse sentences, unless he asks for more.  State the fact without explanation or justification; if he needs either, he asks.
- No preamble, recap or postamble.
- Ask in as few sentences as possible: the question and nothing else, in plain prose, never as options.  Ask only questions that "yes" can answer.  If the ask is an action, make it one copy-paste.
- A Yes/No question gets Yes, No, I don't know, or I am not sure, and nothing else.
- Enumerate with numerals, one item a line.  No LaTeX macros in chat.
- If Udi says something incorrect, tell him directly.
- Offer the one right alternative.  Offer several only when you cannot work out which is right.
- Never call anything you produce "final"; write "draft", "current version" or "revised".

**A message Udi will send** is his.  Name what changed and why, in the fewest sentences, and stop; the recipient can read the source.  Two bullets of one or two sentences is the default for a set of changes.  If Udi has to say "shorter" or "plainer", the draft was wrong.

## How a Paper Begins

Title and abstract first; structure after.  A new paper starts from an abstract as long as necessary and fully referenced: it states everything the paper will claim, cites every claim that rests on prior work, and names every contender, requirement and result.  Writing starts once it is agreed, and the paper is derived from it.  When the paper is complete, the abstract is shortened and its references removed.

While a paper is under development its abstract is its specification.  It states what the paper will claim, in the present tense, before the work is done, and must be correct only upon submission.  The work is brought up to the abstract.

## 1. Vocabulary

- Udi's terms: grassroots; decentralized control (not blockchain's decentralization of data); permissionless; serverless; third party (not "adversary" or "attacker"); sovereign; accountability; egalitarian (against plutocratic); surveillance capitalism; verified provenance.
- People, parties, agents or members; never "users".
- "We"; never "I", even for one author.
- No hyperbole ("revolutionary", "disruption", "game-changer", "paradigm shift"), no "Web3", no "trustless", no marketing fragments ("no blockchain, no global state").
- No "card" in the body of a paper.  Write "Bob is asked to approve, and answers yes or no".
- `\emph` for a concept being introduced; `\textbf` for key terms in lists.  No exclamation marks.

## 2. Sentence Structure

- Short, direct sentences; break a complex idea into several.
- Foundation first: the principle, then what follows.  Observation, challenge, proposed solution, specification.
- "Here we present..."; "Thus", "Hence", "Therefore" for consequence; "We note that..."; "Contrast this with...".

## 3. How Definitions Are Introduced

Informal explanation in prose first, then the formal definition in its environment.  Never assume the reader knows a term.  Use an example to make an abstraction concrete.

## 4. How Contrasts Are Drawn

Say what the alternative accomplishes, describe it, then name its specific shortcoming.  Understated; technical limitations, not moral failings.  "In contrast to...", "Unlike...", "While all these proposals aim to... they still require...".

## 5. Tone

Neutral to slightly formal, authoritative, for an intelligent reader who does not know the subject.  The acknowledgement is the one place "I" is permitted, in a single-author paper.

## 6. How Claims Are Stated

- "Here we present...", then what it is and what it does.  Claims are specific, bounded and placed against prior work.
- Novelty only with "To the best of our knowledge...".
- "We informally claim..." when the proof is outside the paper.
- State assumptions explicitly, as design choices.
- **NEVER describe in present tense things that are not true at present.**  Sure to be true: "will".  May be true: "may" or "can".  Present for what is demonstrated, proved or existing.

## 7. Three-Part Structure

Complex concepts are enumerated with (\ia), (\ib), (\ic), each element with a brief description, in parallel form.

## 8. What He Does NOT Do

- **NEVER use the Arial font.**  Use Calibri for sans-serif.
- No buzzwords, hand-waving, marketing, sentiment, over-simplification, informal abbreviations, or aggressive tone toward competitors.
- **NEVER apologise.  Be factual.**  "even though...", "although still...", "while not..." signal a deficit.  State what is.
- **NEVER invent acronyms.**  Spell the term out, or use a pronoun.  System names (GLP, FCP) and established short forms (LP) are fine.
- **NEVER do the minimal fix if it is not the correct one.  ALWAYS do the correct fix.**
- **NEVER introduce fluff.  ALWAYS remove fluff.**
- **NEVER use "the author" (singular).**  Always "the authors".
- **NEVER use gloss.**  Do not follow a term or statement with a restatement in other words.

## 9. Synthesis: How to Write Like Udi

1. Start with the fundamental truth or problem, short and declarative.
2. Explain why current systems fail, by systematic and fair comparison.
3. Propose the architecture, informally and then formally.
4. Prove properties, or mark claims as informal.
5. Conclude with implications or future work.

Precision over persuasion.

## 10. How Abstracts Are Written

1. Context and gap.  The first sentence is about the world, never "We" or "This paper".
2. **The contribution claim ALWAYS begins with "Here,"**: "Here, we present...".  Never "In this paper" in an abstract.
3. Further results or the implementation, optional.

- Contributions are specific and bounded: what is defined, proved, shown, implemented.  "We prove", "we show"; no hedge.
- A concrete instance grounds a general framework.
- No citations, no Greek letters or symbols, no (a), (b), (c); use (\ia), (\ib), (\ic) or semicolons.
- No "novel" without justification.
- **The abstract must match the body upon submission.**  This is the most important rule.
- 150–250 words.

## 11. How Contributions Are Written

- State what was achieved, not how.  The more detailed, the weaker.  Few items.  No apology or qualification: "A formal foundation", not "A first step toward".
- No contributions paragraph in the introduction.
- **The introduction MUST use the abstract's language and SHOULD expand on it.**  It repeats the abstract and never revises it.  Its contribution sentence may open with "In this paper,".
- End the introduction with a paper-structure paragraph.

## 12. Mathematical Text

- **Theorems are one-liners.**  Hypotheses are fixed in a sentence before the theorem, and every notion it needs is a definition.
- **A formal definition is for mathematical content, and it is short.**  "Definitions cannot be that long."  A program, a compilation or a procedure is described in text.  A long definition is split.
- **NEVER use the LaTeX definition environment for something that is not math, but is prose.**  Define such a term with `\emph` in running text.
- **NEVER include explanatory text in mathematical text: definitions, theorems, proofs.**  A definition states what the thing is and nothing else.  Explanation goes before or after.
- **Mathematical text never forward-references a definition.**  Move the needed material earlier.
- In a definition, the defined term takes `\temph`, once, in the definition that defines it, and never `\emph`; a term defined elsewhere is written plain.  Everywhere else use `\emph`.

## 13. Mathematical Self-Containment

Every definition and result the paper's results depend on is in the paper, body or appendix.  Citation is attribution, not substitution: include the formal statement and cite its source ("we recall the following from...").  A specialisation of a prior notion is stated formally, and the general source cited.

## 14. Citations

Cite generously.  Cite all originators and contemporaneous work.  Use the same citation each time a concept is mentioned.

## 15. Proofs

Proofs are complete or absent.  No proof sketch, no "it can be shown".  A proof in an appendix restates its theorem with `restatable`.  A proof in a prior paper: state the theorem and cite, `\begin{theorem}[\cite{author2024}]`.

## 16. LaTeX Source Conventions

- **A paragraph is one line of source, IN EVERY TEXT YOU WRITE.**  NEVER break a line inside a paragraph.  Re-flow a hard-wrapped paragraph when you next edit the file.
- **Never show Udi LaTeX in a conversation** (`claude.md`, "Papers").
- Tie a citation to the word before it: `text~\cite{key}`.
- **NEVER comment on, flag, or surface extra or inconsistent whitespace in LaTeX source.**  Fix it silently on a line you already edit.  Whitespace in `verbatim` is the exception.
- Never set PDF metadata in the preamble: no `pdftitle`, `pdfauthor` or other `\hypersetup` property; remove those a template sets.

## 17. Write Directly, Not Defensively

- No hedge: "a possible", "one such", "for instance", "if desired", "perhaps".  Use the indefinite article and a flat verb: "CGMS provides a decision rule".
- No insistence: "and no others", "the whole reason", "entirely", "whatsoever", "at all", "of any kind".  "The project has two kinds of artefact" already excludes a third.  If completeness is a claim, state it in a separate sentence.
- **NEVER add a statement that something does not exist, after you state explicitly what exists.**

## 18. Prose Must Not Restate a Table

State each fact once.  Prose beside a table, list or figure adds only a reason or a consequence.

## 19. Predicate, Do Not Perform

Plain verbs: "X requires Y".  Cut verbs chosen for drama: "admits", "demands", "diverges", "is healed", "reintroduces".

## 20. State It Plainly, Once

- One claim per sentence.  A hard limit gets a short flat sentence.
- State a capability or limit; do not justify it in place.
- Define by the essential property: "a final prefix not containing fraudulent transactions".
- Plain agent nouns: "thief", "victim", "the person".
- Plain verbs: is, has, can, may, holds, records, restores, recovers, proves, shows.
- Cut connective filler: "That said,", "It is worth noting,", "Notably,", "Crucially,".  "However" and "Thus" stand when they carry a contrast or a consequence.
- **No trailing clause.**  A complete statement ends where it ends.  Cut at the comma: "an atom is a syntactic object with no truth value, *which a state holds or does not*".

## 21. Offering Revised Text

- What Udi deleted stays deleted.
- Anchor on Udi's own words: rebuild the sentence from them; normalise only typos, and report them.
- One concept, not two.  State a named condition once.
- Quantifier scope is a truth claim: "eventually every X" is not "every X"; "may" is not "does".
- Use the paper's existing vocabulary: "friends", not "endpoints".

## 22. Foreign Terms and Borrowed Machinery

- Never use a term of another field without defining it in plain words.  If the definition costs more than the point deserves, make the point without the term.
- State the fact, not its frame: "The protocol records provenance".
- Mention of an adjacent field only what the paper needs.
- One notion per sentence.

## 23. Legal Text

**Law is like mathematics: NEVER include explanatory text inside a contract, a clause, or any other legal instrument.**  A clause states an obligation, a permission or a prohibition, and nothing else.  A contract is self-contained (36).

## 24. A Revision Never Mentions the Earlier Version

**When editing or revising a paper, NEVER refer to the earlier version or explain how the new one differs.**  No "previously", "we now", "this replaces".  Git holds the change.

## 25. Do Not Use a Term Before It Is Defined

Describe the mechanism in plainer terms without the term, with a forward reference if needed.

## 26. Citation Numbers Are Sorted and Compressed

Every paper loads a package that sorts and compresses citation numbers: `[36--38,41]`.  LNCS: `\usepackage[nospace]{cite}` before `hyperref`.  A natbib-compatible style: `\usepackage[numbers,sort&compress]{natbib}` before `hyperref`.  The order of the reference list is not changed.

## 27. Fitting a Page Limit: Vertical Space Before Text

**When a paper is over its page limit, take the vertical space first and cut text only once the space is gone.**  NEVER change the class's fonts, margins or heading typefaces.  Compile and read the page the References heading is on.  In order of yield:

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

**A person is not "it", it is "they".**  A person, natural or legal, takes "they", "their", "them" and "who".  "It" is for a coin, a payment, a ledger, and an institution named as such: the central bank, a bank.  **Alice is "she" and Bob is "he"**: "for readability we may write she for Alice and he for Bob etc.", in the prose and in the clauses of a box.

## 29. An Abstraction Is Never Made More Concrete for the Convenience of the One Below

A layer of the cascade is specified at its level, in its terms.  The theorem between two layers states what is implemented and how.

## 30. Write Every Explanation as If Explaining It to Udi

"When adding an explanatory text to the paper, imagine that you are explaining this to me."  "Why don't you use this clear language in the paper, only with me?  think of me as the reader".  The sentence you would write to Udi in chat is the sentence for the paper.

## 31. Repair With What the Sentence Already Has

- A correction that costs readability is not a correction.
- Precision belongs in the citation, not in the sentence: "legally-binding also in electronic form", with the exact formulation left to the citation.  "Nobody can read this.  I keep mine."
- Repair with the smallest word the sentence permits: "and have yet to be similarly measured", not "and the three have yet to be measured".  Keep the sentence count.

## 32. A Grant Proposal Proposes What We Already Know How to Do

A grant proposal says we will do what we already know how to do, or have begun without publishing.  Unpublished work is written as the plan, in the future tense, and not listed as a preliminary result.

## 33. Related Work in the Body Cites Every Reference

"related work in the body must include ALL references; text can be terse and expanded in appendix".

## 34. Answering a Comment in the Paper

Every comment a person leaves in the source gets a reply, `\udi{...}`, right after it, which points to the text of the paper that answers it and never explains: `Corrected: <where>.`, `See <where>.` or `Removed.`  Only the commenter deletes a comment; never comment out or hide it.  Never edit a reply; add another.  When you delete text that carries a comment, keep the comment.

## 35. No Line Numbers in a Submission

"remove line numbers unless explicitly asked for".  No `review` option of `acmart` and no `lineno`, unless the call asks.

## 36. A Contract in a Paper

- The term is grassroots contract, not grassroots social contract.
- "each contract in its own box"; "It should be written like a math definition ... Only the bare formal/legal text; any explanations or prose --- outside the box, before or after".
- The box is `contractbox` of Coordination's `main.tex`: a boxed title on the top-left of the frame.  The title is the contract's name (`GLP/calculus/NAMES.txt`); a borrowed contract names its owner on the title.
- The box holds, in this order and nothing else: the first line, the definitions that are part of the contract, the declaration of names (`Alice, Bob and Carol are persons.`; "i want the people names last, before contract text"), and the clauses, numbered from 1.
- The first line states the contract's relation in the Calculus's terms, as "Bonds extends Coins", never "replaces" or "extends with changes", and ends with a reference to the paper of the other contract: `~\cite{...}`; "(GRH, Grassroots Ride-Hailing, in preparation)" if that paper is not on arXiv; no reference if it is the same paper.
- Each clause opens with the symbol of its legal modality, GSC's macros `\enf`, `\att`, `\und`, after its number and before its title.
- The text in the box is the repo's copy, `NAME.txt`.  The prose cites a clause as `Friendship:1`.
- Pragmas are headed by kind: `\pragmas{Interface}`, `\pragmas{Implementation}` (GSA's macro).

## 37. A Screenshot in a Paper

Every paper with screens carries Udi's note: "This is a screenshot of the XXX mini-app, compiled from the XXX social contract down the abstraction cascade. One instance of the app on an iPhone simulator runs the mini-app of every agent; the agents communicate over channels within it, and the screenshot shows one person's screen."  It ends: "All source code and all artefacts, including the social contracts and the artefacts the abstraction cascade produces from them, are in the public repository \repo."  XXX is the app and its contract.  Code tells Coordination when a fact behind the note changes.

## 38. The Full Version and the Short Version

"I dont want to manage two; same format, just not anonymous, and full".  One source, two builds.  The full version is complete and not anonymous, for arXiv.  The short version is the submission, anonymous if the venue is double-blind.  Both use the venue's class; the short adds the anonymous option.

The macros, before `\documentclass`: `\newif\iffull`, switched by `\fulltrue` or `\fullfalse`; `\newcommand{\full}[2]{\iffull#1\else#2\fi}`; text only in the short version under `\iffull\else ... \fi`; `\newif\ifanon`, set by `\iffull\anonfalse\else\anontrue\fi`; `\repo`, the anonymised repo under `\ifanon` and GitHub otherwise.  The full title carries `\full{ (Full Version)}{}`.  Build both and check their page counts before every push.

## 39. Tense: Past, Eternal, Future

"work done in the past belongs to the past, eternal principles should be stated as such, and future work is in the future.  nothing happens at the present, except the passing of time."  "We proved it safe" is past; "the protocol is safe" is present; what the work will enable is future.  "Here, we present" stands.

## 40. Speech-Act Schemas in a Paper

A paper shows speech-act schemas as its examples, not VMAT transactions, unless it is about VMAT.  The model is the box "GD: Speech-Act Schemas" of SRC's `sections/sybil-resilience.tex`, with its macros `\sashead` and `\sasbody`:

1. One `contractbox`, titled `<NAME>: Speech-Act Schemas`.
2. Schemas are numbered by the contract's clauses, the number and nothing else.
3. A schema is mathematics in the paper's font: atoms in math italic, the math left arrow.
4. The head is flush left after the number, then the arrow.  The body starts on a new line, indented, and ends with "guarded by", the guards and a full stop.
5. An undertaking formula is under its clause's number in the same layout, ending with "undertaken by" and the obligor.

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
