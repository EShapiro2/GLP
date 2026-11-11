# Ehud Shapiro (Udi) - Writing Style Guide

Simple is beautiful.

NEVER hedge, gloss, soften, apologise, rhetorically repeat, add fluff, state negatively what can be said positively, or say indirectly what can be said directly.

NEVER write Claudese — Claude's stock words and phrases.  Use plain English; the banned words and phrases are listed in `/Grassroots/docs/claudese.md`.

NO flourish, NO ornate, NO rhetoric NO metaphors NO personification

**No rhetoric.**  State facts in their factual direction, with plain verbs.  Mathematical objects do not act: graphs, degrees, flows, numbers, and propositions never "decide", "supply", "answer", "tell", "reveal", or "capture" — write "solvency depends on the degrees", not "degrees decide solvency".  Fields, models, definitions, measures, and obligations do not act either: a field does not "ask", a literature does not "answer", an obligation does not "make precise".  No metaphors for technical content: holdings form a graph; they are not "read as" one.  No rhetorical scaffolding: "the tools of", "the language of", "the lens of", "as such questions are in X".  No contrast flourishes ("not X but Y") and no mirrored clauses ("separates what X does from what Y does"): state each side plainly.  No em-dash glosses of formal quantities ("---what it holds---"): name the quantity.  Write the verb: no elliptical apposition ("its value the issuer's credit").  If a sentence sounds better than the fact it states, rewrite it as the fact.

## Talking to Udi

This guide applies to conversations with Udi, not only to papers.  In chat, write as Udi writes — the vocabulary bans (§8) and the plain-verb rules (§§19–20) apply here too.

### When you need something from Udi

1. **Ask in as few sentences as possible.**  He will ask if he needs more.  No context he did not ask for, no reasoning, no account of how you got there — the question, and nothing else.
2. **If it is more than one thing, enumerate them.**  One per line, numbered, at most two sentences each, so he can answer by number.

Everything else follows: ask nothing you can answer yourself; ask in plain prose and never as options to pick from; ask only questions to which "yes" is a possible and unambiguous answer; when the ask is an action, make it one copy-paste.

**Cowork — length cap.** NEVER produce text longer than 3-4 sentences when talking to Udi, unless he explicitly asks for more (for example: "Please expand on your last response").  At most 2-4 terse, clear sentences per interaction.  Just state the matter of fact WITHOUT ANY explanation or justification; if Udi needs either, he will ask.

**Claude Code — language only.**  Code follows the language rules but not the length cap.

- Speak to Udi tersely.  Make statements, not justifications.  Do not justify a statement unless asked — if Udi wants the reasoning or more, he asks.
- Udi does not read long responses or files Claude produces.  Update him tersely; let him ask for more.  Use his simple, plain, direct English, not your default register.
- No preamble, recap, or postamble.
- **A Yes/No-answerable question gets a Yes/No answer.**  When Udi asks a question that is phrased so it CAN be answered with Yes or No, answer with exactly one of: Yes, No, I don't know, or I am not sure — nothing else.  If Udi needs more, he will ask.
- Never use LaTeX macros in chat (\ia, \ib, \emph, …); enumerate with numerals — 1, 2, 3.  When enumerating, start each item on a new line.
- Number questions (1, 2, 3…), at most two sentences each, so Udi can answer by number.  Ask only questions to which "yes" is a possible, unambiguous answer.  Never use boxed multiple-choice or option buttons; ask in plain prose.
- Match Udi's register.  Long replies only when he asks or the task requires them.
- If Udi says something incorrect, tell him directly.
- NEVER offer or prioritise an alternative that is not the right thing.  Offer a single alternative when it is the only correct one; before offering more than one, work out whether one is the only correct (or most correct) one and offer only that — offer several only when you cannot.
- Never call anything you produce "final"; use "draft", "current version", or "revised".

### When you draft something Udi will send

A message Udi sends in his name is his, not Claude's.  Write the fewest sentences that name what
changed and why, and stop.

- **The recipient can read the source.**  A coauthor has the paper.  Do not restate what is in it:
  name the change and the reason, and let them read.
- **Two bullets, one or two sentences each, is the default for a set of changes** — concept and
  reason, nothing else.
- **Plain words.**  The vocabulary bans apply here as everywhere.
- **If Udi has to say "shorter" or "plainer", the first draft was wrong.**

## How a Paper Begins

Title and abstract first; structure after.  A paper begins by agreeing what its claim is and what is novel about it.  The title and abstract carry the claim; only once they are agreed does discussion of paper structure begin.

🔴 **A new paper starts from an abstract as long as necessary and fully referenced (Udi, 2026-09-12).**  The abstract states everything the paper will claim, with a citation on every claim that rests on prior work, and names every contender, requirement and result the paper will treat.  Writing commences only once that abstract is agreed, and the paper is derived from it.  Once the paper is complete, the abstract is shortened and its references removed.

🔴 **While a paper is under development, its abstract is the paper's specification (Udi, 2026-09-27).**  It describes what the paper should be, and states what the paper will claim, in the present tense, before the work behind a claim is done.  The abstract must be correct only upon submission, never before: a claim is not kept out of it, hedged, or put in the future tense because the demonstration, the proof or the code is not yet there.  "The abstract must match the body" (§10) is a condition on the submitted paper, and the work is what is brought up to the abstract, not the abstract down to the work.

## 1. Vocabulary

**Characteristic words and phrases:**
- "Grassroots" — signature concept for systems that emerge independently and coalesce
- "Decentralized control" — precise technical term; explicitly distinguished from blockchain's data decentralization
- "Permissionless" — frequently paired with "serverless"
- "Third party" — used to identify threats (not "adversary" or "attacker")
- "Sovereign" / "Sovereignty" — used both technically and philosophically
- "Accountability" — central value in fault tolerance and legal discussions
- "Egalitarian" — contrasted with "plutocratic" (Bitcoin/cryptocurrencies)
- "Surveillance capitalism" — used precisely (not vague "data harvesting")
- "Verified/Verifiable provenance" — for message authenticity

**What he avoids:**
- Marketing-style fragments ("no blockchain, no global state")
- Informal hyperbole ("revolutionary," "disruption," "game-changer," "paradigm shift")
- Vague claims without architectural or formal backing
- "Users" (prefers "people," "parties," "agents," "members")
- "Web3," "trustless," dismissive terminology
- First-person singular ("I"); always "we"
- "Card" in the body of a paper (Udi, 2026-10-16, reported by CSSN: "the term "card" is so technical and obscure, even I dont understand it. what is it? can it be banned from paper bodies? maybe appendix if we must?").  A card is vGLP's word for what a person's screen shows when they are asked to approve an act.  My English: "Bob is asked to approve, and answers yes or no"; in a code fragment, a type named for what it asks, such as `Approval` or `Acceptance`.  The word may stand in an appendix where a paper must use vGLP's definition.

**Emphasis techniques:**
- `\emph{...}` for concepts being defined or highlighted
- `\textbf{Bold}` for key terms in numbered lists or definitions
- Emphasis through structural parallelism, not exclamation marks

## 2. Sentence Structure

**Length and rhythm:**
- Mix of short, direct statements with longer explanatory sentences
- Breaks complex ideas into separate sentences
- Example: "The responsibility for the safety of a child lies first and foremost with their parents. Yet, the ability of parents to exercise this responsibility is forgone when children enter today's commercial social networks."

**Building arguments:**
- Foundation-first: start with fundamental principle, then build implications
- Enumerated lists (\ia, \ib, \ic) to structure complex concepts
- Logical progression: observation -> challenge -> proposed solution -> specification

**Characteristic constructions:**
- "Here we present..." (paper structure framing)
- "Thus," "Hence," "Therefore," for logical consequence
- "We note that..." for important asides
- "One may conceive of..." for raising challenges
- "Contrast this with..." for technical distinctions

## 3. How Definitions Are Introduced

- Formal definitions in boxed environments (Definition, Theorem, Proposition)
- Before formal definition, informal/intuitive explanation in prose
- Never assumes reader knows terminology
- Introduces new concepts via contrast: "A digital social contract is the digital counterpart of..."
- Uses examples to make abstractions concrete
- Distinguishes precisely: "By decentralized we mean decentralized control... Contrast this with standard blockchain applications..."

## 4. How Contrasts Are Drawn

**Precise, systematic comparison:**
- Acknowledges what existing systems accomplish
- Example: "Alternatives to centrally-controlled social networks have been explored for the last two decades. These include federated architectures such as Mastodon..., peer-to-peer architectures such as..., and blockchain-based protocols such as... While all these proposals aim to subvert the centralization of power of global platforms, they still require members to surrender their personal information to third parties..."
- Structure: (1) acknowledge alternative, (2) describe it, (3) identify specific shortcoming

**Key contrast phrases:**
- "In contrast to..."
- "Unlike..."
- "While all these proposals aim to... they still require..."
- "But what if..." (rhetorical question before presenting solution)

**Tone of contrasts:**
- Understated
- Focuses on technical limitations, not moral failings
- Grounds criticism in architectural analysis

## 5. Tone

- Neutral to slightly formal, never casual
- Assumes intelligent but uninformed reader
- Authoritative
- Visionary in scope, grounded in technical detail
- "We" for the paper, not personal pronouns
- Rhetorical questions as tools for thinking, not genuine unknowns
- **Acknowledgements are personal and follow author count.**  Unlike the body, which always uses "we", the acknowledgement uses "I" for a single-author paper and "we" for a multi-author paper.  This is the one place "I" is permitted.

## 6. How Claims Are Stated

**Contribution claims:**
- Avoided: "We propose a novel X that revolutionizes Y"
- Preferred: "Here we present..." followed by what it is and what it does
- Claims are specific and bounded
- Anchored to what exists: positions work relative to prior art

**Novelty claims:**
- Only with explicit qualification: "To the best of our knowledge..."
- Pairs novelty with technical contribution

**Hedges (when appropriate):**
- "We informally claim..." when proof is outside scope
- "May allow," "can support," "seems natural and sufficient"

**Stating assumptions:**
- Explicit: "We assume that parents control the software installed on their children's smartphone..."
- Foregrounds assumptions as design choices, not limitations

**Tense for forward-looking claims:**
- **NEVER describe in present tense things that are not true at present.**  If you are sure they will be true in the future, use "will".  If they may or may not be true in the future, use "may" or "can".
- Future tense for what the project will do, what the technology will enable, or what regulators will be able to do: "will give regulators the freedom to ban...", not "gives regulators the freedom to ban...".
- Present tense for what has already been demonstrated, what is mathematically proved, or what existing systems already do.

## 7. Three-Part Structure

Recurring use of (\ia), (\ib), (\ic):

> "The architecture incorporates (i) a decentralised social graph, where each person controls, maintains and stores only their local neighbourhood in the graph; (ii) personal feeds, with authors and followers who create and store the feeds; and (iii) a grassroots dissemination protocol, in which communication among people occurs only along the edges of their social graph."

- Rhythm: enumeration + brief description of each element
- Parallel structure

## 8. What He Does NOT Do

- **NEVER use the Arial font.**  If a sans-serif font is appropriate, use Calibri.
- No buzzword-driven writing
- No vague aspirational language without specifying mechanism
- No aggressive tone toward competitors
- No hand-waving
- No marketing framing
- No appeals to sentiment without technical grounding
- No over-simplification of complex problems
- No first-person singular
- No informal abbreviations in formal writing
- No rhetorical excess
- **NEVER apologise.  Be factual.**  Phrases like "even though...", "although still...", "while not..." weaken claims by signalling a deficit.  State what is.
- **NEVER invent acronyms.**  Spell terms out each time; use pronouns ("these languages", "it") or rephrasing for repetition.  Proper system names (e.g., GLP, FCP) and well-established short forms (e.g., LP) are fine.
- **NEVER do the minimal fix if it is not the correct one.  ALWAYS do the correct fix.**
- **NEVER introduce fluff.  ALWAYS remove fluff.**
- **NEVER use "the author" (singular).**  Always "the authors" (plural).  The singular form leaks single-authorship in double-blind submissions.
- **NEVER use gloss.**  Do not follow a term or statement with a restatement of it in different words.  A gloss that comes out more opaque than the term it explains is worse than nothing; either the term stands on its own, or the term itself needs fixing.  (This subsumes the narrower "State a named condition once" of section 21.)
- **Never use the words and phrases listed in `/Grassroots/docs/claudese.md`** in any paper, document, or discussion with Udi (e.g. "pattern" — except in pattern-matching — "shape", "involving", "honest").

## 9. Synthesis: How to Write Like Udi

1. Start with fundamental truth or problem state (short, declarative)
2. Explain why current systems fail (systematic comparison)
3. Propose architecture (introduced informally, then formally)
4. Build with three-part breakdowns using (\ia), (\ib), (\ic)
5. Prove properties or explicitly mark informal claims
6. Use parallel structure for list items
7. Conclude with broader implications or future work

**Voice checklist:**
- Claims are bounded and specific
- "We" is used; no "I" or personal narrative
- Technical terms defined before heavy use
- Contrasts are fair; acknowledge what alternatives accomplish
- Assumptions are explicit
- Proofs or formal definitions boxed; informal claims marked
- No marketing language or hyperbole
- Three-part lists used for complex concepts
- Italics used for concept introduction, not mere emphasis
- Precision over persuasion

## 10. How Abstracts Are Written

**Three-part structure:**

The abstract follows a three-part arc:

1. **Context + Problem/Gap** (opening).  The first sentence or two establish the world, not the paper.  Either define the central concept ("Digital social contracts are voluntary agreements among people..."), characterise the domain ("Grassroots platforms aim to offer an egalitarian alternative..."), or state the existing state of affairs ("Global cryptocurrencies are unbacked...").  Then identify what is missing, limited, or problematic.  Typical connectors: "However," "In contrast," "While...".

2. **"Here, we..." contributions**.  The pivot to the paper's own work always uses "Here, we present / introduce / propose / aim to provide / offer..." — never "In this paper, we..."  The word "Here" is the consistent marker.  Contributions are stated with precision: what is defined, what is proved, what is shown, what is implemented.

3. **Additional results or implementation** (closing, optional).  Often ends with secondary contributions, an implementation note ("implemented in GLP," "used by AI to derive a working implementation"), or the scope of what is and is not addressed.

**Rules:**

- **Opening is about the world, not the paper.**  The first sentence never starts with "We" or "This paper."
- **"Here" not "In this paper."**  The pivot word is always "Here."
- **The contribution claim ALWAYS begins with "Here,".**  The sentence stating the paper's own contribution opens with the word "Here," followed directly by the verb ("Here, we develop...", "Here, we present...", "Here, we prove...").  No other opener is acceptable for the contribution sentence.
- **"We" throughout.**  Never "I," even for single-author papers.
- **Contributions are specific and bounded.**  State what is defined, proved, shown — not what is attempted or explored.
- **Concrete examples ground abstract concepts.**  When introducing a general framework, include specific instances in the abstract itself (e.g., "befriending is guarded by both; unfriending is guarded by either").
- **Multiple contributions use (\ia), (\ib), (\ic) or semicolons.**  When contributions are heterogeneous, use the enumerated form.
- **No citations.**  Abstracts do not use \cite commands of any kind.  Refer to prior work descriptively (e.g.\ "in prior work") without an explicit reference; the paper body provides the citation.
- **No marketing.**  No "novel" without genuine justification.  No "revolutionary," "groundbreaking," or "paradigm shift."
- **No Greek letters or symbolic parameters in abstracts.**  Refer to parameters by name, not by symbol — "supermajority threshold," not σ; "decision rule," not φ; "timeout," not Δ; "arithmetic mean," not L₁.  Symbols belong in the body, where they are formally introduced; the abstract reads as prose.
- **No lettered enumerations in abstracts.**  Do not use (a), (b), (c), or (A), (B), (C).  Where enumeration is needed, use (\ia), (\ib), (\ic) or prose with semicolons.
- **The abstract must match the body upon submission.**  Every claim in the abstract must correspond to actual content in the submitted paper.  This is the most important rule.  While the paper is under development the abstract is its specification and runs ahead of the body ("How a Paper Begins").
- **Results stated, not hedged.**  When something is proved, say "we prove."  When shown, say "we show."
- **Length.**  Roughly 150–250 words.

## 11. How Contributions Are Written

Contributions lists state what the paper achieves at the highest useful level of abstraction.

**Rules:**

- **State what was achieved, not how.**  A contribution is not a theorem, a definition, or a proof technique — it is the intellectual advance those enable.  "A formal foundation for X" not "Definition 3.2 which formalises X using Y."
- **The more detailed, the weaker.**  Technical detail narrows the scope of a contribution.  "Safety and liveness machinery for grassroots platforms" is a stronger claim than "Transaction equivalence classes and monotonic transactions with a persistence condition yielding eventuality-based liveness."
- **Few items, not many.**  Three contributions are stronger than six.
- **No apologies or qualifications.**  "A formal foundation" not "A first step toward a formal foundation."
- **Parallel structure.**  Each item should have comparable grammatical form and comparable scope.
- **Match the abstract.**  The contributions list should mirror the abstract's structure — same claims, same order, same level of generality.
- **No contributions paragraph in the introduction.**  The contributions are carried by the "Here we..." sentence of the abstract, expanded upon in the body of the introduction.
- **The introduction MUST use the abstract's language and SHOULD expand on it.**
- **The introduction repeats the abstract; it does not revise it.**  The introduction reuses the abstract's contribution wording and expands upon it.
- **In the introduction, the contribution sentence may open with "In this paper," instead of "Here,".**  This substitution is permitted only in the introduction; in the abstract the opener is always "Here," (§10).
- **End the introduction with a paper-structure paragraph.**  This paragraph may give, for each section, a sentence explaining its content or contribution.

## 12. Mathematical Text

🔴 **Theorems are one-liners.  All the machinery needed to make them so is stated and defined
beforehand (Udi, 2026-08-15).**  The theorem states the result and nothing else: its hypotheses are
fixed in a sentence before it, and every notion it needs is a definition of its own.

🔴 **A formal definition is for mathematical content, and it is short (Udi, 2026-10-14).**  His words: "Definitions cannot be that long"; "Formal definitions are needed for defining mathematical content used in proofs etc.  Precise descriptions can be made without wrapping them in formal definitions.  Compilation can just be described."  So a program, a compilation, an implementation or a procedure is described precisely in text, never as a formal definition or a run of consecutive definitions.  A mathematical definition has no length target, but a long one is broken into several definitions, or its parts are made precise in text outside it.

🔴 **NEVER use the LaTeX definition environment for something that is not math, but is prose.**  You can still define terms, informally, using `\emph`, within running text.

🔴 **NEVER include explanatory text in mathematical text: definitions, theorems, proofs.**  Explanatory prose goes ONLY before or after the formal environment, never inside it.

🔴 **Mathematical text never forward-references a definition.**  A definition, theorem, or proof must not depend on a notion introduced later in the paper.  If notion A is used in the statement of notion B, A must appear before B.  When a forward reference arises, move the earlier-needed material earlier --- do not paper over it with "(defined below)" or a forward `\ref`.

- **Inside definitions, use `\temph` for the defined term, not `\emph` --- and only inside definitions.**  Definition environments typeset their body in italic (as in LIPIcs), where `\emph` flips the defined term back to upright text; `\temph` renders it bold italic so the term being defined stands out.  Use `\temph` only for defined terms inside definitions; everywhere else, use `\emph`.

## 13. Mathematical Self-Containment

A paper is mathematically self-contained: every definition, theorem, lemma, or proposition its results depend on appears in the paper itself, in the body or in an appendix.

- **Citation does not replace inclusion.**  Citing an external source is attribution, not substitution.
- **Reuse with attribution.**  When a definition or result is not novel to this paper, include the formal statement and cite the source.  Phrasing such as "we recall the following from [Smith and Jones 2020]" is appropriate.
- **Specialisations are included.**  When this paper specialises a more general notion from prior work, include the specialised statement formally and cite the general source.
- **Where to put it.**  Foundational definitions the paper depends on but does not itself contribute fit naturally in an appendix; definitions the paper builds on directly appear in the body.

## 14. Citations

Cite generously and inclusively.  When multiple papers contributed to a concept or technique, cite all the relevant ones.

- **Multiple originators get multiple citations.**  Futures and promises emerged independently from several traditions; cite all the foundational papers.
- **Don't omit citations to reduce clutter.**
- **Acknowledge contemporaneous work.**  Independent or parallel contributions deserve citation even when not in the direct line of the present paper.
- **Don't single out one author when several deserve credit.**
- **Be consistent within the paper.**  If `[Smith,Jones]` is the citation at first introduction, use the same when the concept is mentioned later.

## 15. Proofs

Proofs are either complete or absent.

- **Complete proofs**: in the body or in a Proofs appendix; both are acceptable.
- **Proofs in a Proofs appendix**: use `\begin{restatable}{...}{...}` for the theorem in the body and `\restatestmt{...}` in the appendix so the statement appears unchanged in both.
- **No proof sketches**, no "Proof (sketch)", no "It can be shown that...", no "the proof proceeds by induction on $k$".  Either prove it or omit the proof entirely with a cite to where it appears in full.
- **Cited proofs**: if the proof appears in a prior paper, state the theorem and cite the source: `\begin{theorem}[\cite{author2024}]`.  Do not paraphrase the cited proof.

## 16. LaTeX Source Conventions

🔴 **A paragraph is one line of source (Udi, 2026-09-06), IN EVERY TEXT YOU WRITE (Udi, 2026-09-10).**  NEVER break a line inside a paragraph: no hard wrapping at 80 or 100 columns, however long the paragraph.  Udi reads the source in Overleaf, and a paragraph broken across lines is hard to read there.  A line break separates paragraphs, environments, list items, table rows and code lines, and nothing else.  A hard-wrapped paragraph in an existing file is re-flowed into one line when the file is next edited, as a whitespace-only change that needs no approval.  **The rule is not LaTeX's**: it holds for every text you produce --- LaTeX source, Markdown documents, `docs/` notes, inbox entries, handovers, responses to reviewers, README files, and the comment blocks of source files.  Wherever the text is prose meant to be read, one paragraph is one line.

🔴 **RETIRED (Udi, 2026-09-21).**  This section used to require a proposal be shown as LaTeX source in a fenced block, for pasting into Overleaf.  It is replaced by his rule of 2026-09-21: **never show Udi LaTeX in a conversation** --- "I cannot review latex in a conversation.  Either show formatted text (if short or simple edits) or else edit and push and I review in Overleaf".  A short or simple edit is shown as formatted text; anything longer is edited on disc and pushed.  See `/Grassroots/docs/claude.md`, "Papers".  The source conventions of this section stand for the files you write.

**Tie a citation to the text before it: `text~\cite{key}`.**  The `~` is a non-breaking space: it gives the normal interword space and keeps the citation on the same line as the word it cites.  A plain space before `\cite` lets the citation break to the next line; no space at all runs it into the word.

🔴 **NEVER comment on, flag, or surface extra or inconsistent whitespace in LaTeX source** --- double spaces, trailing spaces, blank-line counts, and the like.  LaTeX collapses consecutive whitespace into a single inter-word space at render time, so these are harmless and have no visible effect in the PDF.  The sole exception is whitespace inside `verbatim`, where it renders.  Fix them silently when you are already editing the line; NEVER bother Udi with them --- his time is precious.

**Never introduce PDF document metadata in the LaTeX preamble.**  No `pdftitle`, `pdfauthor`, `pdfsubject`, `pdfkeywords`, or any other `\hypersetup{...}` entry that sets PDF document properties.  If a paper template includes these by default, remove them.  The printed `\title{}` and `\author{}` are the only places paper identity is stated.

## 17. Write Directly, Not Defensively

Hedges like *"a possible…"*, *"one such…"*, *"for instance"*, *"if desired"*, *"perhaps"* invite the objection they try to deflect.  The fix is the **indefinite article** plus a flat verb: *"CGMS provides a decision rule"*, not *"CGMS provides one such decision rule, for instance"*.

The same failure runs the other way, as emphasis that insists on what the sentence already says.  *"and no others"*, *"the whole reason"*, *"the sole rationale"*, *"entirely"*, *"whatsoever"*, *"at all"*, *"of any kind"* --- each answers an objection nobody raised, and the answer plants the doubt.  *"The project has two kinds of artefact"* already excludes a third; *"two kinds of artefact and no others"* asks the reader to wonder about a third.  **Cut the insistence.**  Where exhaustiveness, or exclusivity, or completeness is itself a claim, state it as a claim in its own sentence, where it can be checked.

🔴 **NEVER add a statement that something does not exist, after you state explicitly what exists (Udi, 2026-09-10).**  A universal statement about what there is already excludes what there is not, and the citation on it carries the rest.  This governs the sentence above: a second sentence asserting completeness, after a universal statement, is the same insistence at greater length.

## 18. Prose Must Not Restate a Table

When a table, list, or figure already carries a set of facts, the surrounding prose must not re-narrate those facts.  Prose earns its place only by adding what the structure cannot: the reason behind an entry, or a consequence of it.

- **State each fact once, where it belongs best.**  A paragraph that walks through the rows of an adjacent table is redundant and should be cut.
- **A dangling forward-pointer is the symptom.**  Phrases like "which we contrast next" that point at the duplicated prose break the moment that prose is cut.

## 19. Predicate, Do Not Perform

State claims with plain verbs: *"X requires Y"*, *"X is restored by Y"*.  Cut verbs chosen for drama rather than meaning --- *"admits"*, *"demands"*, *"diverges"*, *"is healed"*, *"reintroduces"*.

## 20. State It Plainly, Once

- **One claim per sentence.**  A hard limit gets its own short sentence, stated flatly ("Recovering ... is impossible."), not buried in a compound clause.
- **State a capability or limit; do not justify it in place.**  Cut crutches like "X is reversible, so ...", "the asynchronous model places no bound, so ...".
- **Define by the essential property, not a boundary and its consequence.**  "a final prefix not containing fraudulent transactions," not "a prefix ending before the compromise, abandoning the fraudulent suffix."
- **Plain agent nouns.**  "thief," "victim," "the person" --- not "an attacker holding the key."
- **Plain verbs.**  is, has, can, may, gives, holds, records, restores, recovers, proves, shows.  Avoid Claude's default verbs, listed in `/Grassroots/docs/claudese.md`.
- **Cut connective filler.**  Drop "Against this,", "That said,", "It is worth noting,", "Notably,", "Crucially,".  ("However" and "Thus" are fine when they carry a real contrast or consequence.)
- **No trailing clause (Udi, 2026-09-04).**  A complete statement ends where it ends.  A relative or adverbial clause bolted on to restate it, extend it, or spell out its consequence is Claudese: "an atom is a syntactic object with no truth value, *which a state holds or does not*"; "an edge each way between them, *whether or not there was one before*".  Cut at the comma.

## 21. Offering Revised Text

- **What Udi deleted stays deleted (Udi, 2026-10-01).**  Restoring text he cut is editing his text, and takes his express consent naming the text; propose it in chat if it seems needed.
- **Anchor on Udi's own words.**  When Udi has already given the wording he wants --- in a LaTeX comment, in chat, or paraphrased from memory --- reconstruct the sentence from his words rather than generating a fresh alternative.  Normalise only typos, and flag them.
- **One concept, not two.**  Do not split a claim into named notions when one answers the question.  "Is the mapping correct?" is one concept; introducing "consistent" alongside "correct" to mark a secondary distinction is machinery the sentence does not need.
- **State a named condition once.**  Do not gloss a condition with a paraphrase of itself in the same sentence: "under eventual synchrony" and "after the network stabilises" are one condition stated twice.
- **Quantifier scope is a truth claim.**  "eventually every X" is not "every X"; "may" is not "does."  An over-reaching quantifier makes a sentence false.  Place "eventually," "may," "some," "every" so the sentence is true.
- **Use the paper's existing vocabulary.**  Before choosing a term, check what word the abstract and body already use for the concept and match it --- "friends," not the graph-theoretic "endpoints."

## 22. Foreign Terms and Borrowed Machinery

- **Never use a term of another field without defining it in plain words.**  If the definition costs more space than the point deserves, the term does not belong --- make the point without it.  Test: an intelligent reader outside the field must not need to ask "what is X?" for any word in the sentence.
- **State the fact, not its frame.**  Write what the mechanism does, not which body of law or literature governs it.  "The protocol records provenance" --- not "transfer by endorsement and delivery, presentment, and protest".
- **Do not import another field's problem inventory.**  When touching an adjacent field, mention only what the paper needs.  Do not survey its concepts, name its doctrines, or answer its objections unprompted --- each imported concept is a question the reader now has to ask.
- **One notion per sentence, plainly ordered.**  A sentence that stacks several borrowed notions is unreadable even when each is defined.

## 23. Legal Text

🔴 **Law is like mathematics: NEVER include explanatory text inside a contract, a clause, or any other legal instrument (Udi, 2026-09-01).**  A clause states an obligation, a permission, or a prohibition, and nothing else.  Explanations, instances, and consequences go after it, never within.  Besides the title and the clauses the box holds the declaration of the contract's names, before clause 1, `Alice, Bob and Carol are persons.`, a formal part of the contract from which the schemas and the pragmas take their names (Udi, 2026-10-25, to GSC #8, reported by GSC; §36), and, after the declaration, every definition that is part of the contract: **a contract is self-contained** (Udi, 2026-11-07, to GSC #13, reported by GSC), as GSG defines trust in its box; a definition outside the box is a formal definition, which the box cites explicitly (GSC's Section 2).

## 24. A Revision Never Mentions the Earlier Version

🔴 **When editing or revising a paper, NEVER refer to the earlier version or explain how the new one differs (Udi, 2026-09-09).**  The reader did not read the old version, so its content is clutter.  No "previously", "in the earlier version", "we now", "what was X is now Y", "this replaces"; no remark, footnote or comment recording what changed.  The paper carries the current text and nothing else; git and the arXiv version history hold the change.

This governs the paper's own earlier drafts.  Prior *published* work is different and is cited as usual.  The internal documents are different again: `/Grassroots/Coordination` and `claude.md` record what they used to say on purpose, because a session that acted on the old text has to find it.

## 25. Do Not Use a Term Before It Is Defined

🔴 **Avoid using technical terms that are defined only later, providing a forward reference to the explanation/definition (Udi, 2026-09-10).**  Instead, use a more abstract description that avoids the technical term, and, if needed, provide a forward reference to the details.

Section 12's rule is the mathematical case and its remedy is different: there the earlier-needed material moves earlier.  Here the term is not needed at all — the sentence states what the mechanism does, and the detail stays where it is defined.

## 26. Citation Numbers Are Sorted and Compressed

🔴 **Every paper loads a package that sorts the numbers inside a citation and compresses runs of three or more (Udi, 2026-09-11).**  `\cite{a,b,c,d}` prints `[36--38,41]`, never `[37,38,36,41]`.  This is obligatory.

- **LNCS (`llncs.cls` with `splncs04.bst`):** `\usepackage[nospace]{cite}`, loaded before `hyperref`.  natbib's numeric mode does not work with `splncs04`, so `cite` is the only route, and `nospace` keeps LNCS's `[1,2]` rather than `[1, 2]`.
- **Anything with a natbib-compatible style** (`article`, IOS, `plainnat`): `\usepackage[numbers,sort&compress]{natbib}` before `hyperref`, as ERC B1 does.

The rule governs the numbers inside a citation.  The reference list keeps the order its `.bst` gives --- alphabetical for `splncs04` and for `plainnat` --- and that order is not changed.

## 27. Fitting a Page Limit: Vertical Space Before Text

🔴 **When a paper is over its page limit, take the vertical space first and cut text only once the space is gone (Udi, 2026-09-11).**  NEVER change the class's fonts, margins, or heading typefaces: Springer and the ERC both reject that, and it shows where spacing does not.  Compile and read which page the References heading lands on; never judge by eye.

Measured on SPM's camera-ready: 15.6 pages to exactly 15, with no text cut.  In order of yield:

1. **The class's own theorem spacing, usually the largest single item.**  `llncs` sets `\topsep 7pt` around every theorem-like environment and around `proof` --- that is the white space above theorems.  Re-issue `\@spthm` and `\@Thm` with `\topsep 2pt`, their bodies copied verbatim.  `\thm@space@setup` does nothing here: `llncs` theorems come from `\spnewtheorem`, not from amsthm.
2. `\smallskipamount`, `\medskipamount`, `\bigskipamount`.  `\mypara` is a `\smallskip`, so this one acts at every paragraph heading in the paper.
3. Heading skips: re-issue `\section`, `\subsection`, `\subsubsection` and `\paragraph` with the class's font argument copied verbatim and only the two skip arguments reduced.
4. Display skips, through `\g@addto@macro\normalsize`.
5. `\setlist{nosep}` (enumitem), wherever the paper has many lists.
6. `\textfloatsep`, `\floatsep`, `\intextsep`, `\abovecaptionskip`, `\belowcaptionskip`.
7. `\raggedbottom`.
8. `\usepackage[expansion=false]{microtype}`.  Plain `microtype` fails on LNCS's Type 1 fonts: "auto expansion is only possible with scalable fonts".
9. Last, for a line or two: `\looseness=-1` at the start of a long paragraph whose last line is short.

## 28. A Person Is "They"

🔴 **A person is not "it", it is "they" (Udi, 2026-09-13).**  A person---natural or legal: an issuer, a holder, a payer, a payee, a member---takes "they", "their", "them", never "it", "its".  The coins paper states the convention in its definitions, "a person, natural or legal (with the gender-neutral pronoun `they')", and its prose keeps it: "Each person backs their coins with their offerings".  "It" stays for what is not a person: a coin, a bond, a payment, a currency, a ledger, and an institution named as such---the central bank, a bank, an intermediary.  A relative clause on a person opens with "who", not "that".  🔴 **Alice is "she" and Bob is "he"** (Udi, 2026-11-07, to GSC #13, reported by GSC; his sentence, in GSC's Section 2: "A person takes the pronoun they; for readability we may write she for Alice and he for Bob etc."): where a pronoun names Alice, Bob, Carol or Dave, in the prose and in the clauses of a box, write "she", "her", "he", "him", and "they" for a person not named; every paper and contract follows when next edited, GSC's and `GKK.txt`, `GSG.txt` and `GSN.txt` since 2026-11-07.

## 29. An Abstraction Is Never Made More Concrete for the Convenience of the One Below

🔴 **An abstraction is never made more concrete for the convenience of the abstraction below (Udi, 2026-09-17).**  A layer of the cascade is specified at its own level, in its own terms.  What the layer below needs in order to implement it is that layer's affair, and the theorem between them states what is implemented and how.  A detail admitted into the upper layer because the lower one would otherwise be harder to write is a defect in the upper layer, whatever it buys below.

## 30. Write Every Explanation as If Explaining It to Udi

🔴 **When you add explanatory text to a paper, imagine you are explaining it to Udi (Udi, 2026-09-22).**  Not to a referee, not to a reader you picture as needing the ground laid.  It is the test that catches Claudese before it is written: you would not say "the lens of", "what X is doing here", "this lands" to him, so it does not go in the paper either.  It also fixes the length --- to him you say the thing and stop --- and the register, which is plain English, his own.  Where the imagined explanation would take a paragraph of scaffolding before the point, the point is not yet clear enough to write.

🔴 **The chat sentence is the standard, and it goes in the paper (Udi, 2026-09-22).**  His words: "Why don't you use this clear language in the paper, only with me?  think of me as the reader", and, on an explanation given in chat after he struck the paper's own sentence, "just use the text you used to explain me, so it is clear".  So where a passage in a paper and the same point made to him in chat differ, his is the My English and the paper's is the Claudese: put the chat text in the paper.  "Talking to Udi" states the converse, that the chat follows the paper's rules; both hold.

## 31. Repair With What the Sentence Already Has

🔴 **A correction that costs readability is not a correction (Udi, 2026-09-22).**  Two of his rulings, on two repairs of his own text that he rejected.

🔴 **Precision belongs in the citation, not in the sentence.**  A loose sentence is not repaired by importing the terms of art --- "not denied legal effect for being electronic, which is what the electronic-signature statutes provide", "negotiable within the bills and notes acts".  His answer was "Nobody can read this.  I keep mine."  His own text says it plainly --- "legally-binding also in electronic form", "whether it is negotiable" --- and leaves the exact formulation to the citation.  A body sentence built of terms of art cannot be parsed by a reader who does not already know the statute, and that reader is the paper's reader.

🔴 **Repair with the smallest word the sentence already permits.**  Where what is unmeasured was throughput, scale and ease of use, his sentence ended "and have yet to be similarly measured"; the proposed repair was "and the three have yet to be measured".  An adverb points back and the reader carries on; a noun phrase makes them count back.  Keep the sentence count too: a two-sentence paragraph is repaired into two sentences.

## 32. A Grant Proposal Proposes What We Already Know How to Do

🔴 **A grant proposal says we will do what we already know how to do, or have already begun without publishing (Udi, 2026-10-02).**  This reduces the risk and makes the proposal more credible.  Unpublished work is written as the plan, in the future tense (§6), and not listed as a preliminary result.


## 33. Related Work in the Body Cites Every Reference

🔴 **"related work in the body must include ALL references; text can be terse and expanded in appendix"** (Udi, 2026-10-03, reported by sGLP, verbatim).  The Related Work section in the main text cites every work the paper relates itself to, each in a terse sentence; an appendix may carry the expansion.  No reference lives in the appendix alone.

## 34. Answering a Comment in the Paper

🔴 **A comment is answered by the paper, never by an explanation** (Udi, 2026-10-12; first given to vGLP, reported by vGLP, 2026-10-11).  Every comment a person leaves in the source --- `\idit{...}`, `\textcolor{blue}{Ohad says: ...}` --- gets a reply, `\udi{...}`, placed right after it.  The reply is a reference to the text of the paper that answers the comment, never an explanation, since the next reader may raise the same point and only the paper reaches them.  The text it points to may answer in the negative: then that text says why the point does not hold.  A reply takes one of three forms, and no other:
- `Corrected: <where>.` --- the text was wrong or unclear, and is fixed;
- `See <where>.` --- the text answers the comment, as it stood or as revised in response to it;
- `Removed.` --- the text the comment is on is cut.

Only the commenter deletes a comment, and the replies with it.  Never delete, comment out or hide another person's comment; never edit a reply already in a thread, add another at the end; and when deleting text that carries a live comment, keep the comment where the text was.


## 35. No Line Numbers in a Submission

🔴 **A submitted paper carries no line numbers unless the venue asks for them** (Udi, 2026-10-18, reported by GSC, the paper named Jurix until that day, verbatim: "remove line numbers unless explicitly asked for (add this directive to style guide for any submission)").  No `review` option of `acmart`, no `lineno`, in any submission, unless the call for papers asks for line numbers.

## 36. A Contract in a Paper

🔴 **The term is grassroots contract, not grassroots social contract** (Udi, 2026-11-02, to GSC #9): a grassroots contract is a social contract that runs, and the genus stays in the definition, "grassroots contracts are social contracts meeting Proudhon's six conditions and two more".  Every paper that writes "grassroots social contract" writes "grassroots contract" when next edited.

🔴 **Each contract a paper quotes goes in a box of its own, like a mathematical definition** (Udi, 2026-10-18, reported by GAI, verbatim: "each contract in its own box, separated top line: \textbf{GSG:  Grassroots Social Graph} then clauses, always numbered starting from 1"; "It should be written like a math definition ... Only the bare formal/legal text; any explanations or prose --- outside the box, before or after").  The box is titled `<NAME>: <name in full>`, as `GSG: Grassroots Social Graph`; its clauses are numbered from 1; it holds the declaration of its names, `Alice, Bob and Carol are persons.`, then every definition that is part of the contract (§23; Udi, 2026-11-07, to GSC #13, reported by GSC), then the clauses, and nothing else, every explanation going before or after it (the declaration: Udi, 2026-10-25, to GSC #8, reported by GSC, verbatim: "they are now a formal part of the contract and schema, and should be inside the box, so that the contract text and schema are self-contained"; GSC's Section 2 states it, GSC 4492c93).  The prose cites a clause as `GSG:1`.  The text in the box is the repository's copy, `GLP/programs/<dir>/<NAME>.txt` (`claude.md`, "Sessions").

🔴 **The box is GSG's tcolorbox, and a borrowed contract names its owner on the top line** (Udi, 2026-10-20, verbatim): "each paper that presents a social contract should use the tcolorbox scheme of GSG (and GAI), and if they are not the owner, the top line of the box should end with a reference to the owner".  On disc the scheme is GAI's `cascadebox` environment, `GAI/main_characterisation_ArXiv.tex`, titled `GSG: Grassroots Social Graph`; the owner of each contract is the `% Owner` line of its `<NAME>.txt`.

🔴 **Each clause opens with its modality mark, by GSC's macros** (Udi, 2026-11-02, to LBM #4: "follow GSC"): `\enf`, `\att` and `\und`, the filled square, the filled diamond and the circle drawn in tikz as GSC's `main.tex` defines them, after the clause's number and before its title, `\item \enf~\emph{Acquaintance.}`; never `$\blacksquare$` or another symbol.  The box holds the names declaration, the definitions that are part of the contract, and the clauses, and nothing else, and the declaration defines what its clauses use, as GnW's does (`GLP/docs/writing-a-contract.md`; Udi, 2026-11-02, to LBM #4: the contract is self-contained).  The model is GSC's `sections/02-contracts.tex`.

🔴 **What a contract extends is the first line of its box, alone, ending with a reference to the paper of the contract it extends** (Udi, 2026-11-05, to GFF #3, reported by GFF, verbatim: "the correct format of this:  GDO extends GSG. is 1. should be in the first and separate line 2. should end with a reference to the owners paper   GDO extends GSG~\cite{...}.").  The declaration of the names follows on the next line.  Where that paper is not on arXiv, the reference is its acronym and title and then ", in preparation" (Udi, 2026-11-05, to Coordination #12, verbatim: "if the paper is not on arxiv but in overleaf, reference it with acronym and title, and then ", in preparation"."): GDO-L extends GDO (GRH, Grassroots Ride-Hailing, in preparation).  Where the extended contract is the same paper's, the line carries no reference (Coordination #12, 2026-11-05, Udi approving).  The repository's `NAME.txt` takes the same line order once Code's and vGLP's readers take it (`GLP/docs/writing-a-contract.md`).

🔴 **The relation between two contracts is stated in GSC's terms** (Udi, 2026-11-06, reported by GSC #11): it is a relation of GSC's paper, Section 8, Relations among Contracts, and the paper and the contract's header say which, as "GSN amends GSG" and "GB extends GC", never "replaces" or "extends with changes".  The first line of the box states that relation, with its verb.

🔴 **Pragmas are of two kinds, interface and implementation, and the heading names the kind** (Udi, 2026-11-07, to GSA #1, reported by GSA and GSC): `\pragmas{Interface}` and `\pragmas{Implementation}`, each its boxed heading on its own line, "Interface pragmas" and "Implementation pragmas", and `\pragmas{}` the plain "Pragmas"; the macro is GSA's `main_SGSG.tex`, and GSC's paper defines the two kinds.

🔴 **The box has a boxed title since 2026-10-30** (Udi, approving Coordination #11's rendering): the title sits in a small framed box on the top-left edge of the frame, the frame 1pt and rounded, the background light grey.  The definition is `contractbox` in `/Grassroots/Coordination/main.tex`, and every paper takes it from there when next edited; a borrowed contract still names its owner on the title.

## 37. A Screenshot in a Paper

🔴 **Every paper with screens carries this note, Udi's text** (Udi, 2026-10-20, to Code #12, verbatim; reported by Code): "This is a screenshot of the XXX mini-app, compiled from the XXX social contract down the abstraction cascade. One instance of the app on an iPhone simulator runs the mini-app of every agent; the agents communicate over channels within it, and the screenshot shows one person's screen."  XXX is the app and its contract.  The note ends with the repository sentence, as GAI's paper has it (GAI 85b6577; Udi, 2026-10-20, to GAI #5): "All source code and all artefacts, including the social contracts and the artefacts the abstraction cascade produces from them, are in the public repository \repo."  `\repo` is the anonymised repository in the short build and GitHub in the full (`claude.md`, "Papers"; §38).  GAI's paper also cites, on each claim of the note, the papers of the abstraction cascade, the grassroots app and GLP's channels.

The facts behind it, as Code reports them at GLP main `aac72fea` (Code #12, 2026-10-20): each screen is a mini-app of `programs/apps/<app>` or `programs/cssn`, its platform the canonical CVA platform vGLP's compiler prints from the contract after GSC's checker certifies it, compiled to GLP when the directory loads and drawn by the app's declared widgets; the scenario `glp_multiagent/test/apps_screens_test.dart` runs every agent's mini-app in one GLP engine, the agents' conversations channels within it, with a scripted agent and no phone; on the simulator (`glp_multiagent/tool/sim_screens.sh`) the same scenario runs in one app on an iPhone 16e, iOS 26.3, and each screen is a screenshot of one person's phone at a time, the status bar set to 9:41 and the bezel drawn by `tool/sim_shot.py`.  The super-app does not host the mini-app in the screens.  Code tells Coordination when any of these facts changes, and the note follows.

## 38. The Full Version and the Short Version

🔴 **One source, two builds** (Udi, 2026-10-20, to GAI #5, reported by GAI, verbatim: "I dont want to manage two; same format, just not anonymous, and full").  The full version is the text, complete and not anonymous, for arXiv; the short version is the submission, anonymous where the venue is double-blind, within the venue's limits.  Both have the same format, the venue's class and options: the short with the anonymous and review options, the full without them (GAI: `\documentclass[sigconf,anonymous,review]{acmart}` and `\documentclass[sigconf,nonacm]{acmart}`).  The full version is never a different class or a different layout.

The macros, at the top of the main file before `\documentclass`: `\newif\iffull`, with `\fulltrue` or `\fullfalse` the one switch; `\newcommand{\full}[2]{\iffull#1\else#2\fi}`, which replaces text point-wise, the full text first; text only in the full version under `\iffull ... \fi`, only in the short under `\iffull\else ... \fi`; `\newif\ifanon`, set by `\iffull\anonfalse\else\anontrue\fi`, for what anonymity decides; and `\repo`, the anonymised repository under `\ifanon` and GitHub otherwise (`claude.md`, "Papers").  The full version's title carries " (Full Version)" through `\full{ (Full Version)}{}`.

What the short version leaves to the full: material moved to an appendix within the limit, and, where the venue limits the appendix, the proofs and the machinery recalled from a published paper, the short carrying the definitions and the statements and citing the published paper (Udi, 2026-10-20; `claude.md`, "Papers").  Both versions are built and their page counts checked before every push.

## 39. Tense: Past, Eternal, Future

🔴 **Work done is written in the past, an eternal principle in the present, and work to come in the future (Udi, 2026-10-21).**  His words: "work done in the past belongs to the past, eternal principles should be stated as such, and future work is in the future.  nothing happens at the present, except the passing of time."  What we did, and what a cited paper did, is past: "we analysed ten patients", "we reconstructed the trees", "the genome was engineered", "such therapies had limited success".  What holds always is present: a definition, a mathematical fact, a property of the world or of a method, "short tandem repeats are selection-neutral".  What is still to be done, and what the work will enable, is future (§6).

§6's present tense for what has already been demonstrated is the standing fact, never the act that demonstrated it: "the protocol is safe" is present, "we proved it safe" is past.  The abstract's "Here, we present ..." (§10) and the introduction's "In this paper, we present ..." (§11) stand, the paper presenting whenever it is read; the work it reports is past.

## 40. Speech-Act Schemas in a Paper

🔴 **A paper shows speech-act schemas as its examples, and not VMAT transactions, unless the paper is specifically about VMAT** (Udi, 2026-11-05, to GGGA #3, reported by GGGA, verbatim: "tell Coordination: 1. that paper should show SAS as examples, not VMAT (unless specifically it is about VMAT), and 2. use the formatting you have just used for SAS").

🔴 **The schemas of a contract are formatted as SRC's paper has GD's** (the same words of Udi's): the model is the box "GD: Speech-Act Schemas" in `/Grassroots/SRC/sections/sybil-resilience.tex`, SRC main 85abcda.

1. One contract box, GSC's `contractbox` (§36), titled `<NAME>: Speech-Act Schemas`.
2. The schemas are numbered by the clauses of the contract, the number and nothing else: no modality mark, no clause title and no schema name, which the contract's box already has.  A clause with two schemas has both under its number.
3. A schema is mathematics in the paper's font, not code: atoms in math italic, the negation sign, and the math left arrow, not GLP's `:-`.
4. The head is flush left after the number, then the arrow.  The body starts on a new line, indented, filled to the width, and continues on further lines at the same indentation; it ends with "guarded by", the guards and a full stop.  Every head starts at the same indentation, a second schema's included.
5. The undertaking formula of an undertaken clause (GSC, Definition Undertaking formula) is under its number in the same layout: the condition and the leads-to arrow on the head line, the response on the body line, ending with "undertaken by" and the obligor.

The macros are two, at the head of that file: `\sashead` for a head that starts a line and `\sasbody` for the indented body.
