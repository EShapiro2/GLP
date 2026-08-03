# agent_roundtrip, retired 2026-08-03

Thirty-two `.glp` files, `social_graph_protocol_tests.md`, and the `plays/`
directory, moved here from `programs/tests/agent_roundtrip/`.  Nothing is
deleted; `git log --follow` on any of them reaches its whole history.

## Why

Of the 48 `.glp` files in that directory, 13 were named by `run_all_tests.sh` or
by a Dart test and one more — `typed_ui_actors.glp` — is reached through the
directory's `-expose`.  The other 34 were named by nothing, no paper referred to
any of them, and `agent_roundtrip` is not a program directory, so no directory
load reached them either.

Two of the 34 turned out to be alive: `play_dglp_boot.glp` and
`play_ui_dglp_boot.glp`, the single-isolate REPL routes `docs/ma/HOW-TO-RUN.md`
documents.  Both load clean.  They stayed, and the suite now tests them, so the
question does not have to be asked again.

The 32 here do not load at all under the current language.  They fail on their
own contents rather than on a missing companion — type errors (`play_alice_bob`,
`play_typed_simple`, `network3`, `social_graph_protocol`), syntax the parser no
longer admits (`agent_full`: a procedure declaration with no clauses), guards the
language no longer permits (`ui_agent`: a multi-clause procedure in guard
position).  They are the pre-typing originals and the superseded protocol
variants that the typed files in `programs/tests/agent_roundtrip/` replaced.

## What this is not

Not a judgement that the programs were wrong.  They are where the typed fixture
came from, and `social_graph_protocol_tests.md` records what the protocol was
expected to do.  If one is wanted again, it comes back with the work to make it
load — which is what keeping it here rather than deleting it is for.

## Which archive this belongs to

Udi has ruled `programs/archive` renamed to `programs/old-archive`, with a fresh
`programs/archive` taking what is retired now.  **This directory belongs to the
fresh `programs/archive`, not to `old-archive`.**  Whoever runs the rename must
move it back out: `git mv programs/archive programs/old-archive` carries this
directory with it, and `old-archive/agent_roundtrip` then has to return to
`programs/archive/agent_roundtrip` in the same commit.

The reason is a collision, measured rather than assumed.  `old-archive` is the
snapshot of 2026-03-07 (`1473fb31`), a whole earlier book kept in its
unparameterised forms; `docs/known-issues.md` records that this is deliberate.
One of its directories, `book/social_graph`, is the same lineage as these files.
Every one of its 24 `.glp` files has a same-named counterpart in the tree and
every one differs in content: 21 of them are here among the 32, and three —
`agent.glp`, `channel.glp`, `streams.glp` — are still live in
`programs/tests/agent_roundtrip/`.

So sending these 32 to `old-archive` would put two ages of the same 21 files in
one archive with nothing to tell them apart, which is the merge of two eras Udi
ruled against when he chose a rename over one.  Kept apart, the directory name
carries the era: `old-archive` is March, `archive` is what was retired on
2026-08-03.  That is what the rename buys, and it is worth the one extra `git mv`
to keep.
