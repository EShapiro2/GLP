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
