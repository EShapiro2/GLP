# The link probes: what crosses a link, and what does not

Eight programs under `programs/tests/linkprobes/`, run with `:boot`, that fence
in the runtime behaviour which stopped the warm call of the SGSG paper (Section
5.2, item 9 / 17 (c)).  Each is a cold call between isolates, after which one
side writes on a stream the other reads, and the reader assigns a variable that
arrived in an element.  The first six differ in two things only: how many
crossings the reader's end made before it was read, and whether the elements
were written in one reduction or in two.

| Probe | Crossings | Write | Result |
|---|---|---|---|
| `linkprobe9` | one | split: greet, then the probe later | works |
| `runprobe` (with `probeapp`) | one | by a mini-app activated by `run/3` | works |
| `linkprobe7` | two | both elements in one reduction | works |
| `linkprobe10` | two | split: greet, then the probe later | works since 2026-09-08 |
| `linkprobe11` | two | split, the later element ground | works since 2026-09-08 |
| `linkprobe12` | two | split, the stream left open | works since 2026-09-08 |

A crossing is an end sent inside an element of a stream: one crossing is an end
in a network message, two is an end sent on a channel that itself arrived that
way --- which is what the social graph does, the root channel's end travelling
inside a message of the friend channel, whose own end crossed at befriending.

The last three were the evidence for the defect: an end that had arrived two
crossings down delivered the elements written in the reduction that handed it
over, and nothing written into its tail afterwards.  `linkprobe11` showed this
had nothing to do with the writer the element carries, and `linkprobe12` that
it had nothing to do with closing the stream: it was the later write itself.
The warm call sat exactly there --- the mini-app greets on the root channel
when the conversation opens (`open_out`), and puts the probe on the tail one
reduction later (`probe_on`), so its friend saw the greeting and never the
probe.  Reported to IGLP on 2026-09-08 and repaired the same day in
`glp_runtime/lib/runtime/heap_fcp.dart` (`fireBoundCallbacks`), where the
`global_send` goal watching such a tail was realised as a heap callback and
never fired when the tail became known by the resolution of a variable chain.
All six now deliver the later element.

| Probe | The channel | Result |
|---|---|---|
| `linkprobe13` | made by a third agent, both ends cross | nothing passes: the links are held |
| `linkprobe14` | made by one party, one end forwarded | nothing passes: the link is held |

`linkprobe13` and `linkprobe14` are a second pair, of three agents each, in
which a channel end reaches its holder through a third agent --- the shape of
the social graph's introduction.  Every link so made is held, and neither probe
calls `authorise_link/2`, so no greeting is delivered and the boot settles with
nothing sent to either person.  That is the held link doing its work: they were
reported as a defect and withdrawn by SGSG Code on 2026-09-09, and they are
kept because the shape is the introduction's.

The suite runs all eight, as block MB5 of Section MB in `test/run_all_tests.sh`.

To run one by hand, from `glp_runtime/` in your worktree:

    :boot ../programs/tests/linkprobes/linkprobe10_boot.glp ../programs/tests/linkprobes/linkprobe10

`runprobe` needs its mini-app's artefact beside it first:

    :artefact ../programs/tests/linkprobes/probeapp ../programs/tests/linkprobes/runprobe
