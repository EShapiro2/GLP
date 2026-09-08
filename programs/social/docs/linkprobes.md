# The link probes: what crosses a link, and what does not

Six two-agent programs under `programs/social/`, run with `:boot`, that fence in
the runtime behaviour which stops the warm call of the SGSG paper (Section 5.2,
item 9 / 17 (c)).  Each is a cold call between two isolates, after which one
side writes on a stream the other reads, and the reader assigns a variable that
arrived in an element.  They differ in two things only: how many crossings the
reader's end made before it was read, and whether the elements were written in
one reduction or in two.

| Probe | Crossings | Write | Result |
|---|---|---|---|
| `linkprobe9` | one | split: greet, then the probe later | works |
| `runprobe` (with `probeapp`) | one | by a mini-app activated by `run/3` | works |
| `linkprobe7` | two | both elements in one reduction | works |
| `linkprobe10` | two | split: greet, then the probe later | **the later element never arrives** |
| `linkprobe11` | two | split, the later element ground | **the later element never arrives** |
| `linkprobe12` | two | split, the stream left open | **the later element never arrives** |

A crossing is an end sent inside an element of a stream: one crossing is an end
in a network message, two is an end sent on a channel that itself arrived that
way --- which is what the social graph does, the root channel's end travelling
inside a message of the friend channel, whose own end crossed at befriending.

So: an end that arrived two crossings down delivers the elements written in the
reduction that handed it over, and nothing written into its tail afterwards.
`linkprobe11` shows this has nothing to do with the writer the element carries,
and `linkprobe12` that it has nothing to do with closing the stream.  It is the
later write itself.

The warm call sits exactly there: the mini-app greets on the root channel when
the conversation opens (`open_out`), and puts the probe on the tail one
reduction later (`probe_on`), so its friend sees the greeting and never the
probe.  Reported to IGLP on 2026-09-08.

To run one:

    :boot /Users/udi/Grassroots/GLP/programs/social/linkprobe10_boot.glp /Users/udi/Grassroots/GLP/programs/social/linkprobe10

`runprobe` needs its mini-app's artefact beside it first:

    :artefact /Users/udi/Grassroots/GLP/programs/social/probeapp /Users/udi/Grassroots/GLP/programs/social/runprobe
