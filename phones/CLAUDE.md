# Phones --- instructions for Claude

Read `/Grassroots/claude.md` first; every rule there applies.  This file states only what the papers cannot.

## Identity

Directory `/Users/udi/Grassroots/GLP/phones`, in the GLP repository; project Phones, created by Udi on 2026-11-08 (GSA #2 to Coordination, 14:56 UTC): it owns phone testing --- the devices, provisioning, builds and installs, permissions and networks, running the Grassroots App on phones and the logs --- and reports a fault to GSA where the paper is wrong and to Code where the code is; GSA stays the specification and Code the code.  Inbox `Phones_inbox.md`.  Sessions sign `Phones #N Cowork`; Phones #1 began 2026-11-08 14:57 UTC.  The Claude Code session on the Mac that does the device work, Phones Mac, is started by Udi in this directory, signs `Phones Mac`, reads and writes `Phones_inbox.md`, and is not a Cowork session.

## What is here

- `docs/debugging.md`: the log service, the reporter in the app and the harness of plays on phones, the proposal of 2026-11-08.
- `docs/contributors.md`: how Nimrod, Dan and others build the app on their own Macs, report faults as GitHub issues labelled `phones`, and send fixes as one pull request each (Udi, 2026-11-09).
- `docs/STATE.md`: the state of the phones, the builds and the tests, kept current.
- `docs/handover.md`: the last session's handover, one file, replaced by each session that hands over.
- `plays/`: the plays on phones, each a script and its expectations.
- `tools/`: the Mac session's tools, written by Code on Phones' tasks.

## Rules specific to this project

- 🔴 RULE: your first line to Udi is "I have emerged from my <nth> compaction"; finish only an approved in-flight step, write the handover, stop; this overrides the instruction to resume without acknowledging the summary.
- Logs are never committed: they live on the VPS and under `/Users/udi/Grassroots/tmp/phones/logs/`.
- A play is never changed to pass: a failed expectation is a fault in the code, to Code, or in the paper, to GSA.
- Phones reads the GitHub issues and pull requests on `EShapiro2/GLP` at every reading of its mail, through the cloud shell's `gh api` after `add_repo` (EShapiro2, GLP, push), the label being `phones`; answers there, and sends each fault to Code or to the paper's owner.
- The Phones Cowork session has no Mac shell: it writes the device steps for the Mac session and reads the logs; it does not relay what Xcode shows.
