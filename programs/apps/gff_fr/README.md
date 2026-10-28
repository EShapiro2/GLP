# GFF-FR's app

The app of GFF-FR, Fault-Resilient Feeds and Followers, GFF's second contract (Udi, 2026-10-28; `programs/gff/GFF-FR.txt` and `GFF-FR.glp` on the branch GFF-chooser), which extends GFF: Redundant following (GFF-FR:1), Alice following Bob through two friends, and Redundant forwarding (GFF-FR:2), `redundant_forward`, Alice forwarding to Dave, through whom she also follows Bob, an item of Bob's feed she received from Carol and not from Dave, a sending.  It is the host of `../gff` on GFF-FR's platform: the same entry and network (`miniapp.glp`, the same file), GFF's scripted person with GFF-FR's scripts, and GFF-FR's play.

## The files

| File | What it holds |
|---|---|
| `platform.vglp` | the canonical CVA platform of GFF-FR, as `programs/vglp/cva/compiler` prints it, calling the app's chooser |
| `chooser.glp` | `choose/4`, GFF-FR's chooser: Following via Friend through two friends known to follow Bob not through Alice, when there are two; Updating as GFF's (`../gff/README.md`, "The chooser") |
| `miniapp.glp` | `miniapp/4`, the same file as `../gsg/miniapp.glp` |
| `person.glp` | the scripted person, `../gff/person.glp` with the roles `r_alice` to `r_eve` |
| `plays.glp` | `play_resilient/5`, the relay that holds a person's conversations, and the cues and watchers |
| `self.glp` | the platform's types written again, the person's steps, and the export |

## The play

From `glp_runtime/`:

```bash
printf '../programs/apps/gff_fr\n:limit 30000000\nplay_resilient(A, B, C, D, E).\n:quit\n' | bin/glpc
```

`play_resilient` (GFF #1 Cowork, 2026-10-28 10:27 UTC), five executions over five conversations, alice's with bob and with eve, and carol's with bob, with eve and with dave:

1. Alice befriends bob and eve and posts an item; bob introduces her to carol.
2. Told by bob's and eve's updates that each follows alice, carol presses Follow, and her machine follows alice through both (GFF-FR:1); carol introduces alice to dave, who follows her through carol.
3. Once dave holds the first item, eve's machine is held: a relay on each of her two conversations, each way, holds what it reads, so eve sends and receives nothing.  The play holds eve so, since a play has no way to stop an agent: a person who does not answer stops nothing, the machine acting by itself, and delaying every message to and from an agent is a delay the asynchronous network allows.
4. Once each relay has closed, alice posts a second item.  Her delivery to eve is a sending, which the relay holds and which holds alice in nothing.
5. Carol receives the second item from bob and not from eve, and forwards it to eve (GFF-FR:2), a sending, which the relay holds.
6. Once carol's screen shows the item sent to eve, the relays open: eve holds the second item from alice and along alice, bob and carol; dave holds each item once, along alice, bob and carol.

The run then suspends, and Section APPS holds the persons' screens.
