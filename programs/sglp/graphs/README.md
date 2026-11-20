# graphs

Runs of a canonical CVA platform under sGLP on a social graph one chooses: a friendship graph of a given family and parameters, and optionally a directed overlay of pointers along the friendships.  Yuval Lande, 2026-11-19.

The graph is built by the persons' own acts at the start of the run, so the platform reaches it by its contract's rules and the log shows each step.  Everything here is an sGLP program and scripts around sGLP's transformation and harness: it uses only what sGLP's paper defines (Section 3, Definitions "Person Process", "Screen, Seen" and "Simulation Program"; Section 4, "The transformation" and "The run"), and changes nothing in `transform/`, `cva/` or the engine.

## A run

```bash
python3 -I programs/sglp/graphs/gen.py regular --n 50 --d 4 --seed 3 --points out:2 > reg50.graph
bash programs/sglp/graphs/run.sh Overlay reg50.graph '2 days' 1 reg50.log
```

`run.sh <platform> <graph> <until> <seed> <log> [<source>]` composes the source (`compose.py`), transforms it against the platform's interface (`transform.sh --interface`), runs it (`cva/run.sh`) and checks the log against the graph (`check.py`).  `<platform>` is a record under `cascade/` or a directory holding `platform.glp` and `interface.glp`.  With no `<source>`, the persons only build the graph.  Beside the log it keeps the graph (`.graph`), the composed source (`_sglp.glp`) and the population's draw (`.draw`).  `check.py` prints:

```
graph reg50.graph: n 50, 100 friendships, 100 pointers
friendships: 100 of 100 at the end, first all at 89420.51 s
pointers: 100 of 100 at the end, first all at 98121.90 s
beyond the graph: 0 friendships, 0 pointers
proposals: 363, 363 of them the graph's steps, 13 of those repeated; 13 refused
```

## The graph

`gen.py <family> --n N [parameters] --seed S [--points SPEC]` prints a graph.  The draws are Python's `random.Random(seed)`, so a graph depends only on its family, parameters and seed.

| Family | Parameters | Graph |
|---|---|---|
| `complete` | | every pair |
| `ring` | `--k K` | each agent and the K nearest on each side |
| `grid` | `--rows R --cols C [--torus]` | an R x C lattice |
| `er` | `--p P` | Erdős–Rényi G(n, p) |
| `regular` | `--d D` | a uniformly random D-regular graph, an expander with high probability for D >= 3 |
| `ws` | `--k K --beta B` | Watts–Strogatz small world |
| `ba` | `--m M` | Barabási–Albert preferential attachment |
| `sbm` | `--sizes A,B,... --p-in P --p-out Q` | a stochastic block model |
| `file` | `--edges PATH` | the edges of a file, one `a b` per line |

`--points` adds the overlay: `all` (each agent points to every friend), `out:D` (to D friends drawn uniformly) or `file:PATH` (the pointers of a file, each along a friendship).

The file: `#` lines with the family, the parameters, the seed and the graph's statistics (degrees, components, diameter, clustering); then `n N`; then `friend a b` (a < b) and `points a b` (a points to b), agents 1..N.  Agent a is keyed `p<a>`, zero-padded to the width of N, as `cva/run.sh` keys it.

## How the graph is built

`compose.py` prints each person's script, `graph_script(Key, Steps)`, each step `step(Proposal, Done, Needs)`: the act, the atom of the person's view that shows it done, and the atoms the act needs first.

1. `establish`, done when the view shows `identity`.
2. `acquaint` each friend of a higher number, done at `knows(b)`, needing `identity`.
3. `befriend` each friend of a higher number, done at `friend(b)`, needing `knows(b)`.
4. `point_friend` each agent it points to, done at `points_to(b)`, needing `friend(b)`.
5. `await(friend(b))` each friend of a lower number, never proposed: done at `friend(b)`.

The lower of a pair proposes, so no pair proposes twice; the higher approves and awaits the friendship, so that `graph_next` says `done` only once all of the person's friendships and pointers are there.  An owner's behaviour that starts on `done` starts on a complete neighbourhood: without the awaits, a friend of the author in a run of Feeds finished its own steps before the author befriended it and tried 20 times to follow the author's feed through friends who did not yet follow it.  A person decides when its answer is due, from the last view its screen showed (`seen/1`): it proposes the first step not done, not in flight and whose needs the view shows, and only while the view shows none of its proposals in flight.  An agent with an attempt in flight queues the next one and its view shows nothing of it until it starts; proposing then, a complete graph of 6 proposed 17 steps twice.  An `acquaint` is refused while the other has not established; the person proposes it again at its next wake.  So a step is proposed again only after a refusal, and no state grows with repeated acts.

`compose.py` checks the platform's interface and refuses, naming what is missing, a platform with no `befriend` or `friend` (Coins), and an overlay on a platform with no `point_friend` or `points_to` (Friendship).  The library needs the types and alternatives of every platform extending Friendship (listed at the head of `library.glp`).

## With one's own profiles

An owner's sGLP source over a canonical platform, in the term form `transform.sh --interface` reads, calls two procedures of `library.glp`:

- `graph_next(Me?, L?, Next)`: what Me does next, from what its screen has shown, `seen(L)`: `pick(Proposal)`, `wait` (each step left is in flight or waits on another agent) or `done`;
- `graph_asked(Asking?, Me?, YesNo)`: whether an approval question asks of an acquaintance or a friendship of the graph, or of Me's own attempt.

and two for the owner's own steps and agents:

- `graph_run(Steps?, L?, Next)`: `graph_next` for a script the owner gives, a list of `step(Proposal, Done, Needs)`, with the same rules;
- `graph_agents(Keys)`: the agents' keys in their order, agent 1's first, of type `GraphKeys`, a list of at least one key.

`example_sglp.glp` is such a source: its person wakes an hour on average, proposes `graph_next`'s pick, waits an hour on `wait`, and on `done` introduces two of its friends to each other once a day; it approves the graph's questions within an hour and others within a day.

```bash
bash programs/sglp/graphs/run.sh Friendship ring6.graph '3 days' 1 ex.log programs/sglp/graphs/example_sglp.glp
```

The source's run declaration names the graph's number of agents, and `compose.py` refuses another; or it names `graph`, `run(graph, [...], C, U, Seed)`, which `compose.py` prints as the graph's number, so that one source runs on graphs of any size.  A census may be used if its keys are the graph's, `p1` ... `pN`.  `compose.py --graph G --interface I --source S` prints the composed source alone.

## Measured

On an Apple M5, on the cascade records at GLP `912c76b9c`, seed 1; "built at" is the simulated time at which the last friendship or pointer appears, and the wall-clock is the run's alone (`cva/run.sh`), after a transformation of about 10 s:

| Platform | Graph | Horizon | Built at | Wall-clock |
|---|---|---|---|---|
| Friendship | ring of 4 | 1 day | 8.2 h | 3 s |
| Friendship | complete graph of 6 | 2 days | 23.3 h | 5 s |
| Overlay | ring of 4, a pointer each | 1 day | 10.3 h | 3 s |
| Overlay | 4-regular of 50, 2 pointers each | 2 days | 27.3 h | 22 s |

## Limits

1. `graph_next` takes the graph as a target: a friendship of the graph that ends later is proposed again.
2. Labelled overlays (`labeled_point_friend`, Labelled Overlay and Feeds) are not yet a layer; `LAYER_NEEDS` in `compose.py` and a step in `scripts` add one.
3. The builder's profiles, `graph_builder` and `graph_approver`, are in `graph_profiles`, apart from `graph_library`, and `compose.py` adds them only to a run with no owner's source: a profile in no dimension of the run declaration makes `transform.sh --interface` print no population ("missing population"), though sGLP's README, "What the transformation prints", item 5, says it is no fault.

## Tests

`bash programs/sglp/graphs/test_graphs.sh`, about a minute: each family's graphs, `compose.py`'s scripts and refusals, the library and the example loading as GLP, and runs on Friendship and Overlay.  It is test (xxi) of `programs/sglp/test_sglp.sh`, Section SGLP of the suite.
