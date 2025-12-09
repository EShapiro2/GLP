# Art of GLP - Example Programs

Example programs for "The Art of Grassroots Logic Programming" organized by book chapter and section.

## Directory Structure

```
AofGLP/
├── constants/                  # Part I: Foundations
│   └── gates/                  # Logic gates (AND, OR, NOT, NAND)
├── streams/                    # Part II: Concurrent Programming
│   ├── producers_consumers/    # Producers, consumers, merge, distribution
│   ├── buffered_communication/ # Bounded buffers, flow control
│   └── objects_monitors/       # Objects, monitors, network switches
├── recursive/                  # Part II: Recursive Programming
│   ├── arithmetic_trees/       # Arithmetic, trees, hanoi
│   ├── list_processing/        # Lists, sorting algorithms
│   └── structure_processing/   # Symbolic computation (future)
├── meta/                       # Part II: Metaprogramming
│   ├── plain/                  # Plain and failsafe meta-interpreters
│   ├── enhanced/               # Control, tracing, snapshot, termination
│   └── debugging/              # Debugging meta-interpreter
├── multiagent/                 # Part III: Simulating Multiagent Systems
│   ├── social_graph/           # Social graph, attestation, security
│   ├── social_networks/        # DM, feeds, groups, replication
│   ├── interlaced_streams/     # Blocklace DAG (future)
│   ├── cryptocurrencies/       # (future)
│   └── constitutional_consensus/ # (future)
├── exercises/                  # Exercise solutions (mirrors above)
└── lib/                        # Utility predicates
```

## Source Priority

Programs are drawn from these sources in order of priority:
1. **GLP Papers** - Authoritative source, used exactly as written
2. **Art of Prolog** - Converted to GLP where applicable
3. **FCP/CP Book** - Converted to GLP only when not available elsewhere

## File Conventions

- `*.glp` - GLP source code
- `*.trace` - Execution traces
- `*.pl` - Original Prolog (not yet converted)

## Usage

Load a file in the GLP REPL:
```
GLP> load("streams/producers_consumers/fair_merge.glp").
GLP> run(merge([1,2,3], [a,b,c], Result)).
```

## References

- GLP Paper: "GLP: A Secure, Multiagent, Grassroots, Concurrent, Logic Programming Language"
- Art of Prolog: Sterling & Shapiro, "The Art of Prolog" (MIT Press)
- Concurrent Prolog / FCP Book: "Concurrent Prolog: Collected Papers" (MIT Press)
