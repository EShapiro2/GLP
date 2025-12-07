# The Art of Grassroots Logic Programming - Example Programs

This repository contains example programs for the textbook "The Art of Grassroots Logic Programming" (AoGLP), demonstrating GLP programming techniques from basic stream operations to secure multiagent protocols.

## Source Priority

Programs are drawn from these sources in order of priority:
1. **GLP Papers** - Authoritative source, used exactly as written
2. **FCP/CP Book** - Converted to GLP only when not available in GLP papers
3. **Test Files** - Standard examples cleaned and documented

## Structure

Each program file includes:
- The GLP program with full comments
- Inline `reduce` clauses for metainterpreter support (until module system exists)

## Directory Organization

### Core Programming

#### streams/ - Stream Processing
- `producer_consumer/` - Basic producer-consumer pattern
- `observation/` - Stream observation without consumption

#### merge/ - Stream Merging
- `binary/` - Fair binary stream merger (the quintessential GLP program)
- `dynamic/` - Dynamic stream merger with runtime stream addition
- `mwm/` - Multiway merge
- `channels/` - Channel-based merging

#### distribution/ - Stream Distribution
- `ground/` - Ground-stream distribution to multiple consumers
- `cooperative/` - Cooperative producers with handover

#### monitors/ - Concurrent Monitors
- `basic/` - Stateful service with add/subtract/value requests
- `observer/` - Type-aware observer, 4-actor play

#### protocols/ - Communication Protocols
- `network_switch/` - 3-way network message routing
- `bounded_buffer/` - Bounded buffer implementations

### Metaprogramming

#### meta/ - Metainterpreters
- `plain/` - Plain GLP metainterpreter
- `failsafe/` - Reports failures instead of aborting
- `control/` - Suspend/resume/abort via control stream
- `termination/` - Termination detection via short-circuit
- `snapshot/` - Collects resolvent snapshot on abort
- `tracing/` - Produces execution trace with timestamps
- `debugging/` - Full runtime control with dump collection

### Data Structures

#### lists/ - List Operations
- `append/`, `reverse/`, `copy/` - Basic operations
- `member/`, `length/`, `nth/` - Query operations

#### sorting/ - Sorting Algorithms
- `quicksort/` - Quicksort with partition
- `insertion_sort/` - Insertion sort
- `bubble_sort/` - Bubble sort
- `merge_sort/` - Merge sort

#### arithmetic/ - Arithmetic Programs
- `factorial/`, `fibonacci/` - Recursive functions
- `primes/` - Sieve of Eratosthenes
- `sum_list/` - List summation

#### logic_gates/ - Hardware Simulation
- `gates/` - AND/OR gate simulation on bit streams

#### puzzles/ - Classic Puzzles
- `hanoi/` - Tower of Hanoi

### Object-Oriented

#### objects/ - Object-Oriented Patterns
- `basics/` - Counter, queue manager, encapsulation

### Grassroots Protocols

#### social_graph/ - Social Graph Foundation
- Core social graph data structures and operations

#### networking/ - Social Networking
- Network protocols, message routing, friend channels

#### security/ - Security Protocols
- Authentication, authorization, secure channels

### Library (lib/)

#### lib/ - Utility Programs
- `channels/` - Channel operations, relay
- `streams/` - Tag stream, inject
- `lookup/` - List lookup utilities
- `broadcast/` - Broadcast to multiple recipients

## Usage

Each `.glp` file is self-contained with its `reduce` clauses for metainterpreter execution. To run a program under the plain metainterpreter:
```glp
run(merge([1,2,3], [a,b,c], Result)).
```

## Directory Correspondence with Book

After reorganization, the correspondence between book chapters and code directories:

```
Book Chapter (AofGLP repo)       Code Directory (GLP repo)
───────────────────────────────────────────────────────────
streams.tex                 →    streams/, merge/, distribution/
buffered_communication.tex  →    protocols/bounded_buffer/
monitors.tex                →    monitors/
lists.tex                   →    lists/
sorting.tex                 →    sorting/
arithmetic.tex              →    arithmetic/
objects.tex                 →    objects/
plain_meta.tex              →    meta/plain/
enhanced_meta.tex           →    meta/control/, meta/tracing/
debugging.tex               →    meta/debugging/
social_graph.tex            →    social_graph/
networking.tex              →    networking/
security.tex                →    security/
```

## References

- GLP Paper: "GLP: A Secure, Multiagent, Grassroots, Concurrent, Logic Programming Language"
- Concurrent Prolog / FCP Book: "Concurrent Prolog: Collected Papers" (MIT Press)
- Art of Prolog: Sterling & Shapiro, "The Art of Prolog" (MIT Press)

## Future Work

- Module system for automatic metainterpreter integration
- Additional examples from FCP book (converted to GLP)
- Test harness integration
